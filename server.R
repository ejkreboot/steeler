library(shiny)
library(shinythemes)
library(jsonlite)
source("compactness.R")

# Load steel shapes data
shapes <- jsonlite::read_json("steel_specs.json", simplifyVector = TRUE)

ui_section <- function(name) {
  div(class = "result-section", h4(name))
}

ui_result_item <- function(label, result, note = NULL, note_class = "result-note") {
  div(
    class = "result-item",
    span(class = "result-label", paste0(label, ": ")),
    span(class = "result-value", result),
    if (!is.null(note)) div(class = note_class, note)
  )
}

format_number <- function(x, digits = 0, unit = NULL) {
  formatted <- format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  if (!is.null(unit)) {
    paste(formatted, unit)
  } else {
    formatted
  }
}

build_moment_capacities <- function(Fy, Zx, Sx, E, ry, J, h0) {
  Mp <- Fy * Zx
  My <- Fy * Sx
  Lp <- 1.76 * ry * sqrt(E / Fy)
  
  Lr <- if (!is.na(J) && !is.na(h0) && !is.na(Sx) &&
            h0 > 0 && Sx > 0) {
    1.95 * ry * (E / (0.7 * Fy)) * sqrt(J / (Sx * h0))
  } else {
    NA
  }
  
  list(
    ui_section("Moment Capacities"),
    
    ui_result_item(
      "Plastic Moment (Mₚ)",
      sprintf("%s kip·in", format_number(Mp)),
      withMathJax("Mₚ = \\( F_y \\cdot Z_x \\)"),
      note_class = "result-note-formula"
    ),
    
    ui_result_item(
      "Elastic Moment (Mᵧ)",
      sprintf("%s kip·in", format_number(My)),
      withMathJax("Mᵧ = \\( F_y \\cdot S_x \\)"),
      note_class = "result-note-formula"
    ),
    
    ui_result_item(
      "Lp",
      sprintf("%.2f in", Lp),
      withMathJax("Lₚ = \\( 1.76 \\cdot r_y \\cdot \\sqrt{E / F_y} \\)"),
      note_class = "result-note-formula"
    ),
    
    if (!is.na(Lr)) ui_result_item(
      "Lr",
      sprintf("%.2f in", Lr),
      withMathJax("Lᵣ = \\( 1.95 \\cdot r_y \\cdot \\frac{E}{0.7 F_y} \\cdot \\sqrt{\\frac{J}{S_x h_0}} \\)"),
      note_class = "result-note-formula"
    )
  )
}

build_classification_summary <- function(selected, E, Fy) {
  classif <- get_compactness(as.list(selected), E, Fy)
  
  tagList(
    ui_section("Section Classification"),
    if (!is.null(classif$flange)) ui_result_item("Flange", classif$flange, classif$flange_rule),
    if (!is.null(classif$web))    ui_result_item("Web", classif$web, classif$web_rule),
    if (!is.null(classif$leg))    ui_result_item("Leg", classif$leg, classif$leg_rule),
    if (any(grepl("Noncompact|Slender", unlist(classif)))) {
      div(
        class = "result-warning",
        HTML("&#9888;&#65039; Warning: Section is not fully compact. Moment reduction per AISC 360 required.")
      )
    }
  )
}

build_max_loads_and_deflection <- function(Mp, My, Lp, Lr, E, Sx, Ix, ry, L, sf = 1.0) {
  if (is.na(L) || L <= 0) return(NULL)
  
  Lb <- L
  Mn <- Mp
  note <- "Plastic capacity used"
  
  interpolation_formula <- withMathJax(
    "\\( M_n = M_p - (M_p - 0.7 F_y S_x) \\cdot \\frac{L_b - L_p}{L_r - L_p} \\)"
  )
  
  if (!is.na(Lr) && Lb > Lp && Lb < Lr) {
    Mn <- My + (Mp - My) * (Lr - Lb) / (Lr - Lp)
    note <- tagList(
      "Interpolated per AISC F2-2",
      div(class = "result-note-formula", style="margin-left: 0px;", interpolation_formula)
    )
  } else if (!is.na(Lr) && Lb >= Lr) {
    Mn <- pi^2 * E * Sx / (Lb / ry)^2
    note <- "Elastic LTB governs"
  }
  
  P <- 4 * Mn / Lb
  delta_max <- ((P/1.67) * Lb^3) / (48 * E * Ix) # at ASD allowable limit
  
  tagList(
    ui_section("Max Loads and Deflection"),
    
    ui_result_item(
      "Moment (Mₙ)",
      format_number(Mn, 2, "kip·in"),
      note
    ),
    
    ui_result_item(
      "Nominal Midpoint Load Limit (Pₙ)",
      sprintf("%s kip·in / %s lb", format_number(P, 2), format_number(P * 1000, 0)),
      withMathJax("Pₙ = \\( \\frac{4 M}{L} \\)"),
      note_class = "result-note-formula"
    ),
    ui_result_item(
      "Allowable (ASD) Midpoint Load Limit (Pₙ/Ω, Ω=1.67 )",
      sprintf("%s kip·in / %s lb", format_number(P/1.67, 2), format_number(P/1.67 * 1000, 0)),
      withMathJax("P = \\( \\frac{4 M}{L \\times 1.67} \\)"),
      note_class = "result-note-formula"
    ),

    ui_result_item(
      "Deflection at allowable limit (δ)",
      format_number(delta_max, 3, "in"),
      withMathJax("\\( \\delta = \\frac{P L^3}{48 E I} \\)"),
      note_class = "result-note-formula"
    ),
    
    if (!is.na(sf) && sf > 1.0)
      tagList(
        ui_result_item(
          "Safety Factor Applied",
          sprintf("Ω = %.2f", sf)
        ),
        ui_result_item(
          "Reduced Load",
          sprintf("%s kip (%s lb)", format_number(P / sf, 2), format_number(P * 1000 / sf, 0))
        )
      )
  )
}


build_bearing_summary <- function(selected, bolt_d) {
  F_u <- 58000  # psi, for result in pounds instead of kips
  
  if (is.na(bolt_d)) return(NULL)
  
  tw_val <- suppressWarnings(as.numeric(selected$tw))
  tf_val <- suppressWarnings(as.numeric(selected$tf))
  
  bearing_ui <- tagList(list(ui_section("Bearing Strengths (bolt hole, where deformation is a design consideration)"),
  
  if (!is.na(tw_val)) {
    bearing_d <- 2.4 * bolt_d * tw_val * F_u
    tagList(
      ui_result_item(
        paste("Web Nominal Bearing Strength (Rₙ)"), 
        format_number(bearing_d, 0, "lb"),
        withMathJax("Using \\( 2.4 \\times t_w \\times \\text{bolt diameter} \\times F_u \\)")
      ),
      ui_result_item(
        paste("Web Allowable Bearing Strength (Rₙ/Ω, Ω=2.0)"), 
        format_number(bearing_d / 2, 0, "lb"),
      )
    )
  },
  
  if (!is.na(tf_val)) {
    bearing_tf <- 2.4 * bolt_d * tf_val * F_u
    tagList(
      ui_result_item(
        HTML("Flange Nominal Bearing Strength"),
        format_number(bearing_tf, 0, "lb"),
      ),
      ui_result_item(
        paste("Flange Allowable Bearing Strength"), 
        format_number(bearing_tf / 2, 0, "lb"),
      )
    )
  }))
  
  bearing_ui
}


server <- function(input, output, session) {
  
  observeEvent(input$type, {
    updateSelectInput(session, "name", choices = shapes$Name[shapes$Type == input$type])
  })
  
  observeEvent(input$name, {
    selected <- shapes[shapes$Name == input$name, ]
    if (nrow(selected) == 1) {
      updateNumericInput(session, "Zx", value = selected$Zx)
      updateNumericInput(session, "Sx", value = selected$Sx)
      updateNumericInput(session, "Ix", value = selected$Ix)
      updateNumericInput(session, "ry", value = selected$ry)
      updateNumericInput(session, "J", value = selected$J)
      
      # Estimate h0 based on shape type
      shape_type <- as.character(selected$Type)
      d <- suppressWarnings(as.numeric(selected$d))
      tf <- suppressWarnings(as.numeric(selected$tf))
      h0_est <- NA
      if (!is.na(d)) {
        if (startsWith(shape_type, "W") || startsWith(shape_type, "HSS")) {
          h0_est <- if (!is.na(tf)) d - tf else NA
        } else if (startsWith(shape_type, "C")) {
          h0_est <- d / 2
        }
      }
      if (!is.na(h0_est)) updateNumericInput(session, "h0", value = h0_est)
    }
  })
  
  output$summary <- renderUI({
    req(input$Fy, input$Zx, input$Sx, input$Ix, input$E, input$ry)
    
    Mp <- input$Fy * input$Zx
    My <- input$Fy * input$Sx
    Lp <- 1.76 * input$ry * sqrt(input$E / input$Fy)
    Lr <- if (!is.na(input$J) && !is.na(input$h0) && !is.na(input$Sx) &&
              input$h0 > 0 && input$Sx > 0) {
      1.95 * input$ry * (input$E / (0.7 * input$Fy)) * sqrt(input$J / (input$Sx * input$h0))
    } else {
      NA
    }
    
    selected <- shapes[shapes$Name == input$name, ]

    output_list <- list()
    output_list <- c(output_list, build_moment_capacities(
      input$Fy, input$Zx, input$Sx, input$E, input$ry, input$J, input$h0
    ))
    
    if (nrow(selected) == 1) 
      output_list <- c(output_list, build_classification_summary(selected, input$E, input$Fy))

    if (!is.na(input$L) && input$L > 0) {
      output_list <- c(output_list,build_max_loads_and_deflection(Mp, My, Lp, Lr, input$E, input$Sx, input$Ix, input$ry, input$L))
    }
    
    # Bearing strength section
    if(!is.na(input$bolt_d) && input$bolt_d > 0) {
      output_list <- c(output_list, build_bearing_summary(selected, input$bolt_d))
    }
    
    do.call(tagList, output_list)
  })
  
  output$shape_properties <- renderTable({
    row <- shapes[shapes$Name == input$name, ]
    if (nrow(row) == 0) return(NULL)  # No match yet
    data.frame(
        Property = names(row),
        Value = unname(unlist(row)),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", align = c("cc"))
}


