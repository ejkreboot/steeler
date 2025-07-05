library(shiny)
library(shinythemes)
library(jsonlite)
source("helpers_ui.R")

shapes <- jsonlite::read_json("steel_specs.json", simplifyVector = TRUE)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
  class = "container",
  titlePanel("Steel Member Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Find section properties at:",
        a(
          "calcs.app steel section properties",
          href = "https://calcs.app/steel/section-properties",
          target = "_blank"
        )
      ),
      
      selectInput("type", "Section Type:", choices = unique(shapes$Type)),
      selectInput("name", "Shape:", choices = NULL),
      numericInput("L", "Span / Unbraced Length L (in):", value = NA),
      numericInput(
        "bolt_d",
        "Bolt Diameter (in, optional):",
        value = NA,
        min = 0.0,
        step = 0.0625
      ), 
      numericInput("E", "Elastic Modulus E (ksi):", value = 29000),
      numericInput("Fy", "Yield Stress Fy (ksi):", value = 36),
      numericInput("Zx", "Plastic Section Modulus Zx (in³):", value = NA),
      numericInput("Sx", "Elastic Section Modulus Sx (in³):", value = NA),
      numericInput("Ix", "Moment of Inertia Ix (in⁴):", value = NA),
      numericInput("ry", "Radius of Gyration ry (in):", value = NA),
      numericInput("h0", "Flange Centroid Distance h₀ (in):", value = NA),
      numericInput("J", "Torsional Constant J (in⁴):", value = NA)

    ),
    
    mainPanel(
      div(class = "result-block", h2("Results"), htmlOutput("summary")),
      h4(class = "result-section", "Selected Shape Properties"),
      tableOutput("shape_properties")
    )
  )
))