get_compactness <- function(shape, E, Fy) {
  result <- list()
  
  # Coerce all fields safely to numeric
  b <- suppressWarnings(as.numeric(shape$b))
  d <- suppressWarnings(as.numeric(shape$d))
  tf <- suppressWarnings(as.numeric(shape$tf))
  tw <- suppressWarnings(as.numeric(shape$tw))
  t <- tf
  type <- if (!is.null(shape$Type)) as.character(shape$Type) else ""
  
  # Flange compactness (e.g., W-shapes): lambda_f = b / (2 * tf)
  if (!is.na(b) && !is.na(tf) && tf > 0) {
    lambda_f <- b / (2 * tf)
    lp_f <- 0.38 * sqrt(E / Fy)
    lr_f <- 1.0 * sqrt(E / Fy)

    result$flange_rule <- withMathJax(
      "Using 0.38 and 1.0 x \\( \\sqrt{E / Fᵧ} \\) per ANSI 360 Table B4.1b for angle leg compactness"
    )
    
    classification <- if (lambda_f <= lp_f) "Compact" else if (lambda_f <= lr_f) "Noncompact" else "Slender"
    result$flange <- sprintf("%s (λ = %.2f, compact if ≤ %.2f, slender if > %.2f)",
                             classification, lambda_f, lp_f, lr_f)
  } else {
    result$flange <- "Unknown (missing b or tf)"
  }
  
  # Web compactness (e.g., I-beams): lambda_w = (d - 2 * tf) / tw
  if (!is.na(d) && !is.na(tf) && !is.na(tw) && tw > 0) {
    h_web <- d - 2 * tf
    lambda_w <- h_web / tw
    lp_w <- 3.76 * sqrt(E / Fy)
    lr_w <- 5.70 * sqrt(E / Fy)
    
    result$web_rule <- withMathJax(
      "Using 3.76 and 5.70 × \\( \\sqrt{E / Fᵧ} \\) per ANSI 360 Table B4.1b for web compactness"
    )
    classification <- if (lambda_w <= lp_w) "Compact" else if (lambda_w <= lr_w) "Noncompact" else "Slender"
    result$web <- sprintf("%s (λ = %.2f, compact if ≤ %.2f, slender if > %.2f)",
                          classification, lambda_w, lp_w, lr_w)
  } else {
    result$web <- "Unknown (missing d, tf, or tw)"
  }
  
  # Angle leg compactness: check both d/tf and b/tf if L-shape
  if (grepl("^L", type) && !is.na(d) && !is.na(b) && !is.na(tf) && tf > 0) {
    lambda_d <- d / tf
    lambda_b <- b / tf
    limit_leg <- 0.56 * sqrt(E / Fy)
    leg_d <- if (lambda_d <= limit_leg) "Compact" else "Slender"
    leg_b <- if (lambda_b <= limit_leg) "Compact" else "Slender"
    result$leg_rule <- withMathJax(
      "Using 0.56 x \\( \\sqrt{E / Fᵧ} \\) per ANSI 360 Table B4.1b for angle leg compactness"
    )
  } else if (grepl("^L", type)) {
    result$leg <- "Unknown (missing d, b, or tf)"
  }
  
  return(result)
}