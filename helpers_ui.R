formulasUsedUI <- function() {
  div(
    class = "result-block",
    h4("Formulas Used"),
    tags$ul(
      tags$li(withMathJax("\\( M_p = F_y \\cdot Z_x \\)")),
      tags$li(withMathJax("\\( M_y = F_y \\cdot S_x \\)")),
      tags$li(withMathJax("\\( L_p = 1.76 \\cdot r_y \\cdot \\sqrt{E / F_y} \\)")),
      tags$li(withMathJax(
        "\\( L_r = 1.95 \\cdot r_y \\cdot \\frac{E}{0.7 F_y} \\cdot \\sqrt{\\frac{J}{S_x h_0}} \\)"
      )),
      tags$li(withMathJax(
        "\\( M_n = M_p - (M_p - 0.7 F_y S_x) \\cdot \\frac{L_b - L_p}{L_r - L_p} \\)"
      )),
      tags$li(withMathJax("\\( P = \\frac{4 M}{L} \\)")),
      tags$li(withMathJax("\\( \\delta = \\frac{P L^3}{48 E I} \\)")),
      tags$li(withMathJax("\\( \\frac{h}{t} \\) vs. \\sqrt{E / F_y} \\)"))
    )
  )
}



