about <- function() {
  div(
    fluidRow(
      h2(style="margin-left: 10%", "About"),
      br(),
      div(style='font-size:16px; margin-left: 10%; margin-right:10%',
          HTML("<p style='font-size: 14px'>[Who owns it, how to cite it, whatever the other suggestions were at the end of the meeting, if any problems or suggestions for improvements email Oli]</p>")
      )
    )
  )
}