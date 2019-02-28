introduction <- function() {
  fluidRow(
    div(class="title",
        column(2, img(class="sanac", src='SANAC.jpg')),
        column(2, h1("KPCascade")),
        column(2, img(class="ucsf", src="UCSF.jpg"))
    ),
  fluidRow(
    div(class="intro_text",
      column(8, offset=2, HTML("
                        <p style='text-align:center; font-size: 20px'>KPCascade assists with the visualisation of HIV treatment cascades in key populations.</p>
                      ")
      )
    )
  )
  )
}