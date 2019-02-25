navigationPanel <- function() {
  navlistPanel(HTML("<b>KPCascade</b>"),
    widths = c(2, 10), well = FALSE,
    tabPanel("Intro", introduction()),
    tabPanel("Upload data", upload_data()),
    tabPanel("Visualise data", plots()),
    tabPanel("About", about())
  )
}
                                
