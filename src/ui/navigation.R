navigationPanel <- function() {
  navlistPanel(
    widths = c(2, 10), well = FALSE,
    # tabPanel("Introduction", introduction()),
    tabPanel("Upload data", upload_data()),
    tabPanel("Plots", plots())
  )
}
                                
