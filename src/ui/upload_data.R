upload_data <- function() {
  shiny::div(
    fluidRow(
      column(3, downloadButton(outputId = "template_download", label = "Download template")),
      column(3, fileInput('data_input', 'Choose file to upload', accept = '.csv')),
      column(3, actionButton("resetToExampleData", label = "Reset to default data"))
    ),
    column(8, DTOutput(outputId = "data_upload_size")),
    br(),
    column(8, DTOutput(outputId = "data_upload_prop"))
  )
}