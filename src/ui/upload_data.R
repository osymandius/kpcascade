upload_data <- function() {
  shiny::div(
    fluidRow(
      column(3, downloadButton(outputId = "template_download", label = "Download template")),
      column(3, fileInput('data_input', 'Choose file to upload', accept = '.csv'))
    ),
    dataTableOutput(outputId = "data_upload")
  )
}