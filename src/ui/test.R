test <- fluidRow(
  downloadButton(outputId = "template_download", label = "Download template"),
  fileInput('data_input', 'Choose file to upload', accept = '.csv')
)