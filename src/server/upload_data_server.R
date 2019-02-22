upload_data_server <- function(input, output) {
    output$template_download <- downloadHandler(
    filename = function() {
      paste("KP_cascade_template.csv", sep='')
    },
    content = function (con) {
      write.csv(example_data, con, na="")
    }
  )
  
  output$data_upload <- renderDataTable({
    if(is.null(input$data_input))
      return(example_data)
    read.csv(input$data_input$datapath, header = TRUE, sep = ",")
  })
}