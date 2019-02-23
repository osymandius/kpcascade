upload_data_server <- function(input, output) {
    output$template_download <- downloadHandler(
    filename = function() {
      paste("KP_cascade_template.csv", sep='')
    },
    content = function (con) {
      write.csv(example_data, con, na="")
    }
  )
    example_data <- read.csv("example_data.csv")
    
    output$data_upload <- renderDataTable({
      example_data
    })
    
    data_clean <- clean_data(example_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
    
  
  observeEvent(input$data_input, {
    inFile <- input$data_input
    
    req(inFile) 
    new_data <<- read.csv(inFile$datapath, header = TRUE, sep = ",")
    output$data_upload <- renderDataTable({
      new_data
    })
    
    data_clean <- clean_data(new_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
  
  observeEvent(input$resetToExampleData, {
    
    output$data_upload <- renderDataTable({
      example_data
    })
    
    data_clean <- clean_data(example_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
}