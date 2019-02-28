upload_data_server <- function(input, output) {
  
  example_data <- read.csv("example_data.csv")
  
  output$template_download <- downloadHandler(
    filename = function() {
      paste("KP_cascade_template.csv", sep='')
    },
    content = function (con) {
      write.csv(example_data, con, na="", row.names = FALSE)
    }
  )
  
  # example_data <- read.csv("~/Documents/GitHub/kpcascade/src/example_data.csv")
  
  output$data_upload_size2 <- output$data_upload_size <- renderDT(
    datatable(example_data %>%
                filter(Cascade.status == "Size Estimate"), 
              container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns=5:7, digits=0)
  )
  
  output$data_upload_prop <- renderDT(
    datatable(example_data %>%
                filter(Cascade.status != "Size Estimate"), 
              container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t'))  %>%
      formatRound(columns=5:7, digits=3)
  )
  
  output$KP_option <- renderUI({
    selectInput(inputId = "kp", label="Choose key population(s)", choices=as.character(unique(example_data$KP)))
  })
  output$year_option <- renderUI({
    selectInput(inputId = "year", label = "Choose survey year(s)", choices=unique(example_data$Year), multiple=TRUE, selected=max(example_data$Year))
  })
  output$city_option <- renderUI({
    selectInput(inputId = "subnat", label = "Choose region(s)", multiple=TRUE, choices=as.character(unique(example_data$City.Region)), selected=as.character(unique(example_data$City.Region)[1:3]))
  })
  
  data_clean <- clean_data(example_data)
  proportion_data <<- proportion_manip(data_clean)
  count_data <<- count_manip(proportion_data)
  
  
  observeEvent(input$data_input, {
    inFile <- input$data_input
    
    req(inFile) 
    new_data <<- read.csv(inFile$datapath, header = TRUE, sep = ",")
    output$data_upload_size <- renderDT(
      datatable(new_data %>%
                  filter(Cascade.status == "Size Estimate"), 
                container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t'))
    )
    
    output$data_upload_prop <- renderDT(
      datatable(new_data %>%
                  filter(Cascade.status != "Size Estimate"), 
                container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t'))
    )
    
    output$KP_option <- renderUI({
      selectInput(inputId = "kp", label="Choose key population(s)", choices=as.character(unique(new_data$KP)))
    })
    output$year_option <- renderUI({
      selectInput(inputId = "year", label = "Choose survey year(s)", choices=as.character(unique(new_data$Year)), multiple=TRUE, selected=as.character(max(new_data$Year)))
    })
    output$city_option <- renderUI({
      selectInput(inputId = "subnat", label = "Choose region(s)", multiple=TRUE, choices=as.character(unique(new_data$City.Region)), selected=as.character(unique(new_data$City.Region)[1:3]))
    })
    
    data_clean <- clean_data(new_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
  
  observeEvent(input$resetToExampleData, {
    
    output$data_upload_size <- renderDT(
      datatable(example_data %>%
                  filter(Cascade.status == "Size Estimate"), 
                container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
        formatRound(columns=5:7, digits=0)
    )
    
    output$data_upload_prop <- renderDT(
      datatable(example_data %>%
                  filter(Cascade.status != "Size Estimate"), 
                container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
        formatRound(columns = 5:7, digits=3)
    )
    
    data_clean <- clean_data(example_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
}