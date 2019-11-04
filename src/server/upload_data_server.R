upload_data_server <- function(input, output) {
  
  example_data <- read.csv("example_data.csv", stringsAsFactors = FALSE) %>%
    arrange(Province)
  
  data_clean <- clean_data(example_data)
  proportion_data <<- proportion_manip(data_clean)
  count_data <<- count_manip(proportion_data)
  
  #example_data <- read.csv("~/Documents/GitHub/kpcascade/src/example_data_dev.csv", stringsAsFactors = FALSE) %>%
  #  arrange(Province)
  
  
  output$template_download <- downloadHandler(
    filename = function() {
      paste("KPCascade Default Data.csv", sep='')
    },
    content = function (con) {
      write.csv(example_data, con, na="NA", row.names = FALSE)
    }
  )
  
  output$data_upload_size2 <- output$data_upload_size <- renderDT(
    datatable(example_data %>%
                select(-c(cas90_81_73, Method, Source)) %>%
                filter(Cascade.status == "Size Estimate"), 
              container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns=5:7, digits=0)
  )
  
  output$data_upload_prop <- renderDT(
    datatable(example_data %>%
                select(-c(cas90_81_73, Method, Source)) %>%
                filter(Cascade.status != "Size Estimate"), 
              container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t'))  %>%
      formatRound(columns=5:7, digits=3)
  )

  
#####
  
  observeEvent(input$data_input, {
    inFile <- input$data_input
    
    req(inFile) 
    new_data <<- read.csv(inFile$datapath, header = TRUE, sep = ",")
    output$data_upload_size <- renderDT(
      datatable(new_data %>%
                  select(-c(cas90_81_73, Method, Source)) %>%
                  filter(Cascade.status == "Size Estimate"), 
                container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t'))
    )
    
    output$data_upload_prop <- renderDT(
      datatable(new_data %>%
                  select(-c(cas90_81_73, Method, Source)) %>%
                  filter(Cascade.status != "Size Estimate"), 
                container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t'))
    )
    
    output$single_KP_option <- renderUI({
      selectInput(inputId = "single_kp", label="Key population", choices=as.character(unique(new_data$KP)))
    })
    output$multiple_year_option <- renderUI({
      selectInput(inputId = "multiple_year", label = "Survey year(s)", choices=unique(new_data$Year), multiple=TRUE, selected=max(new_data$Year))
    })
    output$multiple_KP_option <- renderUI({
      selectInput(inputId = "multiple_kp", label="Key population(s)", multiple=TRUE, choices=as.character(unique(new_data$KP)))
    })
    output$single_year_option <- renderUI({
      selectInput(inputId = "single_year", label = "Survey year", choices=unique(new_data$Year), selected=max(new_data$Year))
    })
    output$city_option <- renderUI({
      selectInput(inputId = "subnat", label = "District(s)", multiple=TRUE, choices=as.character(unique(new_data$City.Region)), selected=as.character(unique(new_data$City.Region)[1:3]))
    })
    
    
    
    data_clean <- clean_data(new_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
  
  observeEvent(input$resetToExampleData, {
    
    output$data_upload_size <- renderDT(
      datatable(example_data %>%
                  select(-c(cas90_81_73, Method, Source)) %>%
                  filter(Cascade.status == "Size Estimate"), 
                container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
        formatRound(columns=5:7, digits=0)
    )
    
    output$data_upload_prop <- renderDT(
      datatable(example_data %>%
                  select(-c(cas90_81_73, Method, Source)) %>%
                  filter(Cascade.status != "Size Estimate"), 
                container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
        formatRound(columns = 5:7, digits=3)
    )
    
    data_clean <- clean_data(example_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
  
  output$download_user_guide <- downloadHandler(
    filename <- function() {
      paste("KPCascade Shiny Guide", "pdf", sep=".")
    },
    
    content <- function(file) {
      file.copy("www/KPCascade Shiny Guide.pdf", file)
    },
    contentType = "application/pdf"
  )
}