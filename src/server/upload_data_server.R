upload_data_server <- function(input, output) {
  
  #example_data <- read.csv("example_data.csv")
  example_data <- read.csv("~/Documents/GitHub/kpcascade/src/example_data_dev.csv", stringsAsFactors = FALSE) %>%
    arrange(Province)
  
  subnational_input_choices <- example_data %>%
    select(Province, District) %>%
    group_by(Province, District) %>%
    summarise() %>%
    ungroup %>%
    arrange(Province, District) %>%
    group_split(Province, keep=FALSE) %>%
    `names<-`(unique(example_data$Province)) %>%
    lapply(unlist) %>%
    lapply(function(x) {
      x %>%
        `names<-` (x)
    })
  
  output$template_download <- downloadHandler(
    filename = function() {
      paste("KP_cascade_template.csv", sep='')
    },
    content = function (con) {
      write.csv(example_data, con, na="")
    }
  )
  
  output$data_upload_size2 <- output$data_upload_size <- renderDT(
    datatable(example_data %>%
                select(-c(Method, Source)) %>%
                filter(Cascade.status == "Size Estimate"), 
              container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns=5:7, digits=0)
  )
  
  output$data_upload_prop <- renderDT(
    datatable(example_data %>%
                select(-c(Method, Source)) %>%
                filter(Cascade.status != "Size Estimate"), 
              container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t'))  %>%
      formatRound(columns=5:7, digits=3)
  )
  
  output$single_KP_option <- renderUI({
    selectInput(inputId = "single_kp", label="Choose key population", choices=as.character(unique(example_data$KP)), selected=unique(example_data$KP)[1])
  })
  output$multiple_year_option <- renderUI({
    selectInput(inputId = "multiple_year", label = "Choose survey year(s)", choices=unique(example_data$Year), multiple=TRUE, selected=example_data$Year[1])
  })
  output$multiple_KP_option <- renderUI({
    selectInput(inputId = "multiple_kp", label="Choose key population(s)", multiple=TRUE, choices=as.character(unique(example_data$KP)), selected=unique(example_data$KP)[1])
  })
  output$single_year_option <- renderUI({
    selectInput(inputId = "single_year", label = "Choose survey year", choices=sort(unique(example_data$Year)), selected=example_data$Year[1])
  })
  
  output$city_option <- renderUI({
    selectizeInput(inputId = "subnat", label = "Choose district(s)", multiple=TRUE, choices=subnational_input_choices, selected=as.character(unique(example_data$District)[1:3]))
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
                  select(-c(Method, Source)) %>%
                  filter(Cascade.status == "Size Estimate"), 
                container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t'))
    )
    
    output$data_upload_prop <- renderDT(
      datatable(new_data %>%
                  select(-c(Method, Source)) %>%
                  filter(Cascade.status != "Size Estimate"), 
                container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t'))
    )
    
    output$single_KP_option <- renderUI({
      selectInput(inputId = "single_kp", label="Choose key population", choices=as.character(unique(new_data$KP)))
    })
    output$multiple_year_option <- renderUI({
      selectInput(inputId = "multiple_year", label = "Choose survey year(s)", choices=unique(new_data$Year), multiple=TRUE, selected=max(new_data$Year))
    })
    output$multiple_KP_option <- renderUI({
      selectInput(inputId = "multiple_kp", label="Choose key population(s)", multiple=TRUE, choices=as.character(unique(new_data$KP)))
    })
    output$single_year_option <- renderUI({
      selectInput(inputId = "single_year", label = "Choose survey year", choices=unique(new_data$Year), selected=max(new_data$Year))
    })
    output$city_option <- renderUI({
      selectInput(inputId = "subnat", label = "Choose district(s)", multiple=TRUE, choices=as.character(unique(new_data$City.Region)), selected=as.character(unique(new_data$City.Region)[1:3]))
    })
    
    
    
    data_clean <- clean_data(new_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
  
  observeEvent(input$subnat, {
    print(input$subnat)
  })
  
  observeEvent(input$subnat, {
    print(input$multiple_year)
  })
  
  observeEvent(input$resetToExampleData, {
    
    output$data_upload_size <- renderDT(
      datatable(example_data %>%
                  select(-c(Method, Source)) %>%
                  filter(Cascade.status == "Size Estimate"), 
                container = wide3_nohead(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
        formatRound(columns=5:7, digits=0)
    )
    
    output$data_upload_prop <- renderDT(
      datatable(example_data %>%
                  select(-c(Method, Source)) %>%
                  filter(Cascade.status != "Size Estimate"), 
                container = wide3_90(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
        formatRound(columns = 5:7, digits=3)
    )
    
    data_clean <- clean_data(example_data)
    proportion_data <<- proportion_manip(data_clean)
    count_data <<- count_manip(proportion_data)
  })
}