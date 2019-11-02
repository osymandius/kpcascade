upload_data_server <- function(input, output) {
  
  example_data <- read.csv("example_data.csv", stringsAsFactors = FALSE) %>%
    arrange(Province)
  
  data_clean <- clean_data(example_data)
  proportion_data <<- proportion_manip(data_clean)
  count_data <<- count_manip(proportion_data)
  
  #example_data <- read.csv("~/Documents/GitHub/kpcascade/src/example_data_dev.csv", stringsAsFactors = FALSE) %>%
  #  arrange(Province)
  
  # subnational_input_choices <- example_data %>%
  #   select(Province, District) %>%
  #   group_by(Province, District) %>%
  #   summarise() %>%
  #   ungroup %>%
  #   arrange(Province, District) %>%
  #   group_split(Province, keep=FALSE) %>%
  #   `names<-`(unique(example_data$Province)) %>%
  #   lapply(unlist) %>%
  #   lapply(function(x) {
  #     x %>%
  #       `names<-` (x)
  #   })
  
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
    selectizeInput(inputId = "single_kp", label="1) Key population", choices=c("Choose KP" = "", as.character(unique(example_data$KP))), selected=NULL)
  })
  
  output$multiple_year_option <- renderUI({
    # selected <- example_data$Year[1]
    # if(is.null(selected)) selected <- unique(example_data$Year)[1]
    selectizeInput(inputId = "multiple_year", label = "3) Survey year(s)", choices=NULL, multiple=TRUE, selected=NULL)
  })
  
  output$multiple_KP_option <- renderUI({
    selectInput(inputId = "multiple_kp", label="1) Key population(s)", multiple=TRUE, choices=as.character(unique(example_data$KP)), selected=unique(example_data$KP)[1])
  })
  
  output$single_year_option <- renderUI({
    selectInput(inputId = "single_year", label = "3) Survey year", choices=NULL, selected=NULL)
  })
  
  output$subnat_option1 <- renderUI({
    selectizeInput(inputId = "subnat1", label = "2) District(s)", multiple=TRUE, choices=NULL, selected=NULL)
  })
  
  output$subnat_option2 <- renderUI({
    selectizeInput(inputId = "subnat2", label = "2) District(s)", multiple=TRUE, choices=NULL, selected=NULL)
  })
  
  ## Updating input choices

  observeEvent(input$single_kp, {

    year_input_choices <- proportion_data %>%
      filter(KP %in% input$single_kp) %>%
      .$year %>%
      unique %>%
      as.numeric %>%
      sort(decreasing = TRUE)
    
    subnational_input_choices <- proportion_data %>%
      filter(KP %in% input$single_kp) %>%
      select(province, district) %>%
      group_by(province, district) %>%
      summarise() %>%
      ungroup %>%
      arrange(province, district) %>%
      group_split(province, keep=FALSE) %>%
      `names<-`(unique(proportion_data %>% filter(KP %in% input$single_kp) %>% .$province)) %>%
      lapply(unlist) %>%
      lapply(function(x) {
        x %>%
          `names<-` (x)
      })

    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "multiple_year", choices = year_input_choices, selected = NULL)
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "subnat1", choices = c("Choose district" = "", subnational_input_choices), selected = NULL)

  })
  
  observeEvent(input$subnat1, {
    
    year_input_choices <- proportion_data %>%
      filter(KP %in% input$single_kp, district %in% input$subnat1) %>%
      .$year %>%
      unique %>%
      as.numeric %>%
      sort(decreasing = TRUE)
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "multiple_year", choices = year_input_choices, selected = year_input_choices)
    
  })
  
####### Multiple population view input updates
  
  observeEvent(input$multiple_kp, {
    
    year_input_choices <- proportion_data %>%
      group_by(year) %>% 
      filter(all(c(input$multiple_kp) %in% KP)) %>%
      ungroup %>%
      .$year %>%
      unique %>%
      as.numeric %>%
      sort(decreasing = TRUE)
    
    subnational_input_choices <- proportion_data %>%
      group_by(year) %>% 
      filter(all(c(input$multiple_kp) %in% KP)) %>%
      ungroup %>%
      select(province, district) %>%
      group_by(province, district) %>%
      summarise() %>%
      ungroup %>%
      arrange(province, district) %>%
      group_split(province, keep=FALSE) %>%
      `names<-`(unique(proportion_data %>%
                         group_by(year) %>% 
                         filter(all(c(input$multiple_kp) %in% KP)) %>%
                         ungroup %>%
                         .$province)) %>%
      lapply(unlist) %>%
      lapply(function(x) {
        x %>%
          `names<-` (x)
      })
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "single_year", choices = year_input_choices, selected = NULL)
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "subnat2", choices = subnational_input_choices, selected = NULL)
    
  })
  
  
  
  observeEvent(input$subnat2, {
    
    year_input_choices <- proportion_data %>%
      filter(district %in% input$subnat2) %>%
      group_by(year) %>% 
      filter(all(c(input$multiple_kp) %in% KP)) %>%
      .$year %>%
      unique %>%
      as.numeric %>%
      sort(decreasing = TRUE)
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "single_year", choices = year_input_choices, selected = year_input_choices)
    
  })
  
#####
  
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