cascade_input_server <- function(input, output) {
  
  
  output$single_KP_option <- renderUI({
    selectizeInput(inputId = "single_kp", label="1) Key population", choices=c("Choose KP" = "", as.character(unique(proportion_data$KP))), selected=NULL)
  })
  
  output$multiple_year_option <- renderUI({
    selectizeInput(inputId = "multiple_year", label = "3) Survey year(s)", choices=NULL, multiple=TRUE, selected=NULL)
  })
  
  output$multiple_KP_option <- renderUI({
    selectizeInput(inputId = "multiple_kp", label="1) Key population(s)", multiple=TRUE, choices=as.character(unique(proportion_data$KP)), selected=NULL)
  })
  
  output$single_year_option <- renderUI({
    selectizeInput(inputId = "single_year", label = "3) Survey year", choices=NULL, selected=NULL)
  })
  
  output$subnat_option1 <- renderUI({
    selectizeInput(inputId = "subnat1", label = "2) District(s)", multiple=TRUE, choices=NULL, selected=NULL)
  })
  
  output$subnat_option2 <- renderUI({
    selectizeInput(inputId = "subnat2", label = "2) District(s)", multiple=TRUE, choices=NULL, selected=NULL)
  })
  
  output$cascade_option <- renderUI({
    selectizeInput(inputId = "cascade", label = "Cascade type", choices=c("90-90-90", "90-81-73", "Custom"), selected=NULL)
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
    
    logic_73 <- proportion_data %>%
      filter(KP %in% input$single_kp, district %in% input$subnat1) %>%
      .$cas90_81_73 %>%
      unique %>%
      isFALSE
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "multiple_year", choices = year_input_choices, selected = year_input_choices[1])
    
    if(logic_73) {
      updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "cascade", choices=c("90-90-90", "90-81-73", "Custom"), selected="90-90-90")
    } else {
      updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "cascade", choices=c("90-90-90", "90-81-73", "Custom"), selected="90-81-73")
    }
    
    
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
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "subnat2", choices = c("Choose district" = "", subnational_input_choices), selected = NULL)
    
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
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "single_year", choices = year_input_choices, selected = year_input_choices[1])
    
  })
  
}