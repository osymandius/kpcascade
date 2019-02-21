library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)

# setwd("~/Documents/GitHub/kpcascade")

example_data <- read_excel("example_data.xlsx", sheet = "Sheet1")

data <- example_data %>%
  rename("casState" = "Cascade status", "KP" = "Key population", "year" = "Year", "city" = "City/Region", "point_90" = "Point Estimate", "ll_90" = "Lower Bound", "ul_90" = "Upper Bound") %>%
  mutate(casState = ifelse(casState == "Size Estimate", "sizeEst", 
                           ifelse(casState == "Prevalence", "prev", 
                                  ifelse(casState == "Aware of Status", "aware",
                                         ifelse(casState == "On ART", "onART", "Virally Suppressed")))))

data$casState <- factor(data$casState, levels=unique(data$casState))
data$year <- as.character(data$year)

data %<>%
  group_by(KP, year, city) %>%
  mutate(point_72 = as.numeric(ifelse(casState == "sizeEst", point_90[casState=="sizeEst"],
                                      ifelse(casState == "prev", point_90[casState=="prev"], 
                                             ifelse(casState == "aware", point_90[casState=="aware"], 
                                                    ifelse(casState == "onART", point_90[casState=="aware"]*point_90[casState=="onART"], 
                                                           ifelse(casState == "vSupp", point_90[casState=="aware"]*point_90[casState=="onART"]*point_90[casState=="vSupp"], ""))))))) %>%
  mutate(ll_72 = as.numeric(ifelse(casState == "sizeEst", ll_90[casState=="sizeEst"],
                                   ifelse(casState == "prev", ll_90[casState=="prev"], 
                                          ifelse(casState == "aware", ll_90[casState=="aware"], 
                                                 ifelse(casState == "onART", ll_90[casState=="aware"]*ll_90[casState=="onART"], 
                                                        ifelse(casState == "vSupp", ll_90[casState=="aware"]*ll_90[casState=="onART"]*ll_90[casState=="vSupp"], ""))))))) %>%
  mutate(ul_72 = as.numeric(ifelse(casState == "sizeEst", ul_90[casState=="sizeEst"],
                                   ifelse(casState == "prev", ul_90[casState=="prev"], 
                                          ifelse(casState == "aware", ul_90[casState=="aware"], 
                                                 ifelse(casState == "onART", ul_90[casState=="aware"]*ul_90[casState=="onART"], 
                                                        ifelse(casState == "vSupp", ul_90[casState=="aware"]*ul_90[casState=="onART"]*ul_90[casState=="vSupp"], "")))))))


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=2
      
    ),
    mainPanel(
      fluidRow(
        downloadButton(outputId = "template_download", label = "Download template"),
        fileInput('data_input', 'Choose file to upload', accept = '.csv')
      ),
      dataTableOutput(outputId = "data_upload"),
      fluidRow(
        column(4, selectInput(inputId = "kp", label="Choose a key population", choices="FSW")),
        column(4, selectInput(inputId = "year", label = "Choose one or more survey years", choices=c(2013, 2018), multiple=TRUE, selected=2018)),
        column(4, selectInput(inputId = "subnat", label = "Choose a region", choices=c("list", "of", "subnational", "regions", "here")))
      ),
      selectInput(inputId = "cascade", label = "Choose cascade type", choices=c("90-90-90", "90-81-72")),
      tabsetPanel(              
        tabPanel(title = "Plots",
                 plotOutput("cascade_plots")
        ),
        tabPanel(title = "Data",
                 column(8, dataTableOutput("cascade_table"))
                 
        )
      )
    )
  )
)


server <- function(input, output) {
  
  output$cascade_plots <- renderPlot({
    cascade_plots <- switch(input$cascade,
                            "90-90-90" = data %>%
                              filter(casState != "sizeEst" & casState!= "prev") %>%
                              melt(id=c("casState", "KP", "year", "city")) %>%
                              filter(variable=="point_90") %>%
                              filter(year %in% input$year) %>%
                              ggplot(aes(x=casState, y=value, group=year)) +
                              geom_col(position="dodge", stat="identity", aes(fill=year)) +
                              geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
                              geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9), color="red")+
                              geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9), color="red")+
                              facet_wrap(~city),
                            "90-81-72" = data %>%
                              filter(casState != "sizeEst" & casState!= "prev") %>%
                              melt(id=c("casState", "KP", "year", "city")) %>%
                              filter(variable=="point_72") %>%
                              filter(year %in% input$year) %>%
                              ggplot(aes(x=casState, y=value, group=year)) +
                              geom_col(position="dodge", stat="identity", aes(fill=year)) +
                              geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
                              geom_segment(aes(x=1.6, xend=2.4, y=0.81, yend=0.81), color="red")+
                              geom_segment(aes(x=2.6, xend=3.5, y=0.72, yend=0.72), color="red")+
                              facet_wrap(~city)
    )
    
    cascade_plots
  })
  
  output$cascade_table <- renderDataTable(data)
  
  output$template_download <- downloadHandler(
    filename = function() {
      paste("KP_cascade_template.csv", sep='')
    },
    content = function (con) {
      write.csv(example_data, con, na="")
    }
  )
  
  output$data_upload <- renderDataTable({
    req(input$data_input)
    read.csv(input$data_input$datapath, header = TRUE, sep = ",")
  })
}

shinyApp(ui = ui, server = server)