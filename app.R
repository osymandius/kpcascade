library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)

example_data <- read_excel("example_data.xlsx", sheet = "Sheet1")

example_data$casState <- factor(example_data$casState, levels=unique(example_data$casState))
example_data$year <- as.character(example_data$year)

example_data %>%
  filter(casState != "sizeEst" & casState!= "prev") %>%
  filter(year==2018) %>%
  select(-c(ll_90, ul_90, point_72, gap_72, ll_72, ul_72)) %>%
  melt(id=c("casState", "KP", "year", "city")) %>%
  ggplot(aes(x=casState, y=value)) +
  geom_bar(position="stack", stat="identity", aes(fill=rev(variable))) +
  facet_wrap(~city)

test <- example_data %>%
  filter(casState != "sizeEst" & casState!= "prev") %>%
  melt(id=c("casState", "KP", "year", "city")) %>%
  filter(variable=="point_90" | variable == "gap_90") %>%
  ggplot(aes(x=casState, y=value, group=year)) +
    geom_bar(position="dodge", stat="identity", aes(fill=year)) +
    geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
    geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9), color="red")+
    geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9), color="red")+
    facet_wrap(~city)
 

#Works if filtered on single year to produce grey bars up to target
# ggplot(data=test, aes(x=casState, y=value, group=year)) +
#     geom_bar(stat="identity", fill="light grey") +
#     geom_bar(data=test %>% filter(variable=="point_90"), stat="identity", aes(fill="red")) +
#     facet_wrap(~city)
    


ui <- fluidPage(
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

server <- function(input, output) {
  
  output$cascade_plots <- renderPlot({
    cascade_plots <- switch(input$cascade,
                     "90-90-90" = example_data %>%
                                     filter(casState != "sizeEst" & casState!= "prev") %>%
                                     melt(id=c("casState", "KP", "year", "city")) %>%
                                     filter(variable=="point_90" | variable == "gap_90") %>%
                                     filter(year %in% input$year) %>%
                                     ggplot(aes(x=casState, y=value, group=year)) +
                                         geom_bar(position="dodge", stat="identity", aes(fill=year)) +
                                         geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
                                         geom_segment(aes(x=1.6, xend=2.4, y=0.9, yend=0.9), color="red")+
                                         geom_segment(aes(x=2.6, xend=3.5, y=0.9, yend=0.9), color="red")+
                                         facet_wrap(~city),
                     "90-81-72" = example_data %>%
                                     filter(casState != "sizeEst" & casState!= "prev") %>%
                                     melt(id=c("casState", "KP", "year", "city")) %>%
                                     filter(variable=="point_72" | variable == "gap_72") %>%
                                     filter(year %in% input$year) %>%
                                     ggplot(aes(x=casState, y=value, group=year)) +
                                         geom_bar(position="dodge", stat="identity", aes(fill=year)) +
                                         geom_segment(aes(x=0.5, xend=1.4, y=0.9, yend=0.9), color="red")+
                                         geom_segment(aes(x=1.6, xend=2.4, y=0.81, yend=0.81), color="red")+
                                         geom_segment(aes(x=2.6, xend=3.5, y=0.72, yend=0.72), color="red")+
                                         facet_wrap(~city)
                     )
      
      cascade_plots
    })
  
  output$cascade_table <- renderDataTable(example_data)
  
}

shinyApp(ui = ui, server = server)
