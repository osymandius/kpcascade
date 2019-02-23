plots <- function() {
  shiny::div(
    fluidRow(
      column(3, selectInput(inputId = "kp", label="Choose a key population", choices="FSW")),
      column(3, selectInput(inputId = "year", label = "Choose one or more survey years", choices=c(2013, 2018), multiple=TRUE, selected=2018)),
      column(3, selectInput(inputId = "subnat", label = "Choose a region", choices=c("list", "of", "subnational", "regions", "here"))),
      column(3, selectInput(inputId = "cascade", label = "Choose cascade type", choices=c("90-90-90", "90-81-72")))
    ),
    tabsetPanel(              
      tabPanel(title = "Plots",
               h3("Cascade by percentage"),
               plotOutput("cascade_percent"),
               h3("Cascade by size estimate"),
               h4(textOutput("cascade_count_warning")),
               plotOutput("cascade_count")
      ),
      tabPanel(title = "Data",
               column(8, dataTableOutput("cascade_table"))
      )
    )
  )
}