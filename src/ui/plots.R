plots <- function() {
  shiny::div(
    fluidRow(
      column(3, uiOutput("year_option")),
      column(3, uiOutput("KP_option")),
      column(3, uiOutput("city_option")),
      # column(3, selectInput(inputId = "kp", label="Choose a key population", choices=output$KP_option)), #as.character(unique(proportion_data$KP))
      # column(3, selectInput(inputId = "year", label = "Choose one or more survey years", choices=as.character(output$year_option), multiple=TRUE, selected=as.character(max(output$year_option)))),
      # column(3, selectInput(inputId = "subnat", label = "Choose one or more regions", multiple=TRUE, choices=as.character(output$city_option), selected=as.character(output$city_option[1:3]))),
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
        br(),
        tabsetPanel(
          tabPanel(title="Proportions",
              column(8, DTOutput("cascade_table_proportion"))
          ),
          tabPanel(title="Counts",
              column(8, DTOutput("data_upload_size2")),
              br(),
              column(8, DTOutput("cascade_table_count"))
          )
        )
      )
    )
  )
}