plots <- function() {
  div(style="margin-left:10%; margin-right: 10%",
    h2("Visualise data"),
    br(),
    fluidRow(
      column(3, uiOutput("KP_option")),
      column(3, uiOutput("year_option")),
      column(3, uiOutput("city_option")),
      column(3, selectInput(inputId = "cascade", label = "Choose cascade type", choices=c("90-90-90", "90-81-72")))
    ),
    tabsetPanel(              
      tabPanel(title = HTML("<b style='font-size:18px'>Plots</b>"),
               h3("Cascade by percentage"),
               plotOutput("cascade_percent"),
               h3("Cascade by size estimate"),
               h4(textOutput("cascade_count_warning")),
               plotOutput("cascade_count")
      ),
      tabPanel(title = HTML("<b style='font-size:18px'>Data</b>"),
        br(),
        tabsetPanel(
          tabPanel(title=HTML("<b style='font-size:18px'>Proportions</b>"),
              column(3, offset=4, downloadButton("download_proportion_cascade", label="Download proportion cascade")),
              column(10, DTOutput("cascade_table_proportion"))
          ),
          tabPanel(title=HTML("<b style='font-size:18px'>Counts</b>"),
              column(3, offset=4, downloadButton("download_count_cascade", label="Download size estimate cascade")),
              column(10, offset=1, DTOutput("data_upload_size2")),
              br(),
              column(10, offset=1, DTOutput("cascade_table_count"))
          )
        )
      )
    )
  )
}