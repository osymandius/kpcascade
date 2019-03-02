plots <- function() {
  div(style="margin-left:5%; margin-right: 5%",
      h2("Visualise data"),
      br(),
      h3("Choose viz option title"),
      fluidRow(
        fluidRow(
          # Option 1. (existing) Single pop, multiple time, multiple location
          column(3, offset=2, actionButton(inputId = "single_pop", label="Single population")), #Find single person icon & add selected fill css/shinyjs
          # Option 2. Single year, Multiple pops, multiple location.
          column(3, offset=3, actionButton(inputId = "multiple_pop", label="Multiple populations")) #Find multi person icon & add selection fill css/shinyjs
        ),
        div(id="viz_examples", style="align-items: center", 
            fluidRow(
              column(5, "Choose this to viz 1 pop in mutliple cities over multiple years"),
              column(5, offset=1, "Choose this to viz multiple pops in multiple cities in 1 year")            
            ),
            fluidRow(
              column(5, img(style="width:100%", src="placeholder.png")),
              column(5, offset=1, img(style="width:100%", src="placeholder.png"))
            )
        ),
        actionButton(inputId = "toggle_viz_examples", label="Toggle example viz")
      ),
      hr(),
      hidden(
        div(id="main_plot_body",
            fluidRow(
              div(id="select_inputs",
                hidden(
                  div(id="single_pop_inputs",
                      column(3, uiOutput("single_KP_option")),
                      column(3, uiOutput("multiple_year_option"))
                  )
                ),
                hidden(
                  div(id="multiple_pop_inputs",
                      column(3, uiOutput("multiple_KP_option")),
                      column(3, uiOutput("single_year_option"))
                  )
                ),
                column(3, uiOutput("city_option")),
                column(3, selectInput(inputId = "cascade", label = "Choose cascade type", choices=c("90-90-90", "90-81-72")))
              )
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
        ) # end of main body div
      ) #end of hidden
  )
}