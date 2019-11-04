plots <- function() {
  div(style="margin-left:2%; margin-right: 2%",
      h2("Visualise data"),
      br(),
      p(style="font-size:16px", "Treatment cascades are available in two configurations - comparing the progress of a single population over time, or comparing multiple populations at a single point in time. If treatment cascades for the general population are uploaded, the latter configuration can assist with the visualisation of the treatment gap."),
      br(),
      fluidRow(
        splitLayout(align="center",
          # Option 1. (existing) Single pop, multiple time, multiple location
          actionButton(inputId = "single_pop", div(
                                                   div(style="display:inline-block",
                                                       icon("child", "fa-2x fa-fw"), br(),
                                                       # icon("clock", "fa-2x fa-fw"), icon("clock", "fa-2x fa-fw"), br(),
                                                       icon("time", "fa-2x fa-fw", lib="glyphicon"), icon("time", "fa-2x fa-fw", lib="glyphicon"), br(),
                                                       icon("home", "fa-2x fa-fw"), icon("home", "fa-2x fa-fw")
                                                   ),
                                                   div(style="display:inline-block",
                                                       p(style="padding-bottom:5px", "Single population", br()),
                                                       p(style="padding-bottom:5px", "Multiple years", br()),
                                                       p(style="margin-bottom:0; padding-bottom:5px", "Multiple regions")
                                                   )
          )
          ),
          actionButton(inputId = "multiple_pop", div(
                                                     div(style="display:inline-block",
                                                         icon("child", "fa-2x fa-fw"), icon("child", "fa-2x fa-fw"), br(),
                                                         icon("time", "fa-2x fa-fw", lib="glyphicon"), br(),
                                                         icon("home", "fa-2x fa-fw"), icon("home", "fa-2x fa-fw")
                                                     ),
                                                     div(style="display:inline-block",
                                                         p(style="padding-bottom:5px", "Multiple population", br()),
                                                         p(style="padding-bottom:5px", "Single year", br()),
                                                         p(style="margin-bottom:0; padding-bottom:5px", "Multiple regions")
                                                        )
                                                     )
          )
        )
      ),
      hr(),
      hidden(
        div(id="main_plot_body",
            div(id="select_inputs",
                fluidRow(
                  hidden(
                    div(id="single_pop_kp",
                        column(2, uiOutput("single_KP_option")),
                        column(3, uiOutput("subnat_option1")),
                        column(2, uiOutput("multiple_year_option"))
                    )
                  ),
                  hidden(
                    div(id="multiple_pop_kp",
                        column(2, uiOutput("multiple_KP_option")),
                        column(3, uiOutput("subnat_option2")),
                        column(2, uiOutput("single_year_option"))
                    )
                  ),
                  column(2, uiOutput("cascade_option"))
                ),
                fluidRow(
                  column(5, 
                         hidden(
                           div(id="custom_90s",
                               splitLayout(
                                 numericInput(inputId = "first90", label = "1st 90", value=95, min=0, max=100, step=1, width="100px"), 
                                 numericInput(inputId = "second90", label = "2nd 90", value=95, min=0, max=100, step=1, width="100px"), 
                                 numericInput(inputId = "third90", label = "3rd 90", value=95, min=0, max=100, step=1, width="100px")
                               ),
                               p("Note that the custom 90s are of the form 90-90-90, not 90-81-73 (i.e. PLHIV not used as the denominator throughout)")
                           )
                         )
                  )
                ),
            ),
            textOutput("test2"),
            div(style="font-size:0px", textOutput("viz_option")),
            tabsetPanel(
              tabPanel(title = HTML("<b style='font-size:18px'>Plots</b>"),
                       conditionalPanel("output.viz_option == 'single'",
                                        h3("Cascade by percentage"),
                                        #downloadButton("viz1_plot_download"),
                                        plotOutput("viz1_cascade_percent"),
                                        #plotlyOutput("viz1_cascade_percent"),
                                        h3("Cascade by size estimate"),
                                        h4(textOutput("cascade_count_warning")),
                                        plotOutput("cascade_count")
                       ),
                       conditionalPanel("output.viz_option == 'multiple'",
                                        h3("Cascade by percentage"),
                                        plotOutput("viz2_cascade_percent"),
                                        h3("Cascade by size estimate"),
                                        br(),
                                        h4("Size estimates only available with single population visualisation"),
                                        br(),
                                        br()
                       )
              ),
              tabPanel(title = HTML("<b style='font-size:18px'>Data</b>"),
                       br(),
                       tabsetPanel(
                         tabPanel(title=HTML("<b style='font-size:18px'>Cascade by proportion</b>"),
                                  conditionalPanel("output.viz_option == 'single'",
                                    #column(3, offset=4, downloadButton("viz1_download_proportion_cascade", label="Download proportion cascade")),
                                    column(10, DTOutput("viz1_cascade_table_proportion"))
                                  ),
                                  conditionalPanel("output.viz_option == 'multiple'",
                                    #column(3, offset=4, downloadButton("download_proportion_cascade", label="Download proportion cascade")),
                                    column(10, DTOutput("viz2_cascade_table_proportion"))
                                  )
                         ),
                         
                         tabPanel(title=HTML("<b style='font-size:18px'>Cascade by size estimate</b>"),
                                  conditionalPanel("output.viz_option == 'single'",
                                    br(),
                                    #column(3, offset=4, downloadButton("download_count_cascade", label="Download size estimate cascade")),
                                    column(10,DTOutput("viz1_cascade_table_count"))
                                  ),
                                  conditionalPanel("output.viz_option == 'multiple'",
                                    br(),
                                    h5("Size estimates only available with single population visualisation")
                                  )
                         )
                       )
              )
            )
        ) # end of main body div
      ) #end of hidden
  )
}