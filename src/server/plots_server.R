plots_server <- function(input, output) {
  
  ## Option control
  
  observeEvent(input$single_pop, {
    hide("viz_examples")
    show("main_plot_body")
    show("select_inputs")
    show("single_pop_inputs")
    hide("multiple_pop_inputs")
    output$viz_option <- renderText("single")
  })
  
  observeEvent(input$multiple_pop, {
    hide("viz_examples")
    show("main_plot_body")
    show("select_inputs")
    show("multiple_pop_inputs")
    hide("single_pop_inputs")
    output$viz_option <- renderText("multiple")
  })
  
  # observeEvent(input$toggle_viz_examples, {
  #   toggle("viz_examples")
  # })
  
  observeEvent(input$cascade, {
    if(input$cascade=="Custom") {
      show("custom_90s")
    } else {
      hide("custom_90s")
    }
  })
  
  # nineties <- c(90,90,90)
  
  # nineties <- c(95,80,40)
  
  nineties <- reactive(
    if(input$cascade=="90-90-90") {
      c(90,90,90)
    } else if(input$cascade=="90-81-73") {
      c(90,81,73)
    } else {
      c(input$first90,input$second90,input$third90)
    }
  )
  
  # Work out how many plots are going to be generated
  
  num_plots_viz1 <- 3
  num_plots_viz1 <- reactive(
    nrow(proportion_data %>%
                   filter(KP == input$single_kp) %>%
                   filter(year %in% input$multiple_year) %>%
                   filter(district %in% input$subnat) %>%
                   group_by(district) %>%
                   summarise()
    )
  )
  
  num_rows_viz1 <- reactive(ceiling(num_plots_viz1()/3))
  
  plot_height_viz1 <- reactive(400*num_rows_viz1())
  plot_width_viz1 <- reactive(ifelse(num_plots_viz1()==1, 350, ifelse(
    num_plots_viz1()==2, 700, 1050)
  )
  )
  
  num_plots_viz2 <- 3
  num_plots_viz2 <- reactive(
    nrow(proportion_data %>%
                   filter(KP %in% input$multiple_kp) %>%
                   filter(year == input$single_year) %>%
                   filter(district %in% input$subnat) %>%
                   group_by(district) %>%
                   summarise()
    )
  )
  
  num_rows_viz2 <- reactive(ceiling(num_plots_viz2()/3))
  
  plot_height_viz2 <- reactive(400*num_rows_viz2())
  plot_width_viz2 <- reactive(ifelse(num_plots_viz2()==1, 350, ifelse(
    num_plots_viz2()==2, 700, 1050)
  )
  )
  
  ### Number of repeats for target lines
  
  viz1_prop <- reactive(
    proportion_data %>%
      filter(casState != "sizeEst" & casState!= "prev") %>%
      filter(KP == input$single_kp) %>%
      filter(year %in% input$multiple_year) %>%
      filter(district %in% input$subnat)
  )
  
  viz2_prop <- reactive(
    proportion_data %>%
      filter(casState != "sizeEst" & casState!= "prev") %>%
      filter(KP %in% input$multiple_kp) %>%
      filter(year == input$single_year) %>%
      filter(district %in% input$subnat)
  )
  
  viz1_count <- reactive(
    count_data %>%
      filter(KP == input$single_kp) %>%
      filter(year %in% input$multiple_year) %>%
      filter(district %in% input$subnat)
  )
  
  #### Viz 1: Single KP, multi year, multi subnat
  # Viz1: Percent
  
  output$viz1_cascade_percent <- renderPlotly({
    ggplotly(
    if(input$cascade=="90-81-73"){
      viz1_prop() %>%
        ggplot(aes(x=casState, fill=year, group=year)) +
        geom_bar(aes(y=point_73), position=position_dodge(), stat="identity") +
        geom_errorbar(aes(ymin=ll_73, ymax=ul_73))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        xlab("")+
        ylab("")+
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
    } else {
      viz1_prop() %>%
        ggplot(aes(x=casState, fill=year, group=year)) +
        geom_bar(aes(y=point_90), position=position_dodge(), stat="identity")+
        geom_errorbar(aes(ymin=ll_90, ymax=ul_90))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        coord_cartesian(clip = 'off') +
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        xlab("")+
        ylab("")+
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
    })
  })
  # 
  # output$viz1_cascade_percent <- renderPlot({
  #   if(input$cascade=="90-81-73"){
  #     viz1_prop() %>%
  #       ggplot(aes(x=casState, group=year)) +
  #       geom_col(aes(y=point_73, fill=year), position="dodge", stat="identity") +
  #       geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
  #       geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
  #       geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
  #       scale_y_continuous(limits=c(0,1), labels=percent)+
  #       scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
  #       theme(axis.text = element_text(size=rel(1.2)),
  #             strip.text = element_text(size=rel(1.3)))+
  #       xlab("")+
  #       ylab("")+
  #       guides(color=FALSE)+
  #       facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
  #   } else {
  #     viz1_prop() %>%
  #       ggplot(aes(x=casState, group=year)) +
  #       geom_col(aes(y=point_90, fill=year), position="dodge", stat="identity") +
  #       geom_errorbar(aes(ymin=ll_90, ymax=ul_90), width=0.2, position=position_dodge(1))+
  #       geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
  #       geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
  #       geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz1_prop())/3), "%", sep="")), color="navy")+
  #       scale_y_continuous(limits=c(0,1), labels=percent)+
  #       scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
  #       coord_cartesian(clip = 'off') +
  #       theme(axis.text = element_text(size=rel(1.2)),
  #             strip.text = element_text(size=rel(1.3)))+
  #       xlab("")+
  #       ylab("")+
  #       guides(color=FALSE)+
  #       facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
  #   }
  # }, height=reactive(plot_height_viz1()), width=reactive(plot_width_viz1()))
  # 
  #Viz 1: Count
  output$cascade_count <- renderPlot({
    if(input$cascade=="90-81-73") {
      if(length(input$multiple_year)==1) {
        viz1_count() %>%
          ggplot(aes(x=casState, group=year)) +
          geom_col(aes(y=point_73, fill=year), position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
          geom_segment(data=count_data %>% filter(!is.na(count_target) & KP == input$single_kp & year %in% input$multiple_year & district %in% input$subnat), aes(x=x, xend=xend, y=count_target, yend=count_target), color="navy", linetype=5)+
          scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
          theme(axis.text = element_text(size=rel(1.15)),
                strip.text = element_text(size=rel(1.3)))+
          xlab("")+
          guides(color=FALSE)+
          ylab("Number of people")+
          facet_wrap(~district, scales="free", ncol=3)
      } else {
        viz1_count() %>%
          ggplot(aes(x=casState, group=year)) +
          geom_col(aes(y=point_73, fill=year), position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
          scale_x_discrete(labels=c("Size\nEstimate", "KPLHIV", "Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
          theme(axis.text = element_text(size=rel(1.15)),
                strip.text = element_text(size=rel(1.3)))+
          xlab("")+
          ylab("Number of people")+
          facet_wrap(~district, scales="free", ncol=3)
      }
    } else {
      NULL
    }
  }, height=reactive(plot_height_viz1()), width=reactive(plot_width_viz1()))
  
  
  ### Viz 2: Multi KP, single year, multi subnat
  #Percent
  
  output$viz2_cascade_percent <- renderPlot({
    if(input$cascade == "90-81-73") {
      viz2_prop() %>%
        ggplot(aes(x=casState, group=KP)) +
        geom_col(aes(y=point_73, fill=KP), position="dodge", stat="identity") +
        geom_errorbar(aes(ymin=ll_73, ymax=ul_73), width=0.2, position=position_dodge(1))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz2_prop())/3), "%", sep="")), color="navy", linetype=5)+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        xlab("")+
        ylab("")+
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
    } else {        
      viz2_prop() %>%
        ggplot(aes(x=casState, group=KP)) +
        geom_col(aes(y=point_90, fill=KP), position="dodge", stat="identity") +
        geom_errorbar(aes(ymin=ll_90, ymax=ul_90), width=0.2, position=position_dodge(1))+
        geom_segment(aes(x=0.5, xend=1.4, y=nineties()[1]/100, yend=nineties()[1]/100), color="navy", linetype=5)+
        geom_segment(aes(x=1.6, xend=2.4, y=nineties()[2]/100, yend=nineties()[2]/100), color="navy", linetype=5)+
        geom_segment(aes(x=2.6, xend=3.5, y=nineties()[3]/100, yend=nineties()[3]/100), color="navy", linetype=5)+
        geom_text(aes(y=1, label=paste(rep(nineties(), times=nrow(viz2_prop())/3), "%", sep="")), color="navy", linetype=5)+
        scale_y_continuous(limits=c(0,1), labels=percent)+
        scale_x_discrete(labels=c("Aware of\nstatus", "On ART", "Virally\nsuppressed"))+
        theme(axis.text = element_text(size=rel(1.2)),
              strip.text = element_text(size=rel(1.3)))+
        xlab("")+
        ylab("")+
        guides(color=FALSE)+
        facet_rep_wrap(~district, ncol=3, repeat.tick.labels = TRUE)
    }
  }, height=reactive(plot_height_viz2()), width=reactive(plot_width_viz2()))

  
  output$cascade_count_warning <- renderText({
    if(input$cascade != "90-81-73") {
      "Size estimate visualisation only available with 90-81-73 cascade selection"
    }
  })
  
  
## Results in tabular form
  
  output$cascade_table_proportion <- renderDT(
    datatable(proportion_data %>%
                filter(casState != "sizeEst") %>%
                namesToHuman(),
              container = wide6(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns = 5:10, digits=3) 
  )
  
  output$cascade_table_count <- renderDT(
    datatable(count_data %>%
                select(-c(count_target, x, xend)) %>%
                filter(casState != "sizeEst") %>%
                namesToHuman() %>%
                mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)),
              container = wide3_73(), rownames=FALSE, options = list(pageLength=999, dom='t')) %>%
      formatRound(columns=5:7, digits=0)
  )
  
  output$download_proportion_cascade <- downloadHandler(
    filename = function() {
      paste("KP_proportion_cascade.csv", sep='')
    },
    content = function (con) {
      write.csv(proportion_data %>%
                  filter(casState != "sizeEst") %>%
                  namesToHuman() %>%
                  mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)) %>%
                  rename("Point 90-90-90" = "point_90", "Lower 90-90-90" = "ll_90", "Upper 90-90-90" = "ul_90", "Point 90-81-73" = "point_73", "Lower 90-81-73" = "ll_73", "Upper 90-81-73" = "ul_73"), 
                con, na="", row.names = FALSE)
    }
  )
  
  output$download_count_cascade <- downloadHandler(
    filename = function() {
      paste("KP_count_cascade.csv", sep='')
    },
    content = function (con) {
      write.csv(count_data %>%
                  select(-c(count_target, x, xend)) %>%
                  filter(casState != "sizeEst") %>%
                  namesToHuman() %>%
                  mutate(Cascade.status = ifelse(Cascade.status=="Prevalence", "KPLHIV", Cascade.status)) %>%
                  rename("Point 90-81-73" = "point_73", "Lower 90-81-73" = "ll_73", "Upper 90-81-73" = "ul_73"),
                con, na="", row.names = FALSE)
    }
  )
}


