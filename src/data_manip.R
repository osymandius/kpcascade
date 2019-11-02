namesToCode <- function(data_input) {
  data_input %<>%
    rename("casState" = "Cascade.status", "year" = "Year", "district" = "District", "province" = "Province") %>%
    mutate(casState = ifelse(casState == "Size Estimate", "sizeEst", 
                             ifelse(casState == "Prevalence", "prev", 
                                    ifelse(casState == "Aware of Status", "aware",
                                           ifelse(casState == "On ART", "onART", "vSupp")))))
  
  return(data_input)
  
}

namesToHuman <- function(data_input) {
  data_input %<>%
    rename("Cascade.status" = "casState", "Year" = "year" , "District" = "district", "Province" = "province") %>%
    mutate(Cascade.status = ifelse(Cascade.status == "sizeEst", "Size Estimate", 
                                   ifelse(Cascade.status == "prev", "Prevalence", 
                                          ifelse(Cascade.status == "aware", "Aware of Status",
                                                 ifelse(Cascade.status == "onART", "On ART", "Virally Suppressed")))))
  
  return(data_input)
  
}

clean_data <- function(data_input) {
  
  cleaned_data <- namesToCode(data_input) %>%
    rename("point_90" = "Point.Estimate", "ll_90" = "Lower.Bound", "ul_90" = "Upper.Bound")
  
  cleaned_data$casState <- factor(cleaned_data$casState, levels=unique(cleaned_data$casState))
  cleaned_data$year = as.character(cleaned_data$year)
  cleaned_data$point_90 <- as.numeric(cleaned_data$point_90)
  
  return(cleaned_data)
  
}

proportion_manip <- function(data_clean) {
  
  df_73 <- data_clean %>%
    filter(cas90_81_73 == TRUE) %>%
    rename(point_73 = point_90, ul_73 = ul_90, ll_73 = ll_90)
  
  data_clean <- data_clean %>%
    filter(cas90_81_73 == FALSE)
  
  proportion_data <- data_clean %>%
    group_by(KP, year, district) %>%
    mutate(point_73 = as.numeric(ifelse(casState == "sizeEst", point_90[casState=="sizeEst"],
                                        ifelse(casState == "prev", point_90[casState=="prev"], 
                                               ifelse(casState == "aware", point_90[casState=="aware"], 
                                                      ifelse(casState == "onART", point_90[casState=="aware"]*point_90[casState=="onART"], 
                                                             ifelse(casState == "vSupp", point_90[casState=="aware"]*point_90[casState=="onART"]*point_90[casState=="vSupp"], ""))))))) %>%
    mutate(ll_73 = as.numeric(ifelse(casState == "sizeEst", ll_90[casState=="sizeEst"],
                                     ifelse(casState == "prev", ll_90[casState=="prev"], 
                                            ifelse(casState == "aware", ll_90[casState=="aware"], 
                                                   ifelse(casState == "onART", ll_90[casState=="aware"]*ll_90[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ll_90[casState=="aware"]*ll_90[casState=="onART"]*ll_90[casState=="vSupp"], ""))))))) %>%
    mutate(ul_73 = as.numeric(ifelse(casState == "sizeEst", ul_90[casState=="sizeEst"],
                                     ifelse(casState == "prev", ul_90[casState=="prev"], 
                                            ifelse(casState == "aware", ul_90[casState=="aware"], 
                                                   ifelse(casState == "onART", ul_90[casState=="aware"]*ul_90[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ul_90[casState=="aware"]*ul_90[casState=="onART"]*ul_90[casState=="vSupp"], ""))))))) %>%
    ungroup
  
  proportion_data <- proportion_data %>%
    bind_rows(df_73)
  
  return(proportion_data)
  
}


count_manip <- function(proportion_data) {
  
  count_data <- proportion_data %>%
    group_by(KP, year, district) %>%
    select(-c(point_90, ul_90, ll_90)) %>%
    mutate(point_73 = as.numeric(ifelse(casState == "sizeEst", point_73[casState=="sizeEst"],
                                        ifelse(casState == "prev", point_73[casState=="sizeEst"]*point_73[casState=="prev"], 
                                               ifelse(casState == "aware", point_73[casState=="sizeEst"]*point_73[casState=="prev"]*point_73[casState=="aware"], 
                                                      ifelse(casState == "onART", point_73[casState=="sizeEst"]*point_73[casState=="prev"]*point_73[casState=="onART"], 
                                                             ifelse(casState == "vSupp", point_73[casState=="sizeEst"]*point_73[casState=="prev"]*point_73[casState=="vSupp"], ""))))))) %>%
    mutate(ll_73 = as.numeric(ifelse(casState == "sizeEst", ll_73[casState=="sizeEst"],
                                     ifelse(casState == "prev", ll_73[casState=="sizeEst"]*ll_73[casState=="prev"], 
                                            ifelse(casState == "aware", ll_73[casState=="sizeEst"]*ll_73[casState=="prev"]*ll_73[casState=="aware"], 
                                                   ifelse(casState == "onART", ll_73[casState=="sizeEst"]*ll_73[casState=="prev"]*ll_73[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ll_73[casState=="sizeEst"]*ll_73[casState=="prev"]*ll_73[casState=="vSupp"], ""))))))) %>%
    mutate(ul_73 = as.numeric(ifelse(casState == "sizeEst", ul_73[casState=="sizeEst"],
                                     ifelse(casState == "prev", ul_73[casState=="sizeEst"]*ul_73[casState=="prev"], 
                                            ifelse(casState == "aware", ul_73[casState=="sizeEst"]*ul_73[casState=="prev"]*ul_73[casState=="aware"], 
                                                   ifelse(casState == "onART", ul_73[casState=="sizeEst"]*ul_73[casState=="prev"]*ul_73[casState=="onART"], 
                                                          ifelse(casState == "vSupp", ul_73[casState=="sizeEst"]*ul_73[casState=="prev"]*ul_73[casState=="vSupp"], ""))))))) %>%
    mutate(count_target = as.numeric(ifelse(casState == "aware", point_73[casState=="prev"]*0.9, 
                                            ifelse(casState == "onART", point_73[casState=="prev"] * 0.81, 
                                                   ifelse(casState == "vSupp", point_73[casState=="prev"]*0.73, ""))))) %>%
    mutate(x=as.numeric(casState)-0.5, xend = as.numeric(casState)+0.5) %>%
    ungroup
  
  return(count_data)
  
}
