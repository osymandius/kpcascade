library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)
library(DT)
library(reshape2)

source("src/ui/navigation.R")
source("src/ui/upload_data.R")
source("src/ui/plots.R")

source("src/data_manip.R")

source("src/server/data_tables.R")
source("src/server/upload_data_server.R")
source("src/server/plots_server.R")


shiny::shinyAppDir("src")