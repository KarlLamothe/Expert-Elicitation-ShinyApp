setwd("App Version 2")

source("Functions and Helpers/Packages-Themes.R")
source("Functions and Helpers/Helper-function.R")
source("Functions and Helpers/Summarize-Pert.R")
source("Functions and Helpers/Plotting-functions.R")
source("Data/demo_data.R")

gs4_auth(email = "XXXXXX")

source("App-UI.R")
source("App-Server.R")

shinyApp(ui, server)
