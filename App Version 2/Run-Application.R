setwd("App Version 2")

source("Functions and Helpers/Packages-Themes.R")
source("Functions and Helpers/Helper-function.R")
source("Functions and Helpers/Summarize-Pert.R")
source("Functions and Helpers/Plotting-functions.R")
source("Data/demo_data.R")

#gs4_auth(email = "YOUR_GOOGLE_EMAIL")

source("Reporting/Report_data.R")
source("Reporting/Report_narrative.R")
source("Reporting/Report_export.R")
source("Reporting/Report_diagnostics.R")
source("Reporting/Report_plots.R")

source("App-UI.R")
source("App-Server.R")

shinyApp(ui, server)
