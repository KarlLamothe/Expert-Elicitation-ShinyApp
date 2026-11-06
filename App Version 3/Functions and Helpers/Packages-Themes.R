# List of packages being used
list.of.packages <- c('shiny', 'readr', 'dplyr', 'tidyr', 'purrr', 'ggplot2', 'scales',
                      'googlesheets4',"bslib","rmarkdown",'kableExtra', 'patchwork')

# Identify packages in the list that are not on the computer
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Install packages in "new.packages"
if(length(new.packages)) install.packages(new.packages); rm(list.of.packages); rm(new.packages)

# packages
library(shiny)      # nice plots
library(readr)    # combine plots
library(dplyr)        # reshape data
library(tidyr)      # text wrap
library(purrr)       # plotting
library(ggplot2)   # plotting
library(scales)       # plotting
library(googlesheets4) # retrieving google sheets data
library(bslib)  # user interface
library(rmarkdown)  # report
library(kableExtra)  # report
library(patchwork)  # plotting

suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(googlesheets4)
  library(bslib)
  library(rmarkdown)  
  library(kableExtra)
  library(patchwork)
  
})

options(scipen=999) # Remove scientific notation

############################################################################# 
#APP THEME (on-screen)
#############################################################################
theme_base <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90",linewidth = 0.4),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.width = unit(1.5, "cm"),
        legend.title= element_blank())

theme_app <- theme_base +
  theme(axis.title   = element_text(size=18, family="sans", colour="black"),
        axis.text.x  = element_text(size=14, family="sans", colour="black"),
        axis.text.y  = element_text(size=14, family="sans", colour="black"),
        strip.text   = element_text(size=15, family="sans", colour = "white", face = "bold"),
        plot.title   = element_text(size=20, family="sans", colour="black"),
        panel.border = element_rect(colour="grey60", linewidth = 0.5),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = 14),
        strip.background = element_rect(fill = "#1F5A7A", colour = "#1F5A7A"))

# Apply it app-wide
theme_set(theme_app)

#############################################################################
#EXPORT THEME (smaller text)
#############################################################################
theme_export_yesno <- theme_base +
  theme(axis.title   = element_text(size=11,   family="sans", colour="black"),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y  = element_blank(),
        strip.text   = element_text(size=10,   family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "none",
        legend.text = element_text(size = 10))

theme_export_yesno_combined <- theme_base +
  theme(axis.title   = element_text(size=11,   family="sans", colour="black"),
        axis.text.x  = element_text(size=9,    family="sans", colour="black"),
        strip.text   = element_text(size=10,   family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title= element_blank(),
        legend.text = element_text(size = 10))

theme_export_estimate <- theme_base +
  theme(axis.title   = element_text(size=11,   family="sans", colour="black"),
        axis.text.x  = element_text(size=9,    family="sans", colour="black"),
        axis.text.y  = element_blank(),
        axis.ticks.y  = element_blank(),
        strip.text   = element_text(size=10,   family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = 10))

theme_export_rank <- theme_base +
  theme(axis.title   = element_text(size=11,   family="sans", colour="black"),
        axis.text.x  = element_text(size=9,    family="sans", colour="black"),
        axis.text.y  = element_blank(),
        axis.ticks.y  = element_blank(),
        strip.text   = element_text(size=10,   family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title= element_blank(),
        legend.text = element_text(size = 10))

theme_export_cdf <- theme_base +
  theme(axis.title   = element_text(size=11,   family="sans", colour="black"),
        axis.text.x  = element_text(size=9,    family="sans", colour="black"),
        axis.text.y  = element_text(size=9,    family="sans", colour="black"),
        strip.text   = element_text(size=10,   family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = 10))