build_revision_plot <- function(report_data){
  
  if(is.null(report_data$revision)){
    return(NULL)
  }
  
  top_question <- report_data$revision$Question[1]
  plot_data <- report_data$results$dens_mix_all %>%
    filter(Question == top_question)
  
  ggplot(plot_data, aes(x = p, y = density, color = Round)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("1" = "black", "2" = "#D55E00"),
                       labels = c("Round 1", "Round 2")) +
    xlim(0, 1) +
    labs(title = paste0("Question ",top_question),
      x = "Probability",y = "Density") +
    theme(axis.title   = element_text(size=18, family="sans", colour="black"),
          axis.text.x  = element_text(size=14, family="sans", colour="black"),
          strip.text   = element_text(size=15, family="sans", colour="black"),
          plot.title   = element_text(size=20, family="sans", colour="black"),
          panel.border = element_rect(colour="grey60", linewidth = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90",linewidth = 0.4),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title=element_blank())
}

build_spread_plot <- function(report_data){
  
  if(is.null(report_data$spread)){
    return(NULL)
  }
  
  top_div <- report_data$spread %>%
    slice(1)
  
  # Individual expert densities
  ind_data <- report_data$results$dens_ind_all %>%
    filter(Question == top_div$Question, Round == top_div$Round)
  
  # Equal-weight pooled density
  mix_data <- report_data$results$dens_mix_all %>%
    filter(Question == top_div$Question,Round == top_div$Round)
  
  #AC-weighted
  acw_data <- report_data$results$dens_mix_w_all %>%
    filter(Question == top_div$Question, Round == top_div$Round)
  
  ggplot() +
    geom_line(data = ind_data, aes(x=p, y=density, group=id),
              colour = "grey60", alpha=0.6, lwd = 0.8) +
    geom_line(data = mix_data, aes(x=p, y=density),
              colour = "black", lwd = 0.8) +
    geom_line(data = acw_data, aes(x = p, y = density),
              colour = "#D55E00", linewidth = 0.8)+
    xlim(0,1) +
    labs(title = paste0("Question ",top_div$Question," (Round ",
                        top_div$Round,")"),
      x = "Probability",y = "Density") +
    theme(axis.title   = element_text(size=18, family="sans", colour="black"),
          axis.text.x  = element_text(size=14, family="sans", colour="black"),
          strip.text   = element_text(size=15, family="sans", colour="black"),
          plot.title   = element_text(size=20, family="sans", colour="black"),
          panel.border = element_rect(colour="grey60", linewidth = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90",linewidth = 0.4),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none")
}