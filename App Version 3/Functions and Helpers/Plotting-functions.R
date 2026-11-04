################################################################################
# Rbuild the ggplots from results() and inputs
################################################################################
################################################################################
split_questions <- function(df, question_col) {
  split(df, as.character(df[[question_col]]))
}

################################################################################
################################################################################
# build individuals plots
################################################################################
################################################################################
# probability
build_probability_individuals_plot <- function(df,
                                               id_col,
                                               lpp_col, bgp_col, hpp_col,
                                               question_col,
                                               round_col){
  
  df <- df %>%
    mutate(Facet_Label = if (!is.null(round_col) && round_col != "<none>") {
          paste0("Question ", .data[[question_col]],"\nRound ", .data[[round_col]] )
        } else {
          paste0("Question ", .data[[question_col]])
        })
  
  n_rounds <- if (!is.null(round_col) &&
                  round_col != "<none>") {
    length(unique(df[[round_col]]))
  } else {
    1
  }
  
  # Long form for 3 raw points (LPP, BGP, HPP)
  df_long <- df %>%
    pivot_longer(cols = c(Lowest_Plausible_Pr, Best_Guess_Pr, Highest_Plausible_Pr),
                 names_to = "Measure", values_to = "Value") %>%
    mutate(Measure = recode(Measure,
                            Lowest_Plausible_Pr  = "LPP",
                            Best_Guess_Pr        = "BGP",
                            Highest_Plausible_Pr = "HPP"))
  
  ggplot() + geom_linerange(data = df, aes(x = Participant,
                                            ymin = Lowest_Plausible_Pr,
                                            ymax = Highest_Plausible_Pr),
                            linewidth = 0.8, colour = "grey55") +
    
    # LPP and HPP endpoints
    geom_point(data = df_long %>% filter(Measure != "BGP"),
               aes(x = Participant, y = Value, shape = Measure),
               size = 2.5, colour = "grey35") +
    
    # BGP gets visual emphasis
    geom_point(data = df_long %>% filter(Measure == "BGP"),
               aes(x = Participant, y = Value, shape = Measure),
               size = 3.5, colour = "#245674") +
    
    scale_shape_manual(name = NULL, 
                       values = c("LPP" = 21,"BGP" = 19, "HPP" = 24),
                       breaks = c("LPP", "BGP", "HPP"),
                       labels = c("Lowest plausible", "Best guess","Highest plausible")) +
    
    coord_flip() +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    labs(x = NULL, y = "Probability")+
    facet_wrap(
      ~ Facet_Label,
      ncol = if (n_rounds > 1) 1 else min(4, length(unique(df$Facet_Label))),
      scales = "fixed") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

##################################
#numeric
##################################
build_numeric_individuals_plot <- function(
    df, id_col, lpp_col, bgp_col, hpp_col,
    question_col,
    round_col) {
  
  df <- df %>%
    mutate(Facet_Label = if (!is.null(round_col) && round_col != "<none>") {
        paste0("Question ", .data[[question_col]], "\nRound ", .data[[round_col]])
      } else {
        paste0("Question ", .data[[question_col]])
      })
  
  n_rounds <- if (!is.null(round_col) &&
                  round_col != "<none>") {
    length(unique(df[[round_col]]))
  } else {
    1
  }
  
  # Long form for 3 raw points (LPP, BGP, HPP)
  df_long <- df %>%
    pivot_longer(cols = c(Lowest_Plausible_Pr, Best_Guess_Pr, Highest_Plausible_Pr),
                 names_to = "Measure", values_to = "Value") %>%
    mutate(Measure = recode(Measure,
                            Lowest_Plausible_Pr  = "LPP",
                            Best_Guess_Pr        = "BGP",
                            Highest_Plausible_Pr = "HPP"))
  
  ggplot() + geom_linerange(data = df, aes(x = Participant,
                                            ymin = Lowest_Plausible_Pr,
                                            ymax = Highest_Plausible_Pr),
                            linewidth = 0.8, colour = "grey55") +
    
    # LPP and HPP endpoints
    geom_point(data = df_long %>% filter(Measure != "BGP"),
               aes(x = Participant, y = Value, shape = Measure),
               size = 2.5, colour = "grey35") +
    
    # BGP gets visual emphasis
    geom_point(data = df_long %>% filter(Measure == "BGP"),
               aes(x = Participant, y = Value, shape = Measure),
               size = 3.5, colour = "#245674") +
    
    scale_shape_manual(name = NULL, 
                       values = c("LPP" = 21,"BGP" = 19, "HPP" = 24),
                       breaks = c("LPP", "BGP", "HPP"),
                       labels = c("Lowest plausible", "Best guess","Highest plausible")) +
    
    coord_flip() +
    labs(x = NULL, y = "Estimate")+
    facet_wrap(
      ~ Facet_Label,
      ncol = if (n_rounds > 1) 1 else min(4, length(unique(df$Facet_Label))),
      scales = "free"
    )+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

################################################################################
################################################################################
# build density plots
################################################################################
################################################################################
# probability
build_probability_density_plot <- function(df_mix,
                                           df_mix_w = NULL,
                                           df_ind = NULL,
                                           df_beta = NULL,
                                           show_individual = TRUE,
                                           show_beta = TRUE,
                                           show_AC_mix = TRUE) {
  
  p <- ggplot() + 
    geom_line(data = df_mix, aes(x = p, y = density,
                                 colour = "Equal-weight mixture",
                                 linetype = "Equal-weight mixture"), linewidth = 0.8) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    labs(x = "Probability", y = "Density") +
    scale_colour_manual(
      name = NULL,
      breaks = c("Equal-weight mixture", "AC-weighted mixture", "Beta approximation"),
      values = c("Equal-weight mixture" = "black",
                 "AC-weighted mixture" = "#D55E00",
                 "Beta approximation" = "#0072B2")) +
    scale_linetype_manual(name = NULL,
                          breaks = c("Equal-weight mixture",
                                     "AC-weighted mixture",
                                     "Beta approximation"),
                          values = c("Equal-weight mixture" = "solid",
                                     "AC-weighted mixture" = "solid",
                                     "Beta approximation" = "dashed"))+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  if (show_individual &&
      !is.null(df_ind)) {
    p <- p + geom_line(data = df_ind, aes(x = p, y = density, group = id),
                       alpha = 0.4,colour = "grey60")
  }
  
  if (show_beta &&
      !is.null(df_beta)) {
    p <- p + geom_line(data = df_beta, aes(x = p, y = density,
                                           colour = "Beta approximation",
                                           linetype = "Beta approximation"),
                       linewidth = 0.8)
  }
  
  if (show_AC_mix &&
      !is.null(df_mix_w)) {
    p <- p + geom_line( data = df_mix_w, aes(x = p, y = density,
                                             colour = "AC-weighted mixture",
                                             linetype = "AC-weighted mixture"),
        linewidth = 0.8)
  }
  
  if ("Question_Round" %in% names(df_mix)) {
    p <- p + facet_wrap(~ Question_Round, ncol = 1,scales = "free_y")
  }
  p
}

##################################
# Numeric
##################################
build_numeric_density_plot <- function(df_mix,
                                       df_mix_w = NULL,
                                       df_ind = NULL,
                                       show_individual = TRUE,
                                       show_AC_mix = TRUE) {
  
  
  
  p <- ggplot() + 
    scale_colour_manual(name = NULL, breaks = c("Equal-weight mixture",
                                                "AC-weighted mixture"),
                        values = c("Equal-weight mixture" = "black",
                                   "AC-weighted mixture" = "#D55E00")) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(x = "Estimate", y = "Density")
  
  if (show_individual &&
      !is.null(df_ind)) {
    p <- p + geom_line(data = df_ind, aes(x = p,y = density, group = id),
                       alpha = 0.4,colour = "grey60") 
  }
  
  p <- p + geom_line(data = df_mix, aes(x = p,y = density,
                                        colour = "Equal-weight mixture",
                                        linetype = "Equal-weight mixture"),
      linewidth = 0.8) 
  
  if (show_AC_mix &&
      !is.null(df_mix_w)) {
    p <- p + geom_line(data = df_mix_w,
                       aes(x = p, y = density, colour = "AC-weighted mixture",
                           linetype = "AC-weighted mixture"),
                       linewidth = 0.8) 
  }

    if ("Question_Round" %in% names(df_mix)) {
    p <- p + facet_wrap( ~ Question_Round, ncol = 1, scales = "free") 
  }
  
  p+theme(legend.position = "none" )
}

################################################################################
################################################################################
# build histogram plots
################################################################################
################################################################################
build_probability_hist_plot <- function(samples,
                                        dens_mix,
                                        dens_mix_w = NULL,
                                        df_beta = NULL,
                                        show_AC_mix = TRUE,show_beta = TRUE) {
  
  if (all(c("Question", "Round") %in% names(samples))) {
    samples <- samples %>%
      mutate(Question_Round = paste0("Question ", Question, "\nRound ", Round))
  }
  
  p <- ggplot() + geom_histogram(data = samples, aes(x = samples,
                                                     y = after_stat(density)),
                                 bins = 50, fill = "grey85", colour = "white") +
    geom_line(data = dens_mix, aes(x = p, y = density,
                                   colour = "Equal-weight mixture",
                                   linetype = "Equal-weight mixture"),linewidth = 0.8) +
    coord_cartesian(xlim = c(0, 1))+
    labs(x = "Probability", y = "Density") +
    scale_colour_manual(name = NULL,breaks = c("Equal-weight mixture",
                                               "AC-weighted mixture",
                                               "Beta approximation"),
                        values = c("Equal-weight mixture" = "black",
                                   "AC-weighted mixture" = "#D55E00",
                                   "Beta approximation" = "#0072B2")) +
    scale_linetype_manual(name = NULL,breaks = c("Equal-weight mixture",
                                                 "AC-weighted mixture",
                                                 "Beta approximation"),
                          values = c("Equal-weight mixture" = "solid",
                                     "AC-weighted mixture" = "solid",
                                     "Beta approximation" = "dashed")) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
  
  if (show_AC_mix &&
      !is.null(dens_mix_w)) {
    p <- p +geom_line(data = dens_mix_w, aes(x = p, y = density,
                                             colour = "AC-weighted mixture",
                                             linetype = "AC-weighted mixture"),linewidth = 0.8)
  }
  
  if (show_beta &&
      !is.null(df_beta)) {
    p <- p + geom_line(data = df_beta, aes(x = p, y = density,
                                           colour = "Beta approximation",
                                           linetype = "Beta approximation"), linewidth = 0.8)
  }
  
  if ("Question_Round" %in% names(samples)) {
    p <- p + facet_wrap( ~ Question_Round, ncol = 1, scales = "free_y")
  }
  
  p
}

###################################
# Numeric
###################################
build_numeric_hist_plot <- function(
    samples,
    dens_mix,
    dens_mix_w = NULL,
    show_AC_mix = TRUE
) {
  
  if (all(c("Question", "Round") %in% names(samples))) {
    samples <- samples %>%
      mutate(Question_Round = paste0("Question ", Question, "\nRound ", Round) )
  }
  
  p <- ggplot() +
    geom_histogram(data = samples, aes(x = samples, y = after_stat(density)),
                   bins = 50,fill = "grey85", colour = "white") +
    geom_line(data = dens_mix, aes(x = p,y = density,
                                   colour = "Equal-weight mixture",
                                   linetype = "Equal-weight mixture"),linewidth = 0.8) +
    scale_colour_manual(name = NULL, breaks = c("Equal-weight mixture",
                                                "AC-weighted mixture"),
                        values = c("Equal-weight mixture" = "black",
                                   "AC-weighted mixture" = "#D55E00")) +
    scale_linetype_manual(name = NULL,breaks = c("Equal-weight mixture",
                                                 "AC-weighted mixture"),
                          values = c("Equal-weight mixture" = "solid",
                                     "AC-weighted mixture" = "solid")) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "none") +
    labs(x = "Estimate", y = "Density")
  
  if (show_AC_mix &&
      !is.null(dens_mix_w)) {
    p <- p + geom_line(data = dens_mix_w, aes( x = p,y = density,
                                               colour = "AC-weighted mixture",
                                               linetype = "AC-weighted mixture"), linewidth = 0.8)
  }
  
  if ("Question_Round" %in% names(samples)) {
    p <- p + facet_wrap( ~ Question_Round, ncol = 1, scales = "free")
  }
  p
}

################################################################################
################################################################################
# build histogram plots
################################################################################
################################################################################
build_probability_cdf_plot <- function(
    df_mix,
    df_mix_w = NULL,
    df_ind = NULL,
    df_beta = NULL,
    show_individual = TRUE,
    show_beta = TRUE,
    show_AC_mix = TRUE
) {
  
  p <- ggplot() +
    
    geom_line(
      data = df_mix,
      aes(
        x = p,
        y = cdf,
        colour = "Equal-weight mixture",
        linetype = "Equal-weight mixture"
      ),
      linewidth = 0.8
    ) +
    
    scale_x_continuous(
      limits = c(0,1),
      breaks = seq(0,1,0.2)
    ) +
    
    labs(
      x = "Probability",
      y = "Cumulative probability"
    ) +
    
    scale_colour_manual(
      name = NULL,
      breaks = c(
        "Equal-weight mixture",
        "AC-weighted mixture",
        "Beta approximation"
      ),
      values = c(
        "Equal-weight mixture" = "black",
        "AC-weighted mixture" = "#D55E00",
        "Beta approximation" = "#0072B2"
      )
    ) +
    
    scale_linetype_manual(
      name = NULL,
      breaks = c(
        "Equal-weight mixture",
        "AC-weighted mixture",
        "Beta approximation"
      ),
      values = c(
        "Equal-weight mixture" = "solid",
        "AC-weighted mixture" = "solid",
        "Beta approximation" = "dashed"
      )
    )
  
  if (show_individual &&
      !is.null(df_ind)) {
    
    p <- p +
      geom_line(
        data = df_ind,
        aes(
          x = p,
          y = cdf,
          group = id
        ),
        alpha = 0.4,
        colour = "grey60"
      )
  }
  
  if (show_AC_mix &&
      !is.null(df_mix_w)) {
    
    p <- p +
      geom_line(
        data = df_mix_w,
        aes(
          x = p,
          y = cdf,
          colour = "AC-weighted mixture",
          linetype = "AC-weighted mixture"
        ),
        linewidth = 0.8
      )
  }
  
  if (show_beta &&
      !is.null(df_beta)) {
    
    p <- p +
      geom_line(
        data = df_beta,
        aes(
          x = p,
          y = cdf,
          colour = "Beta approximation",
          linetype = "Beta approximation"
        ),
        linewidth = 0.8
      )
  }
  
  if ("Question_Round" %in% names(df_mix)) {
    
    p <- p +
      facet_wrap(
        ~ Question_Round,
        ncol = 1,
        scales = "fixed"
      )
  }
  
  p
}

################################
# Numeric
################################
build_numeric_cdf_plot <- function(
    df_mix,
    df_mix_w = NULL,
    df_ind = NULL,
    show_individual = TRUE,
    show_AC_mix = TRUE
) {
  
  p <- ggplot() +
    
    scale_colour_manual(
      name = NULL,
      breaks = c(
        "Equal-weight mixture",
        "AC-weighted mixture"
      ),
      values = c(
        "Equal-weight mixture" = "black",
        "AC-weighted mixture" = "#D55E00"
      )
    ) +
    
    scale_linetype_manual(
      name = NULL,
      breaks = c(
        "Equal-weight mixture",
        "AC-weighted mixture"
      ),
      values = c(
        "Equal-weight mixture" = "solid",
        "AC-weighted mixture" = "solid"
      )
    ) +
    
    labs(
      x = "Estimate",
      y = "Cumulative probability"
    ) +
    
    theme(
      legend.position = "none"
    )
  
  if (show_individual &&
      !is.null(df_ind)) {
    
    p <- p +
      geom_line(
        data = df_ind,
        aes(
          x = p,
          y = cdf,
          group = id
        ),
        alpha = 0.4,
        colour = "grey60"
      )
  }
  
  p <- p +
    geom_line(
      data = df_mix,
      aes(
        x = p,
        y = cdf,
        colour = "Equal-weight mixture",
        linetype = "Equal-weight mixture"
      ),
      linewidth = 0.8
    )
  
  if (show_AC_mix &&
      !is.null(df_mix_w)) {
    
    p <- p +
      geom_line(
        data = df_mix_w,
        aes(
          x = p,
          y = cdf,
          colour = "AC-weighted mixture",
          linetype = "AC-weighted mixture"
        ),
        linewidth = 0.8
      )
  }
  
  if ("Question_Round" %in% names(df_mix)) {
    
    p <- p +
      facet_wrap(
        ~ Question_Round,
        ncol = 1,
        scales = "free"
      )
  }
  
  p
}
