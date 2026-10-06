##########################################################################
# Rbuild the ggplots from results() and inputs
##########################################################################
# LPP / BGP / HPP per participant faceted by Question
build_density_plot <- function(r,
                               show_individual = TRUE,
                               show_beta = TRUE,
                               show_dob_mix = TRUE,
                               facet_cols = NULL) {
  
  p <- ggplot() +
    # Equal-weight mixture
    geom_line(data = r$dens_mix_all, aes(x = p, y = density,
                                         color = "Equal-weight mixture"),
      linewidth = 0.8)+
    xlim(0, 1) +
    labs(x = "Probability", y = "Density") +
    scale_color_manual(name = NULL,
                       values = c("Equal-weight mixture" = "black",
                                  "DoB-weighted mixture" = "#D55E00",
                                  "Beta approximation" = "#0072B2")) +
    guides(color = guide_legend(order = 1, nrow = 1)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal")
  
  # Anonymous individual expert distributions
  if (isTRUE(show_individual)) {
    p <- p + geom_line(data = r$dens_ind_all, aes(x = p,y = density,group = id),
                       alpha = 0.5,linewidth = 0.4,color = "grey55")
    }
  
  # DoB-weighted mixture
  if (isTRUE(show_dob_mix)) {
    p <- p + geom_line(
      data = r$dens_mix_w_all,aes(x = p, y = density, color = "DoB-weighted mixture"),
      linewidth = 0.8)
    }
  
  # Beta approximation
  if (isTRUE(show_beta)) {
    p <- p + geom_line(
      data = r$beta_all,
      aes( x = p,y = density, color = "Beta approximation"),
      linewidth = 0.8, linetype="dashed")
    
    }
  
  # Faceting
  if ("Question_Round" %in% names(r$dens_mix_all)) {
    
    p <- p +
      facet_wrap(
        ~ Question_Round,
        scales = "free_y"
      )
    
  } else {
    
    if (is.null(facet_cols)) {
      
      p <- p +
        facet_wrap(
          ~ Question,
          scales = "free_y"
        )
      
    } else {
      
      p <- p +
        facet_wrap(
          ~ Question,
          ncol = facet_cols,
          scales = "fixed"
        )
    }
  }
p
}

build_hist_plot <- function(r,
                            show_beta = TRUE,
                            show_dob_mix = TRUE,
                            facet_cols = NULL) {
  
  # Prepare histogram data for optional round faceting
  samples_plot <- r$samples_all
  
  if ("Round" %in% names(samples_plot)) {
    
    samples_plot <- samples_plot %>%
      mutate(
        Question_Round = paste0(
          "Question ", Question,
          "\nRound ", Round
        )
      )
  }
  
  p <- ggplot() +
    geom_histogram(data = samples_plot, aes(x = samples, y = after_stat(density)),
                   bins = 50, fill = "grey85", color = "white") +
    geom_line(data = r$dens_mix_all, aes(x = p, y = density,
                                         color = "Equal-weight mixture",
                                         linetype = "Equal-weight mixture"), lwd = 0.5) +
    labs(x = "Probability", y = "Density") +
    scale_color_manual(name = NULL,
                       values = c("Equal-weight mixture" = "black",
                                  "DoB-weighted mixture" = "#D55E00",
                                  "Beta approximation" = "#0072B2")) +
    scale_linetype_manual(name = NULL, 
                          values = c("Equal-weight mixture" = "solid",
                                     "DoB-weighted mixture" = "solid",
                                     "Beta approximation" = "dashed")) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
  
  if (isTRUE(show_dob_mix)) {
    p <- p +geom_line(data = r$dens_mix_w_all, 
                      aes(x = p, y = density,
                          color = "DoB-weighted mixture",
                          linetype = "DoB-weighted mixture"),lwd = 0.5)
  }
  
  if (isTRUE(show_beta)) {
    p <- p +geom_line(data = r$beta_all, aes(x = p, y = density,
                                             color = "Beta approximation",
                                             linetype = "Beta approximation"),
                      lwd = 0.5)
  }
  
  # Faceting
  if ("Question_Round" %in% names(samples_plot)) {
    
    if (is.null(facet_cols)) {
      
      p <- p +
        facet_wrap(
          ~ Question_Round,
          scales = "free_y"
        )
      
    } else {
      
      p <- p +
        facet_wrap(
          ~ Question_Round,
          ncol = facet_cols,
          scales = "free_y"
        )
    }
    
  } else {
    
    if (is.null(facet_cols)) {
      
      p <- p +
        facet_wrap(
          ~ Question,
          scales = "free_y"
        )
      
    } else {
      
      p <- p +
        facet_wrap(
          ~ Question,
          ncol = facet_cols
        )
    }
  }
  p
}


build_cdf_plot <- function(r,
                           show_individual = TRUE,
                           show_beta = TRUE,
                           show_dob_mix = TRUE,
                           facet_cols = NULL) {
  
  p <- ggplot() +
    geom_line(data = r$cdf_mix_all, aes(p, cdf,
                                        color = "Equal-weight mixture",
                                        linetype = "Equal-weight mixture"), lwd = 0.5) +
    scale_color_manual(name = NULL,
                       values = c("Equal-weight mixture" = "black",
                                  "DoB-weighted mixture" = "#D55E00",
                                  "Beta approximation" = "#0072B2")) +
    scale_linetype_manual(name = NULL, 
                          values = c("Equal-weight mixture" = "solid",
                                     "DoB-weighted mixture" = "solid",
                                     "Beta approximation" = "dashed")) +
    labs(x = "Probability",y = "Cumulative probability")
  
  if (isTRUE(show_dob_mix)) {
    p <- p + geom_line(data = r$cdf_mix_w_all, 
                       aes(p, cdf,
                           color = "DoB-weighted mixture",
                           linetype = "DoB-weighted mixture"),lwd = 0.5)
  }
  
  if (isTRUE(show_beta)) {
    p <- p + geom_line(data = r$cdf_beta_all, aes(p, cdf,
                                                  color = "Beta approximation",
                                                  linetype = "Beta approximation"),
                       lwd = 0.5)
  }
  
  if (isTRUE(show_individual)) {
    p <- p +
      geom_line(data = r$cdf_ind_all, aes(p, cdf, color = id), alpha = 0.5,
                lwd = 0.5) + guides(color = "none")
  }
  
  # Faceting
  if ("Question_Round" %in% names(r$cdf_mix_all)) {
    
    # Multi-round data
    if (is.null(facet_cols)) {
      
      p <- p +
        facet_wrap(
          ~ Question_Round
        )
      
    } else {
      
      p <- p +
        facet_wrap(
          ~ Question_Round,
          ncol = facet_cols
        )
    }
    
  } else {
    
    # Single-round data
    if (is.null(facet_cols)) {
      
      p <- p +
        facet_wrap(
          ~ Question
        )
      
    } else {
      
      p <- p +
        facet_wrap(
          ~ Question,
          ncol = facet_cols
        )
    }
  }
  
  p
}

build_individuals_plot <- function(df_raw,
                                   id_col, lpp_col, bgp_col, hpp_col,
                                   question_col = NULL,
                                   round_col = NULL,
                                   selected_questions = NULL,
                                   use_export_theme = FALSE,
                                   theme_export = NULL, 
                                   facet_cols = 4) {
  # Ensure we have a Question column (or synthesize one)
  has_q <- !is.null(question_col) && !identical(question_col, "<none>")
  if (!has_q) {
    q_col <- "__Question__"
    df <- df_raw %>% mutate(`__Question__` = "Q1")
  } else {
    q_col <- question_col
    df <- df_raw
  }
  
  # Filter to the selected subset of questions (if provided)
  if (!is.null(selected_questions) && length(selected_questions) > 0) {
    # When app has an "All" option, remove it before filtering
    sel <- setdiff(selected_questions, "All")
    if (length(sel) > 0) {
      df <- df %>% filter(.data[[q_col]] %in% sel)
    }
  }
  
  # Build plotting frame using raw (unfixed) values
  # Determine whether a Round column is available
  has_round <- !is.null(round_col) &&
    !identical(round_col, "<none>") &&
    round_col %in% names(df)
  
  # Build plotting frame using raw values
  if (has_round) {
    
    dfp <- df %>%
      transmute(
        Question             = .data[[q_col]],
        Round                = as.character(.data[[round_col]]),
        Participant          = as.character(.data[[id_col]]),
        Lowest_Plausible_Pr  = suppressWarnings(as.numeric(.data[[lpp_col]])),
        Best_Guess_Pr        = suppressWarnings(as.numeric(.data[[bgp_col]])),
        Highest_Plausible_Pr = suppressWarnings(as.numeric(.data[[hpp_col]]))
      ) %>%
      mutate(
        Question_Round = paste0(
          "Question ", Question,
          "\nRound ", Round
        )
      )
    
  } else {
    
    dfp <- df %>%
      transmute(
        Question             = .data[[q_col]],
        Participant          = as.character(.data[[id_col]]),
        Lowest_Plausible_Pr  = suppressWarnings(as.numeric(.data[[lpp_col]])),
        Best_Guess_Pr        = suppressWarnings(as.numeric(.data[[bgp_col]])),
        Highest_Plausible_Pr = suppressWarnings(as.numeric(.data[[hpp_col]]))
      )
  }
  
  # Long form for 3 raw points (LPP, BGP, HPP)
  # Give the three measures readable labels
  df_long <- dfp %>%
    pivot_longer(cols = c(Lowest_Plausible_Pr,Best_Guess_Pr,Highest_Plausible_Pr),
      names_to = "Measure", values_to = "Value") %>%
    mutate(Measure = recode(Measure,
        Lowest_Plausible_Pr  = "LPP",
        Best_Guess_Pr        = "BGP",
        Highest_Plausible_Pr = "HPP"))
  
  g <- ggplot() +
    # Plausible range
    geom_linerange(data = dfp,
                   aes(x = Participant,
                       ymin = Lowest_Plausible_Pr,
                       ymax = Highest_Plausible_Pr),
                   linewidth = 0.8,color = "grey55") +
    # LPP and HPP endpoints
    geom_point(data = df_long %>% filter(Measure != "BGP"),
               aes(x = Participant, y = Value,shape = Measure),
      size = 2.5,color = "grey35") +
    
    # BGP gets visual emphasis
    geom_point(data = df_long %>% filter(Measure == "BGP"),
      aes(x = Participant,y = Value,shape = Measure),
      size = 3.5,color = "#245674") +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, 0.2),
                       expand = expansion(mult = c(0.02, 0.02))) +
    scale_shape_manual(name = NULL,
                       values = c("LPP" = 21,"BGP" = 19,"HPP" = 24),
                       breaks = c("LPP", "BGP", "HPP"),
                       labels = c("Lowest plausible",
                                  "Best guess",
                                  "Highest plausible")) +
    labs(x = NULL, y = "Probability") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 14),
      panel.grid.minor = element_blank())
  # Faceting
  if (has_round) {
    
    g <- g +
      facet_wrap(
        ~ Question_Round,
        ncol = facet_cols,
        scales = "fixed"
      )
    
  } else {
    
    g <- g +
      facet_wrap(
        ~ Question,
        ncol = facet_cols,
        scales = "fixed"
      )
  }
  
  # If you defined a smaller export theme and asked to use it, apply it
  if (isTRUE(use_export_theme) && !is.null(theme_export)) {
    g <- g + theme_export + theme(axis.text.y = element_blank(), 
                                  axis.ticks.y = element_blank())
  }
  g
}
