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
    geom_line(data = r$dens_mix_all, aes(x = p, y = density), lwd = 0.5) +
    xlim(0, 1) + labs(x = "Probability", y = "Density") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  if (isTRUE(show_individual)) {
    p <- p +
      geom_line(data = r$dens_ind_all, aes(x = p, y = density, color = id),
                alpha = 0.5, lwd = 0.5) +
      guides(color = "none")
  }
  
  if (isTRUE(show_dob_mix)) {
    p <- p + geom_line(data = r$dens_mix_w_all, aes(x = p, y = density),
                       color = "red", lwd = 0.5)
  }
  
  if (isTRUE(show_beta)) {
    p <- p + geom_line(data = r$beta_all, aes(x = p, y = density),
                       color = "blue", lwd = 0.5, lty = "dashed")
  }
  
  if (is.null(facet_cols)) {
    p <- p + facet_wrap(~ Question, scales = "free_y")
  } else {
    p <- p + facet_wrap(~ Question, ncol = facet_cols, scales = "fixed")
  }
  p
}


build_hist_plot <- function(r,
                            show_beta = TRUE,
                            show_dob_mix = TRUE,
                            facet_cols = NULL) {
  
  p <- ggplot() +
    geom_histogram(data = r$samples_all, aes(x = samples, y = after_stat(density)),
                   bins = 50, fill = "grey85", color = "white") +
    geom_line(data = r$dens_mix_all, aes(x = p, y = density), lwd = 0.5) +
    labs(x = "Probability", y = "Density") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  if (isTRUE(show_dob_mix)) {
    p <- p +geom_line(data = r$dens_mix_w_all, aes(x = p, y = density),
                      color = "red", lwd = 0.5)
  }
  
  if (isTRUE(show_beta)) {
    p <- p +geom_line(data = r$beta_all, aes(x = p, y = density), color = "blue",
                      lwd = 0.5, lty = "dashed")
  }
  
  # Faceting
  if (is.null(facet_cols)) {
    p <- p + facet_wrap(~ Question, scales = "free_y")
  } else {
    p <- p + facet_wrap(~ Question, ncol = facet_cols)
  }
  
  p
}


build_cdf_plot <- function(r,
                           show_individual = TRUE,
                           show_beta = TRUE,
                           show_dob_mix = TRUE,
                           facet_cols = NULL) {
  
  p <- ggplot() +
    geom_line(data = r$cdf_mix_all, aes(p, cdf), lwd = 0.5) +
    labs(x = "Probability",y = "Cumulative probability")
  
  if (isTRUE(show_dob_mix)) {
    p <- p + geom_line(data = r$cdf_mix_w_all, aes(p, cdf),
                       color = "red",lwd = 0.5)
  }
  
  if (isTRUE(show_beta)) {
    p <- p + geom_line(data = r$cdf_beta_all, aes(p, cdf), color = "blue",
                       lwd = 0.5, lty = "dashed")
  }
  
  if (isTRUE(show_individual)) {
    p <- p +
      geom_line(data = r$cdf_ind_all, aes(p, cdf, color = id), alpha = 0.5,
                lwd = 0.5) + guides(color = "none")
  }
  
  # Faceting
  if (is.null(facet_cols)) {
    p <- p + facet_wrap(~ Question)
  } else {
    p <- p + facet_wrap(~ Question, ncol = facet_cols)
  }
  
  p
}


build_individuals_plot <- function(df_raw,
                                   id_col, lpp_col, bgp_col, hpp_col,
                                   question_col = NULL,
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
  dfp <- df %>%
    transmute(
      Question             = .data[[q_col]],
      Participant          = as.character(.data[[id_col]]),
      Lowest_Plausible_Pr  = suppressWarnings(as.numeric(.data[[lpp_col]])),
      Best_Guess_Pr        = suppressWarnings(as.numeric(.data[[bgp_col]])),
      Highest_Plausible_Pr = suppressWarnings(as.numeric(.data[[hpp_col]]))
    )
  
  # Long form for 3 raw points (LPP, BGP, HPP)
  df_long <- dfp %>%
    pivot_longer(
      cols = c(Lowest_Plausible_Pr, Best_Guess_Pr, Highest_Plausible_Pr),
      names_to = "Measure", values_to = "Value"
    )
  
  g <- ggplot() +
    geom_linerange(data = dfp, aes(x = Participant, 
                                   ymin = Lowest_Plausible_Pr, 
                                   ymax = Highest_Plausible_Pr),lwd = 0.5) +
    geom_point(data = df_long, aes(x = Participant, y = Value), size = 1) +
    coord_flip() + ylim(0,1)+
    labs(x = "Participant", y = "Probability") +
    facet_wrap(~ Question, ncol = facet_cols, scales = "fixed")+
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
  
  # If you defined a smaller export theme and asked to use it, apply it
  if (isTRUE(use_export_theme) && !is.null(theme_export)) {
    g <- g + theme_export + theme(axis.text.y = element_blank(), 
                                  axis.ticks.y = element_blank())
  }
  g
}
