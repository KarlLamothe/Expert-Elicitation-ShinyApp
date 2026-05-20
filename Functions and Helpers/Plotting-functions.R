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
