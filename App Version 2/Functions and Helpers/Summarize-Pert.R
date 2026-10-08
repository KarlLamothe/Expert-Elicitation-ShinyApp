############################################################################# 
# Core engine: summarize a single question (hard-bounds PERT)
############################################################################

summarize_question_pert <- function(df,
                                    id_col  = "Participant",
                                    lpp_col = "Lowest_Plausible_Pr",
                                    bgp_col = "Best_Guess_Pr",
                                    hpp_col = "Highest_Plausible_Pr",
                                    AC_col = "Assessment_Confidence",          
                                    lambda  = 4,
                                    Nsim    = 40000,
                                    grid    = seq(0, 1, length.out = 1000),
                                    seed    = NULL,
                                    question_label = NULL) {
  
  # Use a random seed if none provided
  if (is.null(seed)) seed <- sample.int(1e9, 1)
  set.seed(seed)
  
  # Standardize columns (string-safe)
  df2 <- df %>%
    transmute(
      id = .data[[id_col]],
      a  = .data[[lpp_col]],
      m  = .data[[bgp_col]],
      b  = .data[[hpp_col]]
    ) %>%
    mutate(
      a = pmin(a, b),
      b = pmax(a, b),
      m = pmin(pmax(m, a + 1e-8), b - 1e-8),
      alpha = 1 + lambda * (m - a) / (b - a),
      beta  = 1 + lambda * (b - m) / (b - a)
    )
  
  # Assessment Confidence:
  # Missing values remain missing and are excluded from the
  # Assessment Confidence-weighted pool.
  if (!is.null(AC_col) && AC_col %in% names(df)) {
    raw_AC <- suppressWarnings(as.numeric(df[[AC_col]]))
    # Values have already been validated by the application,
    # but retain bounds defensively.
    raw_AC <- pmax(pmin(raw_AC, 100), 0)
    df2$AC <- raw_AC
  } else {
    # No Assessment Confidence data were supplied
    df2$AC <- NA_real_
  }
  
  # Identify participants who supplied Assessment Confidence
  AC_observed <- !is.na(df2$AC)
  N_AC <- sum(AC_observed)
  total_ac <- sum(df2$AC[AC_observed], na.rm = TRUE)
  
  # Assessment Confidence weighting requires:
  #   1. at least two participants with observed AC; and
  #   2. at least one positive AC value.
  AC_available <- (N_AC >= 2 &&
      is.finite(total_ac) &&
      total_ac > 0)
  
  # Reason Assessment Confidence weighting is unavailable
  AC_unavailable_reason <- if (AC_available) {
    NA_character_
  } else if (N_AC < 2) {
    "fewer_than_two"
  } else if (total_ac <= 0) {
    "all_zero"
  } else {
    "unknown"
  }
  
  # Initialize weights as unavailable
  w <- rep(NA_real_, nrow(df2))
  if (AC_available) {
    # Participants with missing AC do not enter the
    # Assessment Confidence-weighted pool.
    w[AC_observed] <- df2$AC[AC_observed] / total_ac
  }
  
  # Equal-weight mixture
  idx_eq <- sample.int(nrow(df2), Nsim, replace = TRUE)
  
  samples_eq <- df2$a[idx_eq] +
    (df2$b[idx_eq] - df2$a[idx_eq]) *
    rbeta(
      Nsim,
      df2$alpha[idx_eq],
      df2$beta[idx_eq]
    )
  
  # Assessment Confidence-weighted mixture
  if (AC_available) {
    
    ac_idx <- which(AC_observed)
    
    idx_w <- sample(
      ac_idx,
      Nsim,
      replace = TRUE,
      prob = w[ac_idx]
    )
    
    samples_w <- df2$a[idx_w] +
      (df2$b[idx_w] - df2$a[idx_w]) *
      rbeta(
        Nsim,
        df2$alpha[idx_w],
        df2$beta[idx_w]
      )
    
  } else {
    
    samples_w <- rep(NA_real_, Nsim)
  }
  
  # Keep 'samples' as equal-weight for backward compatibility
  # with existing plots
  samples <- samples_eq
  
  # Vectorized individual densities
  dens_individual <- df2 %>%
    select(id, a, b, alpha, beta, AC) %>%
    crossing(p = grid) %>%
    mutate(
      density = if_else(
        p >= a & p <= b,
        dbeta((p - a) / (b - a), alpha, beta) / (b - a),
        0
      )
    )
  
  # Equal-weight Linear Opinion Pool
  dens_mixture <- dens_individual %>%
    group_by(p) %>%
    summarise(
      density = mean(density),
      .groups = "drop"
    )
  
  # AC-weighted Linear Opinion Pool
  if (AC_available) {
    dens_mixture_w <- dens_individual %>%
      filter(!is.na(AC)) %>%
      group_by(p) %>%
      summarise(density = weighted.mean(density, w = AC), .groups = "drop")
  } else {
    dens_mixture_w <- tibble(p = grid, density = NA_real_)
  }
  
  # Moment-matched Beta: equal-weight
  m_hat <- mean(samples_eq)
  v_hat <- var(samples_eq)
  
  if (!is.finite(v_hat) || v_hat <= 1e-12) {
    v_hat <- 1e-6
  }
  
  ab_term <- max(m_hat * (1 - m_hat) / v_hat - 1, 2)
  alpha_star <- m_hat * ab_term
  beta_star  <- (1 - m_hat) * ab_term
  grid_beta <- seq(0, 1, length.out = 1000)
  
  df_beta_fit <- data.frame(
    p = grid_beta,
    density = dbeta(
      grid_beta,
      alpha_star,
      beta_star))
  
  # Moment-matched Beta: AC-weighted
  if (AC_available) {
    
    m_hat_w <- mean(samples_w)
    v_hat_w <- var(samples_w)
    
    if (!is.finite(v_hat_w) || v_hat_w <= 1e-12) {
      v_hat_w <- 1e-6
    }
    
    ab_term_w <- max(
      m_hat_w * (1 - m_hat_w) / v_hat_w - 1,
      2
    )
    
    alpha_star_w <- m_hat_w * ab_term_w
    beta_star_w  <- (1 - m_hat_w) * ab_term_w
    
    df_beta_fit_w <- data.frame(
      p = grid_beta,
      density = dbeta(
        grid_beta,
        alpha_star_w,
        beta_star_w
      )
    )
    
  } else {
    
    m_hat_w <- NA_real_
    v_hat_w <- NA_real_
    alpha_star_w <- NA_real_
    beta_star_w <- NA_real_
    
    df_beta_fit_w <- data.frame(
      p = grid_beta,
      density = NA_real_
    )
  }
  
  # Summary table
  summary_tbl <- tibble(
    Question = if (is.null(question_label)) {
      NA_character_
    } else {
      question_label
    },
    
    # Equal-weight columns
    EqW_Mean   = m_hat,
    EqW_Median = median(samples_eq),
    EqW_5th    = as.numeric(quantile(samples_eq, 0.05)),
    EqW_95th   = as.numeric(quantile(samples_eq, 0.95)),
    
    # AC-weighted columns
    AC_Mean = if (AC_available) {
      m_hat_w
    } else {
      NA_real_
    },
    
    AC_Median = if (AC_available) {
      median(samples_w)
    } else {
      NA_real_
    },
    
    AC_5th = if (AC_available) {
      as.numeric(quantile(samples_w, 0.05))
    } else {
      NA_real_
    },
    
    AC_95th = if (AC_available) {
      as.numeric(quantile(samples_w, 0.95))
    } else {
      NA_real_
    },
    
    # assessment confidence summaries
    Mean_AC = if (N_AC > 0) {
      mean(df2$AC, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    Median_AC = if (N_AC > 0) {
      median(df2$AC, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    Min_AC = if (N_AC > 0) {
      min(df2$AC, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    Max_AC = if (N_AC > 0) {
      max(df2$AC, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    # Difference caused by AC weighting
    AC_Effect = if (AC_available) {
      m_hat_w - m_hat
    } else {
      NA_real_
    },
    
    # Whether AC results are available
    AC_available = AC_available,
    AC_unavailable_reason = AC_unavailable_reason,
    N_AC = N_AC,
    AC_Response_Rate = N_AC / nrow(df2),
    
    # Shared
    Hard_Union_LPP = min(df2$a),
    Hard_Union_HPP = max(df2$b),
    N_Participants = nrow(df2),
    Lambda = lambda,
    Nsim = Nsim
  )
  
  # Cumulative density functions
  cdf_individual <- df2 %>%
    select(id, a, b, alpha, beta, AC) %>%
    crossing(p = grid) %>%
    mutate(
      cdf = case_when(
        p <= a ~ 0,
        p >= b ~ 1,
        TRUE ~ pbeta(
          (p - a) / (b - a),
          alpha,
          beta
        )
      )
    )
  
  # Equal-weight CDF mixture
  cdf_mixture <- cdf_individual %>%
    group_by(p) %>%
    summarise(
      cdf = mean(cdf),
      .groups = "drop"
    )
  
  # AC-weighted CDF mixture
  if (AC_available) {
    cdf_mixture_w <- cdf_individual %>%
      filter(!is.na(AC)) %>%
      group_by(p) %>%
      summarise(cdf = weighted.mean( cdf, w = AC), .groups = "drop")
  } else {
    cdf_mixture_w <- tibble(p = grid,cdf = NA_real_)
  }
  
  # Empirical equal-weight CDF
  ec <- ecdf(samples_eq)
  
  cdf_emp <- data.frame(
    p = grid,
    cdf = ec(grid)
  )
  
  # Empirical AC-weighted CDF
  if (AC_available) {
    
    ec_w <- ecdf(samples_w)
    
    cdf_emp_w <- data.frame(
      p = grid,
      cdf = ec_w(grid)
    )
    
  } else {
    
    cdf_emp_w <- data.frame(
      p = grid,
      cdf = NA_real_
    )
  }
  
  # CDF of fitted Beta distributions
  cdf_beta_fit <- data.frame(
    p = grid_beta,
    cdf = pbeta(
      grid_beta,
      alpha_star,
      beta_star
    )
  )
  
  if (AC_available) {
    
    cdf_beta_fit_w <- data.frame(
      p = grid_beta,
      cdf = pbeta(
        grid_beta,
        alpha_star_w,
        beta_star_w
      )
    )
    
  } else {
    
    cdf_beta_fit_w <- data.frame(
      p = grid_beta,
      cdf = NA_real_
    )
  }
  
  # Facet-ready helper
  add_q <- function(df_fac) {
    df_fac %>%
      mutate(
        Question = if (is.null(question_label)) {
          NA_character_
        } else {
          question_label
        }
      ) %>%
      relocate(Question, .before = 1)
  }
  
  # Return results
  list(
    summary              = summary_tbl,
    densities_individual = dens_individual,
    density_mixture      = dens_mixture,
    density_mixture_w    = dens_mixture_w,
    samples              = samples_eq,
    samples_w            = samples_w,
    
    cdfs = list(
      individual  = cdf_individual,
      mixture     = cdf_mixture,
      mixture_w   = cdf_mixture_w,
      empirical   = cdf_emp,
      empirical_w = cdf_emp_w,
      beta_fit    = cdf_beta_fit,
      beta_fit_w  = cdf_beta_fit_w
    ),
    
    facet = list(
      mixture        = add_q(dens_mixture),
      mixture_w      = add_q(dens_mixture_w),
      individual     = add_q(dens_individual),
      beta           = add_q(df_beta_fit),
      beta_w         = add_q(df_beta_fit_w),
      cdf_mixture    = add_q(cdf_mixture),
      cdf_mixture_w  = add_q(cdf_mixture_w),
      cdf_emp        = add_q(cdf_emp),
      cdf_emp_w      = add_q(cdf_emp_w),
      cdf_beta       = add_q(cdf_beta_fit),
      cdf_beta_w     = add_q(cdf_beta_fit_w),
      cdf_individual = add_q(cdf_individual)
    ),
    
    beta_fit_params = list(
      alpha = alpha_star,
      beta  = beta_star
    ),
    
    beta_fit_params_w = list(
      alpha = alpha_star_w,
      beta  = beta_star_w
    ),
    
    AC_available = AC_available,
    N_AC = N_AC,
    AC_unavailable_reason = AC_unavailable_reason
  )
}