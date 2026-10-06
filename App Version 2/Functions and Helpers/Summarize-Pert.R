############################################################################# 
# Core engine: summarize a single question (hard-bounds PERT)
############################################################################

summarize_question_pert <- function(df,
                                    id_col  = "Participant",
                                    lpp_col = "Lowest_Plausible_Pr",
                                    bgp_col = "Best_Guess_Pr",
                                    hpp_col = "Highest_Plausible_Pr",
                                    dob_col = "Degree_of_Belief",          
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
  
  # Degree of Belief weights:
  # If a DoB column is provided, parse it on a 0-100 scale.
  # When DoB is missing, full confidence (100) is assumed.
  if (!is.null(dob_col) && dob_col %in% names(df)) {
    raw_dob <- suppressWarnings(as.numeric(df[[dob_col]]))
    raw_dob <- pmax(pmin(raw_dob, 100), 0)
    raw_dob[is.na(raw_dob)] <- 100
    df2$dob <- raw_dob
  } else {
    df2$dob <- 100
  }
  
  # Determine whether DoB weighting can be calculated
  total_dob <- sum(df2$dob, na.rm = TRUE)
  dob_available <- is.finite(total_dob) && total_dob > 0
  
  if (dob_available) {
    w <- df2$dob / total_dob
  } else {
    w <- rep(NA_real_, nrow(df2))
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
  
  # DoB-weighted mixture
  if (dob_available) {
    
    idx_w <- sample.int(
      nrow(df2),
      Nsim,
      replace = TRUE,
      prob = w
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
    select(id, a, b, alpha, beta, dob) %>%
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
  
  # DoB-weighted Linear Opinion Pool
  if (dob_available) {
    
    dens_mixture_w <- dens_individual %>%
      group_by(p) %>%
      summarise(
        density = weighted.mean(density, w = dob),
        .groups = "drop"
      )
    
  } else {
    
    dens_mixture_w <- tibble(
      p = grid,
      density = NA_real_
    )
  }
  
  # Moment-matched Beta: equal-weight
  m_hat <- mean(samples_eq)
  v_hat <- var(samples_eq)
  
  if (!is.finite(v_hat) || v_hat <= 1e-12) {
    v_hat <- 1e-6
  }
  
  ab_term <- max(
    m_hat * (1 - m_hat) / v_hat - 1,
    2
  )
  
  alpha_star <- m_hat * ab_term
  beta_star  <- (1 - m_hat) * ab_term
  
  grid_beta <- seq(0, 1, length.out = 1000)
  
  df_beta_fit <- data.frame(
    p = grid_beta,
    density = dbeta(
      grid_beta,
      alpha_star,
      beta_star
    )
  )
  
  # Moment-matched Beta: DoB-weighted
  if (dob_available) {
    
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
    
    # DoB-weighted columns
    DoB_Mean = if (dob_available) {
      m_hat_w
    } else {
      NA_real_
    },
    
    DoB_Median = if (dob_available) {
      median(samples_w)
    } else {
      NA_real_
    },
    
    DoB_5th = if (dob_available) {
      as.numeric(quantile(samples_w, 0.05))
    } else {
      NA_real_
    },
    
    DoB_95th = if (dob_available) {
      as.numeric(quantile(samples_w, 0.95))
    } else {
      NA_real_
    },
    
    # Degree of Belief summaries
    Mean_DoB   = mean(df2$dob, na.rm = TRUE),
    Median_DoB = median(df2$dob, na.rm = TRUE),
    Min_DoB    = min(df2$dob, na.rm = TRUE),
    Max_DoB    = max(df2$dob, na.rm = TRUE),
    
    # Difference caused by DoB weighting
    DoB_Effect = if (dob_available) {
      m_hat_w - m_hat
    } else {
      NA_real_
    },
    
    # Whether DoB results are available
    DoB_Available = dob_available,
    
    # Shared
    Hard_Union_LPP = min(df2$a),
    Hard_Union_HPP = max(df2$b),
    N_Participants = nrow(df2),
    Lambda = lambda,
    Nsim = Nsim
  )
  
  # Cumulative density functions
  cdf_individual <- df2 %>%
    select(id, a, b, alpha, beta, dob) %>%
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
  
  # DoB-weighted CDF mixture
  if (dob_available) {
    
    cdf_mixture_w <- cdf_individual %>%
      group_by(p) %>%
      summarise(
        cdf = weighted.mean(cdf, w = dob),
        .groups = "drop"
      )
    
  } else {
    
    cdf_mixture_w <- tibble(
      p = grid,
      cdf = NA_real_
    )
  }
  
  # Empirical equal-weight CDF
  ec <- ecdf(samples_eq)
  
  cdf_emp <- data.frame(
    p = grid,
    cdf = ec(grid)
  )
  
  # Empirical DoB-weighted CDF
  if (dob_available) {
    
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
  
  if (dob_available) {
    
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
    
    dob_available = dob_available
  )
}