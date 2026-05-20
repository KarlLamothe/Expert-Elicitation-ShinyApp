############################################################################# 
#Summarize a single question (hard-bounds PERT)
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
  # If a DoB column is provided, parse it (0–100 scale).
  # When DoB is not provided or is missing, full confidence (100) is assumed,
  # interpreting absence as acceptance of stated choices rather than uncertainty.
  if (!is.null(dob_col) && dob_col %in% names(df)) {
    raw_dob <- suppressWarnings(as.numeric(df[[dob_col]]))
    raw_dob <- pmax(pmin(raw_dob, 100), 0)   # clamp to [0, 100]
    raw_dob[is.na(raw_dob)] <- 100           # missing = full confidence
    df2$dob <- raw_dob
  } else {
    df2$dob <- 100                           # uniform if no column supplied
  }
  
  # Normalize to sum-to-1 sampling probabilities
  w <- df2$dob / sum(df2$dob)
  
  # Equal-weight mixture
  idx_eq <- sample.int(nrow(df2), Nsim, replace = TRUE)
  samples_eq <- df2$a[idx_eq] + (df2$b[idx_eq] - df2$a[idx_eq]) *
    rbeta(Nsim, df2$alpha[idx_eq], df2$beta[idx_eq])
  
  # DoB-weighted mixture
  idx_w <- sample.int(nrow(df2), Nsim, replace = TRUE, prob = w)
  samples_w <- df2$a[idx_w] + (df2$b[idx_w] - df2$a[idx_w]) *
    rbeta(Nsim, df2$alpha[idx_w], df2$beta[idx_w])
  
  # Keep 'samples' as equal-weight for backward compatability with existing plots
  samples <- samples_eq
  
  # Vectorized individual densities (carries dob for weighted pool)
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
    summarise(density = mean(density), .groups = "drop")
  
  # DoB-weighted Linear Opinion Pool
  dens_mixture_w <- dens_individual %>%
    group_by(p) %>%
    summarise(density = weighted.mean(density, w = dob), .groups = "drop")
  
  # Moment-matched Beta: equal-weight
  m_hat <- mean(samples_eq)
  v_hat <- var(samples_eq)
  if (!is.finite(v_hat) || v_hat <= 1e-12) v_hat <- 1e-6
  ab_term <- max(m_hat * (1 - m_hat) / v_hat - 1, 2)
  alpha_star <- m_hat * ab_term
  beta_star  <- (1 - m_hat) * ab_term
  grid_beta  <- seq(0, 1, length.out = 1000)
  df_beta_fit <- data.frame(p = grid_beta, density = dbeta(grid_beta, alpha_star, beta_star))
  
  # Moment-matched Beta: DoB-weighted
  m_hat_w <- mean(samples_w)
  v_hat_w <- var(samples_w)
  if (!is.finite(v_hat_w) || v_hat_w <= 1e-12) v_hat_w <- 1e-6
  ab_term_w    <- max(m_hat_w * (1 - m_hat_w) / v_hat_w - 1, 2)
  alpha_star_w <- m_hat_w * ab_term_w
  beta_star_w  <- (1 - m_hat_w) * ab_term_w
  df_beta_fit_w <- data.frame(p = grid_beta, density = dbeta(grid_beta, alpha_star_w, beta_star_w))
  
  # Summary table
  summary_tbl <- tibble(
    Question              = if (is.null(question_label)) NA_character_ else question_label,
    # Equal-weight columns
    EqW_Mean              = m_hat,
    EqW_Median            = median(samples_eq),
    EqW_5th               = as.numeric(quantile(samples_eq, 0.05)),
    EqW_95th              = as.numeric(quantile(samples_eq, 0.95)),
    # DoB-weighted columns
    DoB_Mean              = m_hat_w,
    DoB_Median            = median(samples_w),
    DoB_5th               = as.numeric(quantile(samples_w, 0.05)),
    DoB_95th              = as.numeric(quantile(samples_w, 0.95)),
    # Shared
    Hard_Union_LPP        = min(df2$a),
    Hard_Union_HPP        = max(df2$b),
    N_Participants        = nrow(df2),
    Lambda                = lambda,
    Nsim                  = Nsim
  )
  
  # Cumulative density functions (CDF)
  cdf_individual <- df2 %>%
    select(id, a, b, alpha, beta, dob) %>%
    crossing(p = grid) %>%
    mutate(
      cdf = case_when(
        p <= a ~ 0,
        p >= b ~ 1,
        TRUE   ~ pbeta((p - a) / (b - a), alpha, beta)
      )
    )
  
  # Equal-weight CDF mixture
  cdf_mixture <- cdf_individual %>%
    group_by(p) %>%
    summarise(cdf = mean(cdf), .groups = "drop")
  
  # DoB-weighted CDF mixture
  cdf_mixture_w <- cdf_individual %>%
    group_by(p) %>%
    summarise(cdf = weighted.mean(cdf, w = dob), .groups = "drop")
  
  ec             <- ecdf(samples_eq)
  ec_w           <- ecdf(samples_w)
  cdf_emp        <- data.frame(p = grid, cdf = ec(grid))
  cdf_emp_w      <- data.frame(p = grid, cdf = ec_w(grid))
  cdf_beta_fit   <- data.frame(p = grid_beta, cdf = pbeta(grid_beta, alpha_star,   beta_star))
  cdf_beta_fit_w <- data.frame(p = grid_beta, cdf = pbeta(grid_beta, alpha_star_w, beta_star_w))
  
  # Facet-ready helper
  add_q <- function(df_fac) {
    df_fac %>%
      mutate(Question = if (is.null(question_label)) NA_character_ else question_label) %>%
      relocate(Question, .before = 1)
  }
  
  list(
    summary              = summary_tbl,
    densities_individual = dens_individual,
    density_mixture      = dens_mixture,
    density_mixture_w    = dens_mixture_w,
    samples              = samples_eq,
    samples_w            = samples_w,
    cdfs                 = list(individual   = cdf_individual,
                                mixture      = cdf_mixture,
                                mixture_w    = cdf_mixture_w,
                                empirical    = cdf_emp,
                                empirical_w  = cdf_emp_w,
                                beta_fit     = cdf_beta_fit,
                                beta_fit_w   = cdf_beta_fit_w),
    facet                = list(
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
    beta_fit_params      = list(alpha = alpha_star,   beta = beta_star),
    beta_fit_params_w    = list(alpha = alpha_star_w, beta = beta_star_w)
  )
}

