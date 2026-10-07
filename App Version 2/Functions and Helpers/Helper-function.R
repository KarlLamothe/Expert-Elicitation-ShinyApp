############################################################################# 
# Choose a reasonable default for "Question" column if multiple exist
############################################################################
best_question_col <- function(df) {
  cols <- names(df)
  cand <- grep("^question(\\.{3}\\d+)?$", tolower(cols), value = TRUE)
  if (length(cand) == 0) {
    cand <- grep("^q(uestion)?", tolower(cols), value = TRUE)
  }
  if (length(cand) <= 1) {
    return(ifelse(length(cand) == 1, cand, "<none>"))
  }
  # Prefer the one with most non-missing + unique values
  score <- sapply(cand, function(cc) {
    v <- df[[cc]]
    sum(!is.na(v)) + 0.001 * length(unique(na.omit(v)))
  })
  cand[order(score, decreasing = TRUE)][1]
}

#############################################################################
# Validate expert elicitation input data
#############################################################################

validate_elicitation_data <- function(df,
                                      id_col,
                                      question_col,
                                      lpp_col,
                                      bgp_col,
                                      hpp_col,
                                      dob_col = NULL,
                                      round_col = NULL) {
  
  problems <- character(0)
  
  # -------------------------------------------------------------------------
  # Extract and standardize mapped columns
  # -------------------------------------------------------------------------
  
  check_df <- data.frame(
    Question = df[[question_col]],
    Participant = df[[id_col]],
    LPP = suppressWarnings(as.numeric(df[[lpp_col]])),
    BGP = suppressWarnings(as.numeric(df[[bgp_col]])),
    HPP = suppressWarnings(as.numeric(df[[hpp_col]])),
    stringsAsFactors = FALSE
  )
  
  # Optional Degree of Belief
  if (!is.null(dob_col) &&
      !identical(dob_col, "<none>") &&
      dob_col %in% names(df)) {
    
    check_df$DoB_raw <- df[[dob_col]]
    check_df$DoB <- suppressWarnings(as.numeric(df[[dob_col]]))
    
  } else {
    
    check_df$DoB_raw <- NA
    check_df$DoB <- NA_real_
  }
  
  # Optional Round
  if (!is.null(round_col) &&
      !identical(round_col, "<none>") &&
      round_col %in% names(df)) {
    
    check_df$Round <- df[[round_col]]
    
  } else {
    
    check_df$Round <- 1
  }
  
  # Preserve original probability values so we can distinguish
  # missing values from non-numeric entries
  check_df$LPP_raw <- df[[lpp_col]]
  check_df$BGP_raw <- df[[bgp_col]]
  check_df$HPP_raw <- df[[hpp_col]]
  
  # -------------------------------------------------------------------------
  # Helper for readable response identifiers
  # -------------------------------------------------------------------------
  
  response_label <- function(i) {
    
    q <- as.character(check_df$Question[i])
    r <- as.character(check_df$Round[i])
    
    # +1 because the first row of a CSV contains column headings
    file_row <- i + 1
    
    if (!is.null(round_col) &&
        !identical(round_col, "<none>")) {
      
      paste0(
        "Question ", q,
        ", Round ", r,
        ", row ", file_row
      )
      
    } else {
      
      paste0(
        "Question ", q,
        ", row ", file_row
      )
    }
  }
  
  # -------------------------------------------------------------------------
  # Validate each submitted response
  # -------------------------------------------------------------------------
  
  for (i in seq_len(nrow(check_df))) {
    
    label <- response_label(i)
    
    # Required identifiers
    if (is.na(check_df$Question[i]) ||
        trimws(as.character(check_df$Question[i])) == "") {
      
      problems <- c(
        problems,
        paste0(
          "Row ", i,
          ": Question is missing."
        )
      )
    }
    
    if (is.na(check_df$Participant[i]) ||
        trimws(as.character(check_df$Participant[i])) == "") {
      
      problems <- c(
        problems,
        paste0(
          "Row ", i,
          ": Participant ID is missing."
        )
      )
    }
    
    # Round is required only when a Round column is supplied
    if (!is.null(round_col) &&
        !identical(round_col, "<none>") &&
        (is.na(check_df$Round[i]) ||
         trimws(as.character(check_df$Round[i])) == "")) {
      
      problems <- c(
        problems,
        paste0(
          label,
          ": Elicitation round is missing."
        )
      )
    }
    
    # -----------------------------------------------------------------------
    # Missing probability estimates
    # -----------------------------------------------------------------------
    
    if (is.na(check_df$LPP_raw[i]) ||
        trimws(as.character(check_df$LPP_raw[i])) == "") {
      
      problems <- c(
        problems,
        paste0(
          label,
          ": Lowest plausible probability is missing."
        )
      )
    }
    
    if (is.na(check_df$BGP_raw[i]) ||
        trimws(as.character(check_df$BGP_raw[i])) == "") {
      
      problems <- c(
        problems,
        paste0(
          label,
          ": Best-guess probability is missing."
        )
      )
    }
    
    if (is.na(check_df$HPP_raw[i]) ||
        trimws(as.character(check_df$HPP_raw[i])) == "") {
      
      problems <- c(
        problems,
        paste0(
          label,
          ": Highest plausible probability is missing."
        )
      )
    }
    
    # -----------------------------------------------------------------------
    # Non-numeric probability estimates
    # -----------------------------------------------------------------------
    
    if (!is.na(check_df$LPP_raw[i]) &&
        trimws(as.character(check_df$LPP_raw[i])) != "" &&
        is.na(check_df$LPP[i])) {
      
      problems <- c(
        problems,
        paste0(
          label,
          ": Lowest plausible probability must be numeric."
        )
      )
    }
    
    if (!is.na(check_df$BGP_raw[i]) &&
        trimws(as.character(check_df$BGP_raw[i])) != "" &&
        is.na(check_df$BGP[i])) {
      
      problems <- c(
        problems,
        paste0(
          label,
          ": Best-guess probability must be numeric."
        )
      )
    }
    
    if (!is.na(check_df$HPP_raw[i]) &&
        trimws(as.character(check_df$HPP_raw[i])) != "" &&
        is.na(check_df$HPP[i])) {
      
      problems <- c(
        problems,
        paste0(
          label,
          ": Highest plausible probability must be numeric."
        )
      )
    }
    
    # Continue probability checks only if all three are numeric
    if (all(is.finite(c(
      check_df$LPP[i],
      check_df$BGP[i],
      check_df$HPP[i]
    )))) {
      
      lpp <- check_df$LPP[i]
      bgp <- check_df$BGP[i]
      hpp <- check_df$HPP[i]
      
      # LPP must be strictly above zero
      if (lpp <= 0) {
        
        problems <- c(
          problems,
          paste0(
            label,
            ": Lowest plausible probability must be greater than 0."
          )
        )
      }
      
      # HPP must be strictly below one
      if (hpp >= 1) {
        
        problems <- c(
          problems,
          paste0(
            label,
            ": Highest plausible probability must be less than 1."
          )
        )
      }
      
      # All probability values must otherwise fall within 0-1
      if (bgp < 0 || bgp > 1) {
        
        problems <- c(
          problems,
          paste0(
            label,
            ": Best-guess probability must be between 0 and 1."
          )
        )
      }
      
      # Plausible interval must have positive width
      if (lpp >= hpp) {
        
        problems <- c(
          problems,
          paste0(
            label,
            ": Lowest plausible probability must be less than ",
            "highest plausible probability."
          )
        )
        
      } else {
        
        # BGP must lie within plausible interval
        if (bgp < lpp || bgp > hpp) {
          
          problems <- c(
            problems,
            paste0(
              label,
              ": Best-guess probability (",
              bgp,
              ") must fall between the lowest plausible probability (",
              lpp,
              ") and highest plausible probability (",
              hpp,
              ")."
            )
          )
        }
      }
    }
    
    # -----------------------------------------------------------------------
    # Degree of Belief
    # -----------------------------------------------------------------------
    
    if (!is.null(dob_col) &&
        !identical(dob_col, "<none>") &&
        dob_col %in% names(df)) {
      
      dob_raw <- check_df$DoB_raw[i]
      dob <- check_df$DoB[i]
      
      # Missing DoB is allowed and will be interpreted as 100
      if (!is.na(dob_raw) &&
          trimws(as.character(dob_raw)) != "") {
        
        if (is.na(dob)) {
          
          problems <- c(
            problems,
            paste0(
              label,
              ": Degree of Belief must be numeric or left blank."
            )
          )
          
        } else if (dob < 0 || dob > 100) {
          
          problems <- c(
            problems,
            paste0(
              label,
              ": Degree of Belief must be between 0 and 100."
            )
          )
        }
      }
    }
  }
  
  # -------------------------------------------------------------------------
  # Duplicate response detection
  # -------------------------------------------------------------------------
  
  duplicate_key <- paste(
    check_df$Question,
    check_df$Round,
    check_df$Participant,
    sep = " | "
  )
  
  duplicated_responses <- duplicated(duplicate_key) |
    duplicated(duplicate_key, fromLast = TRUE)
  
  if (any(duplicated_responses)) {
    
    duplicate_values <- unique(duplicate_key[duplicated_responses])
    
    for (dup in duplicate_values) {
      
      problems <- c(
        problems,
        paste0(
          "Duplicate response detected for ",
          dup,
          ". Each participant should have only one response ",
          "per question and elicitation round."
        )
      )
    }
  }
  
  # -------------------------------------------------------------------------
  # Return validation result
  # -------------------------------------------------------------------------
  
  list(
    valid = length(problems) == 0,
    problems = unique(problems)
  )
}

choose_round_facet_cols <- function(n_questions) {
  n_questions
}