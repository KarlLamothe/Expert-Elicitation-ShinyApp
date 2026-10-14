# Reporting/Report_narrative
generate_executive_summary <- function(report_data){
  n_questions <- length(unique(report_data$summary$Question))
  mean_ac <- round(
    mean(report_data$summary$Mean_AC, na.rm = TRUE),
    1 )
  summary_text <- paste0(
    n_questions,
    " questions were analyzed"
  )
  if("Round" %in% names(report_data$summary)){
    summary_text <- paste0(
      summary_text,
      " across two elicitation rounds. "
    )
  } else {
    summary_text <- paste0(
      summary_text,
      ". ")
  }
  
  # Largest revision
  if(!is.null(report_data$revision)){
    top_rev <- report_data$revision %>%
      slice(1)
    summary_text <- paste0(
      summary_text,
      "The largest revision following discussion occurred for Question ",
      top_rev$Question,
      " (ΔEqW = ",
      round(top_rev$Revision, 3),
      "). "
    )
  }
  
  # Most stable
  if(!is.null(report_data$stability)){
    most_stable <- report_data$stability %>%
      slice(1)
    summary_text <- paste0(
      summary_text,
      "Question ",
      most_stable$Question,
      " exhibited the greatest stability between rounds. "
    )
  }
  
  # Confidence
  summary_text <- paste0(
    summary_text,
    "Mean assessment confidence across all questions was ",
    mean_ac,
    "%. "
  )
  
  # Divergence
  if(!is.null(report_data$divergence)){
    
    top_div <- report_data$divergence %>%
      slice(1)
    summary_text <- paste0(
      summary_text,
      "The greatest divergence in expert judgement occurred for Question ",
      top_div$Question,
      ". "
    )
  }
  
  # Participation retention
  if(!is.null(report_data$participation_change)){
    
    low_part <- report_data$participation_change %>%
      slice(1)
    
    summary_text <- paste0(
      summary_text,
      "The lowest participation retention occurred for Question ",
      low_part$Question,
      " (",
      round(low_part$Participation_Retention, 1),
      "% retention).")
  }
  summary_text
}

generate_revision_narrative <- function(report_data){
  if(is.null(report_data$revision)){
    return(
      "Revision analysis is not available for single-round elicitation exercises."
    )
  }
  top_rev <- report_data$revision %>%
    slice(1)
  paste0(
    "The largest revision following discussion occurred for Question ",
    top_rev$Question,
    ", where the Equal-Weight pooled mean changed by ",
    round(top_rev$Revision, 3),
    "."
  )
}

generate_high_confidence_narrative <- function(report_data){
  top_conf <- report_data$summary %>%
    filter(Mean_AC == max(Mean_AC, na.rm = TRUE)) %>%
    slice(1)
  paste0(
    "The highest average assessment confidence was observed for Question ",
    top_conf$Question,
    " (Round ",
    top_conf$Round,
    "), with a Mean AC of ",
    round(top_conf$Mean_AC, 1),
    "%."
  )
}

generate_low_confidence_narrative <- function(report_data){
  low_conf <- report_data$summary %>%
    filter(Mean_AC == min(Mean_AC, na.rm = TRUE)) %>%
    slice(1)
  paste0(
    "The lowest average assessment confidence was observed for Question ",
    low_conf$Question,
    " (Round ",
    low_conf$Round,
    "), with a Mean AC of ",
    round(low_conf$Mean_AC, 1),
    "%."
  )
}

generate_acw_effect_narrative <- function(report_data){
  top_acw <- report_data$summary %>%
    filter(!is.na(ACW_Effect)) %>%
    arrange(desc(abs(ACW_Effect))) %>%
    slice(1)
  paste0(
    "The largest influence of assessment confidence weighting occurred for Question ",
    top_acw$Question,
    " (Round ",
    top_acw$Round,
    "), where AC-weighting changed the pooled mean by ",
    round(top_acw$ACW_Effect, 3),
    "."
  )
}

generate_ac_change_narrative <- function(report_data){
  
  if(is.null(report_data$ac_change)){
    return(
      "Assessment confidence change is not available for single-round exercises."
    )
  }
  
  top_change <- report_data$ac_change %>%
    slice(1)
  
  direction <- ifelse(
    top_change$AC_Change > 0,
    "increased",
    "decreased"
  )
  
  paste0(
    "The largest change in assessment confidence occurred for Question ",
    top_change$Question,
    ", where mean assessment confidence ",
    direction,
    " by ",
    round(abs(top_change$AC_Change), 1),
    " percentage points."
  )
}

generate_stability_narrative <- function(report_data){
  if(is.null(report_data$stability)){
    return(
      "Stability analysis is not available for single-round elicitation exercises."
    )
  }
  most_stable <- report_data$stability %>%
    slice(1)
  paste0(
    "The most stable question was Question ",
    most_stable$Question,
    ", with a change in Equal-Weight pooled mean of only ",
    round(most_stable$Abs_Revision, 3),
    " between rounds."
  )
}

generate_divergence_narrative <- function(report_data){
  top_div <- report_data$divergence %>%
    slice(1)
  paste0(
    "The greatest divergence in expert judgement occurred for Question ",
    top_div$Question,
    " (Round ",
    top_div$Round,
    "), where the Equal-Weight 90% interval spanned ",
    round(top_div$Divergence, 3),
    "."
  )
}

generate_participation_narrative <- function(report_data){
  if(is.null(report_data$participation_change)){
    return(
      "Participation retention analysis is not available for single-round exercises."
    )
  }
  lowest <- report_data$participation_change %>%
    slice(1)
  paste0(
    "The lowest participation retention occurred for Question ",
    lowest$Question,
    ", where ",
    round(lowest$Participation_Retention, 1),
    "% of Round 1 participants also contributed to Round 2."
  )
}

generate_executive_summary <- function(report_data){
  n_questions <- length(unique(report_data$summary$Question))
  mean_ac <- round(mean(report_data$summary$Mean_AC, na.rm = TRUE), 1)
  summary_text <- paste0(n_questions," questions were analyzed. ")
  if(!is.null(report_data$revision)){
    top_rev <- report_data$revision %>%
      slice(1)
    summary_text <- paste0(
      summary_text,
      "The largest revision following discussion occurred for Question ",
      top_rev$Question,
      " (ΔEqW = ",
      round(top_rev$Revision, 3),
      "). "
    )
  }
  summary_text <- paste0(
    summary_text,
    "Mean assessment confidence across questions was ",
    mean_ac,
    "%. "
  )
  
  if(!is.null(report_data$divergence)){
    top_div <- report_data$divergence %>%
      slice(1)
    summary_text <- paste0(
      summary_text,
      "The greatest divergence in expert judgement occurred for Question ",
      top_div$Question,
      "."
    )
  }
  summary_text
}

generate_warnings <- function(report_data){
  warnings <- c()
  
  # --------------------------------------------------
  # Low assessment confidence
  # --------------------------------------------------
  
  low_conf <- report_data$summary %>%
    filter(Mean_AC == min(Mean_AC, na.rm = TRUE)) %>%
    slice(1)
  if(low_conf$Mean_AC < 50){
    warnings <- c(warnings, paste0(
        "Question ",
        low_conf$Question,
        " had the lowest average assessment confidence (",
        round(low_conf$Mean_AC, 1),
        "%)."))}
  
  # --------------------------------------------------
  # High divergence
  # --------------------------------------------------
  
  if(!is.null(report_data$divergence)){
    top_div <- report_data$divergence %>%
      slice(1)
    if(top_div$Divergence > 0.20){
      warnings <- c(warnings, paste0(
          "Question ",
          top_div$Question,
          " exhibited substantial divergence in expert judgement (90% interval width = ",
          round(top_div$Divergence, 3),
          ")."))
    }
  }
  
  # --------------------------------------------------
  # Low participation retention
  # --------------------------------------------------
  
  if(!is.null(report_data$participation_change)){
    low_part <- report_data$participation_change %>%
      slice(1)
    if(low_part$Participation_Retention < 80){
      warnings <- c(warnings, paste0(
          "Question ",
          low_part$Question,
          " had low participant retention (",
          round(low_part$Participation_Retention, 1),
          "%)."))
    }
  }
  
  # --------------------------------------------------
  # Large AC weighting effect
  # --------------------------------------------------
  
  top_acw <- report_data$summary %>%
    filter(!is.na(ACW_Effect)) %>%
    arrange(desc(abs(ACW_Effect))) %>%
    slice(1)
  
  if(nrow(top_acw) > 0){
    if(abs(top_acw$ACW_Effect) > 0.01){
      warnings <- c(warnings, paste0(
          "Question ",
          top_acw$Question,
          " exhibited a notable assessment confidence weighting effect (",
          round(top_acw$ACW_Effect, 3),
          ")."))
    }
  }
  
  # --------------------------------------------------
  # Large revision following discussion
  # --------------------------------------------------
  
  if(!is.null(report_data$revision)){
    top_rev <- report_data$revision %>%
      slice(1)
    if(abs(top_rev$Revision) > 0.05){
      warnings <- c(warnings, paste0(
          "Question ",
          top_rev$Question,
          " exhibited substantial revision following discussion (ΔEqW = ",
          round(top_rev$Revision, 3),
          ")."))
    }
  }
  
  # --------------------------------------------------
  # No warnings detected
  # --------------------------------------------------
  if(length(warnings) == 0){
    warnings <- "No notable facilitator warnings were identified."
  }
  warnings
}
