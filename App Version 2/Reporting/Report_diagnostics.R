# Reporting/Report_diagnostics.R

calculate_revision <- function(data) {
  NULL
}

calculate_stability <- function(data) {
  NULL
}

calculate_spread <- function(data) {
  NULL
}

calculate_confidence <- function(data) {
  NULL
}

calculate_participation <- function(data) {
  NULL
}

calculate_revision_summary <- function(summary_tbl){
  
  if(!"Round" %in% names(summary_tbl)){
    return(NULL)
  }
  
  revision_tbl <- summary_tbl %>%
    dplyr::select(
      Question,
      Round,
      EqW_Mean
    ) %>%
    tidyr::pivot_wider(
      names_from = Round,
      values_from = EqW_Mean
    )
  
  # Must have both rounds
  if(!all(c("1", "2") %in% names(revision_tbl))){
    return(NULL)
  }
  
  revision_tbl %>%
    dplyr::mutate(
      Revision = `2` - `1`,
      Abs_Revision = abs(Revision)
    ) %>%
    dplyr::arrange(desc(Abs_Revision))
  
}

calculate_ac_change_summary <- function(summary_tbl){
  
  if(!"Round" %in% names(summary_tbl)){
    return(NULL)
  }
  
  ac_tbl <- summary_tbl %>%
    dplyr::select(
      Question,
      Round,
      Mean_AC
    ) %>%
    tidyr::pivot_wider(
      names_from = Round,
      values_from = Mean_AC
    )
  
  if(!all(c("1", "2") %in% names(ac_tbl))){
    return(NULL)
  }
  
  ac_tbl %>%
    dplyr::mutate(
      AC_Change = `2` - `1`,
      Abs_AC_Change = abs(AC_Change)
    ) %>%
    dplyr::arrange(desc(Abs_AC_Change))
  
}

calculate_stability_summary <- function(summary_tbl){
  revision_tbl <- calculate_revision_summary(summary_tbl)
  if(is.null(revision_tbl)){
    return(NULL)
  }
  revision_tbl %>%
    arrange(Abs_Revision)
}

calculate_spread_summary <- function(summary_tbl){
  summary_tbl %>%
    mutate(Spread = EqW_95th - EqW_5th) %>%
    arrange(desc(Spread))
}

calculate_participation_summary <- function(summary_tbl){
  
  if(!("Round" %in% names(summary_tbl))){
    return(NULL)
  }
  
  part_tbl <- summary_tbl %>%
    dplyr::select(
      Question,
      Round,
      N_Participants
    ) %>%
    tidyr::pivot_wider(
      names_from = Round,
      values_from = N_Participants
    )
  
  if(!all(c("1", "2") %in% names(part_tbl))){
    return(NULL)
  }
  
  part_tbl %>%
    dplyr::mutate(
      Participant_Change = `2` - `1`,
      Participation_Retention = (`2` / `1`) * 100
    ) %>%
    dplyr::arrange(Participation_Retention)
  
}

