# Reporting/Report_data.R

build_report_data <- function(results_obj){
  summary_tbl <- results_obj$summaries
  participation <- tibble::tibble(
    Metric = c(
      "Questions",
      "Mean Participants",
      "Mean AC Response Rate"
    ),
    Value = c(
      nrow(summary_tbl),
      round(mean(summary_tbl$N_Participants), 1),
      round(mean(summary_tbl$AC_Response_Rate, na.rm = TRUE) * 100, 1)
    )
  )
  
  revision_tbl <- calculate_revision_summary(summary_tbl)
  ac_change_tbl <- calculate_ac_change_summary(summary_tbl)
  stability_tbl <- calculate_stability_summary(summary_tbl)
  divergence_tbl <- calculate_divergence_summary(summary_tbl)
  participation_change_tbl <- calculate_participation_summary(summary_tbl)
  list(metadata = list(generated = Sys.time(), app_version = "2.0"),
    participation = participation,
    summary = summary_tbl,
    revision = revision_tbl,
    stability = stability_tbl,
    ac_change = ac_change_tbl,
    divergence = divergence_tbl,
    participation_change = participation_change_tbl,
    results = results_obj)
}