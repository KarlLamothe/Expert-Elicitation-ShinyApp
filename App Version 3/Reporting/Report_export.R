# Reporting/Report_export.R

render_facilitator_report <- function(
    report_data, output_file){
  render(
    input = "Reporting/Facilitator_report.Rmd",
    output_file = output_file,
    params = list(report_data = report_data),
    envir = new.env(parent = globalenv()))
}