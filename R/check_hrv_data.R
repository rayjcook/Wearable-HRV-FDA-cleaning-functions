#' Check wearable HRV data before modeling
#'
#' Checks whether the data contains the required columns, reports missing
#' values, summarizes the number of observations per subject, and gives a
#' quick overview of activity levels.
#'
#' @param data A data frame containing wearable HRV data.
#' @param id_var Name of the subject ID column.
#' @param response_var Name of the heart rate or HRV response column.
#' @param activity_var Name of the activity column.
#' @param required_vars Character vector of additional required variables.
#'
#' @return A list containing summary information about the dataset.
#' @export
check_hrv_data <- function(data,
                           id_var = "ID",
                           response_var = "Applewatch.Heart_LE",
                           activity_var = "activity_trimmed",
                           required_vars = c("age", "gender", "height", "weight")) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  vars_to_check <- unique(c(id_var, response_var, activity_var, required_vars))
  missing_cols <- setdiff(vars_to_check, names(data))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "The following required columns are missing:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  missing_summary <- sapply(vars_to_check, function(x) sum(is.na(data[[x]])))
  missing_summary <- as.data.frame(missing_summary)
  missing_summary$variable <- rownames(missing_summary)
  rownames(missing_summary) <- NULL
  names(missing_summary)[1] <- "missing_count"
  missing_summary <- missing_summary[, c("variable", "missing_count")]

  obs_per_subject <- table(data[[id_var]])
  obs_per_subject_df <- data.frame(
    subject_id = names(obs_per_subject),
    n_obs = as.integer(obs_per_subject),
    row.names = NULL
  )

  activity_counts <- table(data[[activity_var]])
  activity_counts_df <- data.frame(
    activity = names(activity_counts),
    n_rows = as.integer(activity_counts),
    row.names = NULL
  )

  summary_list <- list(
    n_rows = nrow(data),
    n_subjects = length(unique(data[[id_var]])),
    response_var = response_var,
    min_obs_per_subject = min(obs_per_subject),
    max_obs_per_subject = max(obs_per_subject),
    mean_obs_per_subject = mean(obs_per_subject),
    missing_summary = missing_summary,
    activity_counts = activity_counts_df,
    obs_per_subject = obs_per_subject_df
  )

  class(summary_list) <- "hrv_data_check"
  return(summary_list)
}
