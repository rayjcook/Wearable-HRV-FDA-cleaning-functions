#' Assign subject IDs based on row blocks
#'
#' Creates a subject ID column by assigning the same ID to each block of rows.
#' This is useful when wearable data does not include an explicit subject ID.
#'
#' @param data A data frame.
#' @param n_time Number of rows expected per subject.
#' @param id_var Name of the new ID column.
#'
#' @return The input data frame with a subject ID column added.
#' @export

assign_subject_ids <- function(data,
                               vars = c("age", "gender", "height", "weight"),
                               id_var = "ID") {

  if (!all(vars %in% names(data))) {
    stop("Some variables in `vars` are not in the dataset.")
  }

  data[[id_var]] <- as.numeric(as.factor(
    do.call(paste, c(data[vars], sep = "_"))
  ))

  return(data)
}

