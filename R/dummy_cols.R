#' Fast creation of dummy variables
#'
#' dummy_cols() quickly creates dummy (binary) columns from character and
#' factor type columns in the inputted data. This function is useful for
#' statistical analysis when you want binary columns rather than
#' character columns.
#'
#' @family dummy functions
#' @seealso \code{\link{dummy_rows}} For creating dummy rows
#'
#' @param data
#' An object with the data set you want to make dummy columns from.
#' @param select_columns
#' Vector of column names that you want to create dummy variables from.
#' If NULL (default), uses all character and factor columns.
#' @param remove_first_dummy
#' Removes the first dummy of every variable that only n-1 Dummies remain.
#' This avoids multicollinearity issues in models.
#' @return
#' A data.frame with same number of rows as inputted data and original
#' columns plus the newly created dummy columns.
#' @export
#' @examples
#' crime <- data.frame(city = c("SF", "SF", "NYC"),
#'     year = c(1990, 2000, 1990),
#'     crime = 1:3)
#' dummy_cols(crime)
#' # Include year column
#' dummy_cols(crime, select_columns = c("city", "year"))
#' # Remove first dummy for each pair of dummy columns made
#' dummy_cols(crime, select_columns = c("city", "year"),
#'     remove_first_dummy = TRUE)
dummy_cols <- function(data,
                       select_columns = NULL,
                       remove_first_dummy = FALSE) {

  stopifnot(is.null(select_columns) || is.character(select_columns),
            select_columns != "",
            is.logical(remove_first_dummy), length(remove_first_dummy) == 1)


  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Grabs column names that are character or factor class -------------------
  if (!is.null(select_columns)) {
    char_cols <- select_columns
    cols_not_in_data <- char_cols[!char_cols %in% names(data)]
    char_cols <- char_cols[!char_cols %in% cols_not_in_data]
    if (length(char_cols) == 0) {
      stop("select_columns is/are not in data. Please check data and spelling.")
    }
  } else if (ncol(data) == 1) {
    char_cols <- names(data)
  } else {
    char_cols <- sapply(data, class)
    char_cols <- char_cols[char_cols %in% c("factor", "character")]
    char_cols <- names(char_cols)
  }

  if (length(char_cols) == 0 && is.null(select_columns)) {
    stop("No character or factor columns found. Please use select_columns to choose columns.")
  }

  if (!is.null(select_columns) && length(cols_not_in_data) > 0) {
    warning("NOTE: The following select_columns input(s) is not a column in data.\n", paste0(names(cols_not_in_data), "\t"))
  }


  for (col_name in char_cols) {
    unique_vals <- as.character(unique(data[[col_name]]))

    if (remove_first_dummy) {
      unique_vals <- unique_vals[-1]
    }

    data.table::set(data, j = paste0(col_name, "_", unique_vals), value = 0L)
    for (unique_value in unique_vals) {
      data.table::set(data, i = which(as.character(data[[col_name]])
                                      %in% unique_value),
                      j = paste0(col_name, "_", unique_value), value = 1L)
    }
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  return(data)

}

#' Fast creation of dummy variables
#'
#' dummy_columns() quickly creates dummy (binary) columns from character and
#' factor type columns in the inputted data. This function is useful for
#' statistical analysis when you want binary columns rather than
#' character columns.
#'
#' @family dummy functions
#' @seealso \code{\link{dummy_rows}} For creating dummy rows
#'
#'
#' @inheritParams dummy_cols
#' @export
#' @examples
#' crime <- data.frame(city = c("SF", "SF", "NYC"),
#'     year = c(1990, 2000, 1990),
#'     crime = 1:3)
#' dummy_cols(crime)
#' # Include year column
#' dummy_cols(crime, select_columns = c("city", "year"))
#' # Remove first dummy for each pair of dummy columns made
#' dummy_cols(crime, select_columns = c("city", "year"),
#'     remove_first_dummy = TRUE)
dummy_columns <- dummy_cols