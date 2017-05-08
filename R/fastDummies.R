#' Fast creation of dummy variables
#'
#' @param dataset
#' data.table or data.frame
#'
#' @param select.columns
#' Vector of column names that you want to create dummy variables from.
#' Default uses all character or factor columns.
#' @param ignore.columns
#' Vector of column names to ignore. Default ignores all numeric columns.
#' @param remove.original
#' Removes the columns used to make dummy variables.
#' Columns that are not used to make dummy variables are not affected.
#' @param dummy.columns.only
#' Removes all columns that didn't create dummy columns (i.e. numeric columns).
#' @param remove.first.dummy
#' Removes the first dummy of every variable that only n-1 Dummies remain
#' @param return.type
#' Type of data you want back. Default is data.table (better for use
#' with large data). Other options are data.frame or matrix.
#'
#' @return
#' data.table, data.frame, or matrix depending on input for return.type.
#' data.table is default.
#' @export
#'
#' @examples
#' data(dummies.example)
#' example <- fastDummy(dummies.example)
#'
#' # Return data.frame
#' example <- fastDummy(dummies.example, return.type = "data.frame")
#'
#' # Only keep created dummy columns
#' example <- fastDummy(dummies.example, dummy.columns.only = TRUE)
#'
#' # Only keep SEX and RACE columns
#' example <- fastDummy(dummies.example, select.columns = c("Sex", "RACE"))
#'
#' # Keep all except SEX column
#' example <- fastDummy(dummies.example, ignore.columns = "SEX")
#'
#' # Removes the first dummy from every category. Avoids perfect
#' # multicollinearity issues in models.
#' example <- fastDummy(dummies.example, remove.first.dummy = TRUE)
fastDummy <- function(dataset,
                    select.columns = NULL,
                    ignore.columns = NULL,
                    remove.original = TRUE,
                    dummy.columns.only = FALSE,
                    remove.first.dummy = FALSE,
                    return.type = "data.table") {

  if (!return.type %in% c("data.table", "data.frame", "matrix")) {
    stop("Return type must be 'data.table', 'data.frame', or 'matrix'")
  }

  if (!data.table::is.data.table(dataset)) {
    dataset <- data.table::as.data.table(dataset)
  }

  if (!is.null(select.columns) && !is.character(select.columns)) {
    stop("select.columns input must be characters")
  }

  if (!is.null(ignore.columns) && !is.character(ignore.columns)) {
    stop("ignore.columns input must be characters")
  }


  char.cols <- names(dataset)[sapply(dataset, class) %in%
                                c("character", "factor")]

  if (!is.null(select.columns)) {
    char.cols <- select.columns
    char.cols <- char.cols[char.cols %in% names(dataset)]
    if (length(char.cols) == 0) {
      stop("No remaining columns. Please use correct column names.")
    }
  }

  if (!is.null(ignore.columns)) {
    char.cols <- char.cols[!char.cols %in% ignore.columns]
    if (length(char.cols) == 0) {
      stop("No remaining columns. Please use correct column names.")
    }
  }

  if (dummy.columns.only) {
    to.remove <- names(dataset)[!names(dataset) %in% char.cols]
    dataset[, (to.remove) := NULL]
  }

  for (col.name in char.cols) {
    unique.vals <- unique(dataset[, get(col.name)])

    if (remove.first.dummy) {
      unique.vals = unique.vals[-1]
    }

    dataset[, (paste0(col.name, ".", unique.vals)) := 0]
    for (unique.values in unique.vals) {
      dataset[get(col.name) == unique.values,
              (paste0(col.name, ".", unique.values)) := 1]
    }
  }

  if (remove.original) {
    dataset[, (char.cols) := NULL]
  }

  if (return.type == "data.table") {
    return(dataset)
  } else if (return.type == "data.frame") {
    return(as.data.frame(dataset))
  } else if (return.type == "matrix") {
    return(as.matrix(dataset))
  }
}

#' United States Census data for 2015
#'
#' A dataset containing Census results from the American Community Survey 2015
#'
#' @format A data frame with 100,000 rows and 17 variables:
#' \describe{
#'   \item{YEAR}{Year of the survey}
#'   \item{STATEFIP}{FIPS ID for the state}
#'   \item{COUNTYFIPS}{FIPS ID for the county}
#'   \item{OWNERSHP}{Respondent's ownership status for their home}
#'   \item{PERWT}{Survey weight for the respondent}
#'   \item{NCHILD}{Number of children in the home}
#'   \item{SEX}{Sex of the respondent}
#'   \item{AGE}{Age of the respondent}
#'   \item{MARST}{Marital status of the respondent}
#'   \item{RACE}{Race of the respondent}
#'   \item{HISPAN}{If the respondent is hispanic}
#'   \item{BPL}{Birthplace of the respondent}
#'   \item{EDUC}{Education of the respondent}
#'   \item{EMPSTAT}{Employment status of the respondent}
#'   \item{FTOTINC}{Family yearly income}
#'   \item{POVERTY}{If they are below the poverty line}
#'   \item{MOVEDIN}{When the respondent moved into their home}
#' }
#' @source \url{https://usa.ipums.org/usa-action/variables/group}
"dummies.example"
