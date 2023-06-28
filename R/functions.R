# Negation
not = function(is){	!is }

#' Convert array of quosure (e.g. c(col_a, col_b)) into character vector
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom rlang quo_name
#' @importFrom rlang quo_squash
#'
#' @param v A array of quosures (e.g. c(col_a, col_b))
#'
#' @return A character vector
quo_names <- function(v) {
  
  v = quo_name(quo_squash(v))
  gsub('^c\\(|`|\\)$', '', v) |>
    strsplit(', ') |>
    unlist()
}

#' Subset columns
#'
#' @export
#' 
#' @importFrom rlang enquo
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @importFrom dplyr distinct_at
#' @importFrom magrittr equals
#' @importFrom dplyr vars
#' 
#' @param .data A tibble
#' @param .column A vector of column names
#'
#' @return A tibble
select_all_related_columns = function(.data, .column)	{
  
  # Make col names
  .column = enquo(.column)
  
  # Check if column present
  if(quo_names(.column) %in% colnames(.data) |> all() |> not())
    stop("nanny says: some of the .column specified do not exist in the input data frame.")
  
    # x-annotation df
    n_x = .data |> distinct_at(vars(!!.column)) |> nrow()
  
  # element wise columns
  my_columns = 
    .data |>
    select(-!!.column) |>
    colnames() |>
    map(~ {
      if(
        .data |>
        distinct_at(vars(!!.column, .x)) |>
        nrow() |>
        equals(n_x)
      ) .x
      else NULL
    }) 
  
  my_columns = 
    my_columns[lengths(my_columns) != 0] |>
    unlist()
  
  .data |>
    
    # Selecting the right columns
    select(	!!.column,	my_columns	) |>
    distinct()
}



