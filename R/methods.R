#' join_features
#'
#'
#' @description join_features() extracts and joins information for specific
#'   features
#'
#'
#'
#' @param .data A tidy SingleCellExperiment object
#' @param features A vector of feature identifiers to join
#' @param all If TRUE return all
#' @param exclude_zeros If TRUE exclude zero values
#' @param shape Format of the returned table "long" or "wide"
#' @param ... Parameters to pass to join wide, i.e. assay name to extract feature abundance from and gene prefix, for shape="wide"
#'
#' @details This function extracts information for specified features and
#'   returns the information in either long or wide format.
#'
#' @return A `tbl` containing the information.for the specified features
#'
#' @examples
#'
#' print("this is a method definition. Example is not applicable")
#' # <SCE_object> |> join_features(features=c("HLA-DRA", "LYZ"))
#'
#' @docType methods
#' @rdname join_features
#' @name join_features
#'
#' @export
setGeneric("join_features", function(.data,
                                       features = NULL,
                                       all = FALSE,
                                       exclude_zeros = FALSE,
                                       shape = "long",
                                       ...
                                       )
  standardGeneric("join_features"))

#' Aggregate cells
#'
#' @description Combine cells into groups based on shared variables and aggregate feature counts.
#'
#' @docType methods
#' 
#' @name aggregate_cells
#' @rdname aggregate_cells
#'
#' @importFrom Matrix rowSums
#'
#' @param .data A tidySingleCellExperiment object
#' @param .sample A vector of variables by which cells are aggregated
#' @param slot The slot to which the function is applied
#' @param assays The assay to which the function is applied
#' @param aggregation_function The method of cell-feature value aggregation
#'
#' @return A tibble object
#'
#' @examples
#' 
#' print("pbmc_small |> aggregate_cells(c(groups, ident), assays = \"counts\")")
#'
#' @export
#'
setGeneric("aggregate_cells", function(.data,
                                       .sample = NULL,
                                       slot = "data",
                                       assays = NULL,
                                       aggregation_function = Matrix::rowSums)
  standardGeneric("aggregate_cells"))


#' #' Efficiently bind multiple data frames by row and column
#'
#' This is an efficient implementation of the common pattern of
#' `do.call(rbind, dfs)` or `do.call(cbind, dfs)` for binding many
#' data frames into one.
#'
#' The output of `bind_rows()` will contain a column if that column
#' appears in any of the inputs.
#'
#' @param ... Data frames to combine.
#'
#'   Each argument can either be a data frame, a list that could be a data
#'   frame, or a list of data frames.
#'
#'   When row-binding, columns are matched by name, and any missing
#'   columns will be filled with NA.
#'
#'   When column-binding, rows are matched by position, so all data
#'   frames must have the same number of rows. To match by value, not
#'   position, see mutate-joins.
#' @param .id Data frame identifier.
#'
#'   When `.id` is supplied, a new column of identifiers is
#'   created to link each row to its original data frame. The labels
#'   are taken from the named arguments to `bind_rows()`. When a
#'   list of data frames is supplied, the labels are taken from the
#'   names of the list. If no names are found a numeric sequence is
#'   used instead.
#' @param add.cell.ids from Seurat 3.0 A character vector of length(x = c(x, y)). Appends the corresponding values to the start of each objects' cell names.
#'
#' @return `bind_rows()` and `bind_cols()` return the same type as
#'   the first input, either a data frame, `tbl_df`, or `grouped_df`.
#' @examples
#' print("small_pbmc |> bind_rows(small_pbmc)")
#'
#'
#'
#' @rdname dplyr-methods
#' @name bind_rows
#'
#'
#' @export
#'
bind_rows <- function(..., .id = NULL,  add.cell.ids = NULL) {
  UseMethod("bind_rows")
}

#' @export
#'
#' @importFrom dplyr bind_rows
bind_rows.default <-  function(..., .id = NULL,  add.cell.ids = NULL)
{
  dplyr::bind_rows(..., .id = .id)
}

#' @export
#'
#' @importFrom dplyr bind_rows
bind_rows.data.frame <-  function(..., .id = NULL,  add.cell.ids = NULL)
{
  dplyr::bind_rows(..., .id = .id)
}

#' @export
#'
#' @importFrom dplyr bind_rows
bind_rows.list <-  function(..., .id = NULL,  add.cell.ids = NULL)
{
  dplyr::bind_rows(..., .id = .id)
}
#' Efficiently bind multiple data frames by row and column
#'
#' This is an efficient implementation of the common pattern of
#' `do.call(rbind, dfs)` or `do.call(cbind, dfs)` for binding many
#' data frames into one.
#'
#' The output of `bind_rows()` will contain a column if that column
#' appears in any of the inputs.
#'
#' @param ... Data frames to combine.
#'
#'   Each argument can either be a data frame, a list that could be a data
#'   frame, or a list of data frames.
#'
#'   When row-binding, columns are matched by name, and any missing
#'   columns will be filled with NA.
#'
#'   When column-binding, rows are matched by position, so all data
#'   frames must have the same number of rows. To match by value, not
#'   position, see mutate-joins.
#' @param .id Data frame identifier.
#'
#'   When `.id` is supplied, a new column of identifiers is
#'   created to link each row to its original data frame. The labels
#'   are taken from the named arguments to `bind_rows()`. When a
#'   list of data frames is supplied, the labels are taken from the
#'   names of the list. If no names are found a numeric sequence is
#'   used instead.
#' @param add.cell.ids from Seurat 3.0 A character vector of length(x = c(x, y)). Appends the corresponding values to the start of each objects' cell names.
#'
#' @return `bind_rows()` and `bind_cols()` return the same type as
#'   the first input, either a data frame, `tbl_df`, or `grouped_df`.
#' @examples
#' print("small_pbmc |> bind_cols(annotation_column)")
#'
#'
#' @rdname dplyr-methods
#' @name bind_cols
#'
NULL

#' @rdname dplyr-methods
#'
#' @inheritParams bind_cols
#'
#' @export
#' @export
#'
bind_cols <- function(..., .id = NULL) {
  UseMethod("bind_cols")
}

#' @export
#'
#' @importFrom dplyr bind_cols
bind_cols.default <-  function(..., .id = NULL)
{
  dplyr::bind_cols(..., .id = .id)
}

#' @export
#'
#' @importFrom dplyr bind_cols
bind_cols.data.frame <-  function(..., .id = NULL)
{
  dplyr::bind_cols(..., .id = .id)
}

#' @export
#'
#' @importFrom dplyr bind_cols
bind_cols.list <-  function(..., .id = NULL)
{
  dplyr::bind_cols(..., .id = .id)
}