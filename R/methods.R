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
#' print("this is a method generics Example is not applicable")
#' # <object> |> join_features(features=c("HLA-DRA", "LYZ"))
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
#' @param ... Used for future extendibility
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
                                       aggregation_function = Matrix::rowSums,
                                      ...)
  standardGeneric("aggregate_cells"))

#' Append samples
#'
#' @description Append multiple samples or datasets together, combining their data while preserving sample-specific information.
#'
#' @docType methods
#'
#' @name append_samples
#' @rdname append_samples
#'
#' @param x A genomic data container to combine with others
#' @param ... Additional genomic data containers to combine
#'
#'   Each argument should be a genomic data object such as a SummarizedExperiment,
#'   SingleCellExperiment, SpatialExperiment, or Seurat object (provided that
#'   the appropriate method extensions are available). You may also provide a list
#'   of such objects.
#'
#'   When row-binding, features (e.g., genes) are matched by name, and any missing
#'   features will be filled with NA or zero as appropriate for the container.
#'
#'   When column-binding, samples (e.g., cells) are matched by position, so all objects
#'   must have the same number of features. To match by value, not position, see mutate-joins.
#'
#' @return A combined genomic object
#'
#' @examples
#'
#' print("combined_data <- append_samples(sample1, sample2, .id = \"sample\")")
#'
#' @export
#'
append_samples <- function(x, ...) UseMethod("append_samples")


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

#' @name plot_ly
#' @rdname plot_ly
#' @inherit plotly::plot_ly
#' @importFrom plotly plot_ly
#' 
#' @export
plot_ly <- function(data=data.frame(), ..., type=NULL, name=NULL,
                    color=NULL, colors=NULL, alpha=NULL,
                    stroke=NULL, strokes=NULL, alpha_stroke=1,
                    size=NULL, sizes=c(10, 100),
                    span=NULL, spans=c(1, 20),
                    symbol=NULL, symbols=NULL,
                    linetype=NULL, linetypes=NULL,
                    split=NULL, frame=NULL,
                    width=NULL, height=NULL, source="A") {
  UseMethod("plot_ly")
}

#' @importFrom plotly plot_ly
#' 
#' @export
plot_ly.default <- function(data=data.frame(), 
                            ..., type=NULL, name=NULL,
                            color=NULL, colors=NULL, alpha=NULL,
                            stroke=NULL, strokes=NULL, alpha_stroke=1,
                            size=NULL, sizes=c(10, 100),
                            span=NULL, spans=c(1, 20),
                            symbol=NULL, symbols=NULL,
                            linetype=NULL, linetypes=NULL,
                            split=NULL, frame=NULL,
                            width=NULL, height=NULL, source="A") {
  
  class(data) <- class(data)[!class(data) %in% "tbl_df"]
  
  plotly::plot_ly(data, ...,
                  type=type, name=name,
                  color=color, colors=colors, alpha=alpha,
                  stroke=stroke, strokes=strokes, alpha_stroke=alpha_stroke,
                  size=size, sizes=sizes,
                  span=span, spans=spans,
                  symbol=symbol, symbols=symbols,
                  linetype=linetype, linetypes=linetypes,
                  split=split, frame=frame,
                  width=width, height=height, source=source)
}

#' Add class to abject
#'
#' @keywords internal
#' @export
#'
#' @param var A tibble
#' @param name A character name of the attribute
#'
#' @return A tibble with an additional attribute
add_class = function(var, name) {
  
  if(!name %in% class(var)) class(var) <- c(name, class(var))
  
  var
}

#' Remove class to abject
#'
#' @keywords internal
#' @noRd
#'
#'
#' @param var A tibble
#' @param name A character name of the class
#'
#' @return A tibble with an additional attribute
drop_class = function(var, name) {
  class(var) <- class(var)[!class(var)%in%name]
  var
}