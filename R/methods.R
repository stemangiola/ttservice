#' join_features
#'
#'
#' @description join_features() extracts and joins information for specific
#'   features
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
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
#' # <SCE_object> %>% join_features(features=c("HLA-DRA", "LYZ"))
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



