#' Check and Install Required Packages
#'
#' This function checks if specified packages are installed and installs them
#' if they are not. It uses BiocManager for installation, which handles both
#' CRAN and Bioconductor packages.
#'
#' @param packages Character vector of package names to check and install
#'
#' @return NULL (invisibly). Called for side effects.
#'
#' @export
#'
#' @importFrom rlang check_installed
#'
#' @examples
#' \dontrun{
#' check_and_install_packages(c("dplyr", "ggplot2"))
#' }
check_and_install_packages <- function(packages) {
  check_installed(
    pkg = packages,
    action = function(...) {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager", repos = getOption("repos"))
      }
      BiocManager::install(..., ask = FALSE, update = FALSE)
    }
  )
  invisible(NULL)
}
