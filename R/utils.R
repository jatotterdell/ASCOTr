#' @title findfirst
#'
#' @param x Logical vector
#' @param v Default if no TRUE elements
#' @return First TRUE element of x
findfirst <- function(x, v = NA) {
  j <- which(x)
  if (length(j)) min(j) else v
}


#' @title save_derived_dataset
#' @description
#' Save a derived dataset to the appropriate directory (ANTICOAG_DATA)
#'
#' @param dir The derived data directory
#' @param dat The dataset
#' @param fn The file name which will be appended to `dir`.
#' @return Nothing
#' @export
save_derived_dataset <- function(dir, dat, fn) {
  path <- file.path(dir, fn)
  saveRDS(dat, path)
}
