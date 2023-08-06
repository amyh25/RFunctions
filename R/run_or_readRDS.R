#' run_or_readRDS
#'
#' A kind of hack cache-ing function that I use in my notebook for
#' convenience. Will read in an RDS object at the specified path if it exists,
#' otherwise will run the specified function.
#'
#' @param path a path to look for an RDS object with the saved results
#' @param f a function that will output results that you want saved
#' @param verbose boolean specifying verbosity
#' @return a tibble
#' 
#' @export

run_or_readRDS <- function(path, f, verbose = TRUE, overwrite = FALSE) {
  if (!file.exists(path)) {
    if (verbose) message(paste0("File at ", path, " does not exist. Running function..."))
    out <- f()
    saveRDS(out, path)
    if (verbose) message(paste0("Saving at ", path, "."))
  } else if (overwrite) {
    if (verbose) message(paste0("File found. Overwriting."))
    out <- f()
    saveRDS(out, path)
  } else  {
    if (verbose) message("Found file at ", path, ". Reading...")
    out <- readRDS(path)
    if (verbose) message("Done!")
  }
  return(out)
}
