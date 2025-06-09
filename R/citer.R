#' Create/updates the `citer_environment.R` file in a package
#' @param path The path to the package directory. Defaults to the current
#' working directory.
#' @param overwrite If `TRUE`, the existing `citer_environment.R` file will be
#' overwritten if it exists. If `FALSE`, an error will be raised if the file
#' already exists.
#' @details
#' This function creates or updates the `citer_environment.R` file in the
#' specified package directory. The file contains the citation information for
#' the package and is used to set up the citation function when the package is
#' loaded. The file is generated from the `citer` package's template and
#' includes the package name and a hash of the file content to ensure it is
#' up to date.
#'
#' This function is called by the the `citer` setup functions.
#' @keywords internal
#' @export
#' @importFrom tools md5sum
#' @importFrom utils citation
citer_write_environment <- function(path = ".", overwrite = TRUE) {
  # Ensure the path is absolute
  pkg_path <- normalizePath(path, mustWork = TRUE)

  # Checking if the file `citer_environment.R` exists
  pkg_env_file <- file.path(pkg_path, "R", "citer_environment.R")

  # Taking the package name from the directory name
  pkg_name <- basename(pkg_path)

  if (file.exists(pkg_env_file) && !overwrite) {
    stop(
      "The file 'citer_environment.R' already exists. ",
      "Use `overwrite = TRUE` to overwrite it."
    )
  }

  # Retrieving the environment file
  env_file <- system.file("citer_environment.R", package = "citer") |>
    readLines(warn = FALSE)

  # Emplacing the package name in the environment file
  env_file <- gsub(
    "[{]{2} pkg_name [}]{2}", pkg_name, env_file
  )

  # Writing the environment file to a temporary file
  # we will skip the first line which contains the hash placeholder
  # and replace it with the actual hash later
  temp_file <- tempfile(fileext = ".R")
  writeLines(env_file[-1], temp_file)

  # Replacing the hash placeholder with the actual hash
  hash <- tools::md5sum(temp_file)
  env_file <- gsub(
    "[{]{2} pkg_hash [}]{2}", hash, env_file
  )

  # Saving the file in the package directory (if the
  # hash is different)
  if (file.exists(pkg_env_file)) {
    # If the file exists, we check if the hash is different
    existing_hash <- gsub(
      "^.+:\\s*", "",
      readLines(pkg_env_file, warn = FALSE)[1]
    )

    if (existing_hash == hash) {
      message("The environment file is already up to date.")
      return(invisible())
    }

  }

  # Write the environment file to the package directory
  writeLines(env_file, pkg_env_file)
  message(
    "The environment file has been written to '", pkg_env_file,
    "'."
  )

  invisible(NULL)

}

#' Set up the citation function on loading the package
#' @param path The path to the package directory. Defaults to the current
#' working directory.
#' @export
citer_on_load <- function(path = ".") {

  pkg_path <- normalizePath(path, mustWork = TRUE)
  citer_write_environment(pkg_path, overwrite = TRUE)

}
