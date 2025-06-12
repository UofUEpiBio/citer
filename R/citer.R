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
#' @importFrom utils citation packageVersion
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

  # Emplacing the citer version in the environment file
  env_file <- gsub(
    "[{]{2} citer_version [}]{2}", utils::packageVersion("citer"), env_file
  )

  # Writing the environment file to a temporary file
  # we will skip the first line which contains the hash placeholder
  # and replace it with the actual hash later
  temp_file <- tempfile(fileext = ".R")
  writeLines(env_file[-1], temp_file)

  # Replacing the hash placeholder with the actual hash
  hash <- tools::md5sum(temp_file)
  env_file <- gsub(
    "^(# file_version:\\s*).+",
    paste0("\\1", hash),
    env_file
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

  # Looking for .onLoad function in the package
  onload_files <- citer_search_onLoad(pkg_path)

  # If none found, we use the default environment file
  # to append the .onLoad function
  if (length(onload_files) == 0) {
    onload_files <- file.path(
      pkg_path, "R", "citer_environment.R"
    )

    cat(
      ".onLoad <- function(libname, pkgname) {\n",
      "  citation_on_load()\n",
      "}\n",
      file = onload_files,
      append = TRUE
    )
  } else {
    # If we found .onLoad files, we append the citation function
    # to the first one found
    onload_file_fn <- onload_files[1]

    # Checking if the `citation_on_load()` call is already present
    onload_file <- readLines(onload_file_fn, warn = FALSE)
    if (!any(grepl("citation_on_load\\(\\)", onload_file))) {

      line_with_onload <- grepl(
        "\\.onLoad\\s*<-", onload_file
      ) |> which()

      onload_file[line_with_onload] <- paste0(
        onload_file[line_with_onload],
        "\n  citation_on_load()"
      )

      cat(
        onload_file,
        file = onload_file_fn,
        sep = "\n"
      )

    }
  }
}

#' Function to search for `.onLoad` function in the package
#' @param path The path to the package directory. Defaults to the current
#' working directory.
#' @details
#' This function searches for the `.onLoad` function in the package
#' directory. It looks for R files in the `R` subdirectory of the package and
#' checks if the `.onLoad` function is defined in any of them. If found, it
#' returns the file names where the `.onLoad` function is defined.
#' If no `.onLoad` function is found, it returns an empty character vector.
#' @keywords internal
#' @export
citer_search_onLoad <- function(path) {

  pkg_path <- normalizePath(path, mustWork = TRUE)

  # Looking for .onLoad() function defined in the package
  r_files <- list.files(
    file.path(pkg_path, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )

  # Reading all R files and checking for .onLoad function
  has_onload <- sapply(r_files, \(fn) {

    lines <- readLines(fn, warn = FALSE)
    if (any(grepl("\\.onLoad\\s*<-", lines))) {
      # Souring the file in a new environment to see
      # if the .onLoad function is defined
      env <- new.env()
      source(fn, local = env, echo = FALSE)

      if (exists(".onLoad", envir = env, inherits = FALSE)) {
        return(TRUE)
      }

    }

    return(FALSE)

  })

  r_files[which(has_onload)]

}
