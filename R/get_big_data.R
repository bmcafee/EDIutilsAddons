#' Fetch Large Data Files from EDI
#'
#' Download a data table published to EDI to your computer and open the dataset in your R environment.
#'
#' @param packageId A string containing a complete or partial package identifier, such as edi.1318 (without a revision number) or edi.1318.4 (with a revision number). Revision numbers included in this ID will overwrite the revision argument.
#' @param revision Either "newest" as a string or a numeric with a specific revision number. This will be overwritten by a revision number included in packageId.
#' @param filenum The number of the entity within the EDI data package.
#' @param env A string describing the repository environment. Can be: "production", "staging", or "development". If you do not know what this means, keep the default value of "production".
#' @param show_col_types A Boolean stating whether or not to display the column information when loading the data. Defaults to TRUE.
#'
#' @return A data frame of the data table uploaded to EDI
#'
#' @export
get_big_data <- function(packageId, revision = "newest", filenum = 1, env = "production", show_col_types = TRUE) {

  if (is.null(curl::nslookup("pasta.lternet.edu", error = FALSE))){
    stop("Error connecting to PASTA. Please check your internet connection and/or the status of the EDI servers.")
  }

  ## Obtaining package ID with requested revision number
  if (stringr::str_count(packageId, "\\.") < 1 | stringr::str_count(packageId, "\\.") >= 3){
    stop("Invalid Package ID")

  } else if (stringr::str_count(packageId, "\\.") == 1){
    scope <- sub("\\..*", "", packageId)
    identifier <- as.numeric(sub(".*\\.", "", packageId))
    if (revision == "newest"){
      intl_revision <- EDIutils::list_data_package_revisions(scope = scope,identifier = identifier, filter = "newest", env = env)
    } else if (typeof(revision == "double")){
      intl_revision <- revision
    } else {
      stop("Revision specified is invalid")
    }
    full_id <- paste0(scope, ".", identifier, ".", intl_revision)

  } else if (stringr::str_count(packageId, "\\.") == 2){
    full_id <- packageId
  } else {
    stop("Double check your packageId")
  }

  ## Creating the download URL
  if (env == "production"){
    env_code <- ""
  } else if (env == "staging"){
    env_code = "-s"
  } else if (env == "development"){
    env_code = "-d"
  } else {
    stop("Invalid env")
  }

  res <- EDIutils::read_data_entity_names(packageId = full_id, env = env)
  entity_url <- paste0("https://portal", env_code, ".edirepository.org/nis/dataviewer?packageid=", full_id, "&entityid=", res$entityId[filenum])
  filename <- EDIutils::read_data_entity_resource_metadata(packageId = full_id, entityId = res$entityId[filenum], as = "data.frame", env = "production")
  filename <- filename$fileName[1]

  ## Downloading the file
  direct_file <- paste0(tempdir(), "\\", filename)
  original_timeout <- getOption("timeout")
  options(timeout = 1000)
  download.file(entity_url, destfile = direct_file)
  options(timeout = original_timeout)

  data <- readr::read_csv(direct_file, show_col_types = show_col_types)
  file.remove(direct_file)


  return(data)
}
