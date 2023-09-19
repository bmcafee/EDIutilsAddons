#' Fetch Data from EDI
#'
#' Import a data table published to EDI directly into your R environment.
#'
#' @param packageId A string containing a complete or partial package identifier, such as edi.1318 (without a revision number) or edi.1318.4 (with a revision number). Revision numbers included in this ID will overwrite the revision argument.
#' @param revision Either "newest" as a string or a numeric with a specific revision number. This will be overwritten by a revision number included in packageId.
#' @param filenum The number of the entity within the EDI data package.
#' @param env A string describing the repository environment. Can be: "production", "staging", or "development". If you do not know what this means, keep the default value of "production".
#'
#' @return A data frame of the data table uploaded to EDI
#'
#' @export
get_data <- function(packageId, revision = "newest", filenum = 1, env = "production") {

  ## Obtaining package ID with requested revision number
  if (stringr::str_count(packageId, "\\.") < 1 | stringr::str_count(packageId, "\\.") >= 3) {
    stop("Invalid Package ID")
  } else if (stringr::str_count(packageId, "\\.") == 1) {
    scope <- sub("\\..*", "", packageId)
    identifier <- as.numeric(sub(".*\\.", "", packageId))
    if (revision == "newest") {
      intl_revision <- EDIutils::list_data_package_revisions(scope = scope, identifier = identifier, filter = "newest", env = env)
    } else if (typeof(revision == "double")) {
      intl_revision <- revision
    } else {
      stop("Revision specified is invalid")
    }
    full_id <- paste0(scope, ".", identifier, ".", intl_revision)
  } else if (stringr::str_count(packageId, "\\.") == 2) {
    full_id <- packageId
  } else {
    stop("Double check your packageId")
  }

  ## Loading the data entity
  res <- EDIutils::read_data_entity_names(packageId = full_id, env = env)
  raw <- EDIutils::read_data_entity(packageId = full_id, entityId = res$entityId[filenum], env = env)
  data <- readr::read_csv(file = raw)

  return(data)
}
