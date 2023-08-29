#' Fetch Data from EDI
#'
#' This is the description.
#'
#' @param packageId A complete or partial package identifier, such as edi.200 or edi.200.12
#' @param revision A description
#' @param filenum A description
#' @param env A string describing the repository environment. Can be: "production", "staging", or "development".
#'
#' @export
get_data <- function(packageId, revision = "newest", filenum = 1, env = "production") {
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

  res <- EDIutils::read_data_entity_names(packageId = full_id, env = env)
  raw <- EDIutils::read_data_entity(packageId = full_id, entityId = res$entityId[filenum], env = env)
  data <- readr::read_csv(file = raw)

  return(data)
}