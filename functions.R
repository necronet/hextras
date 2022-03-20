#' Process an S3 file and ouptuts it to s3 agian
#'
#' @param file_name Filename of s3
#'
#' @return true if file found false otherwise
#' @export
#
source("main.R")
process <- function(file_name, bucket) {
  readRenviron(".env")
  fileName <- generateFiles(bucket = bucket, sourceFile = file_name, isAWS = T)
  list(file_name = fileName, bucket = bucket)
}
