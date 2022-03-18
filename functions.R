#' Process an S3 file and ouptuts it to s3 agian
#'
#' @param s3_path Path of the s3 file
#'
#' @return true if file found false otherwise
#' @export
#
process <- function(s3_path) {
  list(s3_file = s3_path)
}