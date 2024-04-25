#' Define constructor function for creating text data objects (S3) for testing purposes
#'
#' @param text User inputted text
#'
#' @return list of \code{text}
#' @export
#'
#' @examples
#' \dontrun{
#' createTextData(processed_text)
#' }
createTextData <- function(text) {
  text_data <- list(text = text)
  class(text_data) <- "TextData"
  return(text_data)
}
