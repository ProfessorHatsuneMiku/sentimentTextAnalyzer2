#' easyRead
#'
#' Reads in the file the user selected and preprocesses it to determine what type of file or URL it is. The function will prepare the text to then be cleaned through easyClean which is called within easyRead.
#' @param file_path User's file within their chosen directory or a URL link of type htm or html
#'
#' @return A preprocessed matrix of \code{file_path}
#' @export
#' @importFrom XML htmlTreeParse xpathApply xmlValue
#' @importFrom utils URLencode read.csv head
#'
#' @examples
#' \dontrun{
#' easyRead("http://www.historyplace.com/speeches/anthony.htm")
#' # Output: cleaned text
#' }
easyRead <- function(file_path) {
  # Check if the file path is a URL
  if (startsWith(tolower(file_path), "http")) {
    # Encode the URL
    fileLocation <- URLencode(file_path)

    # Read and parse HTML file from URL
    doc.html <- htmlTreeParse(fileLocation, useInternal = TRUE)

    # Extract all paragraphs and flatten to a character vector
    doc.text <- unlist(xpathApply(doc.html, '//p', xmlValue))

    # Replace \n and \r with spaces
    doc.text <- gsub('\\n|\\r', ' ', doc.text)
  } else if (endsWith(tolower(file_path), ".csv")) {
    # Read text data from CSV file
    text <- read.csv(file_path, stringsAsFactors = FALSE)
    text <- text[,1]  # Assuming the text is in the first column

    # Combine text if it's a vector
    if (is.vector(text)) {
      doc.text <- paste(text, collapse = " ")
    }
  } else {
    # Read text data from plain text file
    doc.text <- readLines(file_path, warn = FALSE)

    # Combine text if it's a vector
    if (is.vector(doc.text)) {
      doc.text <- paste(doc.text, collapse = " ")
    }
  }

  # Preprocess text
  cleaned_text <- easyClean(doc.text)

  return(cleaned_text)
}
