#' easyClean
#'
#' easyClean cleans the text data. It makes the text lowercase, removes numbers, removes punctuation, removes numbers, and removes common English stopwords. It will then create a term document matrix. Its function is called and performed for easyRead.
#' @param text User's chosen file or URL link of type htm or html that has been converted into a variable.
#'
#' @return A preprocessed matrix of \code{text}
#' @export
#' @importFrom tm VectorSource Corpus tm_map content_transformer removePunctuation removeNumbers removeWords stopwords TermDocumentMatrix
#'
#' @examples
#' \dontrun{
#' # This function is executed in easyClean
#' }
easyClean <- function(text) {
  # Create Corpus from text vector
  words.vec <- VectorSource(text)
  words.corpus <- Corpus(words.vec)

  # Convert words to lowercase
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))

  # Remove Punctuation, Numbers, and stopwords
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

  # Create a TermDocumentMatrix
  tdm <- TermDocumentMatrix(words.corpus)

  # Convert to matrix
  word_matrix <- as.matrix(tdm)

  return(word_matrix)
}
