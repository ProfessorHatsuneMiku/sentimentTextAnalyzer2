#' easyFrequency
#'
#' Gets the total frequency of words overall but also determines the frequency of positive and negative words found within the text.
#' @param word_matrix A preprocessed term document matrix that has been converted into a matrix.
#' @param pos_words Positive word list from the Bing Dictionary
#' @param neg_words Negative word list from the Bing Dictionary
#'
#' @return A list containing word frequency, positive word frequency, negative word frequency, all positive words, count of positive words, all negative words, count of negative words
#' @export
#'
#' @examples
#' \dontrun{
#' easyFrequency(cleanText, pos, neg)
#' }
easyFrequency <- function(word_matrix, pos_words, neg_words) {

  # Get word frequencies
  word_freq <- rowSums(word_matrix)

  # Determine positive words frequency
  matched_pos <- match(rownames(word_matrix), pos_words, nomatch = 0)
  pos_counts <- word_freq[which(matched_pos != 0)]
  n_pos <- sum(pos_counts)

  # Get positive words
  pos_words_matched <- row.names(word_matrix)[which(matched_pos != 0)]

  # Determine negative words frequency
  matched_neg <- match(rownames(word_matrix), neg_words, nomatch = 0)
  neg_counts <- word_freq[which(matched_neg != 0)]
  n_neg <- sum(neg_counts)

  # Get negative words
  neg_words_matched <- row.names(word_matrix)[which(matched_neg != 0)]

  return(list(word_freq = word_freq, n_pos = n_pos, n_neg = n_neg, pos_words = pos_words_matched, pos_counts = pos_counts, neg_words = neg_words_matched, neg_counts = neg_counts))
}
