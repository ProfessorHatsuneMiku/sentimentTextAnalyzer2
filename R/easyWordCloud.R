#' easyWordCloud
#'
#' Visualize your findings via word clouds courtesy of the wordcloud2 package. Enter current iteration of text data after it has been run through the easyFrequency function. Users must convert the list output of easyFrequency to dataframe for the function to work properly.
#' @param freq_data A dataframe.
#' @param color_scheme A color.
#' @param top_n A number for how many words should appear.
#'
#' @return A wordcloud of \code{freq_data} with optional color selection \code{color_scheme} and number of words shown \code{top_n}
#' @export
#' @importFrom wordcloud2 wordcloud2
#'
#' @examples
#' \dontrun{
#' easyWordCloud(positive.df, "green", 7)
#' }
easyWordCloud <- function(freq_data, color_scheme = "green", top_n = 50) {
  # Convert frequency data to a data frame
  cloud_frame <- as.data.frame(freq_data)
  # Subset the data frame to the top n words
  cloud_frame <- head(cloud_frame[order(-cloud_frame$freq), ], top_n)

  # Generate word cloud
  if(color_scheme == "green") {
    wordcloud2(data = cloud_frame, size = 1, color = "green")
  } else if(color_scheme == "red") {
    wordcloud2(data = cloud_frame, size = 1, color = "red")
  } else {
    wordcloud2(data = cloud_frame, size = 1)
  }
}
