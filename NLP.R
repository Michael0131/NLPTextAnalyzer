# Load necessary libraries for NLP and data visualization
library(NLP)        # For natural language processing functions
library(tm)         # For text mining and preprocessing functions
library(tidytext)   # For accessing sentiment lexicons
library(wordcloud)  # For generating word clouds
library(ggplot2)    # For data visualization using ggplot
library(dplyr)      # For data manipulation and handling data frames
library(readr)      # For reading CSV files

# Function to preprocess text data into a clean format
clean_text_data <- function(text_data) {
  # Convert the input text data into a volatile corpus object
  text_corpus <- VCorpus(VectorSource(text_data))
  # nolint
  # Convert text to lowercase for uniformity in analysis
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  # nolint
  # Remove punctuation to focus solely on words
  text_corpus <- tm_map(text_corpus, removePunctuation)
  # nolint
  # Remove numbers from the text to avoid irrelevant information
  text_corpus <- tm_map(text_corpus, removeNumbers)
  # nolint
  # Remove common English stop words to enhance sentiment focus
  text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))
  # nolint
  # Strip extra whitespace to clean up the text
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  # nolint
  # Return the cleaned corpus for further analysis
  return(text_corpus)
}

# Function to analyze the sentiment of a single sentence
get_sentiment <- function(sentence) {
  # Split the sentence into individual words for analysis
  words_in_sentence <- unlist(strsplit(as.character(sentence), " "))
  # nolint
  # Load the Bing sentiment lexicon that categorizes words as positive or negative # nolint
  sentiment_lexicon <- get_sentiments("bing")
  # nolint
  # Filter the lexicon to keep only words found in the current sentence
  sentiment_data <- sentiment_lexicon %>%
    filter(word %in% words_in_sentence) # nolint
  # nolint
  # Check if any sentiment words were matched
  if (nrow(sentiment_data) > 0) {
    # Assign +1 for positive words and -1 for negative words
    sentiment_scores <- ifelse(sentiment_data$sentiment == "positive", 1, -1)
    # nolint
    # Calculate the total sentiment score by summing individual scores
    total_sentiment_score <- sum(sentiment_scores)
    # nolint
    # Classify the overall sentiment based on the total score
    if (total_sentiment_score > 0) {
      return("Positive")  # More positive words
    } else if (total_sentiment_score < 0) {
      return("Negative")  # More negative words
    } else {
      # If neutral, check the ratio of unmatched words
      matched_word_count <- nrow(sentiment_data)
      total_word_count <- length(words_in_sentence)
      # nolint
      # If more than 50% of words are unmatched, return "Unassigned"
      if ((total_word_count - matched_word_count) / total_word_count > 0.5) {
        return("Unassigned")
      } else {
        return("Neutral")  # Equal count of positive and negative words
      }
    }
  } else {
    # Default to "Neutral" if no words match the sentiment lexicon
    return("Neutral")
  }
}

# Function to generate a word cloud without color-coding
create_wordcloud <- function(corpus) {
  # Convert the cleaned corpus into a term-document matrix for analysis
  term_matrix <- as.matrix(TermDocumentMatrix(corpus))
  # nolint
  # Sum word occurrences across all documents (rows) for frequency analysis
  word_frequency <- rowSums(term_matrix)
  # nolint
  # Convert the frequency data into a data frame for easier manipulation
  word_df <- data.frame(word = names(word_frequency), freq = word_frequency, stringsAsFactors = FALSE) # nolint
  # nolint
  # Generate the word cloud using a single color for all words
  par(mar = c(0, 0, 0, 0))  # Remove margins for a cleaner layout
  wordcloud(
    words = word_df$word,
    freq = word_df$freq,
    min.freq = 1,
    scale = c(3, 0.5),
    colors = "black",  # All words displayed in black
    random.order = FALSE
  )
}

# Function to generate a bar plot of sentiment frequency
plot_sentiment_frequency <- function(sentiment_count) {
  # Convert the sentiment frequency table to a data frame
  sentiment_df <- as.data.frame(sentiment_count)
  colnames(sentiment_df) <- c("Sentiment", "Frequency")
  # nolint
  # Plot the sentiment frequency distribution using ggplot
  print(ggplot(sentiment_df, aes(x = Sentiment, y = Frequency)) + # nolint
          geom_bar(stat = "identity", fill = "skyblue") +
          theme_minimal() +
          ggtitle("Sentiment Frequency Distribution") +
          xlab("Sentiment") +
          ylab("Count"))
}

# Main function to perform NLP analysis on the provided CSV data
perform_nlp_analysis <- function() {
  # Specify the file path for the input CSV file
  file_path <- "userInput.csv"
  # nolint
  # Read the CSV data into a data frame
  data <- read_csv(file_path)
  # nolint
  # Check if the expected column "text" exists in the CSV file
  if (!"text" %in% colnames(data)) {
    cat("Error: Column 'text' not found in the CSV file.\n")
    return(NULL)  # Exit the function if the column is missing
  }
  # nolint
  # Extract the text column and remove any NA values
  text_data <- data$text
  text_data <- text_data[!is.na(text_data)]
  # nolint
  # Preprocess the text data for analysis
  cleaned_corpus <- clean_text_data(text_data)
  # nolint
  # Analyze the sentiment of each sentence in the text data
  sentiments <- sapply(text_data, get_sentiment)
  # nolint
  # Define the table column widths for displaying results
  sentence_width <- 55
  sentiment_width <- 12
  # nolint
  # Display the header row for the sentiment analysis results
  cat(sprintf("%-*s | %-*s\n", sentence_width, "Sentence", sentiment_width, "Sentiment")) # nolint
  cat(strrep("-", sentence_width + sentiment_width + 3), "\n")
  # nolint
  # Display each sentence and its corresponding sentiment in the formatted table
  for (i in 1:length(text_data)) { # nolint
    # Truncate sentences that are too long to fit in the defined width
    truncated_sentence <- substr(text_data[i], 1, sentence_width - 3)
    formatted_sentence <- sprintf("%-*s", sentence_width, truncated_sentence)
    formatted_sentiment <- sprintf("%-*s", sentiment_width, sentiments[i])
    cat(sprintf("%s | %s\n", formatted_sentence, formatted_sentiment))
  }
  # nolint
  # Calculate and display the sentiment frequency table
  sentiment_count <- table(sentiments)
  cat("\nSentiment Frequency:\n")
  cat(sprintf("%-10s | %s\n", "Sentiment", "Frequency"))
  cat(strrep("-", 22), "\n")
  for (i in 1:length(sentiment_count)) { # nolint
    cat(sprintf("%-10s | %d\n", names(sentiment_count)[i], sentiment_count[i]))
  }
  # nolint
  # Determine overall sentiment based on the cumulative score of sentiments
  score <- sum(ifelse(sentiments == "Positive", 1, ifelse(sentiments == "Negative", -1, 0))) # nolint
  overall <- ifelse(score > 0, "Overall Determination: Positive", 
                    ifelse(score < 0, "Overall Determination: Negative", 
                           "Overall Determination: Neutral"))
  cat("\n", overall, "\n")
  # nolint
  # Generate the word cloud and sentiment frequency plot
  cat("\nGenerating word cloud...\n")
  create_wordcloud(cleaned_corpus)
  cat("\nGenerating sentiment frequency plot...\n")
  plot_sentiment_frequency(sentiment_count)
}

# Execute the NLP analysis
perform_nlp_analysis()
