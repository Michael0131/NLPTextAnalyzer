# Load necessary libraries
library(NLP)        # For text processing 
library(tm)         # For text mining and preprocessing
library(tidytext)   # For sentiment lexicon access
library(wordcloud)  # For word cloud generation
library(ggplot2)    # For data visualization
library(dplyr)      # For data manipulation
library(readr)      # For reading CSV files

# Function to preprocess text data
clean_text_data <- function(text_data) {
  text_corpus <- VCorpus(VectorSource(text_data))
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, removePunctuation)
  text_corpus <- tm_map(text_corpus, removeNumbers)
  text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  return(text_corpus)
}

# Function to analyze the sentiment of a single sentence
get_sentiment <- function(sentence) {
  words_in_sentence <- unlist(strsplit(as.character(sentence), " "))
  sentiment_lexicon <- get_sentiments("bing")
  sentiment_data <- sentiment_lexicon %>%
    filter(word %in% words_in_sentence)
  
  if (nrow(sentiment_data) > 0) {
    sentiment_scores <- ifelse(sentiment_data$sentiment == "positive", 1, -1)
    total_sentiment_score <- sum(sentiment_scores)
    
    if (total_sentiment_score > 0) {
      return("Positive")
    } else if (total_sentiment_score < 0) {
      return("Negative")
    } else {
      matched_word_count <- nrow(sentiment_data)
      total_word_count <- length(words_in_sentence)
      if ((total_word_count - matched_word_count) / total_word_count > 0.5) {
        return("Unassigned")
      } else {
        return("Neutral")
      }
    }
  } else {
    return("Neutral")
  }
}

# Function to generate a word cloud without color-coding
create_wordcloud <- function(corpus) {
  # Convert the corpus into a term-document matrix
  term_matrix <- as.matrix(TermDocumentMatrix(corpus))
  
  # Sum word occurrences across all documents (rows)
  word_frequency <- rowSums(term_matrix)
  
  # Convert the frequency data into a data frame for easier manipulation
  word_df <- data.frame(word = names(word_frequency), freq = word_frequency, stringsAsFactors = FALSE)
  
  # Generate the word cloud in a single color (black)
  par(mar = c(0, 0, 0, 0))  # Remove margins for cleaner layout
  wordcloud(
    words = word_df$word,
    freq = word_df$freq,
    min.freq = 1,
    scale = c(3, 0.5),
    colors = "black",  # Single color for all words
    random.order = FALSE
  )
}

# Function to generate a bar plot of sentiment frequency
plot_sentiment_frequency <- function(sentiment_count) {
  sentiment_df <- as.data.frame(sentiment_count)
  colnames(sentiment_df) <- c("Sentiment", "Frequency")
  
  print(ggplot(sentiment_df, aes(x = Sentiment, y = Frequency)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          theme_minimal() +
          ggtitle("Sentiment Frequency Distribution") +
          xlab("Sentiment") +
          ylab("Count"))
}

# Main function to perform NLP analysis
perform_nlp_analysis <- function() {
  file_path <- "userInput.csv"
  data <- read_csv(file_path)
  
  if (!"text" %in% colnames(data)) {
    cat("Error: Column 'text' not found in the CSV file.\n")
    return(NULL)
  }
  
  text_data <- data$text
  text_data <- text_data[!is.na(text_data)]
  
  cleaned_corpus <- clean_text_data(text_data)
  sentiments <- sapply(text_data, get_sentiment)
  
  sentence_width <- 55
  sentiment_width <- 12
  
  cat(sprintf("%-*s | %-*s\n", sentence_width, "Sentence", sentiment_width, "Sentiment"))
  cat(strrep("-", sentence_width + sentiment_width + 3), "\n")
  
  for (i in 1:length(text_data)) {
    truncated_sentence <- substr(text_data[i], 1, sentence_width - 3)
    formatted_sentence <- sprintf("%-*s", sentence_width, truncated_sentence)
    formatted_sentiment <- sprintf("%-*s", sentiment_width, sentiments[i])
    cat(sprintf("%s | %s\n", formatted_sentence, formatted_sentiment))
  }
  
  sentiment_count <- table(sentiments)
  cat("\nSentiment Frequency:\n")
  cat(sprintf("%-10s | %s\n", "Sentiment", "Frequency"))
  cat(strrep("-", 22), "\n")
  for (i in 1:length(sentiment_count)) {
    cat(sprintf("%-10s | %d\n", names(sentiment_count)[i], sentiment_count[i]))
  }
  
  score <- sum(ifelse(sentiments == "Positive", 1, ifelse(sentiments == "Negative", -1, 0)))
  overall <- ifelse(score > 0, "Overall Determination: Positive", ifelse(score < 0, "Overall Determination: Negative", "Overall Determination: Neutral"))
  cat("\n", overall, "\n")
  
  # Generate the word cloud and sentiment frequency plot
  cat("\nGenerating word cloud...\n")
  create_wordcloud(cleaned_corpus)
  cat("\nGenerating sentiment frequency plot...\n")
  plot_sentiment_frequency(sentiment_count)
}

# Run the analysis
perform_nlp_analysis()
