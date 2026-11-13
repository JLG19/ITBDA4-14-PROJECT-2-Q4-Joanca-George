files <- list.files("C:/Users/jlevi/Desktop/UNI/ITBDA4-14/sa_news", full.names = TRUE)

news <- data.frame(
  file_name = basename(files),
  content = sapply(files, function(file) {
    paste(readLines(file, warn = FALSE), collapse = " ")
  })
)

news$content <- tolower(news$content)
news$content <- gsub("[[:punct:]]", "", news$content)
news$content <- gsub("[0-9]", "", news$content)


library(tm)
library(topicmodels)

corpus <- VCorpus(VectorSource(news$content))

corpus <- tm_map(corpus, removeWords, stopwords("en"))

extra_words <- c("said", "also", "would", "could", "one", "two")

corpus <- tm_map(corpus, removeWords, extra_words)

dtm <- DocumentTermMatrix(corpus)

dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

lda_model <- LDA(dtm, k = 5, control = list(seed = 123))

terms(lda_model, 10)

word_distribution <- posterior(lda_model)$terms

for (i in 1:5) {
  cat("\nTopic", i, "\n")
  top_words <- sort(word_distribution[i, ], decreasing = TRUE)[1:10]
  print(round(top_words, 4))
}

library(ggplot2)

words <- terms(lda_model, 10)[1, ]
word_counts <- colSums(as.matrix(dtm))
top_words <- sort(word_counts, decreasing = TRUE)[1:10]

word_data <- data.frame(
  Word = names(top_words),
  Count = as.numeric(top_words)
)

ggplot(word_data, aes(x = reorder(Word, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Words Across All News Documents",
       x = "Words", y = "Count") +
  theme_minimal()

word_data$Percentage <- word_data$Count / sum(word_data$Count) * 100

ggplot(word_data, aes(x = "", y = Percentage, fill = Word)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Top 10 Words Across All News Documents",fill = "Words") + theme_void()

