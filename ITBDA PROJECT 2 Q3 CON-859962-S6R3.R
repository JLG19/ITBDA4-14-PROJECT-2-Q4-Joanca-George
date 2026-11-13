library(httr)
library(jsonlite)

url <- "https://en.wikipedia.org/w/api.php"

response <- GET(
  url,
  query = list(
    action = "query",
    prop = "extracts",
    titles = "Amazon rainforest",
    explaintext = 1,
    format = "json"
  ),
  user_agent("BDA project Q3")
)

data <- fromJSON(content(response, "text", encoding = "UTF-8"))

article_text <- data$query$pages[[1]]$extract

article_data <- data.frame(article = "Amazon rainforest",Clean_text = article_text)

View(article_data)

nchar(article_data$Clean_text)

install.packages("textstem", dependencies = TRUE)

clean_text <- article_data$Clean_text
clean_text <- gsub("==+.*?==+", "", clean_text)
clean_text <- gsub("\\[[0-9]+\\]", "", clean_text)
clean_text <- gsub("\\s+", " ", clean_text)
clean_text <- trimws(clean_text)

article_data$Clean_text <- clean_text
nchar(article_data$Clean_text)

View(article_data)

library(tm)
library(textstem)

text <- article_data$Clean_text
text <- tolower(text)
text <- removePunctuation(text)
text <- removeNumbers(text)
text <- removeWords(text, stopwords("en"))
text <- lemmatize_strings(text)

article_data$Processed_text <- text

View(article_data)

