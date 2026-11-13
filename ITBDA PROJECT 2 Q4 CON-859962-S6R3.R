library(rvest)
library(stringr)

article_links <- c(
  "https://fivebooks.com/best-books/the-best-sci-fi-fantasy-novels-as-chosen-by-fans-the-2026-hugo-awards-sylvia-bishop/",
  "https://fivebooks.com/best-books/the-best-epic-fantasy-novels-christopher-paolini/",
  "https://fivebooks.com/best-books/the-best-fantasy-of-the-past-decade-sylvia-bishop/",
  "https://fivebooks.com/best-books/best-high-fantasy-novels-richard-swan/",
  "https://fivebooks.com/best-books/the-best-fantasy-series-samantha-shannon/"
)

all_books <- data.frame(
  article_title = character(),
  book = character(),
  author = character()
)

for (link in article_links) {
  article <- read_html(link)
  article_title <- html_text2(
    html_element(article, "h1")
  )
  
  text <- html_text2(article)
  book_lines <- str_extract_all(text,"(?m)^\\s*[1-5]\\s+[^\\n]+\\s+by\\s+[^\\n]+")[[1]]

  if (length(book_lines) > 0) {
    book_lines <- str_remove(book_lines, "^\\s*[1-5]\\s+")
    matches <- str_match(
      book_lines,
      "^(.+?)\\s+by\\s+(.+)$"
    )
    
    new_books <- data.frame(
      article_title = article_title,
      book = str_trim(matches[, 2]),
      author = str_trim(matches[, 3])
    )
    
    all_books <- rbind(all_books, new_books)
  }
}

all_books <- all_books[!duplicated(all_books$book), ]
rownames(all_books) <- NULL

all_books$word_count <- str_count(
  all_books$book,
  "\\b[A-Za-z]+\\b"
)

#20 books
all_books <- head(all_books, 20)
View(all_books)

clean_book <- all_books$book
clean_book <- gsub("Make YourOwn List", "", clean_book)
clean_book <- gsub("\\[[0-9]+\\]", "", clean_book)
clean_book <- gsub("\\s+", " ", clean_book)
clean_book <- trimws(clean_book)
 
clean_author <- all_books$author
clean_author <- gsub("\\s+", " ", clean_author)
clean_author <- trimws(clean_author)

all_books$clean_book <- clean_book
all_books$book <- clean_book
all_books$author <- clean_author

colSums(is.na(all_books))
all_books$book[is.na(all_books$book)] <- "Unknown"
all_books$author[is.na(all_books$author)] <- "Unknown"
all_books$word_count <- str_count(
  all_books$book,
  "\\b[A-Za-z]+\\b"
)


write.csv(all_books[, c("article_title", "book", "author", "word_count")],"fantasy_books_cleaned.csv",row.names = FALSE)

View(all_books)



