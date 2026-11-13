keyword <- tolower(readline("Enter keyword: "))

files <- list.files("C:/Users/jlevi/Desktop/UNI/ITBDA4-14/map_reduce_test_docs", full.names = TRUE)

mapper_results <- data.frame(Document = character(), Row = integer(), Column = integer())

for (file in files) {
  text <- readLines(file, warn = FALSE)
  
  for (i in 1:length(text)) {
    line <- tolower(text[i])
    position <- gregexpr(keyword, line)[[1]]
    
    if (position[1] != -1) {
      for (column in position) {
        mapper_results <- rbind(mapper_results, data.frame(
          Document = basename(file),
          Row = i,
          Column = column
        ))
      }
    }
  }
}

print(mapper_results)

documents <- unique(mapper_results$Document)

for (document in documents) {
  print(mapper_results[mapper_results$Document == document, ])
}

