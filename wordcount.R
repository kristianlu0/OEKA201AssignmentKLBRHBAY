#' Count words in a Quarto document
#'
#' This function reads a Quarto (.qmd) file and counts the total number of words,
#' excluding YAML header, code chunks, and inline code.
#'
#' @param qmd_file Path to the Quarto document file
#' @return A named list with word count and document path
#' @export
#' @examples
#' count_words_in_qmd("assignment_r.qmd")
count_words_in_qmd <- function(qmd_file) {
  # Check if file exists
  if (!file.exists(qmd_file)) {
    stop("File not found: ", qmd_file)
  }

  # Read the file
  lines <- readLines(qmd_file, warn = FALSE)

  # Remove YAML header (between --- markers)
  yaml_start <- which(lines == "---")
  if (length(yaml_start) >= 2) {
    lines <- lines[-(yaml_start[1]:yaml_start[2])]
  }

  # Remove code chunks (between ```{r} and ```)
  in_code_chunk <- FALSE
  text_lines <- c()

  for (line in lines) {
    if (grepl("^```\\{", line)) {
      in_code_chunk <- TRUE
    } else if (grepl("^```$", line) && in_code_chunk) {
      in_code_chunk <- FALSE
    } else if (!in_code_chunk) {
      text_lines <- c(text_lines, line)
    }
  }

  # Combine all text
  text <- paste(text_lines, collapse = " ")

  # Remove inline code (`code`)
  text <- gsub("`[^`]+`", "", text)

  # Remove Markdown syntax
  text <- gsub("\\*\\*|\\*|__|_|~~|\\[|\\]|\\(http[^)]+\\)", "", text)
  text <- gsub("^#+\\s+", "", text)  # Remove headers

  # Split into words and count
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]  # Remove empty strings
  word_count <- length(words)

  # Return result
  result <- list(
    file = qmd_file,
    word_count = word_count,
    message = paste("Word count for", basename(qmd_file), ":", word_count, "words")
  )

  return(result)
}
