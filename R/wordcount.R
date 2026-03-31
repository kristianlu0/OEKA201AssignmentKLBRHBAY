count_words_in_qmd <- function(filepath,
                                include_code = FALSE,
                                include_yaml = FALSE) {
  text <- paste(readLines(filepath, warn = FALSE), collapse = "\n")

  # ── 1. Extract YAML front matter ──────────────────────────────────────────
  yaml_text <- ""
  yaml_match <- regmatches(text, regexpr("^---\\s*\n.*?\n---\\s*\n", text, perl = TRUE))
  if (length(yaml_match) > 0) {
    yaml_text <- yaml_match[[1]]
    text <- sub("^---\\s*\n.*?\n---\\s*\n", "", text, perl = TRUE)
  }

  # ── 2. Extract executable code chunks  ```{lang} ... ``` ──────────────────
  code_pattern <- "```+\\{[^}]*\\}.*?```+"
  code_chunks  <- regmatches(text, gregexpr(code_pattern, text, perl = TRUE))[[1]]
  code_text    <- paste(code_chunks, collapse = " ")
  prose_text   <- gsub(code_pattern, "", text, perl = TRUE)

  # Remove plain fenced blocks ``` ... ```
  prose_text <- gsub("```+.*?```+", "", prose_text, perl = TRUE)

  # ── 3. Strip inline code ──────────────────────────────────────────────────
  prose_text <- gsub("`[^`]+`", "", prose_text, perl = TRUE)

  # ── 4. Strip markdown syntax ──────────────────────────────────────────────
  prose_text <- gsub("(?m)^\\s*#{1,6}\\s*", "", prose_text, perl = TRUE)  # headings
  prose_text <- gsub("!?\\[([^\\]]*)\\]\\([^)]*\\)", "\\1", prose_text, perl = TRUE)  # links
  prose_text <- gsub("[*_~]{1,3}([^*_~]+)[*_~]{1,3}", "\\1", prose_text, perl = TRUE) # emphasis
  prose_text <- gsub("(?m)^\\s*[-*+>|]\\s*", "", prose_text, perl = TRUE) # list markers

  # ── 5. Word counter helper ────────────────────────────────────────────────
  count_words <- function(s) {
    length(gregexpr("\\b\\w+\\b", s, perl = TRUE)[[1]])
  }

  counts <- list(
    prose = count_words(prose_text),
    code  = count_words(code_text),
    yaml  = count_words(yaml_text)
  )

  total <- counts$prose
  if (include_code) total <- total + counts$code
  if (include_yaml) total <- total + counts$yaml
  counts$total <- total

  return(counts)
}

# ── Example usage ─────────────────────────────────────────────────────────
result <- count_words_in_qmd("document.qmd", include_code = FALSE, include_yaml = FALSE)

cat(sprintf("Prose words  : %6d\n", result$prose))
cat(sprintf("Code words   : %6d\n", result$code))
cat(sprintf("YAML words   : %6d\n", result$yaml))
cat(sprintf("─────────────────────\n"))
cat(sprintf("Total counted: %6d\n", result$total))
