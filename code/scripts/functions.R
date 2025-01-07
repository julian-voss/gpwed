# Additional functions
calculate_letter_share <- function(column) {
  # Function to calculate the percentage of letter characters within a column
  sapply(column, function(x) {
    if (is.na(x)) {
      return(NA)  # Return NA immediately if the input is NA
    } else {
      letter_count <- str_count(x, "[a-zA-Z]")  # Count letter characters
      total_count <- nchar(x)  # Total characters
      if (total_count > 0) {
        return(letter_count / total_count)  # Calculate percentage
      } else {
        return(0)  # Return 0 if the string is empty
      }
    }}) %>%
    { mean(., na.rm = TRUE) }  # Calculate mean, removing NA values
}
calculate_letter_share_vec <- function(input_vector) {

  not_na <- na.omit(input_vector)

  # Combine all elements of the input vector into a single string
  text <- paste(not_na, collapse = "")

  # Calculate the total length of the string
  total_length <- nchar(text)

  # Calculate the number of letters in the string
  num_letters <- nchar(gsub("[^A-Za-z]", "", text))

  # Calculate the share of letters
  letter_share <- num_letters / total_length

  return(letter_share)
}
str_replace_multiple <- function(input_string, replacements, all = T) {

  if (all) {
    fun.replace <- str_replace_all
  } else {
    fun.replace <- str_replace
  }

  for (pattern in names(replacements)) {
    input_string <- str_replace_all(input_string, pattern, replacements[pattern])
  }
  return(input_string)

}
clear_environment <- function(exclude=NULL) {
  # List all objects in the current environment
  all_objects <- ls(envir = .GlobalEnv)

  if (!is.null(exclude)) {
    all_objects <- Filter(function(x){!grepl(exclude, x)}, all_objects)
  }

  # Use sapply() to iterate and remove non-function objects
  sapply(all_objects, function(x) {
    if (!is.function(get(x, envir = .GlobalEnv))) {
      rm(list = x, envir = .GlobalEnv)
    }
  })

  cat("Non-function objects cleared from the global environment.\n")
}
