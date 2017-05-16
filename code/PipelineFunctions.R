# Amunategui Pipeline functions

# Date Processing/Capture
Fix_Date_Features <- function(data_set) {
  text_features <- c(names(data_set[sapply(data_set, is.character)]),
                     names(data_set[sapply(data_set, is.factor)]))
  for (feature_name in text_features) {
    feature_vector <- as.character(data_set[, feature_name])
    # assuming date pattern: '01/11/2012'
    date_pattern <- '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]'
    if (max(nchar(feature_vector)) == 10) {
      if (sum(grepl(date_pattern, feature_vector)) > 0) {
        print(paste('Casting feature to date:', feature_name))
        data_set[, feature_name] <-
          as.Date(feature_vector, format = "%d/%m/%Y")
      }
    }
  }
  return (data_set)
}