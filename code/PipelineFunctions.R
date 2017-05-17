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

# Free form text vs Categorical text: If the percent of distinct values of a variable
# exceed the threshold, treat as free text and replace column by word count,
# character count and first word.
Get_Free_Text_Measures <- function(data_set,
                                   minimum_unique_threshold = 0.9,
                                   features_to_ignore = c())
{
  # look for text entries that are mostly unique
  text_features <-
    c(names(data_set[sapply(data_set, is.character)]),
      names(data_set[sapply(data_set, is.factor)]))
  for (f_name in setdiff(text_features, features_to_ignore))
  {
    f_vector <- as.character(data_set[, f_name])
    # treat as raw text if data over minimum_precent_unique unique
    if (length(unique(as.character(f_vector))) > (nrow(data_set) * minimum_unique_threshold)) {
      data_set[, paste0(f_name, '_word_count')] <-
        sapply(strsplit(f_vector, " "), length)
      data_set[, paste0(f_name, '_character_count')] <-
        nchar(as.character(f_vector))
      data_set[, paste0(f_name, '_first_word')] <-
        sapply(strsplit(as.character(f_vector), " "), `[`, 1)
      # remove orginal field
      data_set[, f_name] <- NULL
    }
  }
  return(data_set)
}

# Binarize factors or categorical variables
Binarize_Features <-
  function(data_set,
           features_to_ignore = c(),
           leave_out_one_level = FALSE) {
    text_features <-
      c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
    for (feature_name in setdiff(text_features, features_to_ignore)) {
      feature_vector <- as.character(data_set[, feature_name])
      # check that data has more than one level
      if (length(unique(feature_vector)) == 1)
        next
      # We set any non-data to text
      feature_vector[is.na(feature_vector)] <- 'NA'
      feature_vector[is.infinite(feature_vector)] <- 'INF'
      feature_vector[is.nan(feature_vector)] <- 'NAN'
      # loop through each level of a feature and create a new column
      first_level = TRUE
      for (newcol in unique(feature_vector)) {
        if (first_level && leave_out_one_level) {
          # avoid dummy trap and skip first level
          first_level = FALSE
        } else {
          data_set[, paste0(feature_name, "_", newcol)] <-
            ifelse(feature_vector == newcol, 1, 0)
        }
      }
      # remove original feature
      data_set <- data_set[, setdiff(names(data_set), feature_name)]
    }
    return (data_set)
  }
