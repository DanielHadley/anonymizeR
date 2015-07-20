#' Generate a random string for each unique value in a variable from a data frame
#'
#' @param mydf A data frame or data table
#' @param identifier A variable in the data that contains the characters or numbers you would like to replace with an anonymous ID. Should be wrapped in quotes.
#' @param id_length The length of the newly generated random id. Defaults to 10.
#' @param drop_old Do you want to drop the identifier from the data that gets returned? Defaults to TRUE.
#' @return A data frame identical to the original but with a new variable containing the random ID, which will be named <identifier>.anon.ID, and missing the identifier if drop_old is set to TRUE.
#' @examples
#' anonymize_randomly(mydata, "Social.Security", 9, FALSE)

anonymize_randomly <- function(mydf, identifier, id_length = 10, drop_old = TRUE) {
  # First group them using dplyr functions to find only the unique observations
  # byIdentifier <- mydf %>% group_by_(identifier) %>% summarise
  byIdentifier <- dplyr::summarise(dplyr::group_by_(mydf, identifier))
  # Now create an random ID for each unique observation
  for(i in 1:nrow(byIdentifier)){
    byIdentifier$temp[i] <- paste(sample(c(0:9, letters, LETTERS), id_length, replace=TRUE), collapse="")
  }
  # Throw an error if two IDs are randomly the same
  if(length(unique(byIdentifier$temp)) < length(byIdentifier$temp)) warning('Two or more randomly assigned IDs were the same. Please set.seed() to a different number, and/or increase the length of the ID generated.')
  colnames(byIdentifier)[2] <- paste0(identifier, ".anon.ID")
  # Join in back to the original dataset
  DeidentifiedData <- dplyr::left_join(mydf, byIdentifier)
  # Drop the identifier from original dataset if TRUE, or return joined data if FALSE
  if (drop_old) {
    DeidentifiedData[which(colnames(DeidentifiedData)==identifier)] <- NULL
  } else {
    return(DeidentifiedData)
  }
  return(DeidentifiedData)
}
