library(dplyr)

# Created by D Hadley to generate a random string for each unique value in a variable from a dataframe in R
# inputs: dataframe, the identifying variable, ID length (defaults to 10), and whether the identifying variable gets dropped from the returned dataframe (defaults to TRUE)

# As in the Birthday Problem, the likliehood of two or more random IDs overlapping increases quickly as the data gets very large. For a df of 80k unique observations using the default of 10 character strings with 62 (letters, LETTERS, and 0:10) possible choices for each character:

# p <- numeric(80000)
# for (n in 1:80000)      {
#   q <- 1 - (0:(n - 1))/(62^10)
#   p[n] <- 1 - prod(q)	  }
# plot(p)        # Makes Figure 1
# p              # Makes prinout below


# TODO: 

# 1. Allow for vectors of 2+ variables. John Smith could be two different people: another variable could differentiate.
# 2. Create a function that will generate sequential IDS to avoid random overlap
# 3. Create 'key' option
# 4. Maybe seperate functions for numbers only, or letters only

anonymize_randomly <- function(mydf, identifier, IDLength = 10, dropOld = TRUE) {
  # First group them using dplyr functions to find only the unique observations
  byIdentifier <- mydf %>% group_by_(identifier) %>% summarise
  # Now create an random ID for each unique observation 
  for(i in 1:nrow(byIdentifier)){
    byIdentifier$temp[i] <- paste(sample(c(0:9, letters, LETTERS), IDLength, replace=TRUE), collapse="")
  }
  # Throw an error if two IDs are randomly the same
  if(length(unique(byIdentifier$temp)) < length(byIdentifier$temp)) warning('Two or more randomly assigned IDs were the same. Please set.seed() to a different number, and/or increase the length of the ID generated.')
  colnames(byIdentifier)[2] <- paste0(identifier, ".anon.ID")
  # Join in back to the original dataset
  DeidentifiedData <- left_join(mydf, byIdentifier)
  # Drop the identifier from original dataset if TRUE, or return joined data if FALSE
  if (dropOld) {
    DeidentifiedData[which(colnames(DeidentifiedData)==identifier)] <- NULL
  } else {
    return(DeidentifiedData)
  }
  return(DeidentifiedData)
}


name <- c('John Doe','Peter Gynn','John Doe')
social <- c(210005693, 234000592, 268001850)
mydata <- data.frame(name, social)
rm(name, social)

testdata <- anonymize(mydata, "name")
testdata2 <- anonymize(mydata, "name", 5, FALSE)


finalTest <- data.frame(c(1:1000))
finaltestdata2 <- anonymize(finalTest, "c.1.1000.", 1)
