# Springboard Data Science Data Wrangling Exercise 2 Titanic Data set 03/19/16

#Loading package dplyr
library(dplyr)
# Reading titanic_orginal.csv into a data frame
titanic_original <- read.csv("titanic_original.csv")

# Fixing empty embarked value in row 169
titanic_original$embarked[169] = "S"

# Finding mean age, filling in missing values with mean of age column with for loop
mean_age <- mean(titanic_original$age, na.rm = TRUE)
num_rows <- nrow(titanic_original)
for (i in 1:num_rows) {
  if (is.na(titanic_original$age[i]) == TRUE) {
    titanic_original$age[i] = mean_age
  }
}

# Other ways to deal with missing values for age: Doing it this way was interesting because I 
# was worried I wouldn't be able to tell which ages were actually true and which ages were just
# filled in randomly with the mean, which could cause serious issues. It turns out that is not
# the case because the real age values are followed by zeroes after the decimal point. Another way
# I could have dealt with this include putting an obvious number in such as "-99" for all the
# values, but that could have resulted in numerous errors in analysis down the road depending
# on what my goal would have been. I could also have deleted the NA values into '' values so that
# if else statements and possibly other forms of analysis would be better able to handle them.

# Filling in empty lifeboat values with NA :(
for (i in 1:num_rows) {
  if (titanic_original$boat[i] == '') {
    titanic_original$boat[i] = "NA"
  }
}

# Cabin number: It does not make sense to populate the missing cabin numbers with a value here, 
# because these passengers may not have had cabins at all. Passengers with cabin numbers are
# possibly more likely to be rich, for example. In this case, a cabin number could be a useful
# indicator of survival; therefore, a binary column has_cabin_number could be useful here.
titanic_mod <- mutate(titanic_original, has_cabin_number = 1)
for (i in 1:num_rows) {
  if (titanic_mod$cabin[i] == '') {
    titanic_mod$has_cabin_number[i] = 0
  }
}

write.csv(titanic_mod, "titanic_clean.csv")