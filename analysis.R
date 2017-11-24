library(tidyverse)

recipes <- read_csv("recipes_ascii.csv")

# Get rid of weird extra columns
recipes <- recipes[,names(recipes)[1:12]]

recipes %>% 
  sapply(function(x) sum(x==0))

summary(recipes)

# We don't know how many users rated each recipe. However, we know how many people claimed to
# have the recipe and how many written reviews it has. We'll drop all recipes that either
# have been "made" less than 5 times or reviewed less than 5 times.
recipes_filtered <- recipes %>% 
  filter(MadeIt >= 5 | ReviewCount >= 5)

summary(recipes_filtered)

# create recipe ID col
recipes_filtered <- recipes_filtered %>%
  mutate(ID = row_number()) 

# EXTRACTING INGREDIENTS AND AMOUNTS =============================================================

measures <- "(cup|teaspoon|tablespoon|can|pound|package)"
tomatch <- paste("([[:digit:][:space:]a-z/().]+", measures, "s?)|([0-9]+[[:space:]][[:digit:]/]+)|([0-9]+)", sep="")

split_amt <- function(strlist) {
  splits <- regexpr(pattern = tomatch, strlist, useBytes=FALSE)
  amt.stops <- attr(splits,"match.length")
  ingredient.starts <- sapply(amt.stops, sum, 2)
  ingredient.stops <- sapply(strlist, nchar)
  amts <- mapply(substr, strlist, start=1, stop=amt.stops, USE.NAMES=FALSE)
  ingredients <- mapply(substr, strlist, start=ingredient.starts, stop=ingredient.stops, USE.NAMES=FALSE)
  df <- tibble(amts, ingredients)
  df <- df[endsWith(df$ingredients, ":")==FALSE,]
  return(df)
}

get_ingredients <- function(string) {
  string <- gsub('\\"', "'", string) # make quotation marks uniform
  strlist <- gsub("\\[|\\]|\\'", "", unlist(strsplit(as.character(string), "', "))) # remove punctuation and split on commas
  return(split_amt(strlist))
}

######################################################

get_ingredients(recipes_filtered[400, "Ingredients"])
recipes_filtered[1700,"Ingredients"] %>% get_ingredients()
recipes_filtered[1900,"Ingredients"] %>% get_ingredients()
recipes_filtered[1734,"Ingredients"] %>% get_ingredients()
recipes_filtered[1736,"Ingredients"] %>% get_ingredients()
recipes_filtered[1738,"Ingredients"] %>% get_ingredients()
recipes_filtered[834,"Ingredients"] %>% get_ingredients()
recipes_filtered[835,"Ingredients"] %>% get_ingredients()
recipes_filtered[835,"Ingredients"] %>% get_ingredients()
# cloves? sprigs? pinch? box? pan? bag?
