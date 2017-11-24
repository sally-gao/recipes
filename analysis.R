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

# EXTRACTING INGREDIENTS AND CORRESPONDING AMOUNTS =============================================================

measures <- "(cup|teaspoon|tablespoon|can|pound|package|clove|dash|sprig|pinch|slice)"
tomatch <- paste("([[:digit:][:space:]a-z/().]+", measures, "s?[)]? )|([0-9]+[[:space:]][[:digit:]/]+ )|([0-9]+ )", sep="")

split_amt <- function(strlist) {
  
  # split strlist into "ingredients" and "amts" vectors
  splits <- regexpr(pattern = tomatch, strlist, useBytes=FALSE) # pattern match for amount descriptors
  amt.stops <- sapply(attr(splits,"match.length"), sum, -1) # get amount start indices
  ingredient.starts <- sapply(amt.stops, sum, 2) # get ingredient start indices
  ingredient.stops <- sapply(strlist, nchar) # get ingredient stop indices
  amt <- mapply(substr, strlist, start=1, stop=amt.stops, USE.NAMES=FALSE) # get amounts
  ingredient <- mapply(substr, strlist, start=ingredient.starts, stop=ingredient.stops, USE.NAMES=FALSE) # get ingredients
  
  # deal with ingredient amounts that are specified only in ounces or fluid ounces
  ozsplits <- regexpr(pattern = "[[:digit:][:space:]/(.]*(ounce|fluid ounce)s?[)]?", ingredients, useBytes = FALSE)
  amts <- trimws((paste(amts, mapply(substr, ingredients, start=1, stop=attr(ozsplits, "match.length")), sep=" ")))
  ingredient.starts <- sapply(attr(ozsplits, "match.length"), sum, 2)
  ingredients <- mapply(substr, ingredients, start=ingredient.starts, stop=ingredient.stops, USE.NAMES=FALSE)
  
  # return as tibble
  df <- tibble(amts, ingredients)
  df <- df[endsWith(df$ingredients, ":")==FALSE,] # remove subheadings (end with ":")
  return(df)
}

get_ingredients <- function(string, id) {
  string <- gsub('\\"', "'", string) # make quotation marks uniform
  strlist <- gsub("\\[|\\]|\\'", "", unlist(strsplit(as.character(string), "', "))) # remove punctuation and split on commas
  ingredients_df <- split_amt(strlist)
  ingredients_df$recipe_id <- rep(id, nrow(ingredients_df))
  return(ingredients_df)
}

all_ingredients <- tibble()

for (i in 1:nrow(recipes_filtered)) {
  all_ingredients <- all_ingredients %>% 
    rbind(get_ingredients(recipes_filtered[i,"Ingredients"], as.numeric(recipes_filtered[i, "ID"])))
}