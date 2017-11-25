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

## EXTRACT INGREDIENTS AND CORRESPONDING AMOUNTS ====================================================================

measures <- "(cup|teaspoon|tablespoon|can|pound|package|clove|dash|sprig|pinch|slice|quart|drop|stalk)"
tomatch <- paste("([[:digit:][:space:]a-z/().]+", measures, "s?[)]? )|([0-9]+[[:space:][:digit:]/]+ )|([0-9]+ )", sep="")

split_amt <- function(strlist) {
  
  # split strlist into "ingredients" and "amts" vectors
  splits <- regexpr(pattern = tomatch, strlist, useBytes=FALSE) # pattern match for amount descriptors
  amt.stops <- sapply(attr(splits,"match.length"), sum, -1) # get amount start indices
  ingredient.starts <- sapply(amt.stops, sum, 2) # get ingredient start indices
  ingredient.stops <- sapply(strlist, nchar) # get ingredient stop indices
  amt <- mapply(substr, strlist, start=1, stop=amt.stops, USE.NAMES=FALSE) # get amounts
  ingredient <- mapply(substr, strlist, start=ingredient.starts, stop=ingredient.stops, USE.NAMES=FALSE) # get ingredients
  
  # deal with ingredient amounts that are specified only in ounces or fluid ounces
  ozsplits <- regexpr(pattern = "[[:digit:][:space:]/(.]*(ounce|fluid ounce)s?[)]?", ingredient, useBytes = FALSE)
  amt <- trimws((paste(amt, mapply(substr, ingredient, start=1, stop=attr(ozsplits, "match.length")), sep=" ")))
  ingredient.starts <- sapply(attr(ozsplits, "match.length"), sum, 2)
  ingredient <- mapply(substr, ingredient, start=ingredient.starts, stop=ingredient.stops, USE.NAMES=FALSE)
  
  # return as tibble
  df <- tibble(amt, ingredient)
  df <- df[endsWith(df$ingredient, ":")==FALSE,] # remove subheadings (end with ":")
  return(df)
}

get_ingredients <- function(string, id) {
  string <- gsub('\\"', "'", string) # make quotation marks uniform
  strlist <- gsub("\\[|\\]|\\'", "", unlist(strsplit(as.character(string), "', "))) # remove punctuation and split on commas
  ingredients_df <- split_amt(strlist)
  ingredients_df$recipe_id <- rep(id, nrow(ingredients_df)) # attach associated recipe ID to each row
  return(ingredients_df)
}

# Make final tibble

all_ingredients <- tibble()

for (i in 1:nrow(recipes_filtered)) {
  all_ingredients <- all_ingredients %>% 
    rbind(get_ingredients(recipes_filtered[i,"Ingredients"], as.numeric(recipes_filtered[i, "ID"])))
}

## FURTHER CLEAN INGREDIENTS COLUMN ====================================================================

# Check out ingredients in order of frequency of occurence
grouped_ingredients <- all_ingredients %>% 
  group_by(ingredient) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# We want to remove certain words because they are ingredient modifiers, not ingredients
wordsToRemove <- c("ground", "grated", "all-purpose", "melted", "minced", "softened", "chopped", "shredded", "packed",
                   "beaten", "dried", "sliced", "diced", "skinless", "boneless", "thighs", "thigh", "halves",
                   "rolled", "sweetened", "lean", "distilled", "freshly", "flaked", "granulated", "unsalted", "salted",
                   "crushed", "thawed", "extra", "virgin", "extra-virgin", "uncooked", "fresh", "large", "dry",
                   "cold", "finely", "drained", "plain", "light", "sifted", "thinly", "rinsed", "and", "juiced", "divided",
                   "whole", "crumbled", "separated", "firmly", "frozen", "small", "skim", "warm", "canned", "unsweetened",
                   "confectioners", "lightly", "peeled", "refrigerated", "slightly", "breast", "breasts", "boiling",
                   "boiled", "hot", "container", "jar", "seasoned", "unseasoned", "pitted", "trimmed", "zested", "prepared",
                   "coarsely", "cooked", "cubed", "mashed", "pulled", "creamed", "whipped", "fat-free",
                   "seeded", "julienned", "undrained", "soaked", "low-fat", "marinated", "medium", "quartered",
                   "heated", "cored", "powdered", "raw", "thickly", "very", "reduced-fat", "ripe", "roasted", "halved",
                   "shelled", "smoked", "square", "squares", "squeezed", "toasted", "deveined", "thick", "-", "or",
                   "more", "semisweet", "meat", "stewed", "thin", "cut", "thick-cut", "chops", "spareribs", "tenderloin",
                   "loin", "ribs", "roast", "sirloin", "shoulder", "mild")

# pattern to match common phrases and anything in parentheses
pattern <- "( to taste| as needed| cut into| for decoration| for dusting)|(\\(.*\\) ?)|( cut.*(strips|pieces|wedges|chunks|cubes|slices))"

process_ingredients <- function(ingredient_str) {
  ingredient_str <- tolower(gsub(",", "", ingredient_str)) # remove commas and convert to lowercase
  ingredient_str <- gsub(pattern, "", ingredient_str) # remove matches to pattern
  splitstr <- unlist(strsplit(ingredient_str, " ")) # split on spaces
  splitstr <- splitstr[(splitstr %in% wordsToRemove==FALSE)] # remove words named above
  return(paste(splitstr, collapse=" "))
}

all_ingredients$ingredient <- sapply(all_ingredients$ingredient, process_ingredients, USE.NAMES = FALSE)

# get rows whose ingredients are "salt pepper" or "salt black pepper"
saltpepper.toreplace <- all_ingredients$ingredient=="salt pepper"|all_ingredients$ingredient=="salt black pepper"
saltpepper.ids <- all_ingredients$recipe_id[saltpepper.toreplace] # get ids
saltpepper.amts <- all_ingredients$amt[saltpepper.toreplace] # get amts

all_ingredients <- all_ingredients[!saltpepper.toreplace,] # drop those rows

# create separate rows for "salt" and "black pepper" for each id, rbind to all_ingredients
salt <- tibble(amt = saltpepper.amts, ingredient = rep("salt", length(saltpepper.ids)), recipe_id = saltpepper.ids)
pepper <- tibble(amt = saltpepper.amts, ingredient = rep("black pepper", length(saltpepper.ids)), recipe_id = saltpepper.ids)

all_ingredients <- rbind(all_ingredients, salt, pepper)

# replace every instance of "white sugar" with "sugar"
all_ingredients[all_ingredients$ingredient=="white sugar","ingredient"] <- "sugar"

# Update grouped ingredients
grouped_ingredients <- all_ingredients %>% 
  group_by(ingredient) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Dealing with plurals: if an ingredient name +"s" or +"es" matches another ingredient name, make it the plural version
s.plurals <- grouped_ingredients$ingredient[endsWith(grouped_ingredients$ingredient, "s")]
s.matches <- paste(all_ingredients$ingredient, "s", sep="") %in% s.plurals
all_ingredients$ingredient[s.matches] <- paste(all_ingredients$ingredient[s.matches], "s", sep="")

es.plurals <- grouped_ingredients$ingredient[endsWith(grouped_ingredients$ingredient, "es")]
es.matches <- paste(all_ingredients$ingredient, "es", sep="") %in% es.plurals
all_ingredients$ingredient[es.matches] <- paste(all_ingredients$ingredient[es.matches], "es", sep="")

# Remove blank ingredients
all_ingredients <- all_ingredients[!all_ingredients$ingredient=="",]

# Update grouped ingredients one final time
grouped_ingredients <- all_ingredients %>% 
  group_by(ingredient) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

## CONVERT AMTS TO UNIFORM UNITS ====================================================================

