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

measures <- "(cup|teaspoon|tablespoon| can|pound|package|clove|dash|sprig|pinch|slice|quart|drop|stalk|liter|milliliter|gallon|pint|bottle)"
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
                   "loin", "ribs", "roast", "sirloin", "shoulder", "mild", "smashed", "roughly", "pressed", "chilled")

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

## FILTERING & PROCESSING AMTS COLUMN ====================================================================

# Add serving size column to all_ingredients
all_ingredients <- all_ingredients %>%
  merge(y = (recipes_filtered %>% 
               select(ID, Servings) %>% 
               rename(recipe_id = ID)), by = "recipe_id", all.x = TRUE)

# Select unique ingredients for which count > 4
selected_ingredients <- grouped_ingredients %>% 
  filter(count > 4)

# For every unique ingredient for which count > 4, get a list of all the amount descriptors associated with it
amts <- sapply(selected_ingredients$ingredient, function(ingredient)
  {all_ingredients$amt[all_ingredients$ingredient==ingredient]}, USE.NAMES = TRUE)

# Define volume and weight measurements
volumes <- c("cup", "teaspoon", "tablespoon", "quart", "fluid ounce", "liter", "milliliter",
             "gallon", "pint", "pinch", "drop", "dash")
weights <- c("pound", "ounce")

# Find all ingredients for which over 70% of amounts (not including "") are specified in volume
mostly.volumes <- function(ingredient.amts) {
  total.matches <- sapply(volumes, function(x) sum(grepl(x, ingredient.amts)))
  return(sum(total.matches) > (length(ingredient.amts[ingredient.amts!=""]))*0.7)
}

is.mostly.volumes <- rapply(amts, mostly.volumes, how="unlist")
sum(is.mostly.volumes) # 237

# Find all ingredients for which over 70% of amounts (not including "") are specified in weight
mostly.weights <- function(ingredient.amts) {
  total.matches <- sapply(weights, function(x) sum(grepl(x, ingredient.amts)))
  total.matches[2] <- total.matches[2] - sum(grepl("fluid ounce", ingredient.amts))
  return(sum(total.matches) > (length(ingredient.amts[ingredient.amts!=""]))*0.7)
}

is.mostly.weights <- rapply(amts, mostly.weights, how="unlist")
sum(is.mostly.weights) # 69

# Find all ingredients for which over 70% of amounts (not including "") use the ingredient itself as the unit

# First, we need to convert any fractions to floating point numbers
convert_frac <- function(ingredient.amts) {
  matches <- grep("/", ingredient.amts)
  matches <- matches[!(matches %in% grep("[A-Za-z]", ingredient.amts))]
  
  if (length(matches)==0) {return(ingredient.amts)}
  else {
    fracs <- ingredient.amts[matches]
    floats <- sapply(fracs, function(frac) eval(parse(text = (sub(" ", "+", frac)))), USE.NAMES = FALSE)
    ingredient.amts[matches] <- floats
    return(ingredient.amts)
  }
}

mostly.raw <- function(ingredient.amts) {
  ingredient.amts <- convert_frac(ingredient.amts)
  return(sum(is.na(as.numeric(ingredient.amts))) < length(ingredient.amts)*0.3)
}

is.mostly.raw <- rapply(amts, mostly.raw, how="unlist")
sum(is.mostly.raw) # 40

# Find all ingredients for which amounts are specified in a mix of volumes, weights, or others
mixed <- is.mostly.volumes+is.mostly.weights+is.mostly.raw
mixed <- mixed[mixed==0]
length(mixed) # 86 such ingredients to deal with

## CONVERTING WEIGHTS ====================================================================

frac_to_float <- function(vec) {
  if (sum(grepl("/", vec))==0) {return(vec)}
  else {
    matches <- grep("/", vec)
    floats <- sapply(vec[matches], function(frac) eval(parse(text = (sub(" ", "+", frac)))), USE.NAMES = FALSE)
    vec[matches] <- floats
    return(vec)
  }
}

weights.amts <- amts[is.mostly.weights]

process.weights <- function(ingredient.amts) {
  is.weight <- c(grep("ounce", ingredient.amts), grep("pound", ingredient.amts))
  is.weight <- is.weight[!is.weight %in% grep("fluid", ingredient.amts)]
  weights <- ingredient.amts[is.weight]
  
  # if weight is just specified in ounces, remove the word "ounce" or "ounces"
  in.ounces <- grepl("^[0-9 /]+ ounce[s]?$", weights)
  weights[in.ounces] <- frac_to_float(sub(" ounce[s]?", "", weights[in.ounces]))
  
  # if weight is just specified in pounds, remove the word "pound" or "pounds" and multiply by 16
  in.pounds <- grepl("^[0-9 /]+ pound[s]?$", weights)
  weights[in.pounds] <- as.numeric(frac_to_float(sub(" pound[s]?", "", weights[in.pounds]))) * 16
  
  # if weight is specified like this: "2 (8 ounce) packages"
  oz.to.convert <- weights[grepl("^[0-9/ ]+ \\([0-9/ .]+ ounce\\)", weights)]
  
  multiply.ounces <- function(weight) {
    weight <- unlist(strsplit(weight, " \\("))
    weight[2] <- sub(" ounce\\)[ a-z]*", "", weight[2])
    weight <- frac_to_float(weight)
    
    return(as.numeric(weight[1]) * as.numeric(weight[2]))
  }
  
  weights[grepl("^[0-9/ ]+ \\([0-9/ .]+ ounce\\)", weights)] <- sapply(oz.to.convert, multiply.ounces, USE.NAMES = FALSE)
  
  # same thing for pounds, except multiply by 16
  lb.to.convert <- weights[grepl("^[0-9/ ]+ \\([0-9/ .]+ pound\\)", weights)]
  
  multiply.pounds <- function(weight) {
    weight <- unlist(strsplit(weight, " \\("))
    weight[2] <- sub(" pound\\)[ a-z]*", "", weight[2])
    weight <- frac_to_float(weight)
    
    return(as.numeric(weight[1]) * (as.numeric(weight[2])*16))
  }
  
  weights[grepl("^[0-9/ ]+ \\([0-9/ .]+ pound\\)", weights)] <- sapply(lb.to.convert, multiply.pounds, USE.NAMES = FALSE)
  
  # for any non-weights, impute mean weight
  ingredient.amts[-is.weight] <- mean(as.numeric(weights))
  ingredient.amts[is.weight] <- weights
  
  return(as.numeric(ingredient.amts))
}

# Process all weights
weights.amts <- rapply(weights.amts, process.weights, how="replace")

## CONVERTING VOLUMES ====================================================================

vol.conversions <- c(1, 48, 16, 0.25, 8, 0.24, 240, 0.0625, 0.5, 768, 2880, 384)
names(vol.conversions) <- volumes

# Function that converts any volume to cups
convert <- function(vol, unit) {
  return(unname(vol/vol.conversions[unit]))
}

vol.amts <- amts[is.mostly.volumes]

process.volumes <- function(ingredient.amts) {
  is.volume <- grep(paste(volumes, collapse ="|"), ingredient.amts)
  vols <- ingredient.amts[is.volume]
  
  # If volume is specified without brackets, convert straight to cups
  no.brackets <- grepl(paste("^[0-9 /]+ (", paste(volumes, collapse = "|"),")[s]?$", sep = ""), vols)
  numbers <- frac_to_float(sub(paste(" (", paste(volumes, collapse = "|"),")[s]?", sep = ""), "", vols[no.brackets]))
  
  volumes.tomatch <- c(volumes, "fluid", paste(volumes, "s", sep=""))
  units <- rapply(strsplit(vols[no.brackets], " "), function(x) {x[x %in% volumes.tomatch]}, how="unlist")
  units <- sub("s$", "", units)
  units[units=="fluid"] <- "fluid ounce"

  vols[no.brackets] <- mapply(convert, as.numeric(numbers), units)
  
  # Deal with volumes containing brackets (e.g. "1 (12 fluid ounce) can")
  vols.to.convert <- vols[grepl("^[0-9/ ]+ \\([0-9/ .]+ ", vols)]
  
  multiply.vols <- function(vol) {
    quantity <- as.numeric(frac_to_float(unlist(strsplit(vol, " \\("))[1]))
    volume <- as.numeric(sub("[ )a-z]+", "", unlist(strsplit(vol, " \\("))[2]))
    unit <- sub(volume, "", unlist(strsplit(vol, " \\("))[2])
    unit <- trimws(sub("\\)[ a-z]+", "", unit))
    
    return(quantity * convert(volume, unit))
  }
  
  vols[grepl("^[0-9/ ]+ \\([0-9/ .]+ ", vols)] <- sapply(vols.to.convert, multiply.vols, USE.NAMES = FALSE)
  
  # for any non-volumes, impute mean volume
  ingredient.amts[-is.volume] <- mean(as.numeric(vols))
  ingredient.amts[is.volume] <- vols
  
  return(as.numeric(ingredient.amts))
}

# Process all volumes
vol.amts <- rapply(vol.amts, process.volumes, how="replace")

############################
# something like this:
normalized.amts <- mapply(amts, normalize.ingredient, names(amts))

norm_rawquantity <- function(ingredient.amts, ingredient.name) {
  servings <- all_ingredients$Servings[all_ingredients$ingredient==ingredient.name]
  a <- as.numeric(ingredient.amts)/servings # get amounts per serving
  norm <- (a - min(a))/(max(a) - min(a))
  return(norm+1)
}