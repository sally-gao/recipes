library(tidyverse)

recipes <- read_csv("recipes_ascii.csv")
recipes2 <- read_csv("recipes2_ascii.csv")

# Get rid of weird extra columns
recipes <- recipes[,names(recipes)[1:12]]

recipes <- rbind(recipes, recipes2)

# We don't know how many users rated each recipe. However, we know how many people claimed to
# have the recipe and how many written reviews it has. We'll drop all recipes that either
# have been "made" less than 5 times or reviewed less than 5 times.
# recipes_filtered <- recipes %>% 
#   filter(MadeIt >= 5 | ReviewCount >= 5)

# Without filtering recipes
recipes_filtered <- recipes

summary(recipes_filtered)

# create recipe ID col
recipes_filtered <- recipes_filtered %>%
  mutate(ID = row_number()) 

## EXTRACT INGREDIENTS AND CORRESPONDING AMOUNTS ====================================================================

measures <- "(cup|teaspoon|tablespoon| can|pound|package|clove|dash|sprig|pinch|slice|quart|drop|stalk|liter|milliliter|gallon|pint|bottle|bag|carton|container)"
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
                   "loin", "ribs", "roast", "sirloin", "shoulder", "mild", "smashed", "roughly", "pressed", "chilled",
                   "legs", "leg", "coarse", "kosher", "for", "garnish", "decoration", "dusting", "frying", "coating", "at",
                   "wings", "instant", "nonfat", "non-fat", "sharp", "pure", "seedless", "chunks", "bone-in", "creamy",
                   "crunchy", "hard-boiled", "loaf", "loaves", "lowfat", "miniature", "soft", "blanched", "slivered",
                   "firm", "extra-firm", "silken", "semi-sweet", "mini", "hulled", "granules", "strong", "brewed", "bunch",
                   "stemmed", "drumsticks", "drumettes", "pounded", "tenders", "cube", "cubes", "fine", "grilled", "head",
                   "heads", "florets", "leaf", "leaves", "long", "grain", "long-grain", "lukewarm", "pods", "husked",
                   "tenderloins", "washed", "bulb", "bulbs", "cooled", "strips", "deli", "into", "torn", "bite-size",
                   "bite-sized", "pieces", "piece", "pinch", "pinches", "cracked", "quick-cooking", "quick", "serving",
                   "old-fashioned", "liquid", "processed", "unbleached", "unwrapped", "100%", "lactose-free", "all",
                   "purpose", "bunches", "wedges", "dashes", "envelope", "envelopes", "original", "premium", "regular",
                   "reserved", "hard-cooked", "unpeeled", "whisked", "well", "2%", "low-sodium", "reduced-sodium",
                   "bulk", "links", "no-stick", "nonstick", "fire-roasted", "gluten-free", "no-salt-added", "real",
                   "threads", "wheel", "center-cut", "dry-roasted", "fillets", "plus", "fully", "natural", "cleaned",
                   "new", "non-stick", "stick", "sticks", "aged", "box", "jigger", "jiggers", "packet", "part-skim",
                   "scrubbed", "skin-on", "stripped", "lengthwise")

# pattern to match common phrases and anything in parentheses
pattern <- "( to taste| as needed| cut into| room temperature| broken into| to cover| with juice| liquid reserved| juice reserved| with skin|individually wrapped| if needed| in juice | with peel | as desired | casings removed)|(\\(.*\\) ?)|( cut.*(strips|pieces|wedges|chunks|cubes|slices))"
brands <- c("kraft", "criscoapillsbury besta", "pillsbury best", "pillsburya", "pillsbury", "werthers original", "goldhen",
            "huntsa", "land o lakes", "stonemill essentials", "bakers corner", "hersheyas", "hershey", "mccormicka",
            "mccormick", "delallo", "happy farms", "maillea", "maille", "tabascobrand", "lucky leaf", "ortegaa",
            "bob evansa", "bob evans", "reddi-wiporigina", "countryside creamery", "panko", "carlini", "dannon oikosa",
            "dannon oikos", "egglands best", "philadelphia brick", "dijon originale", "pamaoriginal", "athenos traditional",
            "jifa", "red delicious", "spice islands", "college inna", "contadina", "goya", "johnsonvillea", "johnsonville",
            "nestlea toll housea", "nestlea carnationa", "nestle", "planters", "friendly farms", "ghirardelli",
            "hatfieldrecipe essentials", "hass", "musselmansa", "southern grove", "appleton farms", "bosc")

process_ingredients <- function(ingredient_str) {
  ingredient_str <- tolower(gsub(",|[*]", "", ingredient_str)) # remove commas and convert to lowercase
  ingredient_str <- gsub(paste(brands, collapse="|"), "", ingredient_str) # remove matches to brand names
  ingredient_str <- gsub(pattern, "", ingredient_str) # remove matches to pattern
  splitstr <- unlist(strsplit(ingredient_str, " ")) # split on spaces
  splitstr <- splitstr[(splitstr %in% wordsToRemove==FALSE)] # remove words named above
  return(trimws(paste(splitstr, collapse=" ")))
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
  filter(count > 7)

# For every unique ingredient for which count > 7, get a list of all the amount descriptors associated with it
amts <- sapply(selected_ingredients$ingredient, function(ingredient)
  {all_ingredients$amt[all_ingredients$ingredient==ingredient]}, USE.NAMES = TRUE)

# Which amts are mostly empty strings?
percent.empty <- sapply(amts, function(x) {sum(x=="")/length(x)})
which(percent.empty > .8)
# cooking spray       filling 
# 53           383

# Remove cooking spray, filling
amts <- amts[-empties]
selected_ingredients <- selected_ingredients %>% filter(!(ingredient=="cooking spray"|ingredient=="filling"))

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
sum(is.mostly.volumes) # 390

# Find all ingredients for which over 70% of amounts (not including "") are specified in weight
mostly.weights <- function(ingredient.amts) {
  total.matches <- sapply(weights, function(x) sum(grepl(x, ingredient.amts)))
  total.matches[2] <- total.matches[2] - sum(grepl("fluid ounce", ingredient.amts))
  return(sum(total.matches) > (length(ingredient.amts[ingredient.amts!=""]))*0.7)
}

is.mostly.weights <- rapply(amts, mostly.weights, how="unlist")
sum(is.mostly.weights) # 137

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
sum(is.mostly.raw) # 74

# Find all ingredients for which amounts are specified in a mix of volumes, weights, or others
mixed <- is.mostly.volumes+is.mostly.weights+is.mostly.raw
mixed <- mixed[mixed==0]
length(mixed) # 176 such ingredients to deal with

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

get.weights <- function(weights) {
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
  
  return(weights)
}

process.weights <- function(ingredient.amts) {
  is.weight <- c(grep("ounce", ingredient.amts), grep("pound", ingredient.amts))
  is.weight <- is.weight[!is.weight %in% grep("fluid", ingredient.amts)]
  weights <- ingredient.amts[is.weight]
  
  weights <- get.weights(weights)
  
  # for any non-weights, impute mean weight
  ingredient.amts[-is.weight] <- mean(as.numeric(weights), na.rm = TRUE)
  ingredient.amts[is.weight] <- weights
  
  # If there are any NAs, impute mean
  # ingredient.amts[is.na(as.numeric(ingredient.amts))] <- mean(as.numeric(weights), na.rm = TRUE)
  
  return(as.numeric(ingredient.amts))
}

# Process all weights
weights.amts <- rapply(weights.amts, process.weights, how="replace")

weights.NAs <- rapply(weights.amts, is.na, how="replace")
sum.NAs <- rapply(weights.NAs, sum, how="unlist")
which(sum.NAs!=0)
# beef chuck     salmon 
# 32         73 

weights.amts$`beef chuck` # "1 (3 1/2) pound"
weights.amts$salmon # "1 whole (5 pound)"

## CONVERTING VOLUMES ====================================================================

vol.conversions <- c(1, 48, 16, 0.25, 8, 0.24, 240, 0.0625, 0.5, 768, 2880, 384)
names(vol.conversions) <- volumes

# Function that converts any volume to cups
convert <- function(vol, unit) {
  return(unname(vol/vol.conversions[unit]))
}

vol.amts <- amts[is.mostly.volumes]

get.volumes <- function(vols) {
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
  
  return(vols)
}

process.volumes <- function(ingredient.amts) {
  is.volume <- grep(paste(volumes, collapse ="|"), ingredient.amts)
  vols <- ingredient.amts[is.volume]
  
  vols <- get.volumes(vols)
  
  # for any non-volumes, impute mean volume
  ingredient.amts[-is.volume] <- mean(as.numeric(vols))
  ingredient.amts[is.volume] <- vols
  
  # If there are any NAs, impute mean
  # ingredient.amts[is.na(as.numeric(ingredient.amts))] <- mean(as.numeric(vols), na.rm = TRUE)
  
  return(as.numeric(ingredient.amts))
}

# Process all volumes
vol.amts <- rapply(vol.amts, process.volumes, how="replace")

vol.NAs <- rapply(vol.amts, is.na, how="replace")
sum.NAs <- rapply(vol.NAs, sum, how="unlist")
which(sum.NAs!=0)

# baking soda canning jars with lids rings 
# 14                          325

vol.amts$`baking soda` # "1 small pinch"
vol.amts$`canning jars with lids rings` "9 (1 pint)"  "3 (1 pint)"
# Should remove things with all NAs

## CONVERTING RAW QUANTITIES ====================================================================

raw.amts <- amts[is.mostly.raw]

process.raw <- function(ingredient.amt) {
  raw <- ingredient.amt[!grepl("[a-z]+", ingredient.amt)]
  quantities <- convert_frac(raw)
  ingredient.amt[!grepl("[a-z]+", ingredient.amt)] <- quantities
  
  # Impute mean for non-raw quantities
  ingredient.amt[grepl("[a-z]+", ingredient.amt)] <- mean(as.numeric(quantities))
  
  return(as.numeric(ingredient.amt))
}

# Process all raw quantities
raw.amts <- rapply(raw.amts, process.raw, how="replace")

raw.NAs <- rapply(raw.amts, is.na, how="replace")
sum.NAs <- rapply(raw.NAs, sum, how="unlist")
which(sum.NAs!=0)
# lemons            bananas   jalapeno peppers              limes unbaked pie crusts    cinnamon sticks 
# 4                  8                  9                 11                 22                 23 
# romaine lettuce         toothpicks     wooden skewers 
# 27                 30                 74 

## CONVERTING MIXED AMTS & SPECIAL UNITS ====================================================================

is.mixed <- !(is.mostly.volumes+is.mostly.weights+is.mostly.raw)
mixed.amts <- amts[is.mixed]

special.units <- c("clove", "sprig", "slice", "stalk")

# Find all ingredients for which over 70% of amounts (not including "") are specified in one of the 4 special units
mostly.special <- function(ingredient.amts) {
  total.matches <- sapply(special.units, function(x) sum(grepl(x, ingredient.amts)))
  return(sum(total.matches) > (length(ingredient.amts[ingredient.amts!=""]))*0.7)
}

is.mostly.special <- rapply(mixed.amts, mostly.special, how="unlist")
names(is.mostly.special[is.mostly.special==TRUE])
# [1] "garlic"          "bread"           "american cheese" "white bread"     "rye bread"

# Deal with special units
process.special <- function(ingredient.amts, unit) {
  in.special <- grepl(unit, ingredient.amts)
  special <- frac_to_float(sub(" .+$", "", ingredient.amts[in.special]))
  ingredient.amts[!in.special] <- mean(as.numeric(special))
  ingredient.amts[in.special] <- special
  return(as.numeric(ingredient.amts))
}

mixed.amts$garlic <- process.special(amts$garlic, "clove")
mixed.amts$bread <- process.special(mixed.amts$bread, "slice")
mixed.amts$`white bread` <- process.special(mixed.amts$`white bread`, "slice")

# Dealing with mixed amounts
mixed.names <- names(mixed.amts[!is.mostly.special])

# mixed.chart <- tibble(ingredient = mixed.names)
# write_csv(mixed.chart, "mixed_chart.csv", col_names = TRUE)

# Import manually researched conversion chart
mixed.chart <- read_csv("mixed_chart.csv")
mixed.chart$oz <- mixed.chart$grams*0.035274

process.mixed <- function(ingredient.amts, ingredient.name) {
  is.raw <- !grepl("[a-z]+", ingredient.amts)
  raw <- as.numeric(frac_to_float(ingredient.amts[is.raw]))
  
  is.weight <- c(grep("ounce", ingredient.amts), grep("pound", ingredient.amts))
  is.weight <- is.weight[!is.weight %in% grep("fluid", ingredient.amts)]
  weights <- as.numeric(unlist(get.weights(ingredient.amts[is.weight]))) # all weights in ounces
  
  is.volume <- grep(paste(volumes, collapse ="|"), ingredient.amts)
  vols <- as.numeric(unlist(get.volumes(ingredient.amts[is.volume]))) # all volumes in cups
  
  is.slice <- grepl("slice", ingredient.amts)
  slice <- as.numeric(frac_to_float(sub(" .+$", "", ingredient.amts[is.slice])))
  
  converted.amts <- numeric(length(ingredient.amts))
  converted.amts[is.raw] <- raw/mixed.chart$quantity[mixed.chart$ingredient==ingredient.name]
  converted.amts[is.weight] <- weights/mixed.chart$oz[mixed.chart$ingredient==ingredient.name]
  converted.amts[is.volume] <- vols/mixed.chart$cup[mixed.chart$ingredient==ingredient.name]
  converted.amts[is.slice] <- slice/mixed.chart$slice[mixed.chart$ingredient==ingredient.name]
  
  # For NA or "", impute mean
  converted.amts[is.na(converted.amts)|ingredient.amts==""] <- mean(converted.amts, na.rm=TRUE)
  
  return(converted.amts)
}

mixed.amts[!is.mostly.special] <- mapply(process.mixed,
                                         ingredient.name = mixed.names,
                                         ingredient.amt = rapply(mixed.amts[!is.mostly.special],
                                                                 function(x) {return(x)}, how="replace"))

## UPDATE AMTS ====================================================================

amts[is.mostly.weights] <- weights.amts
amts[is.mostly.volumes] <- vol.amts
amts[is.mostly.raw] <- raw.amts
amts[is.mixed] <- mixed.amts

## CREATE UNNORMALIZED MATRIX OF INGREDIENT WEIGHTS ====================================================================

dtm <- recipes_filtered %>% 
  select(ID, Rating, ReviewCount, MadeIt, Calories)

dtm[names(amts)] <- NA

for (name in names(amts)) {
  
  ingredient.amts <- amts[[name]]
  servings <- all_ingredients$Servings[all_ingredients$ingredient==name]
  
  ingredient.amts <- ingredient.amts/servings
  
  amts.ids <- all_ingredients$recipe_id[all_ingredients$ingredient==name]
  
  # Deal with duplicated recipe ids for recipes where ingredient appears more than once
  if (length(amts.ids[duplicated(amts.ids)])!=0) {
    for (id in amts.ids[duplicated(amts.ids)]) {
      ingredient.amts[which(amts.ids==id)[1]] <- sum(ingredient.amts[amts.ids==id])
    }
    
    to.remove <- numeric(0)
    
    for (id in amts.ids[duplicated(amts.ids)]) {
      to.remove <- c(to.remove, which(amts.ids==id)[-1])
    }
    
    ingredient.amts <- ingredient.amts[-to.remove]
  }
  
  dtm[dtm$ID %in% all_ingredients$recipe_id[all_ingredients$ingredient==name],name] <- ingredient.amts
}

# Filter out recipes that don't have at least 3 ingredients
at.least.three.ingredients <- apply(dtm, MARGIN=1, FUN=function(row) {sum(is.na(row[6:428])) < 421})

dtm <- dtm %>% filter(at.least.three.ingredients)

# write_csv(dtm, "dtm.csv", col_names=TRUE)