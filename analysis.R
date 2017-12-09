library(tidyverse)

recipe.ingredients <- read_csv("dtm.csv")

## LINEAR MODEL WITH NON-INGREDIENT VARIABLES ================================================================

# Remove recipes for which Rating is 0, because it's likely that nobody has ever given those recipes a rating.
recipe.ingredients <- recipe.ingredients %>% 
  filter(Rating!=0)

# Look at missing non-ingredient variables
recipe.ingredients[,2:5] %>% sapply(function(x) sum(is.na(x)))
# Rating ReviewCount      MadeIt    Calories 
# 0           0           0          23

# Impute mean for missing Calories
recipe.ingredients$Calories[is.na(recipe.ingredients$Calories)] <- mean(recipe.ingredients$Calories, na.rm=TRUE)

no.ingredients.lm <- lm(Rating ~ ReviewCount+MadeIt+Calories, data=recipe.ingredients)
summary(no.ingredients.lm)
# Multiple R-squared:  0.01571,	Adjusted R-squared:  0.01437

# Only ReviewCount seems to be significant. What if we only used that to predict Rating?
reviewcount.lm <- lm(Rating ~ ReviewCount, data=recipe.ingredients)
summary(reviewcount.lm)
# Multiple R-squared:  0.01548,	Adjusted R-squared:  0.01503

# Training RMSE:
reviewcount.lm$residuals^2 %>% mean %>% sqrt
# 0.4575153

## LINEAR MODEL WITH INGREDIENT VARIABLES ================================================================

# Impute all missing values as 0
recipe.ingredients[is.na(recipe.ingredients)] <- 0

# Convert all ingredients to numeric
recipe.ingredients[] <- lapply(recipe.ingredients, function(x) as.numeric(x))

# Build linear model
ingredients.lm <- lm(Rating ~ . -ID, data=recipe.ingredients)
summary(ingredients.lm)
# Multiple R-squared:  0.2675,	Adjusted R-squared:  0.09165  

# Training RMSE:
ingredients.lm$residuals^2 %>% mean %>% sqrt
# 0.3946461

## LINEAR MODEL WITH INDICATOR INGREDIENT VARIABLES ================================================================

indicator.ingredients <- recipe.ingredients
indicator.ingredients[,6:428][indicator.ingredients[,6:428]!=0] <- 1

indicator.lm <- lm(Rating ~ . -ID, data=indicator.ingredients)
summary(indicator.lm)
# Multiple R-squared:  0.2619,	Adjusted R-squared:  0.08477

# Training RMSE:
indicator.lm$residuals^2 %>% mean %>% sqrt
# 0.3961381
# Similar to the model with ingredient weightings.

## LINEAR MODEL WITH MIN-MAX NORMALIZED INGREDIENT VARIABLES ================================================================

minmax.ingredients <- read_csv("dtm.csv")

minmax.ingredients <- minmax.ingredients %>% 
  filter(Rating!=0)

# Impute mean for missing Calories
recipe.ingredients$Calories[is.na(recipe.ingredients$Calories)] <- mean(recipe.ingredients$Calories, na.rm=TRUE)

# Convert all ingredients to numeric
minmax.ingredients[] <- lapply(minmax.ingredients, function(x) as.numeric(x))

# Indicator (0 or 1) for each ingredient
indicators <- indicator.ingredients[,6:428]
names(indicators) <- sapply(names(indicators), function(name) {paste("has_", name, sep="")})

# Min-max normalize ingredients
minmax <- function(ingredient.amts) {
  norm <- (ingredient.amts-min(ingredient.amts, na.rm = TRUE))/(max(ingredient.amts, na.rm = TRUE)-min(ingredient.amts, na.rm = TRUE))
  return(norm)
}

minmax.ingredients[,6:428] <- apply(minmax.ingredients[,6:428], MARGIN=2, FUN=minmax)

# Impute all missing values as 0
minmax.ingredients[is.na(minmax.ingredients)] <- 0

minmax.ingredients <- minmax.ingredients %>% 
  cbind(indicators)

minmax.lm <- lm(Rating ~ . -ID, data=minmax.ingredients)
summary(minmax.lm)
# Multiple R-squared:  0.4555,	Adjusted R-squared:  0.1162

# Training RMSE:
minmax.lm$residuals^2 %>% mean %>% sqrt
# 0.3402445 - lowest RMSE so far

