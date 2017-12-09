library(tidyverse)
library(tm)

recipe.ingredients <- read.csv("dtm.csv")

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
# Multiple R-squared:  0.2621,	Adjusted R-squared:  0.08503 

# Training RMSE:
indicator.lm$residuals^2 %>% mean %>% sqrt
# 0.3960819
# Similar to the model with ingredient weightings.

## LINEAR MODEL WITH MIN-MAX NORMALIZED INGREDIENT VARIABLES ================================================================

minmax.ingredients <- read.csv("dtm.csv")

minmax.ingredients <- minmax.ingredients %>% 
  filter(Rating!=0)

# Impute mean for missing Calories
minmax.ingredients$Calories[is.na(minmax.ingredients$Calories)] <- mean(minmax.ingredients$Calories, na.rm=TRUE)

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
# 0.33948 - lowest RMSE so far

# How about without the indicator variables?
minmax.lm2 <- lm(Rating ~ . -ID, data=minmax.ingredients[1:428])
summary(minmax.lm2)
# Multiple R-squared:  0.2626,	Adjusted R-squared:  0.0862

# Training RMSE:
minmax.lm2$residuals^2 %>% mean %>% sqrt
# 0.3959411

## LINEAR MODEL W/ INGREDIENTS STANDARDIZED BY MEAN ABSOLUTE DEVIATION ================================================================

standard.ingredients <- read.csv("dtm.csv")

standard.ingredients <- standard.ingredients %>% 
  filter(Rating!=0)

# Impute mean for missing Calories
standard.ingredients$Calories[is.na(standard.ingredients$Calories)] <- mean(standard.ingredients$Calories, na.rm=TRUE)

# Convert all ingredients to numeric
standard.ingredients[] <- lapply(standard.ingredients, function(x) as.numeric(x))

# Standardize ingredients by mean absolute deviation
standardize <- function(ingredient.amts) {
  deviation <- mean(abs(ingredient.amts-mean(ingredient.amts, na.rm = TRUE)), na.rm = TRUE)
  
  if(deviation!=0) {
    standardized <- ingredient.amts/deviation
    return(standardized) 
  }
  
  else{return(ingredient.amts)}
}

standard.ingredients[,6:428] <- apply(standard.ingredients[,6:428], MARGIN=2, FUN=standardize)

# Impute all missing values as 0
standard.ingredients[is.na(standard.ingredients)] <- 0

standard.lm <- lm(Rating ~ . -ID, data=standard.ingredients[,1:290])
summary(standard.lm)
# Multiple R-squared:  0.2228,	Adjusted R-squared:  0.1058

# Training RMSE:
standard.lm$residuals^2 %>% mean %>% sqrt
# 0.4065078

## TOPIC MODELING: CREATE DOCUMENT-TERM MATRIX ================================================================

# Create recipe "documents" based on ingredient weights
norm.ingredients <- minmax.ingredients[6:428]

recipe.to.text <- function(recipe) {
  frequency <- ceiling((recipe/sum(recipe))*100)
  doc <- paste(rep(names(frequency), frequency), collapse=" ")
  return(doc)
}

docs <- apply(norm.ingredients, MARGIN=1, FUN=recipe.to.text)
docs.df <- as.data.frame(docs)

corpus <- VCorpus(DataframeSource(docs.df))

# Document-term matrix with term frequency weighting
recipes.dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))

## TOPIC MODELING W/ LATENT DIRICHLET ALLOCATION ================================================================

# Topic modeling, k = 10
tm10 <- LDA(recipes.dtm, 10)
terms(tm10, 10)

# Topic modeling, k = 25
tm25 <- LDA(recipes.dtm, 25)
terms(tm25, 5)

# k = 100
tm100 <- LDA(recipes.dtm, 100)
terms(tm100, 5)

# k = 200
tm200 <- LDA(recipes.dtm, 200)

tm10.odds <- posterior(tm10, recipes.dtm)$topics %>% as.data.frame
tm25.odds <- posterior(tm25, recipes.dtm)$topics %>% as.data.frame
tm100.odds <- posterior(tm100, recipes.dtm)$topics %>% as.data.frame
tm200.odds <- posterior(tm200, recipes.dtm)$topics %>% as.data.frame

tm25.odds$Rating <- recipe.ingredients$Rating
tm25.lm <- lm(Rating ~ ., data=tm25.odds)
summary(tm25.lm)
# Multiple R-squared:  0.01674,	Adjusted R-squared:  0.005901 

# Training RMSE:
tm25.lm$residuals^2 %>% mean %>% sqrt
# 0.457222

tm100.odds$Rating <- recipe.ingredients$Rating
tm100.lm <- lm(Rating ~ ., data=tm100.odds)
summary(tm100.lm)
# Multiple R-squared:  0.07128,	Adjusted R-squared:  0.02754

# Training RMSE:
tm100.lm$residuals^2 %>% mean %>% sqrt
# 0.4443612

tm200.odds$Rating <- recipe.ingredients$Rating
tm200.lm <- lm(Rating ~ ., data=tm200.odds)
summary(tm200.lm)
# Multiple R-squared:  0.1371,	Adjusted R-squared:  0.05128

tm200.lm$residuals^2 %>% mean %>% sqrt
# 0.4283356

## USING "MOST LIKELY TOPIC" AS A PREDICTOR ================================================================

topics.tm10 <- topics(tm10)
topics.tm25 <- topics(tm25)

minmax.tm10.lm <- lm(Rating ~ . -ID +as.factor(topics.tm10), data=minmax.ingredients)
summary(minmax.tm10.lm)
# Multiple R-squared:  0.4628,	Adjusted R-squared:  0.1202 

minmax.tm10.lm$residuals^2 %>% mean %>% sqrt 
# Training RMSE: 0.3379662

minmax.tm25.lm <- lm(Rating ~ . -ID +as.factor(topics.tm25), data=minmax.ingredients)
summary(minmax.tm25.lm)
# Multiple R-squared:  0.4665,	Adjusted R-squared:  0.1164 

minmax.tm25.lm$residuals^2 %>% mean %>% sqrt 
# 0.3368027

# How about 50 topics?
tm50 <- LDA(recipes.dtm, 50)
topics.tm50 <- topics(tm50)

minmax.tm50.lm <- lm(Rating ~ . -ID +as.factor(topics.tm50), data=minmax.ingredients)
summary(minmax.tm50.lm)
# Multiple R-squared:  0.4745,	Adjusted R-squared:  0.1129 

minmax.tm50.lm$residuals^2 %>% mean %>% sqrt 
# 0.3342703

# With 100 topics
topics.tm100 <- topics(tm100)
minmax.tm100.lm <- lm(Rating ~ . -ID +as.factor(topics.tm100), data=minmax.ingredients)
summary(minmax.tm100.lm)
# Multiple R-squared:  0.4879,	Adjusted R-squared:  0.1012

minmax.tm100.lm$residuals^2 %>% mean %>% sqrt 
# 0.3299677

# With 200 topics
topics.tm200 <- topics(tm200)
minmax.tm200.lm <- lm(Rating ~ . -ID +as.factor(topics.tm200), data=minmax.ingredients)
summary(minmax.tm200.lm)
# Multiple R-squared:  0.5202,	Adjusted R-squared:  0.08655

minmax.tm200.lm$residuals^2 %>% mean %>% sqrt 
# 0.3193766

## CROSS-VALIDATION ================================================================

test.ids <- sample(c(1:nrow(minmax.ingredients)), 200)

minmax.train <- minmax.ingredients[-test.ids,]
minmax.test <- minmax.ingredients[test.ids,]

# Linear regression using only ReviewCount
reviewcount.cv <- lm(Rating ~ ReviewCount, data=minmax.train)
(minmax.test$Rating-predict(reviewcount.cv, minmax.test))^2 %>% mean %>% sqrt
# Test MSE: 0.4732248

# Linear regression using indicator variables and min-max normalized ingredient variab les
minmax.cv <- lm(Rating ~ . -ID, data=minmax.train)
(minmax.test$Rating-predict(minmax.cv, minmax.test))^2 %>% mean %>% sqrt
# [1] 0.6361273
# Warning message:
#   In predict.lm(minmax.cv, minmax.test) :
#   prediction from a rank-deficient fit may be misleading

norm.cv <- minmax.train[6:428]

# Topic modeling
docs.cv <- apply(norm.cv, MARGIN=1, FUN=recipe.to.text) %>% as.data.frame()
corpus <- VCorpus(DataframeSource(docs.cv))

# Document-term matrix with term frequency weighting
recipes.cv <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))

# Test DTM:

test.cv <- minmax.test[6:428]
# Topic modeling
testdocs.cv <- apply(test.cv, MARGIN=1, FUN=recipe.to.text) %>% as.data.frame()
testcorpus <- VCorpus(DataframeSource(testdocs.cv))
# Document-term matrix with term frequency weighting
test.dtm <- DocumentTermMatrix(testcorpus, control = list(weighting = weightTf))

# k = 10
tm10.cv <- LDA(recipes.cv, 10)
minmax.tm10.cv <- lm(Rating ~ . -ID +as.factor(topics(tm10.cv)), data=minmax.train)

tm10.cv.odds <- posterior(tm10.cv, test.dtm)$topics %>% as.data.frame

extract.topic <- function(row) {
  return(which(row==max(row)))
}

tm10.topics <- apply(tm10.cv.odds, MARGIN=1, extract.topic)

(minmax.test$Rating-predict(minmax.tm10.cv, cbind(minmax.test, tm10.topics)))^2 %>% mean %>% sqrt
