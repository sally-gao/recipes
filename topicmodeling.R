library(tm)
library(topicmodels)
library(parallel)

recipe.ingredients <- read.csv("dtm2.csv")

## MIN-MAX NORMALIZE INGREDIENT AMTS ================================================================

# Convert all ingredients to numeric
recipe.ingredients[] <- lapply(recipe.ingredients, function(x) as.numeric(x))

# Min-max normalize ingredients
minmax <- function(ingredient.amts) {
  norm <- (ingredient.amts-min(ingredient.amts, na.rm = TRUE))/(max(ingredient.amts, na.rm = TRUE)-min(ingredient.amts, na.rm = TRUE))
  return(norm)
}

recipe.ingredients[,6:534] <- apply(recipe.ingredients[,6:534], MARGIN=2, FUN=minmax)

# Find minimum amount for every ingredient (that is not 0)
minima <- apply(recipe.ingredients[,6:534], MARGIN=2, FUN=function(ingredient) {min(ingredient[!ingredient==0], na.rm=TRUE)})

# Where are the zeros?
min.lengths <- apply(recipe.ingredients[,6:534], MARGIN=2, FUN=function(x) {length(which(x==0 & !is.na(x)))})
sum(min.lengths) # 733 zeros

# Replace the zeros with minima
for (ingredient in names(recipe.ingredients[,6:534])) {
  col <- recipe.ingredients[,ingredient]
  col[which(col==0 & !is.na(col))] <- minima[ingredient]
  recipe.ingredients[,ingredient] <- col
}

# Check
min.lengths <- apply(recipe.ingredients[,6:534], MARGIN=2, FUN=function(x) {length(which(x==0 & !is.na(x)))})
sum(min.lengths) # 0

# Impute all missing values as 0
recipe.ingredients[is.na(recipe.ingredients)] <- 0

## TURN RECIPES INTO TEXT CORPUS ================================================================

# Create recipe "documents" based on ingredient weights
recipe.to.text <- function(recipe) {
  frequency <- ceiling((recipe/sum(recipe))*100)
  doc <- paste(rep(names(frequency), frequency), collapse=" ")
  return(doc)
}

docs <- apply(recipe.ingredients[6:534], MARGIN=1, FUN=recipe.to.text)
docs.df <- as.data.frame(docs)

corpus <- VCorpus(DataframeSource(docs.df))

# Document-term matrix with term frequency weighting
recipes.dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))

## TOPIC MODELING W/ LATENT DIRICHLET ALLOCATION ================================================================

clust <- makeCluster(3)
clusterEvalQ(clust,library(topicmodels))
clusterExport(clust, "recipes.dtm")

# LDA with k=30, k=50 and k=100, 50 times each
tm30 <- parLapply(clust, 1:50, function(x) {return(LDA(recipes.dtm, 30))})

tm50 <- parLapply(clust, 1:50, function(x) {return(LDA(recipes.dtm, 50))})

tm100 <- parLapply(clust, 1:50, function(x) {return(LDA(recipes.dtm, 100))})

stopCluster(clust)

# Get top 3 ingredients from all topics from all topic models
all.tms <- c(tm30, tm50, tm100)
all.trios <- vector()

for (i in 1:length(all.tms)) {
  top.threes <- terms(all.tms[[i]], 3)
  top.sorted <- apply(top.threes, MARGIN=2, FUN=sort) # sort ingredients alphabetically so we can find duplicates
  top.strings <- apply(top.sorted, MARGIN=2, FUN=paste, collapse=",")
  all.trios <- c(all.trios, top.strings)
}

temp.df <- data.frame(all.trios, row.names=NULL)

duplicates <- temp.df %>% 
  group_by(all.trios) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) %>% 
  arrange(desc(count))

# write.table(duplicates, "ingredient_trios.txt", sep=",")

######################################

# Topic modeling, k = 25
tm25 <- LDA(recipes.dtm, 25)
terms(tm25, 5)