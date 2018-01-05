library(imager)
library(tidyverse)
library(RColorBrewer)

grouped.ingredients.25 <- grouped_ingredients[1:25,]
grouped.ingredients.25$ingredient[16] <- "chicken"

png("imgs/ingredients.png", width=650,height=600)

ggplot(data = grouped.ingredients.25) + 
  geom_col(mapping = aes(x = ingredient, y = count, fill=ingredient)) +
  scale_x_discrete(limits = rev(grouped.ingredients.25$ingredient)) +
  scale_fill_manual(values=rep(brewer.pal(11, "Spectral"), 3)[1:25]) +
  geom_text(aes(x=ingredient, y=count, label=count, hjust=-.1), colour="grey22", size=3.5) +
  guides(fill=FALSE) +
  ggtitle("Top 25 Most Common Ingredients:") +
  theme_light() +
  theme(
    # text = element_text(family="Courier"),
    # panel.background = element_blank(),
    plot.title = element_text(family="Helvetica", size=24, face="bold", hjust=0),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12),
    axis.line.y = element_line(colour="grey"),
    axis.line.x = element_line(colour="grey")
  ) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,2800)) +
  coord_flip()

dev.off()


ingredient.trios <- read.csv("ingredient_trios.txt")
ingredient.trios$all.trios <- as.character(ingredient.trios$all.trios)

top.trios <- strsplit(ingredient.trios$all.trios[1:36], ",")

top.lst <- rapply(top.trios, paste, how = "unlist", collapse = " / ")
top.lst <- gsub("toppings", "whipped.topping", top.lst)
top.counts <- paste("(", ingredient.trios$count[1:36], ")", sep="")
top.combos <- paste(top.lst, top.counts)

# [1] baking.powder / flour / sugar (89)
# [2] baking.soda / flour / sugar (40)
# [3] cocoa.powder / sugar / vanilla.extract (33)
# [4] flour / sugar / vanilla.extract (27)
# [5] active.yeast / bread.flour / wheat.flour (23)
# [6] basil / garlic / olive.oil (14)
# [7] garlic / onions / tomatoes (14)
# [8] butter / flour / sugar (13)
# [9] butter / sugar / vanilla.extract (13)
# [10] celery / chicken.broth / onions (12)
# [11] garlic / lemon.juice / olive.oil (12)
# [12] garlic / lemons / olive.oil (12)
# [13] green.bell.peppers / onions / tomatoes (12)
# [14] baking.soda / sugar / vanilla.extract (9)
# [15] garlic / olive.oil / tomatoes (9)
# [16] beef / onions / tomato.sauce (8)
# [17] brown.sugar / butter / pecans (8)
# [18] brown.sugar / pecans / vanilla.extract (8)
# [19] graham.cracker.crusts / whipped.topping / vanilla.pudding.mix (8)
# [20] active.yeast / bread.flour / cranberries (7)
# [21] butter / salt / sugar (7)
# [22] cheddar.cheese / eggs / milk (7)
# [23] garlic / olive.oil / parsley (7)
# [24] green.bell.peppers / onions / rice (7)
# [25] almond.extract / cream.of.tartar / sugar (6)
# [26] baking.powder / flour / shortening (6)
# [27] beef / chili.powder / onions (6)
# [28] black.pepper / garlic / olive.oil (6)
# [29] butter / cocoa.powder / pecans (6)
# [30] carrots / garlic / onions (6)
# [31] chocolate.chips / vanilla.extract / walnuts (6)
# [32] corn.syrup / sugar / vanilla.extract (6)
# [33] egg.whites / egg.yolks / peanut.butter (6)
# [34] garlic / olive.oil / red.onions (6)
# [35] lemon.juice / soy.sauce / white.vinegar (6)
# [36] onions / tomatoes / zucchinis (6)