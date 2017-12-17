library(wordcloud)
library(wordcloud2)

get.top.topics <- function(tm, num.topics, num.terms) {
  top.terms <- terms(tm, num.terms)
  freq <- sort(table(topics(tm)), decreasing = TRUE)
  top.topics <- as.numeric(names(freq)[1:num.topics])
  return(top.terms[,top.topics])
}

plot.combinations <- function(tm.topics) {
  rows <- ceiling(ncol(tm.topics)/3)
  
  par(mfrow=c(rows, 3))
  
  word.colors <- c("palegreen4", "chocolate1", "seagreen4", "tomato3", "goldenrod3", "deepskyblue4",
                   "indianred3", "mediumorchid3", "firebrick3", "cyan4", "pink4", "aquamarine4")
  
  for (i in 1:ncol(tm.topics)) {
    wordcloud(c(topics.tm100[,i]), c(2,1,1), min.freq=1, random.color=TRUE, rot.per=0, scale=c(2.2,1.7),
              random.order = FALSE, colors=word.colors)
  }
}

topics.tm100 <- get.top.topics(tm100, 12, 3)
topics.tm200 <- get.top.topics(tm200, 12, 3)

png("imgs/tm100wordcloud.png", width=650,height=540)
plot.combinations(topics.tm100)
dev.off()

png("imgs/tm200wordcloud.png", width=650,height=540)
plot.combinations(topics.tm200)
dev.off()