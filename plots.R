## Import data ----
library(tidyverse)
setwd('/Users/ujjwalpeshin/Desktop/sustainable_peace_reddit/')
data <- read.csv('./conflicting_comments.csv')

data$from_to <- paste(data$comment_group,'replied to',data$submission_group)

## Make density plot ----
ggplot() +
  geom_density(data = data, aes(compound_sentiment, color = from_to))

## Make scatterplot over time ----
remove_cate <- c(" replied to " , 
                 "marokayets reward peoriginal posterle provide best product governothing muchent worokay governothing muchents accountable public repreddit enhancement suiteent would governothing muchent incentive provide best service public well replied to []",
                 "yeah would make story header partial graphic deviant replied to []"  )
data <- data[! data$from_to %in% remove_cate,]
data$modi_datetime <- as.POSIXct(strptime(data$timestamp, format = "%Y-%m-%d %H:%M:%S"))

ggplot() +
  geom_point(data = data, aes(x = modi_datetime, y = compound_sentiment, color = from_to), alpha = 0.3)

## One-Way ANOVA ----
shapiro.test(data[data$from_to=="Republican replied to Republican",]$compound_sentiment)










