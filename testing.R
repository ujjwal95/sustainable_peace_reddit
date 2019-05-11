## Import libraries ----
library(tidyverse)
library(scales)   # to access breaks/formatting functions

## Setwd ----
setwd('/Users/ujjwalpeshin/Desktop/sustainable_peace_reddit/')

## Import Data ----
data <- read.csv('./conflicting_comments_new.csv')
## Testing ----
# define interactions
rep_to_rep <- data[(data$submission_group=='Republican') & (data$comment_group=='Republican'), 17]
rep_to_dem <- data[(data$submission_group=='Republican') & (data$comment_group=='democrats'), 17]
dem_to_rep <- data[(data$submission_group=='democrats') & (data$comment_group=='Republican'), 17]
dem_to_dem <- data[(data$submission_group=='democrats') & (data$comment_group=='democrats'), 17]

# define testing parameters
alpha <- 0.05
no_of_tests <- 1000
tests <- 1000

one_and_two_val <- 0
one_and_three_val <- 0
one_and_four_val <- 0
two_and_three_val <- 0
two_and_four_val <- 0
three_and_four_val <- 0

# function for testing
testing <- function(x, y, alt) {
  # find normality of samples
  x_norm <- ifelse(shapiro.test(x)$p.value>alpha, TRUE, FALSE)
  y_norm <- ifelse(shapiro.test(y)$p.value>alpha, TRUE, FALSE)
  
  if ((x_norm == TRUE) & (y_norm == TRUE)){
    # carry out f test for variance
    norm_var_p <- var.test(x, y)$p.value
    if (norm_var_p < alpha){
      return(ifelse(t.test(x, y, alternative = alt, var.equal = FALSE)$p.value< alpha, 1, 0))
    }else{
      return(ifelse(t.test(x, y, alternative = alt, var.equal = TRUE)$p.value< alpha, 1, 0))
    }
  }else
    # carry out wicox-test
    return(ifelse(wilcox.test(x, y, alternative = alt)$p.value< alpha , 1, 0))
}

rep_to_rep_mean<- c()
rep_to_dem_mean<- c()
dem_to_rep_mean<- c()
dem_to_dem_mean<- c()
rep_to_rep_var<- c()
rep_to_dem_var<- c()
dem_to_rep_var<- c()
dem_to_dem_var<- c()
# testing stat
for (i in 1:no_of_tests){
  # find seeds
  seed <- i
  set.seed(seed)
  
  # find bootstrap samples
  rep_to_rep_bootstrap <- replicate(tests, sample(rep_to_rep, replace = TRUE))
  rep_to_dem_bootstrap <- replicate(tests, sample(rep_to_dem, replace = TRUE))
  dem_to_rep_bootstrap <- replicate(tests, sample(dem_to_rep, replace = TRUE))
  dem_to_dem_bootstrap <- replicate(tests, sample(dem_to_dem, replace = TRUE))
  
  # find colMeans
  rep_to_rep_bootstrap_mean <- colMeans(rep_to_rep_bootstrap)
  rep_to_dem_bootstrap_mean <- colMeans(rep_to_dem_bootstrap)
  dem_to_rep_bootstrap_mean <- colMeans(dem_to_rep_bootstrap)
  dem_to_dem_bootstrap_mean <- colMeans(dem_to_dem_bootstrap)
  
  # generate samples
#  rep_to_rep_sample <- sample(rep_to_rep_bootstrap_mean, replace = TRUE)
#  rep_to_dem_sample <- sample(rep_to_dem_bootstrap_mean, replace = TRUE)
#  dem_to_rep_sample <- sample(dem_to_rep_bootstrap_mean, replace = TRUE)
#  dem_to_dem_sample <- sample(dem_to_dem_bootstrap_mean, replace = TRUE)
  
  rep_to_rep_sample <- rep_to_rep_bootstrap_mean
  rep_to_dem_sample <- rep_to_dem_bootstrap_mean
  dem_to_rep_sample <- dem_to_rep_bootstrap_mean
  dem_to_dem_sample <- dem_to_dem_bootstrap_mean
  
  rep_to_rep_mean[i] <- mean(rep_to_rep_sample)
  rep_to_dem_mean[i] <- mean(rep_to_dem_sample)
  dem_to_rep_mean[i] <- mean(dem_to_rep_sample)
  dem_to_dem_mean[i] <- mean(dem_to_dem_sample)
  rep_to_rep_var[i] <- var(rep_to_rep_sample)
  rep_to_dem_var[i] <- var(rep_to_dem_sample)
  dem_to_rep_var[i] <- var(dem_to_rep_sample)
  dem_to_dem_var[i] <- var(dem_to_dem_sample)
  
  # find whether the first group has a higher avg than the other group
  one_and_two_val <- one_and_two_val + testing(rep_to_rep_sample, rep_to_dem_sample, alt = 'greater')
  one_and_three_val <- one_and_three_val + testing(rep_to_rep_sample, dem_to_rep_sample, alt = 'greater')
  one_and_four_val <- one_and_four_val + testing(rep_to_rep_sample, dem_to_dem_sample, alt = 'greater')
  two_and_three_val <- two_and_three_val + testing(rep_to_dem_sample, dem_to_rep_sample, alt = 'less')
  two_and_four_val <- two_and_four_val + testing(rep_to_dem_sample, dem_to_dem_sample, alt = 'greater')
  three_and_four_val <- three_and_four_val + testing(dem_to_rep_sample, dem_to_dem_sample, alt = 'greater')
  
}

print(one_and_two_val)
print(one_and_three_val)
print(one_and_four_val)
print(two_and_three_val)
print(two_and_four_val)
print(three_and_four_val)


data$conversation <- paste(data$submission_group, 'to', data$comment_group)
data_1 <- data[data$conversation == unique(data$conversation)[1] | 
                 data$conversation == unique(data$conversation)[2] | 
                 data$conversation == unique(data$conversation)[5] | 
                 data$conversation == unique(data$conversation)[6],]
ggplot()+ 
  geom_density(aes(compound_sentiment,  fill = conversation, color = conversation), data = data_1, alpha = 0.15) + xlab(label = 'Sentiment')

ggplot() + geom_density(aes(rep_to_rep_sample))
ggplot() + geom_density(aes(rep_to_dem_sample))
ggplot() + geom_density(aes(dem_to_rep_sample))
ggplot() + geom_density(aes(dem_to_dem_sample))


data_1$date <- as.Date(data_1$timestamp)
df <- aggregate(data_1$compound_sentiment, by=list(data_1$date, data_1$conversation), mean)

ggplot(data = data_1, aes(x = date, y = compound_sentiment, color = conversation),) + 
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.3) + geom_smooth(alpha = 0.1) +
  ylab('Sentiment')

## word cloud
library("wordcloud")
library("RColorBrewer")
library("tm")

docs <- Corpus(VectorSource(data[data_1$conversation == unique(data_1$conversation)[3], 5]))
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

