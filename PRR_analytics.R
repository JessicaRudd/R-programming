#Required packages
library(twitteR)
library(sentimentr)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)

#import CSV
tweets = read.csv("C:/Users/Jess/OneDrive/Grad School/Clean_PRR_twtGLOBAL.csv", header=TRUE, stringsAsFactors = FALSE)
names(tweets)

#Pull only text information
#text <- tweets$tw_Text

# Add author to custom reading list
custom_reader <- readTabular(
  mapping = list(content = "tw_Text", 
                 id = "tw_ID", 
                 author = "tw_AuthorScreenName", 
                 date = "tw_CreatedAt")
)

# Make corpus with custom reading
text_corpus <- VCorpus(
  DataframeSource(tweets), 
  readerControl = list(reader = custom_reader)
)


#Remove symbols/utf8 codes that represent emojis
text_corpus <- sapply(text_corpus,function(row) iconv(row, "latin1", "ASCII", sub=""))

#check data
head(text_corpus, 5)

#Convert to vector source
text_source <- VectorSource(text_corpus)

#convert to corpus
text_corpus <- VCorpus(text_source)
print(text_corpus)

text_corpus[[10]]
text_corpus[[10]][1]
text_corpus[[10]][2]

#Remove URLs
removeURL <- function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T)

#Clean corpus function to remove URLs, spaces, punctuation, stopwords, capitalization
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(removeURL))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "rt")) #, "ajcprr", "peachtreeroadrace", "peachtree","race","road"))
  
  return(corpus)
}

#apply function to corpus
clean_text <- clean_corpus(text_corpus)
clean_text[[10]][1]
clean_text[[10]][2]

#Create term document matrix
text_tdm <- TermDocumentMatrix(clean_text)

# Create tfidf_tdm
tfidf_tdm <- TermDocumentMatrix(clean_text, control = list(weighting = weightTfIdf))

#Convert tdm to matrix
text_matrix <- as.matrix(text_tdm)
tfidf_tdm_m <- as.matrix(tfidf_tdm)

#Sum the words
text_freq <- rowSums(text_matrix)
tfidf_freq <- rowSums(tfidf_tdm_m)
#Sort words by decreasing frequency
text_freq <- sort(text_freq, decreasing = TRUE)
tfidf_freq <- sort(tfidf_freq, decreasing = TRUE)
#Check top 50 words
text_freq[1:50]
tfidf_freq[1:50]

#Create barplot for top 50 words
barplot(text_freq[1:50], col = "tan", las = 2)
barplot(tfidf_freq[1:50], col = "tan", las = 2)

#Creat word cloud for top 100 words
# Create chardonnay_freqs
text_frame <- data.frame(term = names(text_freq), num = text_freq)
tfidf_frame <- data.frame(term = names(tfidf_freq), num = tfidf_freq)
display.brewer.all()
RWB<- brewer.pal(6,"Blues")
RWB <- RWB[-(1:3)]
wordcloud(text_frame$term, text_frame$num, max.words = 100, colors = RWB)
wordcloud(tfidf_frame$term, tfidf_frame$num, max.words = 100, colors =RWB)

#Remove sparse terms for TDm
tdm1<- removeSparseTerms(text_tdm, sparse = 0.975)
print(tdm1)

tfidf <- removeSparseTerms(tfidf_tdm, sparse  = 0.975)
print(tfidf)

#Convert to matrix
tdm1_m <- as.matrix(tdm1)
tfidf_m <- as.matrix(tfidf)

#Convert to data frame
tdm1_df <- as.data.frame(tdm1_m)
tfidf_df <- as.data.frame(tfidf_m)

#Distances
dist_tweets <- dist(tdm1_df)
dist_tfidf <- dist(tfidf_df)

tweets_hc <- hclust(dist_tweets)
tfidf_hc <- hclust(dist_tfidf)

#plot dendrograms
plot(tweets_hc)
plot(tfidf_hc)


