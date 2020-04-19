#IMport positive tweets
#import CSV
pos = read.csv("C:/Users/Jess/OneDrive/Grad School/Consulting/AJCPRR/PosTweets.csv", header=TRUE, stringsAsFactors = FALSE)
pos = pos[!apply(is.na(pos) | pos == "", 1, all), ]
names(pos)

neg = read.csv("C:/Users/Jess/OneDrive/Grad School/Consulting/AJCPRR/negTweets.csv", header=TRUE, stringsAsFactors = FALSE)
neg = neg[!apply(is.na(neg) | neg == "", 1, all), ]

#Pull only text information
Pos_text <- pos$Text
Neg_text <- neg$Text


#Remove symbols/utf8 codes that represent emojis
Pos_text <- sapply(Pos_text,function(row) iconv(row, "latin1", "ASCII", sub=""))
Neg_text <- sapply(Neg_text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Convert to vector source
Pos_source <- VectorSource(Pos_text)
Neg_source <- VectorSource(Neg_text)

#convert to corpus
Pos_corpus <- VCorpus(Pos_source)
Neg_corpus <- VCorpus(Neg_source)


#check data
head(Pos_corpus, 5)
head(Neg_corpus, 5)

Pos_corpus[[10]][1]

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
clean_pos <- clean_corpus(Pos_corpus)
clean_neg <- clean_corpus(Neg_corpus)

clean_pos[[10]][1]
clean_pos[[10]][2]

#Create Term Document Matrix
clean_pos_tdm <- TermDocumentMatrix(clean_pos)
clean_neg_tdm <- TermDocumentMatrix(clean_neg)

# Create tfidf_tdm
tfidf_pos <- TermDocumentMatrix(clean_pos, control = list(weighting = weightTfIdf))
tfidf_neg <- TermDocumentMatrix(clean_neg, control = list(weighting = weightTfIdf))

#Create matrix
pos_tdm_m <- as.matrix(clean_pos_tdm)
tfidf_pos_m <- as.matrix(tfidf_pos)

neg_tdm_m <- as.matrix(clean_neg_tdm)
tfidf_neg_m <- as.matrix(tfidf_neg)

#Count word frequencies and sort in descending order
pos_freq <- rowSums(pos_tdm_m)
pos_freq <- sort(pos_freq, decreasing = TRUE)

tfidf_pos_freq <- rowSums(tfidf_pos_m)
tfidf_pos_freq <- sort(tfidf_pos_freq, decreasing = TRUE)

neg_freq <- rowSums(neg_tdm_m)
neg_freq <- sort(neg_freq, decreasing = TRUE)

tfidf_neg_freq <- rowSums(tfidf_neg_m)
tfidf_neg_freq <- sort(tfidf_neg_freq, decreasing = TRUE)

pos_freq[1:50]
tfidf_pos_freq[1:50]

neg_freq[1:50]
tfidf_neg_freq[1:50]

#Create word clouds
####################
pos_frame <- data.frame(term = names(pos_freq), num = pos_freq)
tfidf_pos_frame <- data.frame(term = names(tfidf_pos_freq), num = tfidf_pos_freq)

neg_frame <- data.frame(term = names(neg_freq), num = neg_freq)
tfidf_neg_frame <- data.frame(term = names(tfidf_neg_freq), num = tfidf_neg_freq)

display.brewer.all()
#Positive tweets
greens<- brewer.pal(6,"Greens")
greens <- greens[-(1:3)]
wordcloud(pos_frame$term, pos_frame$num, max.words = 100, colors = greens)

#Negative tweets
reds<- brewer.pal(6,"Reds")
reds <- reds[-(1:3)]
wordcloud(neg_frame$term, neg_frame$num, max.words = 100, colors =reds)

#Tf-IDF weighted positive tweets
wordcloud(tfidf_pos_frame$term, tfidf_pos_frame$num, max.words = 100, colors = greens)

#Finding associations with "ajcprr"
findAssocs(clean_pos_tdm, "ajcprr", 0.2) #--> atltrackclub
# Find associations with ajcprr
findAssocs(clean_neg_tdm, "ajcprr", 0.2)

