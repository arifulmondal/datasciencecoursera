# swiftkey.R  for coursera data science  capstone project
# Author: Ariful Mondal 
# Last Modified: 11/03/2018
#----------------------------------------------------------------
## Step 1: Download data from coursera project page https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
## Step 2: Define/Install/Load necessary R-Packages as needed
## Step 3: Read/Load data for US English in RStudio
## Step 4: EDA - on US Blog, Twitter and News data
## Step 5: Select/draw random sample from entire data
## Step 6: Pre-process/clean data using gsub/regular expression
## Step 7: Create a corpus 
## Step 8: More cleaning of text using tm_map from tm package
## Step 9: Generate N-grams and create plots - histogram & wordclouds(additional - not in scope of the project)
#-----------------------------------------------------------------------------------------------------------------

#-------------------------------------------
# Load Packages
#---------------------------------------------
library("tm") # https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
library("stringi") 
library("wordcloud")
library("clue")
library("ggplot2")
library("RColorBrewer")
library("SnowballC")
library("RWeka")

#------------------------------------------------
#Set up project directory - base location
#-----------------------------------------
setwd("D:\\Training & development\\dscapstone")


# Check if the file has been extracted 
if (!file.exists("./final")) {
  
  if(!file.exists("Swiftkey.zip")){ 
    url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"  
    download.file(url, "Swiftkey.zip", method = "curl")
    dateDownload <- date()
  }
  SwiftKey.zip<- "Coursera-SwiftKey.zip"
  outDir<-"."
  unzip(SwiftKey.zip,exdir=outDir)
}

#------------------------------------------------
# Read files in the R-Environment using
#--------------------------------------------------
us_blogs<-readLines("final\\en_US\\en_US.blogs.txt",  encoding = "UTF-8", skipNul = TRUE, warn=FALSE)
us_news<-readLines("final\\en_US\\en_US.news.txt",  encoding = "UTF-8", skipNul = TRUE, warn=FALSE)
us_twitter<-readLines("final\\en_US\\en_US.twitter.txt",  encoding = "UTF-8", skipNul = TRUE, warn=FALSE)


#------------------------------------------------
# Summary US blogs
#--------------------------------------
summary(us_blogs)
# Structure of the data
str(us_blogs)
us_blogs[3:5]

# Summary US News
summary(us_news)
str(us_news)
us_news[3:5]


# Summary US Twitter
summary(us_twitter)
str(us_twitter)
us_twitter[3:5]


### Longest lines in the files

longest_lines<- (c(max(nchar(us_blogs)), max(nchar(us_news)), max(nchar(us_twitter))))
type <- c("US-Blogs", "US-News", "US-Twitter")
longest_lines <- as.data.frame(cbind(type,longest_lines))
colnames(longest_lines) <- c("file.name", "line.length")
longest_lines$line.length <- as.numeric(longest_lines$line.length)
print(longest_lines)


### Shortest lines in the files

shortest_lines<-c(min(nchar(us_blogs)), min(nchar(us_news)), min(nchar(us_twitter)))
type <- c("US-Blogs", "US-News", "US-Twitter")
shortest_lines<-as.data.frame(cbind(type, shortest_lines))
colnames(shortest_lines) <- c("file.name", "line.length")
print(shortest_lines)

#------------------------------------------------
# Select smaller sample from the population
#--------------------------------------------------

#Assign Sample Size
sample.size <- 0.10

# Set random seed so that samples do not change
set.seed(12345)

# # Sample from US-Blogs
# # Help ?sample
# us_blogs_sample <- sample(us_blogs, length(us_blogs)*sample.size)
# 
# # Sample from US-News
# us_news_sample <- sample(us_news, length(us_news)*sample.size)
# 
# # Sample from US Twitter
# us_twitter_sample <- sample(us_twitter, length(us_twitter)*sample.size)

# Combine all the sample
#us_data_sample <- c(us_blogs_sample,us_news_sample,us_twitter_sample)

# remove data to make some space
#rm(list=c("type", "us_blogs", "us_news", "us_twitter", "us_blogs_sample", "us_news_sample", "us_twitter_sample"))

us_data <- c(us_blogs,us_news,us_twitter)

# House Keeping
rm(list=c( "us_blogs", "us_news", "us_twitter"))

# Sample 1%
us_data_sample <- sample(us_data, length(us_data)*sample.size)

# Quick check on data structure
us_data_sample[1:3]
summary(us_data_sample)

#------------------------------------------------
#Pre-processing data - Additional activities
#-------------------------------------------------

# Remove non-English characters, letters etc.
# Help ?inconv
 us_data_sample<-iconv(us_data_sample, "latin1", "ASCII", sub="")
 us_data_sample[1:3]
# Remove special characters with spaces
# Help ?gsub
us_data_sample_1 <- gsub("[^0-9a-z]", " ", us_data_sample, ignore.case = TRUE)
#rm(us_data_sample)
us_data_sample_1[1:3]

# Remove duplicate characters
us_data_sample_1 <- gsub('([[:alpha:]])\\1+', '\\1\\1', us_data_sample_1)
us_data_sample_1[1:3]

# Remove special numbers with spaces
us_data_sample_1 <- gsub("[^a-z]", " ", us_data_sample_1, ignore.case = TRUE)
us_data_sample_1[1:3]

# Remove multiple spaces to one
us_data_sample_1 <- gsub("\\s+", " ", us_data_sample_1)
us_data_sample_1[1:3]

us_data_sample_1 <- gsub("^\\s", "", us_data_sample_1)
us_data_sample_1[1:3]

us_data_sample_1 <- gsub("\\s$", "", us_data_sample_1)
us_data_sample_1[1:3]


#------------------------------------------------
# create Corpus
#--------------------------------
# Help ??VCorpus


myCorpus <- VCorpus(VectorSource(us_data_sample_1))
#rm(us_data_sample_1)

inspect(myCorpus[1])
#summary(myCorpus)
writeLines(as.character(myCorpus[2]))



#------------------------------------------------
# Transformation of text data
#--------------------------------------------------

# Help ??tm_map'

# Normalize to small cases
myCorpus <- tm_map(myCorpus, content_transformer(tolower))  

# Remove Stop Words
#myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

# Remove Punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# Remove Numbers 
#myCorpus <- tm_map(myCorpus, removeNumbers)

# Create plain text documents
myCorpus <- tm_map(myCorpus, PlainTextDocument)

# Stem words in a text document using Porter's stemming algorithm.
#myCorpus <- tm_map(myCorpus, stemDocument, "english")

# Strip White Spaces
myCorpus <- tm_map(myCorpus, stripWhitespace)
writeLines(as.character(myCorpus[4]))

# remove badwords + words banned by google
# Source: https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/
bad.words <- readLines("full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt", encoding = "UTF-8", skipNul = TRUE, warn=FALSE)
# Remove multiple spaces to one
bad.words <- gsub("\\s+", " ", bad.words)
# Remove special characters with spaces
bad.words <- gsub("[^0-9a-z]", " ", bad.words, ignore.case = TRUE)
#str(bad.words)

myCorpus <- tm_map(myCorpus, removeWords, bad.words)

# Write my corpus to file disk for resue
saveRDS(myCorpus, file = "outdata/mycorpus.rds")

# Restore the object
# readRDS(file = "mycorpus.rds")
#------------------------------------------------

#House Keeping - cleaning up workspace
rm(us_data)
rm(bad.words)
rm(us_data_sample)

#Most frequently occurred words (uni-gram, bi-gram and tri-gram) are shown in the plot.

## Most frequent terms:
#findMostFreqTerms(dtm)

# N-gram analysis
#---------------------------------------------------------------------
#Unigram model: N = 1
#------------------------------------------------------------------
### Unigram: Most Frequently Occured Words 

uni_token <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))} # Define Unigram
uni_tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = uni_token, wordLengths=c(1,Inf))) # Generate Unigram

uni_tdm_1 <- removeSparseTerms(uni_tdm, 0.99)#remove the infrequently used words

# Write my unigram to file disk for resue
saveRDS(uni_tdm, file = "outdata/uni_tdm.rds")
saveRDS(uni_tdm_1, file = "outdata/uni_tdm_1.rds")


# uni_corpus <- findFreqTerms(uni_tdm,lowfreq = 50)
# uni_corpus_freq <- rowSums(as.matrix(uni_tdm[uni_corpus,]))

uni_corpus <- findFreqTerms(uni_tdm_1,lowfreq = 20)

uni_corpus_freq <- rowSums(as.matrix(uni_tdm_1[uni_corpus,]))

uni_corpus_freq <- data.frame(word=names(uni_corpus_freq), frequency=uni_corpus_freq)
df1<- uni_corpus_freq[order(-uni_corpus_freq$frequency),][1:100,] # top 100


uni_corpus[1:100]   #Top 100 frequenty occured words are..

#Word Cloud
wordcloud(words = uni_corpus_freq$word, freq = uni_corpus_freq$frequency, min.freq = 20,
          max.words=50, random.order=TRUE, rot.per=0.75,
          colors=brewer.pal(8, "Dark2"), c(5,.5), vfont=c("script","plain"))

#Barplot - top 20
barplot(df1[1:20,]$freq, las = 2, names.arg = df1[1:20,]$word,
        col =df1[1:20,]$freq, main ="",
        ylab = "Word frequencies", cex.axis=.5, cex = .5, cex.lab=0.75, cex.main=.75)



#------------------------------------------------
#Bigram model: N = 2
#---------------------------------------------
## Bigram: Most Frequently occured "sequence of two adjacent elements"

bi_token <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}  # Define Bi-gram
bi_tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = bi_token))  # Generate Bi-gram
bi_tdm_1 <- removeSparseTerms(bi_tdm, 0.99) #emove the infrequently used words

findMostFreqTerms(bi_tdm_1, 5)

# Write my bigram to file disk for resue
saveRDS(bi_tdm, file = "outdata/bi_tdm.rds")
saveRDS(bi_tdm_1, file = "outdata/bi_tdm_1.rds")


bi_corpus <- findFreqTerms(bi_tdm_1,lowfreq = 10)
bi_corpus_freq <- rowSums(as.matrix(bi_tdm_1[bi_corpus,]))
bi_corpus_freq <- data.frame(word=names(bi_corpus_freq), frequency=bi_corpus_freq)
df2 <- bi_corpus_freq[order(-bi_corpus_freq$frequency),][1:100,] # top 100

#Word Cloud
wordcloud(words = bi_corpus_freq$word, freq = bi_corpus_freq$frequency, min.freq = 20,
          max.words=100, random.order=FALSE, rot.per=0.60,
          colors=brewer.pal(8, "Dark2"), c(4,.5), vfont=c("script","plain"))

#Barplot -  top 100
ggplot(df2, aes(x = df2$word, y = frequency)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = " ") +
  xlab("Words") +
  ylab("Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#------------------------------------------------
#Trigram model: N = 3
#----------------------------------------------------
## Trigram: Most Frequently occured "a group of three consecutive written units such as letters, syllables, or words"

tri_token <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}  # Define Tri-Gram
tri_tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = tri_token)) # Generate Tri-Gram 
tri_tdm_1 <- removeSparseTerms(tri_tdm, 0.95)#emove the infrequently used words

# Write my trigram to file disk for resue
saveRDS(tri_tdm, file = "outdata/tri_tdm.rds")
saveRDS(tri_tdm_1, file = "outdata/tri_tdm_1.rds")


tri_corpus <- findFreqTerms(tri_tdm_1,lowfreq = 5)
tri_corpus_freq <- rowSums(as.matrix(tri_tdm_1[tri_corpus,]))
tri_corpus_freq <- data.frame(word=names(tri_corpus_freq), frequency=tri_corpus_freq)
df3<-tri_corpus_freq[order(-tri_corpus_freq$frequency),][1:100,] # top 100


#Word Cloud
wordcloud(words = tri_corpus_freq$word, freq = tri_corpus_freq$frequency, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.30,
          colors=brewer.pal(8, "Dark2"), c(3,.4), vfont=c("script","plain"))

#Barplot
ggplot(df3, aes(x = df3$word, y = frequency)) +
  geom_bar(stat = "identity", fill = "#FF6666") +
  labs(title = " ") +
  xlab("Words") +
  ylab("Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#------------------------------------------------
#Quadgram model: N = 4
#-------------------------------------------------------------------
## Quadgram: Most Frequently occured "a group of four consecutive written units such as letters, syllables, or words"

qd_token <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))} # Define Quad-gram
qd_tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = qd_token)) # Generate Quad-gram
qd_tdm_1 <- removeSparseTerms(qd_tdm, 0.99)#emove the infrequently used words

# Write my trigram to file disk for resue
saveRDS(qd_tdm, file = "outdata/qd_tdm.rds")
saveRDS(qd_tdm_1, file = "outdata/qd_tdm_1.rds")


qd_corpus <- findFreqTerms(qd_tdm_1,lowfreq = 5)
qd_corpus_freq <- rowSums(as.matrix(qd_tdm_1[qd_corpus,]))
qd_corpus_freq <- data.frame(word=names(qd_corpus_freq), frequency=qd_corpus_freq)
df4<-qd_corpus_freq[order(-qd_corpus_freq$frequency),][1:100,] 

#Word Cloud

wordcloud(words = qd_corpus_freq$word, freq = qd_corpus_freq$frequency, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.30,
          colors=brewer.pal(8, "Dark2"), c(2,.4), vfont=c("script","plain"))

#Barplot
ggplot(df4, aes(x = df4$word, y = frequency)) +
  geom_bar(stat = "identity", fill = "#FF6666") +
  labs(title = " ") +
  xlab("Words") +
  ylab("Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#------------------------------------------------

# House keeping - clean workspace
#------------------------------------------------------------------
rm(list=ls())


#------------------- End of Data Processing & N-Gram Generation -------------------
