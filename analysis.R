
source("htmlToText.R")
packages = c("rvest","dplyr","ggplot2","RCurl","XML","data.table","Rcrawler",
             "grid", "plyr", "reshape","ScottKnott","lda","tm", "SnowballC",
             "wordcloud","RColorBrewer", "slam", "topicmodels")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = T)) {
    install.packages(x, dependencies = T)
    library(x, character.only = T)
  }
})
# 2) Collecting textual information from the internet. 
# Select a topic - Deep learning
# On Google (or in some other way) prepare a list of at least 100 HTML pages 
# dealing (relevant) with the selected topic. 
Rcrawler(Website = "http://deeplearning.net/", 
         no_cores = 4, no_conn = 4, URLlenlimit = 1500)
index <- as.data.table(INDEX)
write.csv(index)
page_data <- index[,c("Id","Url")]
ListProjects()
raw_data <- LoadHTMLFiles("deeplearning.net-020309", type = "list")
# remove sep symbols
raw_text <- htmlToText(raw_data)
raw_text <- gsub("\\r", "", raw_text)
raw_text <- gsub("\\n", "", raw_text)
raw_text <- gsub("\\t", "", raw_text)
raw_text <- gsub(":", " ", raw_text)
# prepare corpus
corpus <- VectorSource(raw_text)
corpus <- Corpus(corpus)
# convert all text to lower case
corpus <- tm_map(corpus, tolower) 
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 
                                       stopwords('SMART')))
## Stemming the words
corpus <- tm_map(corpus, stemDocument, language = "english")
#  lemmatization?
corpus <- tm_map(corpus,stripWhitespace)
##create a term document matrix 
corpus.tdm <- TermDocumentMatrix(corpus)
inspect(corpus.tdm[1:10,1:10])
findFreqTerms(corpus.tdm, lowfreq = 20)
dim(corpus.tdm)
corpus.tdm.sp <- removeSparseTerms(corpus.tdm, sparse = 0.88)
dim(corpus.tdm.sp)
## Show Remining words per 20 Document.
inspect(corpus.tdm.sp[1:10,1:20])
frequency <- rowSums(as.matrix(corpus.tdm))
frequency <- subset(frequency,frequency >= 300)
df <- data.frame(terms = names(frequency), freq = frequency)
#PLOT_frequency
ggplot(df, aes(x = reorder(terms,freq), y = freq)) + 
  geom_bar(stat = "identity") + 
  xlab("Terms") + ylab("Count") + 
  coord_flip()
freq_plot <- ggplot(df, aes(x = reorder(terms,freq), y = freq)) + 
  geom_bar(stat = "identity") + 
  xlab("Terms") + ylab("Count") + 
  coord_flip()
ggsave(freq_plot, filename=paste0("freq_plot",".pdf"),
       width = 15, height = 6,path = getwd())
ggsave(freq_plot, filename=paste0("freq_plot",".png"),
       width = 15, height = 6,path = getwd())
## Words Cloud Visualizing
mTDM <- as.matrix(corpus.tdm)
v <- sort(rowSums(mTDM),decreasing=T)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "Set3")
pal <- pal[-(1:2)]
png("wordcloud.png", width = 1280,height = 800)
wordcloud(d$word, d$freq, scale = c(8,.3),min.freq = 2 ,max.words = 100, 
          random.order=T, rot.per=.15, colors = pal, 
          vfont = c("sans serif","plain"))
dev.off()
#Topic Modelling
dtm <- as.DocumentTermMatrix(corpus.tdm.sp)
lda <- LDA(dtm,k = 5)
# Topics found in documents
term <- terms(lda, 20)
term
# Frequency distribution of terms across all html files
df






