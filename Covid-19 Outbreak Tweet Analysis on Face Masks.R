###Obtaining and Analyzing Twitter Data using R###

#1. Insert Values
api_key <- 'db9w4v7CNeHDEgWMNpKieeUPi'
api_secret <- 'LA9psxLyxzaJjla2JYvVi8Se7LuF8UuvpeJNFKFedf9IOvUzKP'
access_token <- '1181316409344684032-G2auM4VXNLlQrXFIlfFuhBajGQvAgc'
access_token_secret <-'zJSxDnFJiIlilApBE97lywc7e36vhGWuQn7pxVFlJbAor'

library(twitteR)
setup_twitter_oauth(api_key, 
                    api_secret,
                    access_token, 
                    access_token_secret)
#2. Extract tweets
#Take a look at Masks tweets
#pull 1200 tweets
tweets <-searchTwitter("#mask", n=1200, lang='en')
length(tweets)

tweets

#convert into list to data frame
mask<-twListToDF(tweets)

#3. Create CSV file and save it
write.csv(mask, file = 'mask.csv', row.names = F)

# Data Cleaning and Preparation
#1. Reading Data File
mask<-read.csv(file.choose(), header=T) #choose the mask.csv file
str(mask) #look at structure of the  file (has 1200 obs and 16 var)
#first col is text, whether it is favorited, what is the count, when it was created, 
#id of person who tweeted, whether it is a retweet etc.....)

#clean the text of special characters such as symbols and emoticons
mask$text <- sapply(mask$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#2. Building Corpus
library(tm)
library(NLP)
corpus <-iconv(mask$text, to='utf-8') #need only the first col text from file
corpus <- Corpus(VectorSource(corpus)) #corpus is a collection of texts
inspect(corpus[1:5]) #inspect the first five tweets

#3. Cleaning Data
#convert data to lower case for analysis
corpus <-tm_map(corpus, tolower) #convert all alphabet to lower case
inspect(corpus[1:5]) #inspect the first five tweets

#remove punctuations
corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:5]) #inspect the first five tweets

#remove numbers
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:5]) #inspect the first five tweets

#remove common words-they dont add any informational value
#use the stopwords function in english
#select stopwords(english) to see what words are removed
cleanset <-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#remove URLs (https://etc.)
#make use of function http
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
cleanset <-tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

#tweets were pulled using mask or facemask so we can clean it from the text
cleanset <-tm_map(cleanset, removeWords, c('mask','masks','amp','tipsuf','will',
                                           'craftyourfandom','says','facemask',
                                           'mouth','face','wearing','ubcuccuc','ucuc',
                                           'globaltimesnews','uccucucb','ubcucaudc'))
inspect(cleanset[1:5])                  

#remove white spaces
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#lets now provide some structure to tweets by creating a matrix of rows/coloums
#this is called term document matrix (tdm)
#Create term document matrixs

tdm <- TermDocumentMatrix(cleanset)
tdm 

#<<TermDocumentMatrix (terms: 2243, documents: 1000)>>
#Non-/sparse entries: 9948/2233052
#Sparsity           : 100% (it is rounded)
#Maximal term length: 34
#Weighting          : term frequency (tf)
#if you would like to look at this matrix, you have to convert this into matrix first
tdm <- as.matrix(tdm)
tdm[1:10, 1:20] #look at first 10 rows/terms and 20 tweets

#VISUALIZE TEXT DATA
#in the tdm if you sum rows, it will tell you how many times a term appears
#also there are many words/terms so we create a subset of w where row sum is >30
# Bar Plot
w <- rowSums(tdm)
w <- subset(w, w>=30) #run "w" to see which words appear how many times
par(mar=c(8,4,2,2))
barplot(w, las = 2, col=rainbow(40)) #words represented vertically using las=2, rainbow colors 
#find that words such as facemask and people's names also appears so go back and combine them into a clean data dictionary
#clean the dataset of these words using dictionary created and then redo term document matrix

##################
#######after creating bar plot, you can go back and combine words or clean up further if needed
#and recreate the term document matrix
cleanset <-tm_map(cleanset, removeWords, c('mask','masks','amp','tipsuf','will',
                                           'craftyourfandom','says','facemask','face'))
inspect(cleanset[1:5])
##################

# Word Cloud
library(wordcloud)
library(RColorBrewer)
w <- sort(rowSums(tdm), decreasing=TRUE) #sort words in decreasing order
set.seed(9999)
par(mar=c(2,2,2,2))

wordcloud(words = names(w), 
          freq=w, max.words = 300, 
          random.order =FALSE)  #words are specified in names in w dataframe, frequency is stored in w, random order=false

#specifying options in word cloud
#Specify that max words be no more than say, 200
#Freq for terms to be included in wordcloud (say they have to appear 5 times to be included)
#color words, specify scale (bigger words max size =3, smaller =0.2)
#rotate some words (rotation percentage = 30%)

wordcloud(words = names(w), 
          freq=w, 
          random.order =FALSE,
          max.words = 200, 
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'), 
          scale = c(3, 0.2), 
          rot.per = .3) 

#SENTIMENT ANALYSIS USING R
#load packages
install.packages('syuzhet')
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

#Reading Files
#take the initial apple tweet file (1200 obs and 16 vars for this)
#take the first column, text and put it into tweets dataframe
tweets <- iconv(mask$text, to="utf-8")

#obtain sentiment scores for each 1200 tweets
#nrc_sentiment dictionary is called to calculate presence of 
#eight emotions & their corresponding valence in their text file
s <-get_nrc_sentiment(tweets)
head(s) 

#runs through each tweet and finds words corresponding to each sentiment
#and a score is given (last 2 cols are positive and negative tweet categories)
tail(s)
tweets[300] #look at tweet number 300

#plot the sentiment scores
#lets sum the column scores across tweets for the plot
#label y axis as total count, main title of plot label
par(mar=c(6,4,3,3))
barplot(colSums(s), 
        las = 2,
        ylab = 'Total Count', 
        main ='Sentiment Scores for Mask Tweets')

####SOCIAL NETWORK ANALYSIS###
tdm[1:20, 1:20] #lets look at our term document matrix, 10 rows, 10 cols
library(igraph)
tdm[tdm>1] <-1 
#whenever our tdm value is more than 1 for a tweet we convert into 1 because we dont need the values 2, 3,
#we only need that the term appeared (freq of terms is not required in network analysis)
termM <-tdm %*% t(tdm) #transpose of tdm matrix; create tweet adjacency matrix using %*%
termM[1:10, 1:10] #term term matrix, alerts appeared in 8 tweets, alerts and nflx appeared in 3 tweets

g <- graph.adjacency(termM, weighted=T, mode ='undirected') #convert it into graph, no direction for edges
g

#remove terms that have loops (going to self) 
g <- simplify(g)

#set labels and degrees of Vertices (V), each word is a vertices
V(g)$label <- V(g)$name #label is name
V(g)$label

V(g)$degree <- degree(g) #degree is the number of connections between terms
V(g)$degree

#Histogram of node degree, lets just use 100 bars (too many words), label of y and x axis
hist(V(g)$degree, 
     breaks=100, 
     col='green', 
     main ='Histogram of node degree', 
     ylab ='frequency',
     xlab='degree of vertices') #right skewed 

#Network diagram
set.seed(9999)
plot(g) #interpretation is difficult so recreate more meaningful visuals

#Recreate this by looking at just the top terms/nodes by degree
tdm <- tdm[rowSums(tdm)>30,] #lets reduce the size and counts of total frequency (rowSum) 
#include only terms having frequency more than 30
#it will take out all very infrequent terms
#Rerun all other code
tdm[tdm>1] <-1 
termM <-tdm %*% t(tdm)
g <- graph.adjacency(termM, weighted=T, mode ='undirected')
g <- simplify(g)
V(g)$label <- V(g)$name 
V(g)$degree <- degree(g)

#play with options such as size of vertex, distance of labels, etc. then have labels of vertex
set.seed(9999)
plot(g, 
     vertex.color='green', 
     vertex.size = 8, 
     vertex.label.dist =0.5)
#much more cleaner than earlier. You can further increase size of vertex by changing options
#there are some dense connections in the nodes (to near nodes)

#Community creation (edge betweenness)
comm <- cluster_edge_betweenness(g)
plot(comm, g)

#you can also do this by using propagating labels
prop <-cluster_label_prop(g)
plot(prop, g) #groupings for community detection are different - algorithms are different

greed <-cluster_fast_greedy(as.undirected(g)) #greedy algorithm for clustering
plot(greed, as.undirected(g))

#highlighting degrees for a different kind of plot 
V(g)$label.cex <- 2.2*V(g)$degree / max(V(g)$degree) + 0.3
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight) + 0.4) / max(log(E(g)$weight) + .4)
E(g)$color <- rgb(0.5, 0.5, 0, egam)
E(g)$width <- egam
node.size= c(10,10,10)
par(mai=c(2,2,2,2))
par(mar=c(2,2,2,2))

plot(g, 
     vertex.color ='green', 
     vertex.size = V(g)$degree*0.5
) #vertex size vary by degree

##VISUALIZATION and INTERPRETATION

#Network of tweets
tweetM <- t(tdm)%*%tdm #transpose of tdm, create tweet adjacency matrix of tdm usign %*%
g <- graph.adjacency(tweetM, weighted =T, mode = 'undirected') #store graph adjacency in g
V(g)$degree <- degree(g)
g<- simplify(g) #remove loops

#Use 100 tweets to make histogram of degree
hist(V(g)$degree,
     breaks = 100, 
     col='green',
     main='Histogram of degree',
     ylabl='frequencies', 
     xlab='degree')

#Set labels of vertices to tweet IDs
V(g)$label <- V(g)$name #vertices g label is the name
V(g)$label.cex <-1  # label size
V(g)$label.color <- rgb(0.4, 0, 0, 0.7) #change the numbers and play around for color diff
V(g)$size <- 2 #size of g
V(g)$frame.color <- NA #no frame color or lines of frame
plot(g, vertex.label =NA, vertex.size=5) #indicate size of vertex, for now, dont put labels (too much crowding)

#delete some vertices
egam <- (log(E(g)$weight) + 0.2)/ max(log(E(g)$weight) + 0.2) 
E(g)$color <- rgb(0.5, 0.5, 0, egam)
E(g)$width <- egam
g2 <- delete.vertices(g, V(g)[degree(g)<100]) #degree of g less than 100; get rid of no.of connections less than 100
#if you lose too many nodes, reduce the number 
par(mai=c(1,1,1,1))
par(mar=c(1,1,1,1))
plot(g2, vertex.label.cex =0.90, 
     vertex.label.color ='black')

# look at clustering of tweets (1000 tweets), look at increasing/decreasing the tweet vertices #

#Delete edges - delete some edges to make the network better 
#(delete edges less than 2) and (delete vertices less than 120)
E(g)$color <- rgb(0.5, 0.5, 0, egam)
E(g)$width <- egam
g3<- delete.edges(g, E(g)$weight<- 2) 
g3 <- delete.vertices(g3, V(g3)[degree(g3)<40])
plot(g3)

mask$text[c(606,326)] #check difference between two different tweets
#take some samples of two/three major groups and find what the differences among the tweets are
mask$text[c(728, 1118)]
mask$text[c(374, 806)]

mask$text[728]
mask$text[1110]
mask$text[461]
mask$text[268]
mask$text[700]
