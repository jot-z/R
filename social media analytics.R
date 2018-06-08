
###########################################
##########   Topic Modelling   ############
###########################################
setwd("/Users/zhangting/Desktop/spritzer")
docs<-Corpus( DirSource("15/") )
mystop=c('http', 'https', '“', '”', 'amp', 'via', 'don', 'dont','follow','can','will','lol','much','like','going','got','get')
dtm <- DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=c(mystop, stopwords("english"),stopwords("spanish"), stopwords("portuguese"))))
dtm = removeSparseTerms(dtm,0.996)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs[idx]
dtm = dtm[idx,]
# Estimate the LDA model
lda.model = LDA(dtm, 12)
perplexity(lda.model) # the perplexity of the fitted model: 150.5215
myposterior <- posterior(lda.model)
# topic distribution of each document, one row per document, one column per topic
topics = myposterior$topics 
barplot(topics[1:2,], beside=TRUE, col=c("red","blue")) # plot topic distribution of specific documents
##########  terms for 12 topics   ############
get_terms(lda.model, 5)
# term distribution of each topic, one row per topic, one column per term
terms = myposterior$terms
tid <- 2
freqterms = sort(terms[tid,], decreasing=TRUE)
barplot(freqterms[1:10]) # plot topic distribution of specific documents
barplot(terms[tid,])
##########  Wordcloud for a specific topic   ############
tid <- 2
freq <- terms[tid, ] # the probability of each term in a given topic
wordcloud(names(freq), freq, max.words=30, colors=brewer.pal(6,"Dark2"))
# Check sample documents
myidx <- sample( which(topics[,tid]> max(topics[,tid])*0.8), 10) # get document index of those at 80th percentile of covering a given topic
newdocs[myidx]$"content"
##########  Wordcloud for 12 topics   ############
TopicSelection = 12
for (i in 1:TopicSelection){
  tid <- i
  freq <- terms[tid, ]
  wordcloud(names(freq), freq, max.words=30, colors=brewer.pal(6,"Dark2"))
}











###########################################
##########   Text Clustering   ############
###########################################
##### Use LSA to reduce the dimensionality #####
rm(list=ls())
library(tm)
setwd("/Users/zhangting/Desktop/Text Clustering")
docs<-Corpus( DirSource("specialty3/") )
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"), stopwords("portuguese")),stemming=T, weighting=weightTfIdf)
tdm.full = TermDocumentMatrix(docs, control=dtm.control)
tdm = removeSparseTerms(tdm.full,0.99)
idx <- colSums(as.matrix(tdm))>0
newdocs <- docs[idx]
tdm = tdm[,idx]
###  SVD on TermDocumentMatrix  
X = as.matrix(tdm)
dim(X) ### 975 300
X.svd = svd(X)
D = diag(X.svd$d)
U <- X.svd$u
V <- X.svd$v
U %*% D %*% t(V) #  X = U D V' 
round( U %*% D %*% t(V) )
round(D, digits=2)
### 2-Dimensional Reconstruction
dim=2
Uk = U[,seq(1,dim)]
Dk = D[seq(1,dim),seq(1,dim)]
Vk = V[,seq(1,dim)]
rownames(Uk) = rownames(X)
rownames(Vk) = colnames(X)
R = Uk %*% Dk %*% t(Vk)
round(R, digits=2)
cor(X)
cor(R)
### projection on semantic space
# with adjustment of semantic units
term.proj = Uk %*% Dk
doc.proj = Dk %*% t(Vk)
# without adjustment of semantic units
term.proj2 = Uk
doc.proj2 = t(Vk)
dim(doc.proj2) ### 2 300
### LSA
require(lsa)
myLSAspace = lsa(tdm, dims=dimcalc_raw())
round(as.textmatrix(myLSAspace),2)
myLSAspace = lsa(tdm, dims=dimcalc_share())
myNewMatrix = as.textmatrix(myLSAspace)
round(myNewMatrix,2)
require(ggplot2)
term.plot <- data.frame(x=term.proj[,1], y=term.proj[,2], names=rownames(term.proj))
docnames = gsub("-\\d+", "", colnames(X))
doc.plot <- data.frame(x=doc.proj[1,], y=doc.proj[2,])
doc.plot2 <- data.frame(x=doc.proj2[1,], y=doc.proj2[2,])
ggplot(doc.plot, aes(x,y)) + geom_point(aes(color = docnames))
ggplot(doc.plot2, aes(x,y)) + geom_point(aes(color = docnames))
##### Use k-means to cluster the documents #####
km3 = kmeans(doc.plot,3)
km3$cluster
km2 = kmeans(doc.plot,2)
km3$cluster
table(docnames, km3$cluster)
table(docnames, km2$cluster)
install.packages('cluster')
library(cluster)
clusplot(doc.plot, km3$cluster,color=T, shade=T, lines=0)
clusplot(doc.plot, km2$cluster,color=T, shade=T, lines=0)
#####    Visualize the findings    #####
# before clusterng
ggplot(doc.plot, aes(x,y)) + geom_point(aes(color = docnames))
# 3 clusters
ggplot(doc.plot, aes(x,y)) + geom_point(aes(color = as.factor(km3$cluster)))
# 2 clusters
ggplot(doc.plot, aes(x,y)) + geom_point(aes(color = as.factor(km2$cluster)))













###########################################
########## Syntactic Analysis  ############
###########################################
library('tm')
library("NLP")
library("openNLP")
library('wordcloud')
file_1 <- readLines("adjective/11.txt")
doc_1<-Corpus(VectorSource(file_1))
s <-doc_1
## Sentence
sent_token_annotator <- Maxent_Sent_Token_Annotator()
a1 <- annotate(s, sent_token_annotator)
s <- as.String(s)
### Word
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, word_token_annotator, a1)
### POS Tag
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(s, pos_tag_annotator, a2)
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, "[[", "POS")
table(tags)
### find all adjectives
mypos = a3w[tags=="JJ"|tags=="JJR"|tags=="JJS"]
doc_1 = s[mypos]
doc_1 <- Corpus(VectorSource(doc_1))
### DocumentTermMatrix
dtm <- DocumentTermMatrix(doc_1, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=stopwords("english")))
freq_1 = colSums( as.matrix(dtm) )
freq.sorted_1 = sort(freq_1, decreasing=TRUE )
doc_1_freq = as.data.frame(freq.sorted_1[1:20])
### Wordcloud
wordcloud(names(freq.sorted_1),freq.sorted_1, max.words=20, colors=brewer.pal(6,"Dark2"))










###########################################
########## Text Classification ############
###########################################
df <- read.csv("movietweets.csv", header=TRUE)
#############   Evaluation   ##############
Evaluation <- function(pred, true, class)
{
  tp <- sum( pred==class & true==class)
  fp <- sum( pred==class & true!=class)
  tn <- sum( pred!=class & true!=class)
  fn <- sum( pred!=class & true==class)
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  F1 <- 2/(1/precision + 1/recall)
  print(sprintf("precision: %f", precision))
  print(sprintf("recall: %f", recall))
  print(sprintf("precision: %f", F1))
}
#######  Supervised Classification  #######
Y = as.numeric( df$sentiment)
temp <- df[,c("id","tweet")]
names(temp) = c("doc_id", "text")
docs <- Corpus(DataframeSource(temp))
mystopwords <- c("movie")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"), mystopwords), stripWhitespace=T, stemming=F)
dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm <- removeSparseTerms(dtm.full,0.99)
X <- as.matrix(dtm)
set.seed(1) # fixing the seed value for the random selection guarantees the same results in repeated runs
n=length(Y)
n1=round(n*0.8)
n2=n-n1
train=sample(1:n,n1)
##########   Naive Bayesion   #############
nb.model <- naiveBayes( X[train,], factor( Y[train]) ) 
pred.class <- predict( nb.model, X[-train,] ) # test
table( pred.class, Y[-train] )
# plot
pred <- predict( nb.model, X[-train,], type = "raw" )
nb.roc <- roc( Y[-train], pred[,2] )
plot.roc( nb.roc )
auc( Y[-train], pred[,2] ) # 0.743
# precision & recall & F1
Evaluation( pred.class, Y[-train], -1)
Evaluation( pred.class, Y[-train], 0 )
Evaluation( pred.class, Y[-train], 1 )
##########   Maximum Entropy   ############
maxent.model <- maxent( X[train,], Y[train] )
pred.max <- predict( maxent.model, X[-train,] )
table( pred.max[,1], Y[-train] )
Evaluation( pred.max, Y[-train], -1)
Evaluation( pred.max, Y[-train], 0 )
Evaluation( pred.max, Y[-train], 1 )
# plot
maxent.roc <- roc( Y[-train], as.numeric(pred.max[,2]) )
plot.roc( maxent.roc )
auc( Y[-train], as.numeric(pred.max[,2]) ) # 0.5345
# precision & recall & F1
Evaluation( pred.max, Y[-train], -1)
Evaluation( pred.max, Y[-train], 0 )
Evaluation( pred.max, Y[-train], 1 )
#######   Support Vector Machine   ########
svm.model <- svm(Y[train] ~ ., data = X[train,], kernel='linear')
pred <- predict( svm.model, X[-train,] )
pred.svm <- as.numeric( pred>0.5 )
table(pred.svm, Y[-train])
# plot
svm.roc <- roc( Y[-train], pred )
plot.roc( svm.roc )
auc( Y[-train], pred ) # 0.6292
# precision & recall & F1
Evaluation( pred.svm, Y[-train], -1)
Evaluation( pred.svm, Y[-train], 0 )
Evaluation( pred.svm, Y[-train], 1 )




