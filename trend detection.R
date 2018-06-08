###########################################
##########   Trend detection   ############
###########################################
rm(list=ls())
library(tm)
library(topicmodels)
library(wordcloud)
library(ggplot2)
#############   Read files into 60 dataframes ############
for (j in 1:5){
  for (i in 1:12){
    filepath <- file.path(paste("~/fb201",j,"/fpost-201",j,"-",i,".csv",sep="")) # file name
    dfname = paste("df", j,"_", i, sep="") # dataframe
    con <- file(filepath,open="r") 
    assign(dfname, readLines(con))
    close(con)
  }}
#############   Frequency  of ingredient.txt ############
ingred <- readLines('~/ingredients.txt')
ingred <- tolower(ingred)
matrix <- data.frame(row.names = ingred)
mystop=c("&amp","amp", "http", "www")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=F, stopwords=c(stopwords("english"),mystop), dictionary = ingred)
m =0
for (j in 1:5) {
  for (i in 1:12) {
    filename = paste("~/fb201",j,"/fpost-201",j,"-",i,".csv",sep="")
    csvdata <- read.csv(filename, header=FALSE, sep=',', quote='"')
    cols <- colnames(csvdata)
    csvdata$text <- apply( csvdata[ cols ] , 1 , paste , collapse = " " )
    data <- csvdata['text']
    data$doc_id <- 1:nrow(data)
    data <- data[,c('doc_id', 'text')]
    docs <- Corpus(DataframeSource(data))# Create dtm
    dtm = DocumentTermMatrix(docs, control=dtm.control)
    freq = colSums( as.matrix(dtm) )    
    #time_col <- paste("201", j,"-" ,i)
    m <- m+1
    time_col <- paste(m)
    for (ingred_name in ingred) {
      # change column name
      matrix[ingred_name, time_col] <- freq[ingred_name]}}}
## cauliflower
cauliflower = as.data.frame(t(matrix["cauliflower",]))
plot(ts(cauliflower$cauliflower,frequency =12))
## pumpkin
pumpkin = as.data.frame(t(matrix["pumpkin",]))
plot(ts(pumpkin$pumpkin,frequency =12))
## blueberry
blueberry = as.data.frame(t(matrix["blueberry",]))
plot(ts(blueberry$blueberry,frequency =12))
###########  Top 5 ############
#ingredients that appears more than 0 times
standout <- matrix[rowSums(matrix)>0,]
#choose ingredients that appears most (top 5) to analyze
threshold <- sort(rowSums(standout), decreasing = TRUE) [5]
x <- which(rowSums(standout) >= threshold)
top5 <- standout[x,]
top5_t <- as.data.frame(t(top5))
#plot time series for top 5 in ingredient.txt
#cake
cake = as.data.frame(t(matrix["cake",]))
plot(ts(cake$cake,frequency =12))
## cheese
cheese = as.data.frame(t(matrix["cheese",]))
plot(ts(cheese$cheese,frequency =12))
#chicken
chicken = as.data.frame(t(matrix["chicken",]))
plot(ts(chicken$chicken,frequency =12))
#cream
cream = as.data.frame(t(matrix["cream",]))
plot(ts(cream$cream,frequency =12))
#chocolate
chocolate = as.data.frame(t(matrix["chocolate",]))
plot(ts(chocolate$chocolate,frequency =12))










