
########### Trend Detection ############

rm(list=ls())
library(tm)
library(topicmodels)
library(wordcloud)
library(ggplot2)
###########################################
#############   Read files   ##############
########### into 60 dataframes ############
###########################################
for (j in 1:5){
  for (i in 1:12){
    filepath <- file.path(paste("~/fb201",j,"/fpost-201",j,"-",i,".csv",sep="")) # file name
    dfname = paste("df", j,"_", i, sep="") # dataframe
    con <- file(filepath,open="r") 
    assign(dfname, readLines(con))
    close(con)
  }}




###########################################
#############   Frequency    ##############
###########  of ingredient.txt ############
###########################################
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
## turkey
turkey = as.data.frame(t(matrix["turkey",]))
plot(ts(turkey$turkey,frequency =12))
## haddock
haddock = as.data.frame(t(matrix["haddock",]))
plot(ts(haddock$haddock,frequency =12))
## salmon
salmon = as.data.frame(t(matrix["salmon",]))
plot(ts(salmon$salmon,frequency =12))
## trout
trout = as.data.frame(t(matrix["trout",]))
plot(ts(trout$trout,frequency =12))
## pecan
pecan = as.data.frame(t(matrix["pecan",]))
plot(ts(pecan$pecan,frequency =12))
## kale
kale = as.data.frame(t(matrix["kale",]))
plot(ts(kale$kale,frequency =12))
## marshmallow
marshmallow = as.data.frame(t(matrix["marshmallow",]))
plot(ts(marshmallow$marshmallow,frequency =12))
## tea
tea = as.data.frame(t(matrix["tea",]))
plot(ts(tea$tea,frequency =12))
## avocado
avocado = as.data.frame(t(matrix["avocado",]))
plot(ts(avocado$avocado,frequency =12))
## fish
fish = as.data.frame(t(matrix["fish",]))
plot(ts(fish$fish,frequency =12))





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



###########################################
#############   Frequency    ##############
###########   of mylist.txt    ############
###########################################
mylist <- readLines('~/mylist.txt')
mylist <- tolower(mylist)
mymatrix <- data.frame(row.names = mylist)
mystop=c("&amp","amp", "http", "www")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=F, stopwords=c(stopwords("english"),mystop), dictionary = mylist)
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
    for (mylist_name in mylist) {
      # change column name
      mymatrix[mylist_name, time_col] <- freq[mylist_name]}}}

###########   time serires plot    ############
## zoodle
zood = as.data.frame(t(mymatrix["zoodle",]))
plot(ts(zood$zoodle,frequency =12))
## spiralizer
spi = as.data.frame(t(mymatrix["spiralizer",]))
plot(ts(spi$spiralizer,frequency =12))
## dietary
dietary = as.data.frame(t(mymatrix["dietary",]))
plot(ts(dietary$dietary,frequency =12))
## granola
granola = as.data.frame(t(mymatrix["granola",]))
plot(ts(granola$granola,frequency =12))
## local
local = as.data.frame(t(mymatrix["local",]))
plot(ts(local$local,frequency =12))
## organic
organic = as.data.frame(t(mymatrix["organic",]))
plot(ts(organic$organic,frequency =12))
## kimchi
kimchi = as.data.frame(t(mymatrix["kimchi",]))
plot(ts(kimchi$kimchi,frequency =12))
## mocktails
mocktails = as.data.frame(t(mymatrix["mocktails",]))
plot(ts(mocktails$mocktails,frequency =12))
## poke
poke = as.data.frame(t(mymatrix["poke",]))
plot(ts(poke$poke,frequency =12))
## bowl
bowl = as.data.frame(t(mymatrix["bowl",]))
plot(ts(bowl$bowl,frequency =12))

