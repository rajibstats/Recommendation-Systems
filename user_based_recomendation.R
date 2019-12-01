## Import data set 
user_data=read.csv("E:/UpWork/Predictive Analytics (Phani)/data_client.csv")

## extract some field from main data 
data_extrct=user_data[,c("custno","igor_item_class_desc")]


## decasting the data set as per user buying pattern
library(reshape2)
d2<-dcast(data_extrct,custno~igor_item_class_desc, value.var='igor_item_class_desc', fun.aggregate=length)
View(d2)


## omit some column a 
dd_recom=d2[,-c(2,4,11)]
#dd_recom=d2[,-2]


## convert O to NA
dd_recom[, 1:ncol(dd_recom)][dd_recom[, 1:ncol(dd_recom)] == 0] <- NA


## remove the heading of 1st row
rownames(dd_recom) <- dd_recom$custno
dd_recom$custno <- NULL




## Apply recomendation engine 

library(recommenderlab)
R<-as.matrix(dd_recom)
r <- as(R, "realRatingMatrix")    

## cosine similarity
rec=Recommender(r,method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))  
names(getModel(rec))
getModel(rec)$nn



## Give Input User Id
xx=readline(prompt="Enter an userid: ")
xx=as.character(xx)


## Give input Total number of recomnded product you want to see 
no_prod=readline(prompt="Enter no of Product you want to see: ")
no_prod=as.integer(no_prod)

recom <- predict(rec, r[xx], n=no_prod)
#recom <- predict(rec, r[["100000041"]], n=5)
as(recom, "list")
