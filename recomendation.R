##memory clear
rm(list = ls())

#load data
library(readr)
MyData <- read_csv("E:/UpWork/Predictive Analytics (Phani)/MyData.csv")
View(MyData)


## extract customer and item 
cust_data=MyData[,c("customer_number","item_id")]
library(reshape2)
dd <-dcast(cust_data,customer_number~item_id,value.var='item_id', fun.aggregate=length)


############################
#  Item Based Similarity   #
############################   

# Drop the user column and make a new data frame
data.germany.ibs <- (dd[,!(names(dd) %in% c("customer_number"))])

# Create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# Create a placeholder dataframe listing item vs. item
holder <- matrix(NA, nrow=ncol(data.germany.ibs),ncol=ncol(data.germany.ibs),dimnames=list(colnames(data.germany.ibs),colnames(data.germany.ibs)))
data.germany.ibs.similarity <- as.data.frame(holder)

# Lets fill in those empty spaces with cosine similarities
for(i in 1:ncol(data.germany.ibs)) {
  for(j in 1:ncol(data.germany.ibs)) {
    data.germany.ibs.similarity[i,j]= getCosine(data.germany.ibs[i],data.germany.ibs[j])
  }
}

# Output similarity results to a file
#write.csv(data.germany.ibs.similarity,file="final-germany-similarity.csv")

# Get the top 10 neighbours for each
data.germany.neighbours <- matrix(NA, nrow=ncol(data.germany.ibs.similarity),ncol=11,dimnames=list(colnames(data.germany.ibs.similarity)))

for(i in 1:ncol(data.germany.ibs)) 
{
  data.germany.neighbours[i,] <- (t(head(n=11,rownames(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,i],decreasing=TRUE),][i]))))
}

# Output neighbour results to a file  
#write.csv(file="final-germany-item-neighbours.csv",x=data.germany.neighbours[,-1])


############################
# User Scores Matrix       #
############################    
# Process:
# Choose a product, see if the user purchased a product
# Get the similarities of that product's top 10 neighbours
# Get the purchase record of that user of the top 10 neighbours
# Do the formula: sumproduct(purchaseHistory, similarities)/sum(similarities)

# Lets make a helper function to calculate the scores
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

# A placeholder matrix
holder <- matrix(NA, nrow=nrow(dd),ncol=ncol(dd)-1,dimnames=list((dd$customer_number),colnames(dd[-1])))

# Loop through the users (rows)
for(i in 1:nrow(holder)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(holder)) 
  {
    # Get the user's name and th product's name
    # We do this not to conform with vectors sorted differently 
    customer_number <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
    if(as.integer(dd[dd$customer_number==customer_number,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same song
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- dd[,c("customer_number",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$customer_number==customer_number,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("customer_number"))])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # close else statement
  } # end product for loop   
} # end user for loop

# Output the results to a file
data.germany.user.scores <- holder
View(data.gremany.user.scores)
