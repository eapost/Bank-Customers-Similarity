####### Part 1 ####### 

# Read csv file
customers <- read.csv2("bank.csv", sep = ";", header= TRUE, stringsAsFactors=FALSE)
customers$Class <- NULL
library(data.table)
setDT(customers, keep.rownames=TRUE)[]

for (i in 1:nrow(customers)){
  if (customers$Education[i]=="primary") {
    customers$Education[i]<-1
  } else if (customers$Education[i]=="secondary") {
    customers$Education[i]<-2
  } else {
    customers$Education[i]<-3
  }
}

str(customers)
customers$rn <- as.numeric(customers$rn)
customers$Job <- as.factor(customers$Job)
customers$Marital <- as.factor(customers$Marital)
customers$Education <- as.numeric(customers$Education)
customers$Default <- as.factor(customers$Default)
customers$Housing <- as.factor(customers$Housing)
customers$Loan <- as.factor(customers$Loan)

####### Part 2 ####### 

# Create a function
dissimilarityFunction <- function(a,b) {
  
  # Age dissimilarity calculation
  AgeDissimilarity<-abs(subset(customers$Age, customers$rn==a) - subset(customers$Age, customers$rn==b))/(max(customers$Age) - min(customers$Age))

  # Balance dissimilarity calculation
  BalanceDissimilarity<-abs(subset(customers$Balance, customers$rn==a) - subset(customers$Balance, customers$rn==b))/(max(customers$Balance) - min(customers$Balance))

  # Job dissimilarity calculation
  if (subset(customers$Job, customers$rn==a)==subset(customers$Job, customers$rn==b)) {
      JobSimilarity <- 1
    } else {
      JobSimilarity <- 0
    }

  JobDissimilarity <- 1 - JobSimilarity

  # Marital dissimilarity calculation
  if (subset(customers$Marital, customers$rn==a)==subset(customers$Marital, customers$rn==b)) {
    MaritalSimilarity <- 1
  } else {
    MaritalSimilarity <- 0
  }
  
  MaritalDissimilarity <- 1 - MaritalSimilarity

  # Education dissimilarity calculation
  EducationDissimilarity<- abs(subset(customers$Education, customers$rn==a)-subset(customers$Education, customers$rn==b)) / (max(customers$Education)-min(customers$Education)) 

  # Default dissimilarity calculation
  if (subset(customers$Default, customers$rn==a)==subset(customers$Default, customers$rn==b)) {
    DefaultSimilarity <- 1
  } else {
    DefaultSimilarity <- 0
  }
  
  DefaultDissimilarity <- 1 - DefaultSimilarity

  # Housing dissimilarity calculation
  if (subset(customers$Housing, customers$rn==a)==subset(customers$Housing, customers$rn==b)) {
    HousingSimilarity <- 1
  } else {
    HousingSimilarity <- 0
  }
  
  HousingDissimilarity <- 1 - HousingSimilarity

  # Loan dissimilarity calculation
  if (subset(customers$Loan, customers$rn==a)==subset(customers$Loan, customers$rn==b)) {
    LoanSimilarity <- 1
  } else {
    LoanSimilarity <- 0
  }
  
  LoanDissimilarity <- 1 - LoanSimilarity

  avgDissimilarity<- (AgeDissimilarity+BalanceDissimilarity+JobDissimilarity+MaritalDissimilarity+EducationDissimilarity+DefaultDissimilarity+HousingDissimilarity+LoanDissimilarity) / 8  
  print(round(avgDissimilarity, digits = 3))
  
}

# Call the function to get the average dissimilarity between 2 customers
  dissimilarityFunction(1,2)

####### Part 3 ####### 

# User with ID 1230
avgDissimilarityData1 <- data.frame(matrix(ncol = 2))
coln <- c("UserID", "AverageDissimilarity")
colnames(avgDissimilarityData1) <- coln

for (i in (1:nrow(customers))) {
  if (i==1230) {
    next
  } else {
    avgDissimilarityData1[i,1] <- i
    avgDissimilarityData1[i,2] <- dissimilarityFunction(1230,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData1 <- avgDissimilarityData1[order(avgDissimilarityData1$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 1230 are these who have the less dissimilarity with this customer
top_10_similar_neigh1 <- head(avgDissimilarityData1, n=10)
print(top_10_similar_neigh1)

# User with ID 5032

avgDissimilarityData2 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData2) <- coln

for (i in (1:nrow(customers))) {
  if (i==5032) {
    next
  } else {
    avgDissimilarityData2[i,1] <- i
    avgDissimilarityData2[i,2] <- dissimilarityFunction(5032,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData2 <- avgDissimilarityData2[order(avgDissimilarityData2$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 5032 are these who have the less dissimilarity with this customer
top_10_similar_neigh2 <- head(avgDissimilarityData2, n=10)
print(top_10_similar_neigh2)

# User with ID 10001

avgDissimilarityData3 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData3) <- coln

for (i in (1:nrow(customers))) {
  if (i==10001) {
    next
  } else {
    avgDissimilarityData3[i,1] <- i
    avgDissimilarityData3[i,2] <- dissimilarityFunction(10001,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData3 <- avgDissimilarityData3[order(avgDissimilarityData3$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 10001 are these who have the less dissimilarity with this customer
top_10_similar_neigh3 <- head(avgDissimilarityData3, n=10)
print(top_10_similar_neigh3)

# User with ID 24035

avgDissimilarityData4 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData4) <- coln

for (i in (1:nrow(customers))) {
  if (i==24035) {
    next
  } else {
    avgDissimilarityData4[i,1] <- i
    avgDissimilarityData4[i,2] <- dissimilarityFunction(24035,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData4 <- avgDissimilarityData4[order(avgDissimilarityData4$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 24035 are these who have the less dissimilarity with this customer
top_10_similar_neigh4 <- head(avgDissimilarityData4, n=10)
print(top_10_similar_neigh4)

# User with ID 28948

avgDissimilarityData5 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData5) <- coln

for (i in (1:nrow(customers))) {
  if (i==28948) {
    next
  } else {
    avgDissimilarityData5[i,1] <- i
    avgDissimilarityData5[i,2] <- dissimilarityFunction(28948,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData5 <- avgDissimilarityData5[order(avgDissimilarityData5$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 28948 are these who have the less dissimilarity with this customer
top_10_similar_neigh5 <- head(avgDissimilarityData5, n=10)
print(top_10_similar_neigh5)

# User with ID 35099

avgDissimilarityData6 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData6) <- coln

for (i in (1:nrow(customers))) {
  if (i==35099) {
    next
  } else {
    avgDissimilarityData6[i,1] <- i
    avgDissimilarityData6[i,2] <- dissimilarityFunction(35099,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData6 <- avgDissimilarityData6[order(avgDissimilarityData6$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 35099 are these who have the less dissimilarity with this customer
top_10_similar_neigh6 <- head(avgDissimilarityData6, n=10)
print(top_10_similar_neigh6)

# User with ID 37693

avgDissimilarityData7 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData7) <- coln

for (i in (1:nrow(customers))) {
  if (i==37693) {
    next
  } else {
    avgDissimilarityData7[i,1] <- i
    avgDissimilarityData7[i,2] <- dissimilarityFunction(37693,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData7 <- avgDissimilarityData7[order(avgDissimilarityData7$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 37693 are these who have the less dissimilarity with this customer
top_10_similar_neigh7 <- head(avgDissimilarityData7, n=10)
print(top_10_similar_neigh7)

# User with ID 39543

avgDissimilarityData8 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData8) <- coln

for (i in (1:nrow(customers))) {
  if (i==39543) {
    next
  } else {
    avgDissimilarityData8[i,1] <- i
    avgDissimilarityData8[i,2] <- dissimilarityFunction(39543,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData8 <- avgDissimilarityData8[order(avgDissimilarityData8$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 39543 are these who have the less dissimilarity with this customer
top_10_similar_neigh8 <- head(avgDissimilarityData8, n=10)
print(top_10_similar_neigh8)

# User with ID 40002

avgDissimilarityData9 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData9) <- coln

for (i in (1:nrow(customers))) {
  if (i==40002) {
    next
  } else {
    avgDissimilarityData9[i,1] <- i
    avgDissimilarityData9[i,2] <- dissimilarityFunction(40002,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData9 <- avgDissimilarityData9[order(avgDissimilarityData9$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 40002 are these who have the less dissimilarity with this customer
top_10_similar_neigh9 <- head(avgDissimilarityData9, n=10)
print(top_10_similar_neigh9)

# User with ID 42192

avgDissimilarityData10 <- data.frame(matrix(ncol = 2))
colnames(avgDissimilarityData10) <- coln

for (i in (1:nrow(customers))) {
  if (i==42192) {
    next
  } else {
    avgDissimilarityData10[i,1] <- i
    avgDissimilarityData10[i,2] <- dissimilarityFunction(42192,i)
    print(paste0("Number of unprocessed users:", nrow(customers)-i))
  }
}  

avgDissimilarityData10 <- avgDissimilarityData10[order(avgDissimilarityData10$AverageDissimilarity),]
# The top 10 similar customers with the customer of ID 42192 are these who have the less dissimilarity with this customer
top_10_similar_neigh10 <- head(avgDissimilarityData10, n=10)
print(top_10_similar_neigh10)
