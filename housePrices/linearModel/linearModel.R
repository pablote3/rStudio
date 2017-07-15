#https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/25247

house<-read.csv("../housePrices/data/train.csv")
houset<-read.csv("../housePrices/data/test.csv")

houset$SalePrice <- 0                         #initialize SalesPrice to 0
houseto <- rbind(house,houset)                #combines data sets
summary(houseto)

#replace na values in numeric columns with mean
c<-lapply(houseto,mean,na.rm=TRUE)            #build list of mean values for each numeric column
for (j in 1:80) {
  if (!is.na(c[j])) {                         #numeric columns return na on is.na
    k=0
    for (i in 1:2919)  {
      if (is.na(houseto[i,j])) {
        houseto[i,j]<-c[j]
        k = k + 1                             #track number of updates per column
      }
    }
    if (k>0) {
      cat(colnames(houseto[j]), k, "\n")
    }
  }
}

#replace missing values with mode
getmode <- function(v) {
  uniqv <- unique(v)                          #build unique value list
  matchVector <- match(v, uniqv)              #determine position from uniqv
  tabulateVector <- tabulate(matchVector)     #count occurances of each uniqv
  maxIndex <- which.max(tabulateVector)       #determine uniqv index with highest count
  
#  fact<-as.factor(uniqv)
  
  cat(maxIndex, "  ", uniqv[maxIndex], fact[maxIndex], "\n")
  uniqv[maxIndex]
}

df<-unique(houseto[3]) ### notes for resolving category name
df


c<-lapply(houseto,getmode)                    #build list of indexes with highest count
d<-lapply(as.data.frame(is.na(houseto)),sum)  #sum na per column

for (j in 1:80) {
  if (d[j] > 0) {                             #na count more than zero
    k=0
    for (i in 1:2919) {
      if (is.na(houseto[i,j])) {
        houseto[,j]<-as.character(houseto[,j])#convert column to characters???needed???
        if(d[j]>100) {                        #more than 100 na per column
          houseto[i,j]<-'others'              #switch na to others
          k = k + 1
        }
        else {
          houseto[i,j]<-as.character(c[j])
          k = k + 1
        }
        houseto[,j]=as.factor(houseto[,j])
      }
    }
    if(d[j]>100) {
      cat(colnames(houseto[j]), k, " others", "\n")
    }
    else {
      cat(colnames(houseto[j]), k, " ", as.character(c[j]), "\n")
    }
  }
}  

d<-lapply(as.data.frame(is.na(houseto)),sum) #checking#

house<-houseto[1:1460,]
houset<-houseto[1461:2919,]
houset$SalePrice <- NULL

#Creating new columns for each catogery
m=82
k=1
for (k in 1:80) {
  d <- levels(houseto[,k])
  l <- length(levels(houseto[,k]))
  if (l>0) {
    for (x in 1:l) {
      for (i in 1:2919) {
        if (houseto[i,k]==d[x]) {
          houseto[i,m]=1
        }
        else {
          houseto[i,m]=0
        }
      }
      names(houseto)[m]<-paste(names(houseto)[k],'-',x,'.',d[x])
      m=m+1
    }
  }
}

k=1
while(k < 81) {
  l <- length(levels(houseto[,k]))
  if(l>0) {
    houseto[,k]<- NULL
  }
  else {
    k = k+1
  }
}

house<-houseto[1:1460,]
houset<-houseto[1461:2919,]

e <- lapply(house,sum)

#Removing columns with less than 100 values
j=1
for (i in 1:length(house)) {
  if(e[i]<100) {
    house[,j]<-NULL
    houset[,j]<- NULL
  }
  else {
    j=j+1
  }
}

#Only considering columns with correlation with target variable greater than 0.6
k=1
for (i in 1:length(house)) {
  if(is.na(cor(house$SalePrice,house[,k]))) {
    k=k+1
  }
  else {
    if(cor(house$SalePrice,house[,k])<0.6) {
      house[,k]<-NULL
      houset[,k]<- NULL
    }
    else {
      k = k+1
    }
  }
}

#Fitting Linear model
#fit<-lm(house$SalePrice~house$OverallQual+house$TotalBsmtSF+house$X1stFlrSF+house$GrLivArea+house$GarageCars+house$GarageArea)
fit <- lm(SalePrice ~ OverallQual + TotalBsmtSF + X1stFlrSF + GrLivArea + GarageCars + GarageArea, data = house)

houset$SalePrice<- NULL

housing<-predict(fit,houset)

summary(fit)

summary(house)

summary(houset)