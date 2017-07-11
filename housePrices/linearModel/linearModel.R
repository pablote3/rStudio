#https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/25247

house<-read.csv("../housePrices/train.csv")
houset<-read.csv("../housePrices/test.csv")

houset$SalePrice <- 0            #initialize SalesPrice to 0
houseto <- rbind(house,houset)   #combines data sets

#summary(houseto)

#replace missing numeric values with mean
c<-lapply(houseto,mean,na.rm=T) 
for (j in 1:80) {
  if (!is.na(c[j])) {
    i=1
    while (i<=2919)  {
      k=0
      if (is.na(houseto[i,j])) {
        houseto[i,j]<-c[j]
        k=k+1
      }
      i=i+1
    }
    cat(k, sep="\n")
  }
}

#replace missing values with mode
getmode <- function(v) {
  uniqv <- unique(v)                          #build unique value list
  matchVector <- match(v, uniqv)              #determine position from uniqv
  tabulateVector <- tabulate(matchVector)     #count occurances of each uniqv
  maxIndex <- which.max(tabulateVector)       #determine uniqv index with highest count
  uniqv[maxIndex]returns
  #uniqv[which.max(tabulate(match(v, uniqv)))]
}

c<-lapply(houseto,getmode)                    #list of indexes with highest count
d<-lapply(as.data.frame(is.na(houseto)),sum)  #sum na per column

for (i in 1:2919) {
  for (j in 1:80) {
    if (is.na(houseto[i,j])) {
      houseto[,j]<-as.character(houseto[,j])
    }  
    if(d[j]>100) {
      houseto[i,j]<-'others'
    }
    else {
      houseto[i,j]<-as.character(c[j])
    }
    houseto[,j]=as.factor(houseto[,j])
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