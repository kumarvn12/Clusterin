library('randomForest')

library('Metrics')

#set random seed
set.seed(101)

#loading dataset

data<-read.csv("stock_data.csv",stringsAsFactors= T)

#checking dimensions of data
dim(data)


#specifying outcome variable as factor


data$Y<-as.factor(data$Y)

#dividing the dataset into train and test
train<-data[1:2000,]
test<-data[2001:3000,]

#applying randomForest 
model_rf<-randomForest(Y~.,data=train)

preds<-predict(object=model_rf,test[,-101])

table(preds)


#checking accuracy

auc(preds,test$Y)

######################################################################
#So, the accuracy we get is 0.50. Now let's create five clusters based on 
#values of independent variables using k-means clustering and reapply 
#randomforest.
######################################################################

#combine test and train

all<-rbind(train,test)

#creating 5 clusters using K- means clustering

Cluster <- kmeans(all[,-101], 11)

#adding clusters as independent variable to the dataset.
all$cluster<-as.factor(Cluster$cluster)

#dividing the dataset into train and test
train<-all[1:2000,]
test<-all[2001:3000,]

#applying randomforest
model_rf<-randomForest(Y~.,data=train)

preds2<-predict(object=model_rf,test[,-101])

table(preds2)

## preds2


auc(preds2,test$Y)

## [1] 0.5345908
