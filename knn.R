set.seed(125)

# Import the dataset in R
data<-read.csv('wine_data.csv',se=',')

# Converting the target variable from character to factor datatype
data$Class<-factor(data$Class,levels=c(1:3),labels=c('Low','Moderate','High'))

# Splitting that data into training and testing set
id<-sample(2,nrow(data),replace=T,prob=c(.75,.25))
train<-as.data.frame(scale(data[id==1,-1]))
test<-as.data.frame(scale(data[id==2,-1]))
train_lab<-data[id==1,1]
test_lab<-data[id==2,1]

library(class)
## Fitting a k-nearest neighbor model
i=1
optm=1
for(i in 1:13){
model<-knn(train=train,test=test,cl=train_lab,k=i)
optm[i]<-100*sum(test_lab==model)/nrow(test)
k=i
cat(k,'=',optm[i],'\n')
}

# Plot the accuracy levels for each value of k
par(bg='red',fg='white',col.main='white',font=4,family='mono',
col.axis='white',bty='u',pch=11,lty='13',col.lab='white')
plot(optm,type='b',main='K-Nearest Neighbor',ylab='Accuracy Level',
xlab='K-Value')

