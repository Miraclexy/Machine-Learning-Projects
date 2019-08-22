library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(tm)
library(wordcloud2)
#读取数据
DataCol.Type<-vector(mode="character",length=0)  
DataCol.Type[1] <- 'character'
for(i in 2:6236) 
{
  DataCol.Type[i]<- 'numeric'   
}  

Data <- read.csv("D://d类课/统计计算/DataClear.csv",header = T,colClasses = DataCol.Type)


ModelData <- Data 
conver_counts <- function(x){
  x<- ifelse(x>0,1,0)
  return(x)
}

ModelData[,-c(1)]<- apply(ModelData[,-c(1)], MARGIN =2, FUN=conver_counts)  
for(i in 2:ncol(ModelData)) 
{
  ModelData[,i]<- factor(ModelData[,i],levels = c(0,1),labels = c("0","1"))
}  

ModelData.Split <- sample(nrow(ModelData),8/10*nrow(ModelData))
train_data <- ModelData[ModelData.Split,]
test_data <- ModelData[-ModelData.Split,]

train.Feature <- train_data[,-1]  #训练集各个样本的特征
train.Label <- train_data[,1]     #训练集各个样本的分类标签
test.Feature <- test_data[,-1]    #测试集各个样本的特征
test.Label <- test_data[,1]       #测试集各个样本的分类标签

library(e1071)  
#训练朴素贝叶斯分类模型
NaiveBayesClassifier <- naiveBayes(train.Feature,as.factor(train.Label),laplace = 0)
#用训练好的模型预测测试集
test.Predict <- predict(NaiveBayesClassifier,test.Feature,type = "class")
#统计混淆矩阵，并计算得到预测准确率
Confusion.Matrix<-table(test.Predict,test.Label)
rate <- sum(diag(Confusion.Matrix))/ nrow(test_data)
rate

x <- Confusion.Matrix
x <- x.orig <- unclass(Confusion.Matrix)
#绘制混淆矩阵
opar <- par(mar=c(5.1, 6.1, 2, 2))
image(1:ncol(x), 1:ncol(x),
      -(x[, nrow(x):1]), xlab='预测类别', ylab='实际类别',                    
      col=colorRampPalette(c(hsv(h = 0, s = 0.9, v = 0.9, alpha = 1),         
                             hsv(h = 0, s = 0, v = 0.9, alpha = 1), 
                             hsv(h = 2/6, s = 0.9, v = 0.9, alpha = 1)))(41), 
      xaxt='n', yaxt='n', zlim=c(-10, 10))
axis(1, at=1:ncol(x), labels=colnames(x), cex.axis=0.6)                         
axis(2, at=ncol(x):1, labels=colnames(x), las=1, cex.axis= 0.6)              
abline(h = 0:ncol(x) + 0.5, col = 'gray')                                     
abline(v = 0:ncol(x) + 0.5, col = 'gray')                              
text( 1:7,rep(7:1, each=7),                                                   
      labels = sub('^0$', '', c(x.orig)))                                  
box(lwd=2)                                                                  


