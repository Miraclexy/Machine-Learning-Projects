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
Count <- as.data.frame(table(Data$单位名称))
Count <- `colnames<-`(Count,c('单位名称','样本量'))  
ggplot(Count, aes(x = 单位名称, y = 样本量)) + geom_bar(stat = "identity")
Count.reorder <- data.frame(Count[order(Count$样本量),])
OfficeRank <- Count.reorder$单位名称
Count$单位名称 <- factor(Count$单位名称, levels=OfficeRank)
#用ggplot2绘制柱状图，按照接到投诉量从小到大的顺序
ggplot(Count, aes(x=单位名称, y=样本量)) + geom_bar(stat = "identity") +              #指定绘制条形图
  labs(x='单位名称',y='市民投诉处理量') 
Words.PerComplaints <-  as.data.frame(apply(Data[,-1], 1, sum))  
Words.PerComplaints <- data.frame(Data[,1],Words.PerComplaints)
Words.PerComplaints  <- `colnames<-`(Words.PerComplaints ,c('单位名称','每条投诉用词数'))
head(Words.PerComplaints)
ggplot(Words.PerComplaints,aes(x=每条投诉用词数)) + geom_histogram(binwidth=5,color = "white") +          #设置直方图格式
labs(x='用词数量',y="频数")   
ggplot(Words.PerComplaints, aes(x=单位名称, y=每条投诉用词数)) + geom_boxplot() +                                                
  labs(x='单位名称',y='每条投诉用词数') 

Data.Office <- Data[Data$单位名称== '市房地集团',-c(1)]

Data.Office <-t(Data.Office) 
Data.Office.WordFreq <- as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq <- data.frame(word=as.character(rownames(Data.Office.WordFreq)),Freq = Data.Office.WordFreq[,1])

# 用wordcloud2绘制词云
Sys.setlocale("LC_CTYPE","chs") 

head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")

Data.Office <- Data[Data$单位名称== '市公交集团',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")   

Data.Office <- Data[Data$单位名称== '市供电公司',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")

Data.Office <- Data[Data$单位名称== '市供热公司',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")      

Data.Office <- Data[Data$单位名称== '市水务集团',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")    

Data.Office <- Data[Data$单位名称== '市运输管理局',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")   

Data.Office <- Data[Data$单位名称== '市燃气集团',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")       