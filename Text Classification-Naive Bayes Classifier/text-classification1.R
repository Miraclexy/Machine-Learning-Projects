library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(tm)
library(wordcloud2)
#��ȡ����
DataCol.Type<-vector(mode="character",length=0)  
DataCol.Type[1] <- 'character'
for(i in 2:6236) 
{
  DataCol.Type[i]<- 'numeric'   
}  

Data <- read.csv("D://d���/ͳ�Ƽ���/DataClear.csv",header = T,colClasses = DataCol.Type)
Count <- as.data.frame(table(Data$��λ����))
Count <- `colnames<-`(Count,c('��λ����','������'))  
ggplot(Count, aes(x = ��λ����, y = ������)) + geom_bar(stat = "identity")
Count.reorder <- data.frame(Count[order(Count$������),])
OfficeRank <- Count.reorder$��λ����
Count$��λ���� <- factor(Count$��λ����, levels=OfficeRank)
#��ggplot2������״ͼ�����սӵ�Ͷ������С�����˳��
ggplot(Count, aes(x=��λ����, y=������)) + geom_bar(stat = "identity") +              #ָ����������ͼ
  labs(x='��λ����',y='����Ͷ�ߴ�����') 
Words.PerComplaints <-  as.data.frame(apply(Data[,-1], 1, sum))  
Words.PerComplaints <- data.frame(Data[,1],Words.PerComplaints)
Words.PerComplaints  <- `colnames<-`(Words.PerComplaints ,c('��λ����','ÿ��Ͷ���ô���'))
head(Words.PerComplaints)
ggplot(Words.PerComplaints,aes(x=ÿ��Ͷ���ô���)) + geom_histogram(binwidth=5,color = "white") +          #����ֱ��ͼ��ʽ
labs(x='�ô�����',y="Ƶ��")   
ggplot(Words.PerComplaints, aes(x=��λ����, y=ÿ��Ͷ���ô���)) + geom_boxplot() +                                                
  labs(x='��λ����',y='ÿ��Ͷ���ô���') 

Data.Office <- Data[Data$��λ����== '�з��ؼ���',-c(1)]

Data.Office <-t(Data.Office) 
Data.Office.WordFreq <- as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq <- data.frame(word=as.character(rownames(Data.Office.WordFreq)),Freq = Data.Office.WordFreq[,1])

# ��wordcloud2���ƴ���
Sys.setlocale("LC_CTYPE","chs") 

head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")

Data.Office <- Data[Data$��λ����== '�й�������',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")   

Data.Office <- Data[Data$��λ����== '�й��繫˾',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")

Data.Office <- Data[Data$��λ����== '�й��ȹ�˾',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")      

Data.Office <- Data[Data$��λ����== '��ˮ����',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")    

Data.Office <- Data[Data$��λ����== '�����������',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")   

Data.Office <- Data[Data$��λ����== '��ȼ������',-c(1)]
Data.Office <-t(Data.Office) 
Data.Office.WordFreq <-  as.data.frame(apply(Data.Office, 1, sum))  
Data.Office.WordFreq<-data.frame(word=as.character(rownames(Data.Office.WordFreq)),num=Data.Office.WordFreq[,1])
Sys.setlocale("LC_CTYPE","chs") 
head(Data.Office.WordFreq)
wordcloud2(Data.Office.WordFreq,size=1,color="black")       