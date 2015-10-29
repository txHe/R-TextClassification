library(tm)
#library(sqldf)
library(rJava)
library(Rwordseg)

text= read.table("maxent_10.txt",header = TRUE,row.names=1,fileEncoding = "UTF-8")

removeStopWords<-function(x,words){
  ret<-character(0)
  index<-1
  it_max<-length(x)
  while(index<=it_max){
    if(length(words[words==x[index]])<1)ret<-c(ret,x[index])
    index<-index+1
  }
  ret
}

#读取停用词
data_stw <- readLines("stopwords_CN.txt",encoding  = "UTF-8")

doc_CN <- list()

for(j in 1:dim(text)[1])
{
  x <- c(segmentCN(as.character(text[j,1])),nosymbol=TRUE)
  doc_CN[[j]] <- removeStopWords(x,data_stw)
}

kvid <- Corpus(VectorSource(doc_CN))



#去除加载的Rwordseg包

detach(package:Rwordseg)


#kvid <- tm_map(kvid,removeWords,data_stw)#去除停用词

kvid <- tm_map(kvid,stripWhitespace)

#生成词项-文档矩阵(TDM)
#control=list(removePunctuation=T,minDocFreq=1,wordLengths = c(1, Inf),weighting = weightTfIdf)
control=list(removePunctuation=T,minDocFreq=1,wordLengths = c(1, Inf))
tdm=TermDocumentMatrix(kvid,control)
#tdm = DocumentTermMatrix(kvid,control)

length(tdm$dimnames$Terms)
tdm_removed=removeSparseTerms(tdm, 0.99) 
length(tdm_removed$dimnames$Terms)


typ_text = read.table("部门类别及数量.txt",sep='\t',header = TRUE,row.names=1,fileEncoding = "UTF-8")

n=1
for(i in 1:10)
{
  m <- n + typ_text[i,2]
  ts <- inspect(tdm_removed[1:length(tdm_removed$dimnames$Terms),n:m-1])
  tk <- t(ts) #
  type <- data.frame(tp=rep(typ_text[i,1],dim(tk)[1])) 
  tm <- cbind(tk,type)
  filename <- paste(i,'.txt',sep = "")
  write.table(tm,filename,sep = "\t", col.names = NA,fileEncoding = "UTF-8")
  n <- typ_text[i,2] + n
}

#--------------乱七八糟
# ts <- inspect(tdm_removed[1:length(tdm_removed$dimnames$Terms),61990:61989])# 
# tk <- t(ts) #
# type <- data.frame(tp=rep("C",dim(tk)[1])) 
# tm <- cbind(tk,type)
# write.table(tm,"3.txt",sep = "\t", col.names = NA,fileEncoding = "UTF-8")
# 
# ts <- inspect(tdm_removed[1:length(tdm_removed$dimnames$Terms),4918:10418])# 姝ｄ緥
# tm <- t(ts) #杞疆
# type <- data.frame(tp=rep("T",dim(tm)[1])) 
# tm <- cbind(tm,type)
# write.table(tm,"TTS.txt",sep = "\t", col.names = NA,fileEncoding = "UTF-8")