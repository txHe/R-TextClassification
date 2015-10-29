# LOAD LIBRARY
library(tm)
library(maxent)

traindata <- data.frame(NULL)
testdata <- data.frame(NULL)

#已经创建好文档特征矩阵
for(i in 1:10)
{
  filename <- paste(i,'.txt',sep="")
  text = read.table(filename, header = TRUE, sep = "\t", row.names=1,fileEncoding = "UTF-8")
  len <- dim(text)[1] 
  sam <- trunc(len * 2 / 3) #取文档2/3的数据。trunc函数用于取整
  traindata <- rbind(traindata,text[1:sam,]) #将2/3的数据放置于训练集
  k <- sam + 1
  testdata <- rbind(testdata,text[k:len,]) #剩余的数据放置于测试集
  
}

#计算用于最大熵的训练时间
ptm <- proc.time()
model <- maxent(traindata[,1:556],traindata$tp)
ptms <- proc.time() - ptm
print(ptms)

m <- testdata[,1:556]
n <- testdata$tp
#计算最大熵模型用于测试的时间
ptm <- proc.time()

ms <- predict.maxent(model,m)

ptms <- proc.time() - ptm
print(ptms)



kn <- as.character(n) #类别数组
km <- ms[,1]          #预测后的类别数组


calculate_mean <- function(kn,km)
{
  num <- 0
  for(i in 1:length(kn))
  {
    if(kn[i]==km[i])
    {
      num <- num + 1
    }
  }
  return (num/length(kn))
}

print(calculate_mean(kn,km))#计算准确率

typ_text = read.table("部门类别及数量.txt",sep='\t',header = TRUE,row.names=1,fileEncoding = "UTF-8")
case_name <- c(NULL)

for(i in 1:10)
{
  typ_name = as.character(typ_text[i,1])
  case_name <- append(case_name,typ_name)
}


#计算每个类别的准确率
calculate_m <- function(kn,km,name)
{
  num1 <- 0
  num2 <- 0 
  for(i in 1:length(kn))
  {
    if(km[i] == name)
    {
      num1 <- num1 + 1
      if(kn[i] == km[i])
      {
        num2 <- num2 + 1
      }
    }
  }
  #   print(num1)
  #   print(num2)
  return (num2/num1)
}


#计算每个类别的召回率
calculate_n <- function(kn,km,name)
{
  num1 <- 0
  num2 <- 0 
  for(i in 1:length(kn))
  {
    if(kn[i] == name) 
    {
      num1 <- num1 + 1
      if(kn[i] == km[i]) 
      {
        num2 <- num2 + 1
      }
    }
  }
#   print(num1)
#   print(num2)
  return (num2/num1)
}

calculate_P <- function(kn,km,case_name)
{
  arrp <- c(NULL)
  for(i in 1:length(case_name))
  {
    arrp <- append(arrp,calculate_m(kn,km,case_name[i]))
  }
  return (arrp)
}

calculate_R <- function(kn,km,case_name)
{
  arrp <- c(NULL)
  for(i in 1:length(case_name))
  {
    arrp <- append(arrp,calculate_n(kn,km,case_name[i]))
  }
  return (arrp)
}


arrP <- calculate_P(kn,km,case_name)#计算准确率
arrR <- calculate_R(kn,km,case_name)#计算召回率


precise <- data.frame(case_name,arrP)
write.table(precise,"precise.txt",col.names = NA,fileEncoding = "UTF-8")
recall <- data.frame(case_name,arrR)
write.table(recall,"recall.txt",col.names = NA,fileEncoding = "UTF-8")

calculate_F <-function(arrR,arrP)
{
  arrF <- c(NULL)
  for(i in 1:length(arrR))
  {
    af1 <- 2 * arrR[i] * arrP[i]
    af2 <- arrR[i] + arrP[i]
    arrF <- append(arrF,af1/af2)
  }
  return (arrF)
}
arrF <- calculate_F(arrR,arrP)
f_value <- data.frame(case_name,arrF)
write.table(f_value,"f_value.txt",col.names = NA,fileEncoding = "UTF-8")


