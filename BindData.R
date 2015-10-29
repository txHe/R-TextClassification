#读取数据并将前10类分出
library(RODBC)
myconn <- odbcConnect("zhhz")

typ_text = read.table("部门类别及数量.txt",sep='\t',header = TRUE,row.names=1,fileEncoding = "UTF-8")

text = data.frame(Des=NULL,Typ=NULL)
for(i in 1:10)
{
  typ_name = typ_text[i,1]
  sql1 <- "select * from master.dbo.TempCase where Typ = "
  sql <- paste(sql1,'\'',typ_name,'\'',sep = "")
  
  temptext <- sqlQuery(myconn,sql)
  text <- rbind(text,temptext)
  
#   filename <- paste(i,".txt",sep="")
#   
#   write.table(temptext, file = filename, sep = "\t", col.names = NA,fileEncoding = "UTF-8")
}

write.table(text, file = "maxent_10.txt", sep = "\t", col.names = NA,fileEncoding = "UTF-8")
