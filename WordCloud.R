library(wordcloud)
library(RColorBrewer)
png(file="WordCloud.png", bg="white",width = 600, height = 780) 

colors = brewer.pal(8,"Dark2")[-(1:4)]
#colors = brewer.pal(8,"Dark2")
#colors = c('red','blue','green','orange','purple')

#data = read.csv("kelly_count.txt", header = TRUE, sep = ",", row.names=1,fileEncoding = "UTF-8")
data = read.csv("wordcount.txt")

dim(data)
wordcloud(data$name,data$count,scale=c(3,0.4),min.freq = -Inf,max.words=178,colors = colors,random.order = F,random.color = T,ordered.colors = F)

dev.off() 

# 1)words --- 关键词列表
# the words
# 2)freq---关键词对应的词频列表
# their frequencies
# 3)scale---显示字体大小的范围，例如c(3,0.3),最大字体是3，最小字体是0.3
# A vector of length 2 indicating the range of the size of the words
# 4)min.freq---最小词频，低于最小词频的词不会被显示
# words with frequency below min.freq will not be plotted
# 5)max.words---显示的最大词数量。
# Maximum number of words to be plotted. least frequent terms dropped
# 6)random.order---词在图上的排列顺序。T：词随机排列；F：词按频数从图中心位置往外降序排列，即频数大的词出现在中心位置。
# plot words in random order. If false, they will be plotted in decreasing frequency
# 7)random.color---控制词的字体颜色。T：字体颜色随机分配；F：根据频数分配字体颜色。
# choose colors randomly from the colors. If false, the color is chosen based onthe frequency
# 8）rot.per---控制词摆放角度。T：旋转90度；F：水平摆放。
# proportion words with 90 degree rotation
# 9）colors---字体颜色列表
# color words from least to most frequent
# 10）ordered.colors---控制字体颜色使用顺序。T：按照指定的顺序给出每个关键词字体颜色，（似乎是要求颜色列表中每个颜色一一对应关键词列表）；F：任意给出字体颜色。
# if true, then colors are assigned to words in order
# 11）use.r.layout
# if false, then c++ code is used for collision detection, otherwise R is used
# 12) fixed.asp
# if TRUE, the aspect ratio is fixed. Variable aspect ratio only supported if rot.per==0