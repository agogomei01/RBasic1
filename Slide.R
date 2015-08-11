library(MASS)
#介紹引入套件
data(Boston)
head(Boston)
#解釋1非常正相關 -1是非常負相關
cor(Boston$medv,Boston)
barplot(cor(Boston$medv,Boston))


plot(Boston$rm,Boston$medv)
#x軸為房間數 y軸為房價
LM=lm(Boston$medv~Boston$rm)
#找出線性關係
abline(LM,col=2)
#畫出圖

quantile(Boston$medv, probs = c(0.95, 0.9))
hist(Boston$medv,breaks=20)
hist(Boston$medv, breaks = 20)
abline(v = 43.4, col="red")
MEDVhight = subset(Boston,Boston$medv>=43.4)
MEDVOther = subset(Boston,Boston$medv< 43.4)
# MEDVhight 為房價高的資料
# MEDVOther 其他資料
colMeans(MEDVhight)
colMeans(MEDVOther)
rbind(colMeans(MEDVhight),colMeans(MEDVOther))
Boston2=Boston
Boston2$Income=cut(Boston2$lstat,breaks=3)
#將低收入戶比例分成三區
Boston2$Income
Boston2$Income= cut(Boston2$lstat,breaks=3,labels=c("H","M","L"))
MEDVhight2 = subset(Boston2,Boston$medv>=43.4)
MEDVhight2$Income
table(MEDVhight2$Income)
str(Boston2)
x=1 
Month1=c(1,2,3,4,5,6,7,8,9,10,11,12)
length(Month1)
Month2=1:12
length(Month2)
TaipeiTem=c(16.1,16.5,18.5 ,	21.9 ,	25.2 ,	27.7 ,	29.6 ,	29.2,27.4,24.5 ,	21.5 ,	17.9)
HengchunTem=c(20.7 , 21.4 , 23.2 ,  25.2 ,	27.0 ,	27.9 ,	28.4 ,	28.1 ,	27.4 ,	26.3 ,	24.3 ,	21.7)
names(TaipeiTem)=Month1
names(HengchunTem)=Month2
1:3


c(1, 2, 3) + 1
1:3 * 2
## x <- c(1L, 2.0, "3")
## class(x)
## x
x <- "abc";y <- "dbbbe"
paste(x, y, sep=",")
strsplit(x, "b")
name1<-"郭雪芙"
substring(name1, 1, 1)
name2<-"金城武"
substring(name2, 1, 2)
x <- c("F","M","F","F")
x
x <- factor(c("F","M","F","F"), levels=c("F","M"))
x
x <- factor(c("F","M","F","F"), levels=c("F"))
levels(x)
as.integer(x)
#農業社會 男尊女卑
Argri <- factor(c("F","M","F","F"),order=TRUE,levels=c("F","M"))

#阿美族 女尊男卑
Amis <- factor(c("F","M","F","F"),order=FALSE,levels=c("F","M"))

#應該要用
Amis <- factor(c("F","M","F","F"),order=TRUE,levels=c("M","F"))
#舉一個認真的例子 - 班上一號到六號分別拿到A,B,C的級別
rank=factor(c("C","A","B","B","C","C"),order=TRUE,level=c("C","B","A"))
rank
rank[1]<rank[2]

1 + 2
1 - 2
1 * 2
1L / 2L
x <- 1
x < 2
x <= 1
x<-list(1L,2,"3")
x<-list(1L,2,"3",mean)
Serial=c(1,2,3)
RBasicTeacher=c("Dboy", "Ning", "Noha")
RBasicRbind=rbind(Serial,RBasicTeacher)
Serial=c(1,2,3)
RBasicTeacher=c("Dboy", "Ning", "Noha")
RBasicCbind=cbind(Serial,RBasicTeacher)
RBasic=cbind(c(1,2,3),c("Dboy", "Ning", "Noha"))
PageView <- read.table(header=TRUE, text='
     Date       PageView Rating
           7/1      1231     6.7
           7/2      5423    4.5
           7/3      3219    3.7
           7/4       998    3.3
           7/5       3241    4.1
           7/6       1223    5.2')
Promotion <- read.table(header=TRUE, text='
     Date       Promotion 
           7/1      N
           7/2      Y
           7/3      Y
           7/4      N
           7/5      Y
           7/6      N   ')

merge(PageView,Promotion,"Date")
as.numeric("2")
as.integer("a")
x <- c(1, 2, 3, 2, 3, 2, 1)
as.character(x) # 字串
factor(x) # ??用剛才dataframe作
x <- c("1", "2", "3", "2", "a")
as.numeric(x)
gdp <- c("5,023,763", "5,614,679", "6,205,338")
as.numeric(gsub(",", "", gdp))
factor_example=factor(c("三年甲班", "三年乙班", "三年甲班"))
summary(factor_example)
character_example=c("a","b","c")
summary(character_example)
list_example <- list(1L, 2.0, "3")
summary(list_example)
summary(Boston)
x <- 1:5
x[2:3]
x <- 1:5
x > 3
x[x > 3]
# 先印出台北跟恆春每個月平均溫度
TaipeiTem
HengchunTem
TaipeiTem>20
TaipeiHotMonths = TaipeiTem[TaipeiTem>20]
SelectMonth= HengchunTem>20
HengchunHotMonths = HengchunTem[SelectMonth==TRUE]
 TaipeiTem[7:9]
mean(TaipeiTem[7:9])
HengchunTem[7:9]
mean(HengchunTem[7:9])
# TaipeiTem[1,2,12]

mean(TaipeiTem[c(1,2,12)])
mean(HengchunTem[c(1,2,12)])
TemTable=rbind(TaipeiTem,HengchunTem)
TemTable
#選取台北二月的溫度
TemTable[1,2]
#選取二月的溫度
TemTable[,2]
TemTable[1,]

head(Boston)
head(Boston[["crim"]]) # head(Boston$crim)
