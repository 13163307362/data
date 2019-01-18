train<-read.csv("C:\\Users\\86103554\\Desktop\\jinnan\\jinnan_round1_train_20181227.csv",header = T,stringsAsFactors = F)
test_data<-read.csv("C:\\Users\\86103554\\Desktop\\jinnan\\jinnan_round1_testA_20181227.csv",header = T,stringsAsFactors = F)
submit<-read.csv("C:\\Users\\86103554\\Desktop\\jinnan\\jinnan_round1_submit_20181227.csv",header = T,stringsAsFactors = F)
str(train)
head(submit)

#查看缺失
v_name<-c();missing_cnt<-data.frame()
for (i in names(train)){
  temp<-nrow(train[is.na(train[,i]) | train[,i]=='' ,])/nrow(train)
  v_name<-c(v_name,i)
  missing_cnt<-rbind(missing_cnt,temp)
}
missing_cnt$variable<-v_name
names(missing_cnt)[1]<-'missing_cnt'
str(missing_cnt)
remove_var1<-missing_cnt[missing_cnt$missing_cnt>0.85,]$variable
train<-train[,!names(train) %in% remove_var1]
#查看唯一值
library(dplyr)
v_name<-c();unique_cnt<-data.frame()
for (i in names(train)){
  temp<-n_distinct(train[,i])
  v_name<-c(v_name,i)
  unique_cnt<-rbind(unique_cnt,temp)
}
unique_cnt$variable<-v_name
names(unique_cnt)[1]<-'unique_cnt'
str(unique_cnt)
remove_var2<-unique_cnt[unique_cnt$unique_cnt<2,]$variable
train<-train[,!names(train) %in% remove_var2]

#查看类别最大占比
v_name<-c();type_max<-data.frame()
for (i in names(train)){
  temp<-max(table(train[,i])/length(train[,i]))
  v_name<-c(v_name,i)
  type_max<-rbind(type_max,temp)
}
type_max$variable<-v_name
names(type_max)[1]<-'type_max'
str(type_max)
remove_var3<-type_max[type_max$type_max>0.9,]$variable
train<-train[,!names(train) %in% remove_var3]
str(train)
#############时间变量处理
#提取字符型变量
character_var<-c()
for(i in names(train)){
  if(is.character(train[,i])){
    character_var<-c(character_var,i)
  }
}
str(train[,character_var])

train1<-train
str(train)
# https://blog.csdn.net/shuaishuai3409/article/details/50180845
parts<-function(x) {
      m <- regexec("([0-9]+)(:)*([0-9]*)(:)*([0-9]*)", x)
      parts <- unlist(lapply(regmatches(x, m), `[`,  c(2L, 4L, 6L)))
      parts
}
time_var<-c('A5','A9','A11','A14','A16','A24','A26','B5','B7')

for(i in time_var){
  rr<-do.call(rbind,lapply(train1[,i],parts))
  colnames(rr)<-c("V2","V3","V4")
  train1[,i]<-(3600*as.numeric(rr[,"V2"])+60*as.numeric(rr[,"V3"])+as.numeric(rr[,"V4"]))/3600
}

str(train[,character_var])
timediff_var<-c('A20','A28','B4','B9','B10','B11')

#处理时间间隔
library(stringr)
train2<-train1
timediff<-function(x){
  for(i in 1:length(x)){
    dd<-unlist(str_extract_all(x, "[0-9]+")[i])
    dd<-as.numeric(dd)
    if(dd[3]<dd[1]){
      dd[3]<-dd[3]+24
      d<-(dd[3]-dd[1])*3600+(dd[4]-dd[2])*60
      x[i]<-d
    }
    if(dd[3]>=dd[1]){
      d<-((dd[3]-dd[1])*3600+(dd[4]-dd[2])*60)/3600
      x[i]<-d
    }
    
    else{
      
      x[i]<- -1
    }
  }
  x
}

for(i in timediff_var){
  train2[train[,i]=='',i]<- "00:00-00:00"
  t<-timediff(train2[,i])
  train2[,i]<-t
}

train2[,1]<-unlist(str_extract_all(train[,'样本id'], "[0-9]+"))

for(i in names(train2)){
  if(is.character(train2[,i])){
    train2[,i]<-as.numeric(train2[,i])
  }
} 
str(train2)
apply(train2,2,function(x) sum(is.na(x)))
train3<-na.omit(train2)
lm.fit <- glm(收率~., data=train3)
summary(lm.fit)

#######################训练数据的均方误差#########################
pr.lm_train<- predict(lm.fit,train3)
train3$predict <- pr.lm_train
(MSE.lm_train<- sum((pr.lm_train - train3$收率)^2)/nrow(train3))  #多元线性回归的均方误差
(MAD.lm_train <- sum(abs(pr.lm_train - train3$收率))/nrow(train3)) #########MAD是平均绝对误差

# 在训练神经网络时需要先对数据进行标准化。

maxs <- apply(train3, 2, max)
mins <- apply(train3, 2, min)
scaled <- as.data.frame(scale(train3, center = mins, scale = maxs - mins))  #scale函数返回的是一个矩阵，需要将其转换成一个数据框格式，以便后面的数据处理。

library(neuralnet)
f <- as.formula(收率~.)
##################训练一个三层的隐藏层，分别有10个神经元，4个神经元，4个神经元
#https://blog.csdn.net/u013421629/article/details/77775713
nn <- neuralnet(收率~样本id+A5+A6+A9+A10+A11+A12+A14+A15+A16+A17++A19+A20+A21+A22+A24+A25+A26+A27+A28+B1+B4++B5+B6+B7+B8+B9+B10+B11+B12+B14,train3,hidden=c(10,4,4),linear.output=T)
plot(nn) #画出神经网络图




#划分数据集
index<-sample(nrow(train3),0.7*nrow(train3),replace = F)
traindata<-train3[index,]
test<-c(1:nrow(train3))[!(c(1:nrow(train3)) %in% index)]
testdata<-train3[test,]
nrow(traindata)
########多元########
library(MASS)
library(ISLR)
lm_fit<-lm(收率~.,data=traindata)
pred1<-predict(lm_fit,testdata[,-33])
#均方误差
(MSE.lm_train<- sum((pred1- testdata$收率)^2)/nrow(testdata)) 
########Xgboost模型######
#https://blog.csdn.net/qq_26074991/article/details/51737030
library(xgboost)
str(traindata)
labels<-traindata$收率
length(labels)
traindata_1<-traindata[-grep('收率',colnames(traindata))]#加载预测变量以外的变量
xgb <- xgboost(data = data.matrix(traindata_1),
               label = labels,
               eta = 0.1,
               max_depth =15,
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
               )
pred_xgb<-predict(xgb,data.matrix(testdata[,-32]))
(MSE.xgb_train<- sum((pred_xgb- testdata$收率)^2)/nrow(testdata)) 




####预测提交######
names(train3)
names(test_data)
test_data1<-test_data[,names(test_data) %in% names(train3)[-32]]

#时间变量处理
#提取字符型变量

test_data2<-test_data1
parts<-function(x) {
  m <- regexec("([0-9]+)(:)*([0-9]*)(:)*([0-9]*)", x)
  parts <- unlist(lapply(regmatches(x, m), `[`,  c(2L, 4L, 6L)))
  parts
}
time_var1<-c('A5','A9','A11','A14','A16','A24','A26','B5','B7')

for(i in time_var1){
  rr<-do.call(rbind,lapply(test_data2[,i],parts))
  colnames(rr)<-c("V2","V3","V4")
  test_data2[,i]<-(3600*as.numeric(rr[,"V2"])+60*as.numeric(rr[,"V3"])+as.numeric(rr[,"V4"]))/3600
}

timediff_var1<-c('A20','A28','B4','B9','B10','B11')

#处理时间间隔
library(stringr)
test_data3<-test_data2
timediff<-function(x){
  for(i in 1:length(x)){
    dd<-unlist(str_extract_all(x, "[0-9]+")[i])
    dd<-as.numeric(dd)
    if(dd[3]<dd[1]){
      dd[3]<-dd[3]+24
      d<-(dd[3]-dd[1])*3600+(dd[4]-dd[2])*60
      x[i]<-d
    }
    if(dd[3]>=dd[1]){
      d<-((dd[3]-dd[1])*3600+(dd[4]-dd[2])*60)/3600
      x[i]<-d
    }
    
    else{
      
      x[i]<- -1
    }
  }
  x
}

for(i in timediff_var){
  test_data3[test_data3[,i]=='',i]<- "00:00-00:00"
  t<-timediff(test_data3[,i])
  test_data3[,i]<-t
}

test_data3[,1]<-unlist(str_extract_all(test_data3[,'样本id'], "[0-9]+"))

for(i in names(test_data3)){
  if(is.character(test_data3[,i])){
    test_data3[,i]<-as.numeric(test_data3[,i])
  }
} 
str(test_data3)
apply(test_data3,2,function(x) sum(is.na(x)))
test_data3[is.na(test_data3$A25),]$A25<-mean(test_data3[!is.na(test_data3$A25),]$A25)
test_data3[is.na(test_data3$A5),]$A5<-mean(test_data3[!is.na(test_data3$A5),]$A5)
test_data3[is.na(test_data3$A27),]$A27<-mean(test_data3[!is.na(test_data3$A27),]$A27)
test_data3[is.na(test_data3$B1),]$B1<-mean(test_data3[!is.na(test_data3$B1),]$B1)
pre<-predict(lm_fit,test_data3)
result<-as.data.frame(cbind(x=test_data$样本id,y=pre))
#write.csv(result,'C:\\Users\\86103554\\Desktop\\jinnan\\resule.csv')
