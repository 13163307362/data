setwd("F:\\liqianwen\\liqianwen-master\\liqianwen-master")
library(plyr)
library(dplyr)
library(data.table)
library(Rmisc)
library(e1071)#求数据偏态
library(gsubfn)
library(proto)
library(RSQLite)
library(partykit)
library(grid)
library(smbinning)#分箱
library(Formula)#求数据偏态
library(psych)
library(ggplot2)
#---------------------------------------人口特征--------------------------------
contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")
table(contest_basic_train$AGENT)
###描述性统计
head(contest_basic_train)
str(contest_basic_train)
table(contest_basic_train$SALARY)
table(contest_basic_train$EDU_LEVEL)
table(contest_basic_train[contest_basic_train$Y==1,]$AGENT)
table(contest_basic_train$AGENT)
table(contest_basic_train$IS_LOCAL)
table(contest_basic_train$WORK_PROVINCE)
table(contest_basic_train$EDU_LEVEL)
table(contest_basic_train$MARRY_STATUS)
table(contest_basic_train$SALARY)
table(contest_basic_train$HAS_FUND)
table(contest_basic_train$Y)
str(contest_basic_train)

#提取身份证户籍和性别信息
contest_basic_train<-mutate(contest_basic_train,huji=substr(contest_basic_train$ID_CARD,1,6),sex=substr(contest_basic_train$ID_CARD,17,17))
str(contest_basic_train)
contest_basic_train$sex[contest_basic_train$sex %in% c("1","3","5","7","9")]<-"男"
contest_basic_train$sex[contest_basic_train$sex %in% c("0","2","4","6","8")]<-"女"

#Agent合为APP、wechat、rongzhijia、ELSE、missing
contest_basic_train$AGENT[contest_basic_train$AGENT==""]<-"missing"
contest_basic_train$AGENT[contest_basic_train$AGENT %in% c("bestpay","chinapnr","fenqile","huifusdb", "ifpp","kuaiqian","orgloan","weijinhui")]<-"others"

#EDU_LEVEL合并为硕士及以上（包含博士研究生、硕士及以上、硕士研究生），本科，专科，专科以下（专科及以下、初中、高中），缺失（包含缺失和其他）
contest_basic_train$EDU_LEVEL[contest_basic_train$EDU_LEVEL %in% c("博士研究生","硕士及以上","硕士研究生")]<-"硕士及以上"
contest_basic_train$EDU_LEVEL[contest_basic_train$EDU_LEVEL %in% c("专科","专科及以下","初中","高中")]<-"专科及以下"
contest_basic_train$EDU_LEVEL[contest_basic_train$EDU_LEVEL %in% c("其他","")]<-"未知"

#SALARY缺失率为0.66665，取值为1、2、3、4、5、6、7，将缺失值编码为-1表示缺失
contest_basic_train$SALARY[is.na(contest_basic_train$SALARY)]<- -1
summary(contest_basic_train)
names(contest_basic_train)[13]<-"gender"
names(contest_basic_train)[12]<-"domicile"

basic<-contest_basic_train[,c("REPORT_ID","AGENT","IS_LOCAL","EDU_LEVEL","MARRY_STATUS","SALARY","HAS_FUND","Y","gender","domicile")]
str(basic)
#------------------------------------资产负债状况------------------------------
loans<-fread("contest_ext_crd_cd_ln.tsv",encoding="UTF-8")
str(loans)
table(loans$state)
table(loans$finance_org)
table(loans$type_dw)
table(loans$guarantee_type)
table(loans$class5_state)
table(loans$balance)
table(loans$balance)
#loans$state为正常的用户 投资（经营）性贷款笔数(每个借款人个人经营性贷款、个人助学贷款和农户贷款账户数目统计)；消费性贷款笔数(个人汽车贷款、个人消费贷款和其他贷款账户数目统计)；住房性贷款笔数(个人商用房（包括商住两用）贷款、个人住房贷款和个人住房公积金贷款账户数目统计)；
#各种贷款笔数
loans_cnt1<-loans[loans$state=="正常",c(2,5)]
n_distinct(loans[loans$state=="正常",c(2,5)]$report_id)
report_wd_amount_n<- loans_cnt1[, .(.N), by = .(report_id,type_dw)]
head(report_wd_amount_n1)
report_wd_amount_n1<-dcast(report_wd_amount_n, report_id ~ type_dw, value.var = 'N')
report_wd_amount_n1[is.na(report_wd_amount_n1)]<-0
nrow(report_wd_amount_n1)
names(report_wd_amount_n1)[5]<-"个人商用房贷款"
loans_cnt<-transmute(report_wd_amount_n1,report_id=report_id,loantype1_cnt=个人经营性贷款+农户贷款+个人助学贷款,loantype2_cnt=个人汽车贷款+个人消费贷款+其他贷款,loantype3_cnt=个人商用房贷款+个人住房贷款+个人住房公积金贷款)

#非信用担保贷记卡账户数表示持有人当期未结清的采用非信用担保方式（保证、抵押担保、其他担保、质押（含保证金）担保、组合（不含保证）担保）的贷记卡数。信用担保贷记卡账户数表示持有人当期未结清的采用信用/免担保方式的贷记卡数；
daijika<-fread("contest_ext_crd_cd_lnd.tsv",encoding="UTF-8")
daijika_cnt1<-daijika[daijika$state=="正常",c(1,8)]
n_distinct(daijika[daijika$state=="正常",c(1,8)]$report_id)
report_daijika_n<- daijika_cnt1[, .(.N), by = .(report_id,guarantee_type)]
head(report_daijika_n1)
report_daijika_n1<-dcast(report_daijika_n, report_id ~ guarantee_type, value.var = 'N')
report_daijika_n1[is.na(report_daijika_n1)]<-0
head(report_daijika_n1)
names(report_daijika_n1)[6]<-"组合担保"
names(report_daijika_n1)[3]<-"信用免担保"
names(report_daijika_n1)[7]<-"质押担保"
names(report_daijika_n1)
daijika_cnt<-transmute(report_daijika_n1,report_id=report_id,guarantee1_cnt=保证+其他担保+抵押担保+质押担保,guarantee2_cnt=信用免担保)
n_distinct(daijika_cnt$guarantee1_cnt)
str(daijika_cnt)
str(loans_cnt)
str(daijika_cnt)

#未销户贷记卡或者未结清贷款合同金额
sharedebt<-fread("contest_ext_crd_is_sharedebt.csv",encoding="UTF-8")
str(sharedebt)
sharedebt_cnt1<-sharedebt[,c(1,2,6)]
head(sharedebt_cnt1)
str(sharedebt_cnt1)
sharedebt_cnt1$CREDIT_LIMIT<-as.numeric(sharedebt_cnt1$CREDIT_LIMIT)

sharedebt_amount<- sharedebt_cnt1[, .(sum(CREDIT_LIMIT)), by = .(REPORT_ID)]
head(sharedebt_amount)
names(sharedebt_amount)[2]<-"debt_amount"
names(sharedebt_amount)[1]<-"report_id"
sharedebt_amount$report_id<-as.numeric(sharedebt_amount$report_id)
str(sharedebt_amount)

union1<-left_join(sharedebt_amount,loans_cnt)
loan_var<-left_join(union1,daijika_cnt)
str(loan_var)

#------------------------------------------信用历史-----------------------------------------
recorddtlinfo<-fread("contest_ext_crd_qr_recorddtlinfo.tsv",encoding="UTF-8")[,-c(2,3)]
head(recorddtlinfo)
str(recorddtlinfo)
apply(recorddtlinfo,2,function(x) sum(is.na(x)))
apply(recorddtlinfo,2,function(x) sum(x==""))
table(recorddtlinfo$query_reason)
##各report_id查询次数
query_n<- recorddtlinfo[, .(.N), by = .(report_id,query_reason)]
query_n1<-dcast(query_n, report_id ~ query_reason, value.var = 'N')

head(query_n1)
query_n1[is.na(query_n1),]<-0
head(query_n1)
names(query_n1)[2]<-"query1"
names(query_n1)[3]<-"query2"
names(query_n1)[4]<-"query3"
names(query_n1)
str(query_n1)

#---------------------------------------------信用行为--------------------------------------
#异常贷款账户数和异常贷记卡账户数是每个借款人当前未结清贷款账户和未注销贷记卡账户的异常账户数统计；
str(sharedebt)
table(sharedebt$TYPE_DW)
abnormal_account_n<- sharedebt[, .(.N), by = .(REPORT_ID,TYPE_DW)]
abnormal_account_n1<-dcast(abnormal_account_n, REPORT_ID ~ TYPE_DW, value.var = 'N')

head(abnormal_account_n1)
abnormal_account_n1[is.na(abnormal_account_n1),]<-0
head(abnormal_account_n1)
names(abnormal_account_n1)[2]<-"account1"
names(abnormal_account_n1)[3]<-"account2"
names(abnormal_account_n1)[4]<-"account3"
names(abnormal_account_n1)
names(abnormal_account_n1)[1]<-"report_id"
abnormal_account_n1$report_id<-as.numeric(abnormal_account_n1$report_id)
str(abnormal_account_n1)


#贷款余额汇总
str(sharedebt)
balance<-sharedebt[sharedebt$TYPE_DW=="未结清贷款信息汇总",c(1,9)]
head(balance)
balance$BALANCE<-as.numeric(balance$BALANCE)
balance_sum<- balance[, .(sum(BALANCE)), by = .(REPORT_ID)]
head(balance_sum)
names(balance_sum)[2]<-"balance_sum"
names(balance_sum)[1]<-"report_id"
balance_sum$report_id<-as.numeric(balance_sum$report_id)
str(balance_sum)

#贷款机构数
str(sharedebt)
institutions<-sharedebt[,c(1,4)]
head(institutions)
institutions$FINANCE_ORG_COUNT<-as.numeric(institutions$FINANCE_ORG_COUNT)
#institutions[is.na(institutions$FINANCE_ORG_COUNT),]
institutions_cnt<- institutions[, .(sum(FINANCE_ORG_COUNT)), by = .(REPORT_ID)]
head(institutions_cnt)
names(institutions_cnt)[2]<-"institutions_cnt"
names(institutions_cnt)[1]<-"report_id"
institutions_cnt$report_id<-as.numeric(institutions_cnt$report_id)
str(institutions_cnt)

#最近6个月平均使用额度汇总
str(sharedebt)
used_amount<-sharedebt[,c(1,11)]
used_amount$LATEST_6M_USED_AVG_AMOUNT <-as.numeric(used_amount$LATEST_6M_USED_AVG_AMOUNT)
#used_amount[is.na(used_amount$LATEST_6M_USED_AVG_AMOUNT),]

head(used_amount)
used_amount_sum<- used_amount[, .(sum(LATEST_6M_USED_AVG_AMOUNT)), by = .(REPORT_ID)]
head(used_amount_sum)
names(used_amount_sum)[2]<-"used_amount_sum"
names(used_amount_sum)[1]<-"report_id"
used_amount_sum$report_id<-as.numeric(used_amount_sum$report_id)
str(used_amount_sum)


#逾期笔数汇总
ovdsummary<-fread("contest_ext_crd_is_ovdsummary.csv",encoding="UTF-8")
apply(ovdsummary,2,function(x) sum(is.na(x)))
apply(ovdsummary,2,function(x) sum(x==""))
head(ovdsummary)
str(ovdsummary)
ovdsummary_cdw_n1<-ovdsummary[,c(1,2,3)]
head(ovdsummary_cdw_n1)
ovdsummary_cdw_n1$COUNT_DW<-as.numeric(ovdsummary_cdw_n1$COUNT_DW)
ovdsummary_cdw_n2<-dcast(ovdsummary_cdw_n1, REPORT_ID ~ TYPE_DW, value.var = 'COUNT_DW')
names(ovdsummary_cdw_n2)[2:4]<-c("ovd_yuqi_n1","ovd_yuqi_n2","ovd_yuqi_n3")
names(ovdsummary_cdw_n2)[1]<-"report_id"
ovdsummary_cdw_n2$report_id<-as.numeric(ovdsummary_cdw_n2$report_id)
str(ovdsummary_cdw_n2)

t4_union1<-left_join(abnormal_account_n1,balance_sum)
t4_union2<-left_join(t4_union1,institutions_cnt)
t4_union3<-left_join(t4_union2,used_amount_sum)
t4_union4<-left_join(t4_union3,ovdsummary_cdw_n2)
str(t4_union4)

#-------------------------------数据汇总--------------------------------------
str(basic)
names(basic)[1]<-"report_id"
str(loan_var)
str(query_n1)
str(t4_union4)
data_union1<-left_join(basic,loan_var)
data_union2<-left_join(data_union1,query_n1)
data_union3<-left_join(data_union2,t4_union4)
str(data_union3)
apply(data_union3,2,function(x) sum(is.na(x)))
table(data_union3$ovd_yuqi_n3)

#ovd_yuqi_n1      ovd_yuqi_n2      ovd_yuqi_n3 的缺失用0填充（表示没有逾期）
data_union3[is.na(data_union3$ovd_yuqi_n1),]$ovd_yuqi_n1<-0
data_union3[is.na(data_union3$ovd_yuqi_n2),]$ovd_yuqi_n2<-0
data_union3[is.na(data_union3$ovd_yuqi_n3),]$ovd_yuqi_n3<-0

#loantype1_cnt    loantype2_cnt    loantype3_cnt的缺失用0填充（表示没有相应的贷款）
data_union3[is.na(data_union3$loantype1_cnt),]$loantype1_cnt<-0
data_union3[is.na(data_union3$loantype2_cnt),]$loantype2_cnt<-0
data_union3[is.na(data_union3$loantype3_cnt),]$loantype3_cnt<-0

#query1           query2           query3的缺失用0填充（表示没有相应的查询）
data_union3[is.na(data_union3$query1),]$query1<-0
data_union3[is.na(data_union3$query2),]$query2<-0
data_union3[is.na(data_union3$query3),]$query3<-0

#guarantee1_cnt         guarantee2_cnt         的缺失用0填充（表示没有相应的担保种类）
data_union3[is.na(data_union3$guarantee1_cnt),]$guarantee1_cnt<-0
data_union3[is.na(data_union3$guarantee2_cnt),]$guarantee2_cnt<-0

#account1          account2  account3         的缺失用0填充（表示没有相应的账户异常）
data_union3[is.na(data_union3$account1),]$account1 <-0
data_union3[is.na(data_union3$account2),]$account2 <-0
data_union3[is.na(data_union3$account3),]$account3 <-0

#institutions_cnt  used_amount_sum  的缺失用0填充（表示没有）
data_union3[is.na(data_union3$institutions_cnt),]$institutions_cnt <-0
data_union3[is.na(data_union3$used_amount_sum),]$used_amount_sum <-0
data_union3[is.na(data_union3$balance_sum),]$balance_sum<-0
data_union3[is.na(data_union3$debt_amount),]$debt_amount<-0
apply(data_union3,2,function(x) sum(is.na(x)))
data1<-na.omit(data_union3)
apply(data1,2,function(x) sum(is.na(x)))
summary(data1)
str(data1)
# nrow(data1[data1$loantype1_cnt==0 & data1$loantype2_cnt==0 & data1$loantype3_cnt==0 & data1$guarantee1_cnt==0 & data1$guarantee2_cnt==0 & data1$query1==0 & data1$query2==0 & data1$query3==0 & data1$account1==0 & data1$account2==0  & data1$account3==0 & data1$ovd_yuqi_n1==0 & data1$ovd_yuqi_n2==0  & data1$ovd_yuqi_n3==0 ,])
# nrow(data1[data1$loantype1_cnt==0 & data1$loantype2_cnt==0 & data1$loantype3_cnt==0 & data1$guarantee1_cnt==0 & data1$guarantee2_cnt==0 & data1$query1==0 & data1$query2==0 & data1$query3==0  & data1$ovd_yuqi_n1==0 & data1$ovd_yuqi_n2==0  & data1$ovd_yuqi_n3==0 ,])
nrow(data1)
nrow(data_union3)
table(data_union3$Y)
table(data1$Y)



##############读入清洗数据##############
data_t<-read.csv("data1.csv",header = T,stringsAsFactors = F)
data_t$report_id<-as.character(data_t$report_id)
data_t$domicile<-as.character(data_t$domicile)
data_t$Y<-as.factor(data_t$Y)
data_t$HAS_FUND<-as.character(data_t$HAS_FUND)
data_t$SALARY<-as.character(data_t$SALARY)

str(data_t)
table(data_t)
#check numeric distribution 
num_distribution<-data.frame();temp_name<-c()
for(i in names(data_t)){
  if(is.numeric(data_t[,i])){
    temp<-quantile(data_t[,i],probs=c(0,0.1,0.25,0.5,0.75,0.9,0.95,0.98,0.99,1),na.rm=T,names=T)
    temp_name<-c(temp_name,i)
    num_distribution<-rbind(num_distribution,temp)
  }
}
head(num_distribution)
str(num_distribution)
row.names(num_distribution)<-temp_name
num_distribution$variable<-temp_name
#rm(i,temp,temp_name)
#skewness
library(e1071)
skew<-data.frame();skew_temp<-c()
for(i in temp_name){
    skew_temp<-skewness(data_t[,i])
    skew<-rbind(skew,skew_temp)
}
head(skew)
row.names(skew)<-temp_name
str(data_t[,temp_name])
#查看原始数值型变量分布
ggplot(data_t, aes(x = debt_amount, fill = Y)) +
  geom_density(alpha = 0.3)
ggplot(data_t, aes(x = balance_sum, fill = Y)) +
  geom_density(alpha = 0.3)

ggplot(data_t, aes(x = used_amount_sum, fill = Y)) +
  geom_density(alpha = 0.3)

#查看原始数值型变量log化后的分布
ggplot(data_t, aes(x = log(debt_amount), fill = Y)) +
  geom_density(alpha = 0.3)
ggplot(data_t, aes(x = log(balance_sum), fill = Y)) +
  geom_density(alpha = 0.3)
ggplot(data_t, aes(x = log(used_amount_sum), fill = Y)) +
  geom_density(alpha = 0.3)

#check character distribution 
char_distribution<-data.frame(stringsAsFactors = F)
for(i in names(data_t)){
  if(!is.numeric(data_t[,i])){
    temp<-data.frame(table(data_t[,i]),variable=i,stringsAsFactors = F)
    char_distribution<-rbind(char_distribution,temp)
  }
}
head(char_distribution)
char_distribution$Freq<-char_distribution$Freq/nrow(data_t)
rm_var<-subset(char_distribution,Freq>0.9)$variable


###########对debt_amount、balance_sum、used_amount_sum对数化
#对数化时当取值为0时数值变为无限值
data_t$debt_amount<-log(data_t$debt_amount)
data_t$balance_sum<-log(data_t$balance_sum)
data_t$used_amount_sum<-log(data_t$used_amount_sum)


###########bin###########
str(data_t)
#不考虑地区
data_t1<-data_t[,-10]
#查看变量唯一值
val_num<-data.frame()
for(i in names(data_t1)){
  t1<-n_distinct(data_t1[,i])
  t2<-i
  val_num<-rbind(data.frame(variable=t2,num=t1,stringsAsFactors = F),val_num)
}
str(val_num)
head(val_num)

#字符型变量转化为因子型
data_t1$account1<-as.character(data_t1$account1)
data_t1$account2<-as.character(data_t1$account2)
data_t1$account3<-as.character(data_t1$account3)

for(i in names(data_t1)){
  if(is.character(data_t1[,i])==TRUE){
    data_t1[,i]<-as.factor(data_t1[,i])
  }
}
str(data_t1)

data_t1$Y<-as.numeric(data_t1$Y)#smbinning要求目标变量为数值型，自变量为数值型或因子型

#分箱
bin_iv<-data.frame();bin_var<-c()
var_name<-names(data_t1)
for(i in var_name){
  if(is.numeric(data_t1[,i]) & i!='Y'){
    bin_tbl<-smbinning(data_t1,y='Y',x=i,p=0.1)
    if(list(bin_tbl) != 'No Bins' & bin_tbl != 'No significant splits'){
      bin_var<-rbind(bin_iv,data.frame(bin_tbl$ivtable,variable=i))
      new_var<-paste('bin',i,sep='_')
      data_t2<-sumbinning.gen(data_t1,bin_tbl,new_var)
    }
  }
}
# bin_tbl<-smbinning(data_t1,y='Y',x='balance_sum',p=0.1)
# data_t1$balance_sum
# data_t1$Y<-as.numeric(data_t1$Y)
# bin_tbl<-smbinning(data_t1,y='Y',x='balance_sum',p=0.5)
library(woeBinning)
woe(Y~.,data=data_t1)
str(data_t1)
??woe
library(woeBinning)
#library(devtools)
library(klaR)
library(MASS)
#install_github("riv","tomasgreif")


#查看相关性

table(data_t1$Y)
corelation<-cor(data_t1[,!names(data_t1) %in% 'Y'])

# #划分数据集
# set.seed(1234)
# train<-sample(nrow(data_t1),0.7*nrow(data_t1),replace = F)
# traindata<-data_t1[train,]



#选择最优子集
library(leaps)
regfit<-regsubsets(Y~.,data=data_t1,nvmax=10,method = 'seqrep')


#logistic regression
glmodel<-glm(Y~.,traindata,family = binomial)
glm_summary<-summary(glmodel)
glm_coef<-as.data.frame(glm_summary$coefficients)
#删除vif超过2的
sqrt(vif(glmodel))>2


#auc
glmodel<-glm(Y~.,traindata,family = binomial)
temp0<-predict(glmodel,traindata,type='response')
summary(temp)
library(ROCR)
t0<-prediction(temp0,traindata$Y)
t01<-performance(t0,'tpr','fpr')
t02<-performance(t0,'auc')
t02@y.values#计算auc值
max(attr(t01,'y.values')[[1]]-attr(t01,'x.values')[[1]])
plot(to1,main='ROC(train data)',sub='AUC=',col='darkblue')
abline(a=0,b=1,col='red',xlim=c(0,1),ylim=c(0,1))

test<-c(1:nrow(data_t1))[!(c(1:nrow(data_t1)) %in% train)]
testdata<-data_t1[test,]
temp1<-predict(glmodel,testdata,type='response')
summary(temp1)
t1<-prediction(temp1,testdata$Y)
t11<-performance(t1,'tpr','fpr')
t12<-performance(t1,'auc')
t12@y.values#计算auc值
max(attr(t11,'y.values')[[1]]-attr(t11,'x.values')[[1]])
plot(t11,main='ROC(test data)',sub='AUC=',col='darkblue')
abline(a=0,b=1,col='red',xlim=c(0,1),ylim=c(0,1))

#ks曲线
library(ggplot2)
library(Hmisc)
Ecdf(temp0,group=traindata$Y,lty=2,xlab="Score",
     label.curves=list(keys=1:2),
     main="KS Curve : 'Y=1' vs 'Y=0' for Train data")
Ecdf(temp1,group=testdata$Y,lty=2,xlab="Score",
     label.curves=list(keys=1:2),
     main="KS Curve : 'Y=1' vs 'Y=0' for Test data")
#查看模型稳定性
factor<- -20/log(2)
offset<-660+factor*log(20)
traindata$score<-round(factor*log((1-temp0)/temp0)+offset)
hist(traindata$score,freq=FALSE,density = 20,col="magenta",font.main=4,breaks=30,
     main="Score的直方图(traindata)")
Ecdf(traindata$score,group=traindata$Y,lty=2,xlab="Score",
     label.curves=list(keys=1:2),
     main="KS Curve : 'Y=1' vs 'Y=0' for Train data")
testdata$score<-round(factor*log((1-temp0)/temp0)+offset)
hist(testdata$score,freq=FALSE,density = 20,col="magenta",font.main=4,breaks=30,
     main="Score的直方图(testdata)")
Ecdf(testdata$score,group=testdata$Y,lty=2,xlab="Score",
     label.curves=list(keys=1:2),
     main="KS Curve : 'Y=1' vs 'Y=0' for Test data")


#----------------读入数据 训练样本1----------
df<-read.csv("data_union3.csv",header = T,stringsAsFactors = F)
apply(df,2,function(x) sum(is.na(x)))
# df[is.na(df$HAS_FUND),]$Y
# str(df)
# nrow(df[is.na(df$HAS_FUND) | is.na(df$debt_amount) | is.na(df$loantype1_cnt) | is.na(df$guarantee1_cnt) |is.na(df$query1) | is.na(df$account1)| is.na(df$balance_sum) |is.na(df$institutions_cnt)|is.na(df$used_amount_sum)|is.na(df$ovd_yuqi_n1) & df$Y==0,])
# df1<-df[(is.na(df$HAS_FUND) | is.na(df$debt_amount) | is.na(df$loantype1_cnt) | is.na(df$guarantee1_cnt) |is.na(df$query1) | is.na(df$account1)| is.na(df$balance_sum) |is.na(df$institutions_cnt)|is.na(df$used_amount_sum)|is.na(df$ovd_yuqi_n1)) & df$Y==0,]
# nrow(df1)
# df2<-df[!((is.na(df$HAS_FUND) | is.na(df$debt_amount) | is.na(df$loantype1_cnt) | is.na(df$guarantee1_cnt) |is.na(df$query1) | is.na(df$account1)| is.na(df$balance_sum) |is.na(df$institutions_cnt)|is.na(df$used_amount_sum)|is.na(df$ovd_yuqi_n1)) & df$Y==0),]
# nrow(df2)
# table(df1$Y)
# table(df2$Y)
# table(df$Y)
df<-df[,!names(df) %in% c('X','report_id','domicile')]
train<-sample(nrow(df),0.7*nrow(df),replace = F)
traindata<-df[train,]
str(df)
summary(df)
glmodel<-glm(Y~.,traindata,family = binomial)
summary(glmodel)
temp0<-predict(glmodel,df,type='response')
summary(temp)
library(ROCR)
t0<-prediction(temp0,df$Y)
t01<-performance(t0,'tpr','fpr')
t02<-performance(t0,'auc')
t02@y.values#计算auc值
max(attr(t01,'y.values')[[1]]-attr(t01,'x.values')[[1]])
plot(t01,main='ROC(train data)',sub='AUC=0.8538422',col='darkblue')
abline(a=0,b=1,col='red',xlim=c(0,1),ylim=c(0,1))

test<-c(1:nrow(df))[!(c(1:nrow(df)) %in% train)]
testdata<-df[test,]
temp1<-predict(glmodel,testdata,type='response')
summary(temp1)
t1<-prediction(temp1,testdata$Y)
t11<-performance(t1,'tpr','fpr')
t12<-performance(t1,'auc')
t12@y.values#计算auc值
max(attr(t11,'y.values')[[1]]-attr(t11,'x.values')[[1]])
plot(t11,main='ROC(test data)',sub='AUC=0.8511738',col='darkblue')
abline(a=0,b=1,col='red',xlim=c(0,1),ylim=c(0,1))


#----------------读入数据 训练样本2----------
df<-read.csv("data_union3.csv",header = T,stringsAsFactors = F)
apply(df,2,function(x) sum(is.na(x)))
str(df)
# df[is.na(df$HAS_FUND),]$Y
# str(df)
# nrow(df[is.na(df$HAS_FUND) | is.na(df$debt_amount) | is.na(df$loantype1_cnt) | is.na(df$guarantee1_cnt) |is.na(df$query1) | is.na(df$account1)| is.na(df$balance_sum) |is.na(df$institutions_cnt)|is.na(df$used_amount_sum)|is.na(df$ovd_yuqi_n1) & df$Y==0,])
# df1<-df[(is.na(df$HAS_FUND) | is.na(df$debt_amount) | is.na(df$loantype1_cnt) | is.na(df$guarantee1_cnt) |is.na(df$query1) | is.na(df$account1)| is.na(df$balance_sum) |is.na(df$institutions_cnt)|is.na(df$used_amount_sum)|is.na(df$ovd_yuqi_n1)) & df$Y==0,]
# nrow(df1)
# df2<-df[!((is.na(df$HAS_FUND) | is.na(df$debt_amount) | is.na(df$loantype1_cnt) | is.na(df$guarantee1_cnt) |is.na(df$query1) | is.na(df$account1)| is.na(df$balance_sum) |is.na(df$institutions_cnt)|is.na(df$used_amount_sum)|is.na(df$ovd_yuqi_n1)) & df$Y==0),]
# nrow(df2)
# table(df1$Y)
# table(df2$Y)
# table(df$Y)
df<-df[,!names(df) %in% c('X','report_id','domicile')]
train<-sample(nrow(df),0.7*nrow(df),replace = F)
traindata<-df[train,]
str(df)
summary(df)
glmodel<-glm(Y~.,traindata,family = binomial)
summary(glmodel)
temp0<-predict(glmodel,df,type='response')
summary(temp)
library(ROCR)
t0<-prediction(temp0,df$Y)
t01<-performance(t0,'tpr','fpr')
t02<-performance(t0,'auc')
t02@y.values#计算auc值
max(attr(t01,'y.values')[[1]]-attr(t01,'x.values')[[1]])
plot(t01,main='ROC(train data)',sub='AUC=0.8538422',col='darkblue')
abline(a=0,b=1,col='red',xlim=c(0,1),ylim=c(0,1))

test<-c(1:nrow(df))[!(c(1:nrow(df)) %in% train)]
testdata<-df[test,]
temp1<-predict(glmodel,testdata,type='response')
summary(temp1)
t1<-prediction(temp1,testdata$Y)
t11<-performance(t1,'tpr','fpr')
t12<-performance(t1,'auc')
t12@y.values#计算auc值
max(attr(t11,'y.values')[[1]]-attr(t11,'x.values')[[1]])
plot(t11,main='ROC(test data)',sub='AUC=0.8511738',col='darkblue')
abline(a=0,b=1,col='red',xlim=c(0,1),ylim=c(0,1))

table(df$guarantee1_cnt)
table(df$guarantee2_cnt)

table(df$AGENT,df$Y)
table(df$IS_LOCAL,df$Y)

table(df$EDU_LEVEL,df$Y)

