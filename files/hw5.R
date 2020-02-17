library(data.table)
library(cluster)
setwd("/Users/elifkonyar/Desktop/IE 582/HW5")
data<-fread("Musk1.csv")

##github deneme

data_distance<-data[,c(-1,-2)]

#dist matrix
scaled_data<-scale(data_distance)
dist_euc<-as.matrix(dist(scaled_data,"euclidean"))
dist_man<-as.matrix(dist(scaled_data,"manhattan"))

###K-MEDOIDS
##euclidean case
dist_euc_dt<-as.data.table(dist_euc)
k_max<-15
totwss <- rep(0, k_max-1)
sum<-0
for(i in 2:k_max){
  kmed <- pam(scaled_data,k=i,diss=FALSE,metric='euclidean')
  sum<-0
  for(z in 1:i){
    ins<-kmed$clustering==z
    ins<-dist_euc_dt[ins]
    med<-kmed$id.med[z]
    colname<-as.character(as.numeric(med))
    that_clus<-ins[,get(colname)]
    squared<-that_clus*that_clus
    sum<-sum+sum(squared)
  }
  totwss[i-1] <- sum
}
# Plot
plot(2:k_max, totwss, type = "b", pch = 19,main="Total Within SS for Different K Values"
     ,xlab = "Number of Clusters (k)",ylab="Total Within SS")
#k is chosen to be 6

final_med_euc<-pam(scaled_data,k=6,diss=FALSE,metric='euclidean')
final_medoids_euc<-final_med_euc$id.med

table_med_euc<-dist_euc_dt[,get(as.character(as.numeric(final_medoids_euc[1])))]
for(i in 2:6){
  table_med_euc<-cbind(table_med_euc,dist_euc_dt[,get(as.character(as.numeric(final_medoids_euc[i])))])
}
table_med_euc<-as.data.table(table_med_euc)
setnames(table_med_euc,old=colnames(table_med_euc),new=c("C1","C2","C3","C4","C5","C6"))
baginfo<-data[,c(1,2)]
table_med_euc<-cbind(baginfo,table_med_euc)
setnames(table_med_euc,old=c("V1","V2"),new=c("BagClass","BagID"))

avg_table_med_euc<-table_med_euc[,.(C1=mean(C1),C2=mean(C2),C3=mean(C3),C4=mean(C4),C5=mean(C5),C6=mean(C6)),keyby=.(BagID)]
bagclass<-baginfo[!duplicated(baginfo),]
avg_table_med_euc<-cbind(bagclass$V1,avg_table_med_euc)
setnames(avg_table_med_euc,old="V1",new="BagClass")


##manhattan case
dist_man_dt<-as.data.table(dist_man)
totwss <- rep(0, k_max-1)
sum<-0
for(i in 2:k_max){
  kmed <- pam(scaled_data,k=i,diss=FALSE,metric='manhattan')
  sum<-0
  for(z in 1:i){
    ins<-kmed$clustering==z
    ins<-dist_man_dt[ins]
    med<-kmed$id.med[z]
    colname<-as.character(as.numeric(med))
    that_clus<-ins[,get(colname)]
    squared<-that_clus*that_clus
    sum<-sum+sum(squared)
  }
  totwss[i-1] <- sum
}
# Plot
plot(2:k_max, totwss, type = "b", pch = 19,main="Total Within SS for Different K Values"
     ,xlab = "Number of Clusters (k)",ylab="Total Within SS")
#k is chosen to be 7

final_med_man<-pam(scaled_data,k=7,diss=FALSE,metric='manhattan')
final_medoids_man<-final_med_man$id.med

table_med_man<-dist_man_dt[,get(as.character(as.numeric(final_medoids_man[1])))]
for(i in 2:7){
  table_med_man<-cbind(table_med_man,dist_man_dt[,get(as.character(as.numeric(final_medoids_man[i])))])
}
table_med_man<-as.data.table(table_med_man)
setnames(table_med_man,old=colnames(table_med_man),new=c("C1","C2","C3","C4","C5","C6","C7"))
baginfo<-data[,c(1,2)]
table_med_man<-cbind(baginfo,table_med_man)
setnames(table_med_man,old=c("V1","V2"),new=c("BagClass","BagID"))

avg_table_med_man<-table_med_man[,.(C1=mean(C1),C2=mean(C2),C3=mean(C3),C4=mean(C4),C5=mean(C5),C6=mean(C6),C7=mean(C7)),keyby=.(BagID)]
bagclass<-baginfo[!duplicated(baginfo),]
avg_table_med_man<-cbind(bagclass$V1,avg_table_med_man)
setnames(avg_table_med_man,old="V1",new="BagClass")


##HIERARCHICAL CLUSTERING
#euclidean distance
hier_clus_euc<-hclust(dist(scaled_data,"euclidean"),method="centroid")
#bad
hier_clus_euc<-hclust(dist(scaled_data,"euclidean"),method="complete")
hier_clus_euc
plot(hier_clus_euc)
model_euc<-cutree(hier_clus_euc, k = 3)
model_euc
plot(model_euc)
euc_1<-scaled_data[model_euc==1,]
c1med<-colMeans(euc_1)
euc_2<-scaled_data[model_euc==2,]
c2med<-colMeans(euc_2)
euc_3<-scaled_data[model_euc==3,]
c3med<-colMeans(euc_3)

dis_euc<-rbind(scaled_data,c1med,c2med,c3med)
temp<-as.data.table(as.matrix(dist(dis_euc,method = "euclidean")))
temp<-temp[c(-477,-478,-479),c(477,478,479)]
table_hier_euc<-cbind(baginfo,temp)

avg_table_hier_euc<-table_hier_euc[,.(C1=mean(c1med),C2=mean(c2med),C3=mean(c3med)),keyby=.(V2)]
bagclass<-baginfo[!duplicated(baginfo),]
avg_table_hier_euc<-cbind(bagclass$V1,avg_table_hier_euc)
setnames(avg_table_hier_euc,old=c("V1","V2"),new=c("BagClass","BagID"))


#manhattan distance
hier_clus_man<-hclust(dist(scaled_data,"manhattan"),method="centroid")
#bad
hier_clus_man<-hclust(dist(scaled_data,"manhattan"),method="complete")
plot(hier_clus_man)
model_man<-cutree(hier_clus_man, k = 4)
model_man
plot(model_man)

man_1<-scaled_data[model_man==1,]
c1med<-colMeans(man_1)
man_2<-scaled_data[model_man==2,]
c2med<-colMeans(man_2)
man_3<-scaled_data[model_man==3,]
c3med<-colMeans(man_3)
man_4<-scaled_data[model_man==4,]
c4med<-colMeans(man_4)

dis_man<-rbind(scaled_data,c1med,c2med,c3med,c4med)
temp<-as.data.table(as.matrix(dist(dis_man,method = "manhattan")))
temp<-temp[c(-477,-478,-479,-480),c(477,478,479,480)]
table_hier_man<-cbind(baginfo,temp)

avg_table_hier_man<-table_hier_man[,.(C1=mean(c1med),C2=mean(c2med),C3=mean(c3med),C4=mean(c4med)),keyby=.(V2)]
bagclass<-baginfo[!duplicated(baginfo),]
avg_table_hier_man<-cbind(bagclass$V1,avg_table_hier_man)
setnames(avg_table_hier_man,old=c("V1","V2"),new=c("BagClass","BagID"))



library(glmnet)

##Logistic Regression K-medoids euclidean case
train_features<-avg_table_med_euc[,c(-1,-2)]
train_features_target<-as.factor(avg_table_med_euc$BagClass)

library(TunePareto)
set.seed(1)
nofReplications<-1
nFolds<-10
indices=generateCVRuns(train_features_target,nofReplications,nFolds,stratified=TRUE)

glm_train_data<-train_features
glmnet_alldata = glmnet(as.matrix(glm_train_data),train_features_target, family="binomial", alpha = 1, nlambda=50,standardize = TRUE)
lambda_sequence = glmnet_alldata$lambda

cvresult=vector('list',nofReplications*nFolds)
iter=1

for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=(thisReplication[[j]])
    
    cvtrain=glm_train_data[-testindices]    
    cvtrainclass=train_features_target[-testindices]   
    cvtest=glm_train_data[testindices]
    cvtestclass=train_features_target[testindices] 
    
    inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="binomial", alpha = 1,lambda=lambda_sequence,standardize = TRUE)
    valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
    
    foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,x],result=cvtestclass)}))
    cvresult[[iter]]=foldresult
    iter=iter+1
  }
}

cvresult=rbindlist(cvresult)
library(ROCR)
cvresultcopy<-cvresult
aucresult<-data.table()
for(i in 1:368){
  tempauc<-cvresultcopy[1:10,]
  predROC=prediction(tempauc$V4,tempauc$result)
  perf=performance(predROC,"tpr","fpr")
  auc<-as.numeric(performance(predROC,"auc")@y.values)
  aucresult<-rbind(aucresult,data.table(Replication=tempauc[1,1],Fold=tempauc[1,2],Lambda=tempauc[1,3],AUC=auc))
  cvresultcopy<-cvresultcopy[-(1:10),]
}

final<-aucresult[,.(Mean_AUC=mean(AUC)),keyby=.(Lambda.lambda)]

cv_lambda_max=final[which.max(Mean_AUC)]$Lambda.lambda
cv_auc_max=final[which.max(Mean_AUC)]$Mean_AUC



##Logistic Regression K-medoids manhattan case
train_features<-avg_table_med_man[,c(-1,-2)]
train_features_target<-as.factor(avg_table_med_man$BagClass)

set.seed(1)
nofReplications<-1
nFolds<-10
indices=generateCVRuns(train_features_target,nofReplications,nFolds,stratified=TRUE)

glm_train_data<-train_features
glmnet_alldata = glmnet(as.matrix(glm_train_data),train_features_target, family="binomial", alpha = 1, nlambda=47,standardize = TRUE)
lambda_sequence = glmnet_alldata$lambda

cvresult=vector('list',nofReplications*nFolds)
iter=1

for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=(thisReplication[[j]])
    
    cvtrain=glm_train_data[-testindices]    
    cvtrainclass=train_features_target[-testindices]   
    cvtest=glm_train_data[testindices]
    cvtestclass=train_features_target[testindices] 
    
    inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="binomial", alpha = 1,lambda=lambda_sequence,standardize = TRUE)
    valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
    
    foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,x],result=cvtestclass)}))
    cvresult[[iter]]=foldresult
    iter=iter+1
  }
}

cvresult=rbindlist(cvresult)
cvresultcopy<-cvresult
aucresult<-data.table()
for(i in 1:368){
  tempauc<-cvresultcopy[1:10,]
  predROC=prediction(tempauc$V4,tempauc$result)
  perf=performance(predROC,"tpr","fpr")
  auc<-as.numeric(performance(predROC,"auc")@y.values)
  aucresult<-rbind(aucresult,data.table(Replication=tempauc[1,1],Fold=tempauc[1,2],Lambda=tempauc[1,3],AUC=auc))
  cvresultcopy<-cvresultcopy[-(1:10),]
}

final<-aucresult[,.(Mean_AUC=mean(AUC)),keyby=.(Lambda.lambda)]

cv_lambda_max2=final[which.max(Mean_AUC)]$Lambda.lambda
cv_auc_max2=final[which.max(Mean_AUC)]$Mean_AUC


##Logistic Regression Hierarchical clustering euclidean case
train_features<-avg_table_hier_euc[,c(-1,-2)]
train_features_target<-as.factor(avg_table_hier_euc$BagClass)
set.seed(1)
nofReplications<-1
nFolds<-10
indices=generateCVRuns(train_features_target,nofReplications,nFolds,stratified=TRUE)

glm_train_data<-train_features
glmnet_alldata = glmnet(as.matrix(glm_train_data),train_features_target, family="binomial", alpha = 1, nlambda=65,standardize = TRUE)
lambda_sequence = glmnet_alldata$lambda

cvresult=vector('list',nofReplications*nFolds)
iter=1

for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=(thisReplication[[j]])
    
    cvtrain=glm_train_data[-testindices]    
    cvtrainclass=train_features_target[-testindices]   
    cvtest=glm_train_data[testindices]
    cvtestclass=train_features_target[testindices] 
    
    inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="binomial", alpha = 1,lambda=lambda_sequence,standardize = TRUE)
    valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
    
    foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,x],result=cvtestclass)}))
    cvresult[[iter]]=foldresult
    iter=iter+1
  }
}

cvresult=rbindlist(cvresult)
cvresultcopy<-cvresult
aucresult<-data.table()
for(i in 1:368){
  tempauc<-cvresultcopy[1:10,]
  predROC=prediction(tempauc$V4,tempauc$result)
  perf=performance(predROC,"tpr","fpr")
  auc<-as.numeric(performance(predROC,"auc")@y.values)
  aucresult<-rbind(aucresult,data.table(Replication=tempauc[1,1],Fold=tempauc[1,2],Lambda=tempauc[1,3],AUC=auc))
  cvresultcopy<-cvresultcopy[-(1:10),]
}

final<-aucresult[,.(Mean_AUC=mean(AUC)),keyby=.(Lambda.lambda)]

cv_lambda_max3=final[which.max(Mean_AUC)]$Lambda.lambda
cv_auc_max3=final[which.max(Mean_AUC)]$Mean_AUC


##Logistic Regression Hierarchical clustering manhattan case
train_features<-avg_table_hier_man[,c(-1,-2)]
train_features_target<-as.factor(avg_table_hier_man$BagClass)

set.seed(1)
nofReplications<-1
nFolds<-10
indices=generateCVRuns(train_features_target,nofReplications,nFolds,stratified=TRUE)

glm_train_data<-train_features
glmnet_alldata = glmnet(as.matrix(glm_train_data),train_features_target, family="binomial", alpha = 1, nlambda=55,standardize = TRUE)
lambda_sequence = glmnet_alldata$lambda

cvresult=vector('list',nofReplications*nFolds)
iter=1

for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=(thisReplication[[j]])
    
    cvtrain=glm_train_data[-testindices]    
    cvtrainclass=train_features_target[-testindices]   
    cvtest=glm_train_data[testindices]
    cvtestclass=train_features_target[testindices] 
    
    inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="binomial", alpha = 1,lambda=lambda_sequence,standardize = TRUE)
    valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
    
    foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,x],result=cvtestclass)}))
    cvresult[[iter]]=foldresult
    iter=iter+1
  }
}

cvresult=rbindlist(cvresult)
cvresultcopy<-cvresult
aucresult<-data.table()
for(i in 1:368){
  tempauc<-cvresultcopy[1:10,]
  predROC=prediction(tempauc$V4,tempauc$result)
  perf=performance(predROC,"tpr","fpr")
  auc<-as.numeric(performance(predROC,"auc")@y.values)
  aucresult<-rbind(aucresult,data.table(Replication=tempauc[1,1],Fold=tempauc[1,2],Lambda=tempauc[1,3],AUC=auc))
  cvresultcopy<-cvresultcopy[-(1:10),]
}

final<-aucresult[,.(Mean_AUC=mean(AUC)),keyby=.(Lambda.lambda)]

cv_lambda_max4=final[which.max(Mean_AUC)]$Lambda.lambda
cv_auc_max4=final[which.max(Mean_AUC)]$Mean_AUC







