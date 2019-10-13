

#### ===================================================================
## ================= Binary data
#### ===================================================================
library(readr)
library(dplyr)  
library(MCMCpack)
library(brms) # esta libreria trabajo con Stan
library(caret)
library(BoomSpikeSlab)
# ==========================
#************** data preparation ************************
# ==========================

df <- read_csv("~/databinarystudents.csv")
df<-df[1:150,]


set.seed(9876)
trainIndex <- createDataPartition(df$yL, p = .7,
                                  list = FALSE,
                                  times = 1)
train <- df[ trainIndex,]
test <- df[-trainIndex,]

data_preparation<-function(df){
  
  cont_ind<-c(1,2,8:12,21:27,30:32)
  cont_var<-paste(rep("x",length(cont_ind)),cont_ind,sep = "")  
  cate_ind<-c(3:7,13:20,28,29)    
  cate_var<-paste(rep("x",length(cate_ind)),cate_ind,sep = "") 
  
  df_cont<-df[,cont_var]
  df_cat<-df[,cate_var]
  
  df_cont_scale<-scale(df_cont)%>%as_tibble()
  X<-cbind(y=df$yL,df_cat,df_cont_scale)
  res=list(X=X,df_cont=df_cont,df_cat=df_cat)
  return(res)
}


#=== ========
restrain<-data_preparation(train)

X_train<-restrain$X
train_cont<-restrain$df_cont
train_cat<-restrain$df_cat

cont_ind<-c(1,2,8:12,21:27,30:32)
cont_var<-paste(rep("x",length(cont_ind)),cont_ind,sep = "")  
cate_ind<-c(3:7,13:20,28,29)    
cate_var<-paste(rep("x",length(cate_ind)),cate_ind,sep = "") 

corrplot::corrplot(cor(X_train[,cont_var],method = "spearman"),method="ellipse",type="lower",
                   number.cex = .5,addCoef.col = "black")

#=== ========
restest<-data_preparation(test)

X_test<-restest$X
test_cont<-restest$df_cont
test_cat<-restest$df_cat

# ==========================
#************** Variables Candidatas por pruebas bivariadas ************************
# ==========================

res_catego<-data.frame()
for (i in names(train_cat)) {
  tb<-table(df[,"yL"][[1]],df[,i][[1]])
  pr<-chisq.test(tb)
  res_cat<-data_frame(x_cat=i,chival=pr$statistic,pval=pr$p.value)
  res_catego<-rbind(res_catego,res_cat)
}
x_cat_selected<-res_catego%>%filter(pval<.2)


res_continu<-data.frame()
for (j in names(train_cont)) {
  pr<-wilcox.test(df[,j][[1]]~ df[,"yL"][[1]], paired=FALSE)
  res_cont<-data_frame(x_cont=j,Wval=pr$statistic,pval=pr$p.value)
  res_continu<-rbind(res_continu,res_cont)
}
x_cont_selected<-res_continu%>%filter(pval<.2)

# ==========================
#************** Bayesian model ************************
# ==========================


#===== Spike and labs

niter = 100000
m_select0 <- logit.spike(y ~ ., 
                                       data = X_train,
                                       niter = niter)


fit<-m_select0
PIP <- colMeans(fit$beta != 0) 
PIP
plot(fit,inclusion.threshold = .05)


# =================================
#************** Fit  *******
# =================================

fit <- brm(y ~ x1+x17+x4, data = X_train, 
             family = bernoulli(link = "logit"),iter = 2000) 


summary(fit)
plot(fit)
pp = pp_check(fit)
pp + theme_bw()


##=====Predictions 

newdata <- predict(fit, newdata = X_test,
                   type = "response")%>%as_tibble()
threshold<-0.5
newdata$class<-ifelse(newdata[,1]<=threshold,0,1)
(accuracy<-round(sum(X_test$y==newdata$class)/nrow(X_test)*100,3))

library(pROC)
ROC <- roc(test$yL,newdata$Estimate,
           smoothed = TRUE,
           ci=TRUE, ci.alpha=0.9, stratified=FALSE,
           plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
           print.auc=TRUE, show.thres=TRUE)
sens.ci <- ci.se(ROC)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")
