
#### ===================================================================
## ================= Continuos data
#### ===================================================================


library(caret)
library(dplyr)
library(readr)
library(basad)

df<- read_csv("~/datacontinuousstudents.csv")



#######===============================================================
## ==== data prepartion
set.seed(9877)
trainIndex <- createDataPartition(df$y, p = .7,
                                  list = FALSE,
                                  times = 1)
train <- df[ trainIndex,]
test <- df[-trainIndex,]

data_preparation<-function(df){
  cont_ind<-c(1,2,8:12,21:27,30:32)
  cont_var<-paste("x",cont_ind,sep = "")  
  cate_ind<-c(3:7,13:20,28,29)    
  cate_var<-paste("x",cate_ind,sep = "") 
  
  df_cont<-df[,cont_var]
  df_cat<-df[,cate_var]
  
  df_cont_scale<-scale(df_cont)%>%as_tibble()
  X<-cbind(y=df$y,df_cat,df_cont_scale)
  res=list(X=X,df_cont=df_cont,df_cat=df_cat)
  return(res)
}


#=== ========
restrain<-data_preparation(train)

X_train<-restrain$X
train_cont<-restrain$df_cont
train_cat<-restrain$df_cat

x = model.matrix (y~.,X_train )
y = X_train[,1]

#=== ========
restest<-data_preparation(test)

X_test<-restest$X
test_cont<-restest$df_cont
test_cat<-restest$df_cat

cont_ind<-c(1,2,8:12,21:27,30:32)
cont_var<-paste("x",cont_ind,sep = "") 
corrplot::corrplot(cor(df[,c("y",cont_var)],method = "spearman"),method="ellipse",type="lower",
                   number.cex = .5,addCoef.col = "black")

# ==========================
#************** Bivariate analysis ************************
# ==========================

res_catego<-data.frame()
for (i in names(train_cat)) {
  pr<-wilcox.test(X_train[,"y"]~train_cat[,i][[1]], paired=FALSE)
  res_cat<-data_frame(x_cont=i,Wval=pr$statistic,pval=pr$p.value)
  res_catego<-rbind(res_catego,res_cat)
}
x_cat_selected<-res_catego%>%filter(pval<.2)


res_continu<-data.frame()
for (j in names(train_cont)) {
  pr<-wilcox.test(train_cont[,j][[1]],X_train[,"y"], paired=FALSE)
  res_cont<-data_frame(x_cont=j,Wval=pr$statistic,pval=pr$p.value)
  res_continu<-rbind(res_continu,res_cont)
}
x_cont_selected<-res_continu%>%filter(pval<.2)




###===== basad package: shrinkage spike-slab

iter<-50000
basad_model <- basad( x = X_train[,-1], y = y, prior.dist = "t", select.cri = "BIC",niter = iter) # for 
print( basad_model )
coeff<-basad_model$est.B[basad_model$model.index]
estimate<-X_test$x23*coeff["x23"]+X_test$x25*coeff["x25"] +coeff["intercept"]
check<-tibble(pred=estimate,y=X_test$y)%>%mutate(err2=(pred-y)^2,check=case_when(
  ((y< -1)&(pred< -1))| ((y> -1)&(pred> -1))~1))
(indicador<-sum(check$check,na.rm = T)/nrow(X_test)*100)
(mse<-sum(check$err2)/nrow(X_test))

