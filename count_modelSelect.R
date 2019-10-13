
#### ===================================================================
## ================= Modelo conteo
#### ==================================================================

library(caret)
library(dplyr)
library(readr)
library(basad)
df <- read_csv("~/datacountstudents.csv")


#######===============================================================
## ==== data prepartion

df<-df%>%rename(y=yC)
df_small<-df%>%filter(y>4)
df<-df%>%filter(y<=4)

set.seed(9883)

trainIndex <- createDataPartition(df$y, p = .7,
                                  list = FALSE,
                                  times = 1)
train <- df[ trainIndex,]
test <- df[-trainIndex,]
train<-rbind(train,df_small)

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



#======  Set training control
y_val<-as.numeric(names(table(X_train$y)))
w<-1/table(X_train$y)

myweights<-w/sum(w)

model_weights<-NA
for (i in y_val) {
  model_weights<-ifelse(X_train$y==i,w[i+1],model_weights)
}



###===== basad package: shrinkage spike-slab

iter<-100000
basad_model <- basad( x = X_train[,-1], y = X_train$y, prior.dist = "t", select.cri = "BIC",niter = iter) # for 
print( basad_model )



#============ Fit model

train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              verboseIter = F)

# Train the model

elastic <- train(y ~ .,
                 data = X_train[,c("y","x25","x3")],
                 method = "glmnet",
                 tuneLength = 25,
                 #weights = model_weights,
                 trControl = train_control)




# Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)
# Make predictions
predictions <- elastic %>% predict(X_test)
# Model prediction performance
#mse<-sum((predictions-X_test$y)^2)/nrow(X_test)
data.frame(
  RMSE = RMSE(predictions, X_test$y),
  MSE=RMSE(predictions, X_test$y)^2,
  Rsquare = R2(predictions, X_test$y)
  
)

check<-tibble(pred=predictions,y=X_test$y,pred_round=round(pred,0))%>%mutate(check=case_when(
  ((y==0)&(pred_round==0))| ((y>0 )&(pred_round> 0))~1
))

(precision<-sum(check$check,na.rm = T)/nrow(X_test)*100)
