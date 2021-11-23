# Author = KJM
# filter some columns using Chi-squared test and then make Random forest Toxicity model
library(dplyr)
library(randomForest)
library(ROCR)
require(pROC)

df <- read.csv("hERG_Sample_Discrete_Dataset.csv",sep=',',row.names = 1)

df_rename<-sub("#","_",colnames(df))
#colnames(df)

colnames(df[2247])

#df$cD.Toxicity[df$cD.Toxicity == "1"]<= 'Toxicity'
#df$cD.Toxicity[df$cD.Toxicity == "0"]<= 'Non-toxic'

df$cD.Toxicity <- gsub("1","toxi",df$cD.Toxicity)
df$cD.Toxicity <- gsub("0","non-toxi",df$cD.Toxicity)


View(summary(df))

p_val_pass <-c()

for(i in names(df)){
  median_val<- median(df[[i]])
  print(i)
  df_sel<-select(df,i,"cD.Toxicity")
  df_sel_mu <- df_sel %>%  mutate(!!sym(i) := ifelse(!!sym(i) > median_val, "high", "low"))
  #df_sel_mu <- df_sel %>%  mutate(!!sym(i) := ifelse(!!sym(i) <= median_val, "low", !!sym(i)))
  df_sel_mu
  table_result=table(df_sel_mu)
  chi_result=chisq.test(table_result, correct = FALSE)
  pval<-chi_result$p.value
  if (pval < 0.05) {
    p_val_pass <- c(p_val_pass, i)
  }
}
p_val_pass<-c(p_val_pass,"cD.Toxicity")
df_pass<-df[p_val_pass]

dim(df_pass)

write.csv(df_pass, file="pass.table.csv",sep='\t',quote=FALSE)

df_pass$cD.Toxicity <- gsub("1","toxi",df_pass$cD.Toxicity)
df_pass$cD.Toxicity <- gsub("0","non-toxi",df_pass$cD.Toxicity)

data_fac=df_pass %>% mutate_if(is.character, as.factor)

sn <- sample(1:nrow(data_fac), size = nrow(data_fac)*0.7)
sn

train<-data_fac[sn,]
test<-data_fac[-sn,]

rf_fit<-randomForest(cD.Toxicity ~ ., train, keep.forest=TRUE)

rf_p_train<-predict(rf_fit,type="prob")[,2]
rf_pr_train<-prediction(rf_p_train, train$cD.Toxicity)
r_auc_train1 <- performance(rf_pr_train, measure = "auc")@y.values[[1]] 
r_auc_train1

test_prediction <- predict(rf_fit, newdata=test, type="prob")[,2]
test_pr_train<-prediction(test_prediction, test$cD.Toxicity)
test_auc<-performance(test_pr_train, measure = "auc")@y.values[[1]] 
test_auc

