# PART A-1

library(readxl)  
library(dplyr)
library(tidyr)
library(e1071)
library(caret)
library(caTools)
library(cowplot)
library(pROC)
library(ggcorrplot)
library(corrplot)
library(MASS)

credit_1 = read.csv("creditcard.csv")

cr_missing_data = credit_1 %>% summarise_all(funs(sum(is.na(.))/n()))
cr_missing_data = gather(cr_missing_data, key = "variables", value = "percent_missing")
ggplot(cr_missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()

summary(cr_missing_data)
str(credit_1)

## EDA

# Default
credit_1 %>% 
  group_by(default.payment.next.month) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(default.payment.next.month, -percent), percent), fill = default.payment.next.month)+
  geom_col(fill = c("grey", "light blue"))+
  geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = 0.2, vjust = 2, size = 5)+ 
  theme_bw()+  
  xlab("Default") + ylab("Percent") + ggtitle("Default Percent")

corrplot(cor(credit_1[,-1]), type="lower", method="number")

credit_1$SEX = as.factor(credit_1$SEX)
credit_1$EDUCATION = as.factor(credit_1$EDUCATION)
credit_1$MARRIAGE = as.factor(credit_1$MARRIAGE)
credit_1$default.payment.next.month = as.factor(credit_1$default.payment.next.month)

# Default against catergorial variables
plot_grid(ggplot(credit_1, aes(x=SEX,fill=default.payment.next.month))+ geom_bar(position = 'fill')+ theme_bw(), 
          ggplot(credit_1, aes(x=EDUCATION,fill=default.payment.next.month))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(credit_1, aes(x=MARRIAGE,fill=default.payment.next.month))+ geom_bar(position = 'fill')+theme_bw(),
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10,align = "h")))

plot_grid(ggplot(credit_1, aes(x=PAY_0,fill=default.payment.next.month))+ geom_bar(position = 'fill')+ theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(credit_1, aes(x=PAY_2,fill=default.payment.next.month))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(credit_1, aes(x=PAY_3,fill=default.payment.next.month))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(credit_1, aes(x=PAY_4,fill=default.payment.next.month))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(credit_1, aes(x=PAY_5,fill=default.payment.next.month))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(credit_1, aes(x=PAY_6,fill=default.payment.next.month))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),align = "h")

# Default against numeric variables
ggplot(credit_1, aes(x=default.payment.next.month, y=AGE, fill=default.payment.next.month)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Age")
ggplot(credit_1, aes(x=default.payment.next.month, y=LIMIT_BAL, fill=default.payment.next.month)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Balance Limit")

# Split the data for train and test set
indices = sample.split(credit_1$default.payment.next.month, SplitRatio = 0.7)
train = credit_1[indices,]
validation = credit_1[!indices,]

# Decide the model
credit_model01 = glm(default.payment.next.month ~ .-ID, data = train, family = "binomial")
summary(credit_model01)

credit_model02 = glm(default.payment.next.month ~ .-ID-EDUCATION, data = train, family = "binomial")
summary(credit_model02)

credit_model03 = stepAIC(credit_model01, direction="both")
summary(credit_model03)

pred_credit = predict(credit_model03, type = "response", newdata = validation)
summary(pred_credit)

# ROC Curve
glm.roc.credit = roc(response = validation$default.payment.next.month, predictor = pred_credit)
plot(glm.roc.credit, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)

# Threshold = 0.5
pred_credit_1 = factor(ifelse(pred_credit >=0.5, "Yes", "No"))
real_credit = factor(ifelse(validation$default.payment.next.month == 1, "Yes", "No"))
table(pred_credit_1, real_credit)
confusionMatrix(pred_credit_1, real_credit, positive = "Yes")

conf.1=confusionMatrix(pred_credit_1, real_credit, positive = "Yes")
conf.1$overall[1]
conf.1$byClass[1]
conf.1$byClass[2]

# In search of an optimal threshold
perform_fn <- function(cutoff) 
{
  pred_credit_1 <- factor(ifelse(pred_credit >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(pred_credit_1, real_credit, positive = "Yes")
  accuracy <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuracy))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(0.01,0.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.22, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))


# PART A-2

credit_2 = credit_1[-c(25)]
credit_2$SEX = as.numeric(credit_2$SEX)
credit_2$EDUCATION = as.numeric(credit_2$EDUCATION)
credit_2$MARRIAGE = as.numeric(credit_2$MARRIAGE)

str(credit_2)

pr.credit = prcomp(credit_2, scale=TRUE)
summary(pr.credit)
names(pr.credit)
pr.credit$center
pr.credit$scale
pr.credit$rotation
pr.credit$x

# PART A-3

km.credit = kmeans(credit_2, 4, nstart=5)
km.credit$cluster
ggplot(credit_2, aes(LIMIT_BAL, PAY_AMT1)) +
  geom_point(aes(color = km.credit$cluster))

