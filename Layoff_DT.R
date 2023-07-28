df=read.csv("C:/Users/Dell/Downloads/finalds.csv",stringsAsFactors=T)
drop <- c("Attrition","Over18")
df = df[,!(names(df) %in% drop)]
df=na.omit(df)
head(df)
dim(df)


df$Age=as.factor(df$Age)
df$Gender=as.factor(df$Gender)
df$Department=as.factor(df$Department)
df$MonthlyIncome=as.numeric(df$MonthlyIncome)
df$NumCompaniesWorked=as.factor(df$NumCompaniesWorked)
df$YearsAtCompany=as.factor(df$YearsAtCompany)
df$JobLevel=as.factor(df$JobLevel)
#df$Layoff=as.character(df$Layoff)
df$Layoff=as.factor(df$Layoff)


#Age, Gender, YearsAtCompany, Department
#NumCompaniesWorkd, MonthlyIncome


install.packages("caTools")
install.packages("party")

library(caTools)
library(party)

# Splitting data in train and test data
split <- sample.split(df, SplitRatio = 0.8)
split

train <- subset(df, split == "TRUE")
test <- subset(df, split == "FALSE")

set.seed(120)  # Setting seed

model<- ctree(Layoff ~ Age+Gender+YearsAtCompany+Department+MonthlyIncome+NumCompaniesWorked, train)
plot(model)


predict_model<-predict(model, test)
predict_model

# Confusion Matrix
m_at <- table(test$Layoff, predict_model)
m_at

ac_Test=sum(diag(m_at)) / sum(m_at)*100
print(paste('Accuracy for test is found to be', ac_Test))


install.packages("MLmetrics")
library("MLmetrics")

dec_f1=F1_Score(predict_model,test$Layoff)
#dec_f1=round(dec_f1,2)
print(paste("F1 Score:",dec_f1))

dec_pre=Precision(predict_model,test$Layoff)
#dec_pre=round(dec_pre,2)
print(paste("Precision:",dec_pre))

dec_call=Recall(predict_model,test$Layoff)
#dec_call=round(dec_call,2)
print(paste("Recall:",dec_call))

