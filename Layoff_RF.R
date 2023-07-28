df=read.csv("C:/Users/Dell/Downloads/finalds.csv",stringsAsFactors=T)
#drop <- c("Attrition","Over18")
#df = df[,!(names(df) %in% drop)]
df=na.omit(df)
head(df)
dim(df)
names(df)


df$Age=as.factor(df$Age)
df$Gender=as.factor(df$Gender)
df$Department=as.factor(df$Department)
df$NumCompaniesWorked=as.factor(df$NumCompaniesWorked)
df$YearsAtCompany=as.factor(df$YearsAtCompany)
df$Layoff=as.factor(df$Layoff)


#Age, Gender, YearsAtCompany, YearsSinceLastPromotion, Department
#NumCompaniesWorkd, MonthlyIncome


install.packages("caTools") 
install.packages("randomForest")

library(caTools)
library(randomForest)


# Splitting data in train and test data
split <- sample.split(df, SplitRatio = 0.8)
split

train <- subset(df, split == "TRUE")
test <- subset(df, split == "FALSE")

# Fitting Random Forest to the train data set
set.seed(120)


output.forest <- randomForest(Layoff ~ Age+Gender+YearsAtCompany+Department+MonthlyIncome+NumCompaniesWorked, train)

# View the forest results.
print(output.forest) 


# Predicting the Test set results
y_pred = predict(output.forest, newdata = test)

# Confusion Matrix
m_at = table(test$Layoff, y_pred)
m_at

ac_Test=sum(diag(m_at)) / sum(m_at)*100
print(paste('Accuracy for test is found to be', ac_Test))


install.packages("MLmetrics")
library("MLmetrics")


dec_f1=F1_Score(y_pred,test$Layoff)
#dec_f1=round(dec_f1,2)
dec_f1

dec_pre=Precision(y_pred,test$Layoff)
#dec_pre=round(dec_pre,2)
dec_pre

dec_call=Recall(y_pred,test$Layoff)
#dec_call=round(dec_call,2)
dec_call
