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
#df$Layoff=as.character(df$Layoff)
df$Layoff=as.factor(df$Layoff)


install.packages("e1071")
install.packages("caTools")
install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)

# Splitting data into train
# and test data
split <- sample.split(df, SplitRatio = 0.8)
train_cl <- subset(df, split == "TRUE")
test_cl <- subset(df, split == "FALSE")

# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Layoff ~ Age+Gender+YearsAtCompany+Department+MonthlyIncome+NumCompaniesWorked, data = train_cl)
classifier_cl

#Department=test_cl$Department
#Gender=test_cl$Gender
#Layoff=test_cl$Layoff
#test_c1=data.frame(Department,Gender,Layoff)


# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)
#y_pred

# Confusion Matrix
cm <- table(test_cl$Layoff, y_pred)
cm

ac_Test=sum(diag(cm)) / sum(cm)*100
print(paste('Accuracy for test is found to be', ac_Test))


install.packages("MLmetrics")
library("MLmetrics")


dec_f1=F1_Score(y_pred,test_cl$Layoff)
#dec_f1=round(dec_f1,2)
dec_f1

dec_pre=Precision(y_pred,test_cl$Layoff)
#dec_pre=round(dec_pre,2)
dec_pre

dec_call=Recall(y_pred,test_cl$Layoff)
#dec_call=round(dec_call,2)
dec_call
