df=read.csv("C:/Users/Dell/Downloads/finalds.csv",stringsAsFactors=T)
#drop <- c("Attrition","Over18")
#df = df[,!(names(df) %in% drop)]
df=na.omit(df)
head(df)
dim(df)
names(df)


df$Age=as.numeric(df$Age)
df$NumCompaniesWorked=as.numeric(df$NumCompaniesWorked)
df$YearsAtCompany=as.numeric(df$YearsAtCompany)
df$MonthlyIncome=as.numeric(df$MonthlyIncome)
df$Layoff=as.numeric(df$Layoff)
df$JobLevel=as.numeric(df$JobLevel)
df$TotalWorkingYears=as.numeric(df$TotalWorkingYears)

#Age, Gender, YearsAtCompany, Department
#NumCompaniesWorkd, MonthlyIncome

df=df[,c('Age',"YearsAtCompany","NumCompaniesWorked","MonthlyIncome","JobLevel","TotalWorkingYears","Layoff")]
head(df)


# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("class")

# Loading package
library(e1071)
library(caTools)
library(class)

# Splitting data into train
# and test data
split <- sample.split(df, SplitRatio = 0.75)
train_cl <- subset(df, split == "TRUE")
test_cl <- subset(df, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:6])
test_scale <- scale(test_cl[, 1:6])

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Layoff,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$Layoff)
print(paste('Accuracy =', 1-misClassError))


# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Layoff,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$Layoff)
print(paste('Accuracy =', 1-misClassError))




install.packages("MLmetrics")
library("MLmetrics")


dec_f1=F1_Score(classifier_knn,test_cl$Layoff)
#dec_f1=round(dec_f1,2)
dec_f1

dec_pre=Precision(classifier_knn,test_cl$Layoff)
#dec_pre=round(dec_pre,2)
dec_pre

dec_call=Recall(classifier_knn,test_cl$Layoff)
#dec_call=round(dec_call,2)
dec_call

