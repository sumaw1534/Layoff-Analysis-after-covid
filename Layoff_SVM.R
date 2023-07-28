df=read.csv("C:/Users/Dell/Downloads/finalds.csv",stringsAsFactors=T)
#drop <- c("Attrition","Over18")
#df = df[,!(names(df) %in% drop)]
df=na.omit(df)
head(df)
dim(df)
names(df)

df$Age=as.numeric(df$Age)
df$YearsAtCompany=as.numeric(df$YearsAtCompany)
df$MonthlyIncome=as.numeric(df$MonthlyIncome)

install.packages('caTools')
library(caTools)

df=df[,c("Age","MonthlyIncome","Layoff")]
head(df)

set.seed(123)
split = sample.split(df$Layoff, SplitRatio = 0.8)

training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)



# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

head(training_set)

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)

classifier = svm(formula = Layoff ~ Age+ MonthlyIncome,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

classifier


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])
#y_pred


# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
#cm

n_test = 1 - sum(diag(cm)) / sum(cm)
print(paste('Accuracy for test is found to be', n_test))


install.packages("MLmetrics")
library("MLmetrics")


dec_f1=F1_Score(y_pred,test_set$Layoff)
#dec_f1=round(dec_f1,2)
dec_f1

dec_pre=Precision(y_pred,test_set$Layoff)
#dec_pre=round(dec_pre,2)
dec_pre

dec_call=Recall(y_pred,test_set$Layoff)
#dec_call=round(dec_call,2)
dec_call
