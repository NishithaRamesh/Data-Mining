library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lattice)
library(DataExplorer)
library(grDevices)
library(factoextra)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ranger)
library(Metrics)
library(ROCit)
library(kableExtra)

#Reading the File
install.packages("GGally")
library("readxl")
mydata = read_excel(file.choose())
attach(mydata)
library(car)
#Basic Data Analysis
dim(mydata)
#Data has 14 columns with 5000 observations
summary(mydata)
#family member has got 18 observations missing
str(mydata)
#all 14 colums are represented in nature which is not correct
#Columns like Personal Loan, CD Account,Securities Acoount, Online and Personal Loan Card  are categorical in nature with levels "0" and "1" 
#Education is with 3 levels 1 < 2 < 3 which is categorical in Nature which should be converted into Factors

## Converting multiple columns into factor columns
col = c("Education","Personal Loan","Securities Account", "CD Account", "Online", "CreditCard")
mydata[col] = lapply(mydata[col], factor)
str(mydata)

## Converting Education into ordered factors
mydata$Education = factor(mydata$Education, levels = c("1", "2", "3"), order = TRUE)
str(mydata)
report(mydata)
#Final Report after Data treatemant
install.packages("Data explorer")
library("DataExplorer")
create_report(mydata)

#Null(Missing) Value Treatement and data treatement
#Family Member Value Treatment. 18 missing value should be replaced with 1 as the lowest number of members a person can have is he/she themselves
FamilySummary=table(mydata$`Family members`)
FamilySummary
#mode of family summary is 1 
any(is.na(mydata)) ## check for missing values
sapply(mydata, function(x){sum(is.na(x))}) ## checking columns which have missing values
mydata[is.na(mydata)] = 1
summary(mydata)
# check again after replacing with 1

#Experience has negative values 
summary(mydata$`Experience (in years)`)
## checking for rows having negative values as Experience
head(mydata[mydata$`Experience (in years)`<0,])  
mydata$`Experience (in years)` = abs(mydata$`Experience (in years)`)
#After treatment analysis
summary(mydata$`Experience (in years)`)
head(mydata[mydata$`Experience (in years)`<0,])  

#ID and Zip code columns will not help much in analyis since they are basically addon - para information
mydata = mydata[, -c(1,5)]  ## removing ID and Zip code column from dataset
summary(mydata)
str(mydata)
create_report(mydata)

#Exploratory Data Analysis
#Univariate Analysis
boxplot(mydata$`Age (in years)`)
summary(mydata$`Age (in years)`)
boxplot(mydata$`Experience (in years)`)
summary(mydata$`Experience (in years)`)
boxplot(mydata$`Income (in K/month)`)
summary(mydata$`Income (in K/month)`)
boxplot(mydata$`Family members`)
summary(mydata$`Family members`)
boxplot(mydata$CCAvg)
summary(mydata$CCAvg)
boxplot(mydata$Mortgage)
summary(mydata$Mortgage)

#bivariate Analysis
## Plotting boxplot for Personal Loan (Response variable) for all numerical variables
plot_boxplot(mydata, by = "Personal Loan",geom_boxplot_args = list("outlier.color" = "blue"))

#Stacked bar chart for Personal loan for all categorical vairables
ggplot(mydata, aes(fill=`Personal Loan`, x=Education, y=as.numeric(Education))) +
  geom_bar( stat="identity", position="fill")

ggplot(mydata, aes(fill=`Personal Loan`, x=`Securities Account`, y=as.numeric(`Securities Account`))) +
  geom_bar( stat="identity")

ggplot(mydata, aes(fill=`Personal Loan`, x=`CD Account`, y=as.numeric(`CD Account`))) +
  geom_bar( stat="identity")

ggplot(mydata, aes(fill=`Personal Loan`, x=Online, y=as.numeric(Online))) +
  geom_bar( stat="identity")

ggplot(mydata, aes(fill=`Personal Loan`, x=CreditCard, y=as.numeric(CreditCard))) +
  geom_bar( stat="identity")


#Splitting of Dataset into Train - Test set
set.seed(1234)
## sampling 70% of data for training the algorithms using random sampling 
mydata.index = sample(1:nrow(mydata), nrow(mydata)*0.70)
mydata.train = mydata[mydata.index,]
mydata.test = mydata[-mydata.index,]
dim(mydata.train)
dim(mydata.test)

# checking the ration of personal loans in each partition 
prop.table(table(mydata.train$`Personal Loan`))
prop.table(table(mydata.test$`Personal Loan`))

#Builing the full grown tree

library(rpart)
library(rpart.plot)
set.seed(1234)

tree_full = rpart(formula = `Personal Loan`~., data = mydata.train, cp=-1, minsplit=2, minbucket=1)
rpart.plot(tree_full, cex=0.7)

print(tree_full)

## Predict using the CART model
mydata.train$predict.class=predict(tree_full,mydata.train,type="class")
mydata.train$predict.score=predict(tree_full,mydata.train)

## Creating the confusion matrix
tabtrain=with(mydata.train,table(`Personal Loan`,predict.class))
tabtrain

TN_train = tabtrain[1,1]
TP_train = tabtrain[2,2]
FN_train = tabtrain[2,1]
FP_train = tabtrain[1,2]

train_acc = (TN_train+TP_train)/(TN_train+TP_train+FN_train+FP_train)
train_acc

train_sens = TP_train/(TP_train+FN_train)
train_sens


train_spec = TN_train/(TN_train+FP_train)
train_spec

## Predict using the CART model
mydata.test$predict.class=predict(tree_full,mydata.test,type="class")
mydata.test$predict.score=predict(tree_full,mydata.test)

## Creating the confusion matrix
tabtest=with(mydata.test,table(`Personal Loan`,predict.class))
tabtest

TN_test = tabtest[1,1]
TP_test = tabtest[2,2]
FN_test = tabtest[2,1]
FP_test = tabtest[1,2]

test_acc = (TN_test+TP_test)/(TN_test+TP_test+FN_test+FP_test)
test_acc

test_sens = TP_test/(TP_test+FN_test)
test_sens


test_spec = TN_test/(TN_test+FP_test)
test_spec

df_results_train = data.frame(train_acc, train_sens, train_spec)
names(df_results_train) = c("ACC", "SENS", "SPEC")
df_results_test = data.frame(test_acc, test_sens, test_spec)
names(df_results_test) = c("ACC", "SENS", "SPEC")

?rbind
df_fin =rbind(df_results_train, df_results_test)
row.names(df_fin) = c('tree_full_train', 'tree_full_test')
df_fin

#remove predicted score and class before running other models
mydata.train$predict.class = NULL
mydata.train$predict.score = NULL
mydata.test$predict.class = NULL
mydata.test$predict.score = NULL

#Pruning using minbucket and minsplit
set.seed(1234)
?rpart

tree_manual_prune=rpart(formula = `Personal Loan` ~ ., data = mydata.train, method="class",control = rpart.control(minsplit = 35,  minbucket = 12))



rpart.plot(tree_manual_prune, cex=0.8)

## Predict using the CART model
mydata.train$predict.class=predict(tree_manual_prune,mydata.train,type="class")
mydata.train$predict.score=predict(tree_manual_prune,mydata.train)

## Creating the confusion matrix
tabtrain=with(mydata.train,table(`Personal Loan`,predict.class))
tabtrain

TN_train = tabtrain[1,1]
TP_train = tabtrain[2,2]
FN_train = tabtrain[2,1]
FP_train = tabtrain[1,2]

train_acc = (TN_train+TP_train)/(TN_train+TP_train+FN_train+FP_train)
train_acc

train_sens = TP_train/(TP_train+FN_train)
train_sens


train_spec = TN_train/(TN_train+FP_train)
train_spec

## Predict using the CART model
mydata.test$predict.class=predict(tree_manual_prune,mydata.test,type="class")
mydata.test$predict.score=predict(tree_manual_prune,mydata.test)

## Creating the confusion matrix
tabtest=with(mydata.test,table(`Personal Loan`,predict.class))
tabtest

TN_test = tabtest[1,1]
TP_test = tabtest[2,2]
FN_test = tabtest[2,1]
FP_test = tabtest[1,2]

test_acc = (TN_test+TP_test)/(TN_test+TP_test+FN_test+FP_test)
test_acc

test_sens = TP_test/(TP_test+FN_test)
test_sens


test_spec = TN_test/(TN_test+FP_test)
test_spec

df_results_train = data.frame(train_acc, train_sens, train_spec)
names(df_results_train) = c("ACC", "SENS", "SPEC")
df_results_test = data.frame(test_acc, test_sens, test_spec)
names(df_results_test) = c("ACC", "SENS", "SPEC")

?rbind
df_fin =rbind(df_fin, df_results_train, df_results_test)
row.names(df_fin) = c('tree_full_train', 'tree_full_test', 'tree_manual_prune_train', 'tree_manual_prune_test')
df_fin

#remove predicted score and class before running other models
mydata.train$predict.class = NULL
mydata.train$predict.score = NULL
mydata.test$predict.class = NULL
mydata.test$predict.score = NULL

#Prune using cp
printcp(tree_full)
plotcp(tree_full)

bestcp=tree_full$cptable[which.min(tree_full$cptable[,"xerror"]),"CP"]
bestcp


ptree=prune(tree_full,cp=bestcp)
print(ptree)
rpart.plot(ptree, cex = 0.8)

## Predict using the CART model
mydata.train$predict.class=predict(ptree,mydata.train,type="class")
mydata.train$predict.score=predict(ptree,mydata.train)

## Creating the confusion matrix
tabtrain=with(mydata.train,table(`Personal Loan`,predict.class))
tabtrain

TN_train = tabtrain[1,1]
TP_train = tabtrain[2,2]
FN_train = tabtrain[2,1]
FP_train = tabtrain[1,2]

train_acc = (TN_train+TP_train)/(TN_train+TP_train+FN_train+FP_train)
train_acc

train_sens = TP_train/(TP_train+FN_train)
train_sens


train_spec = TN_train/(TN_train+FP_train)
train_spec

## Predict using the CART model
mydata.test$predict.class=predict(ptree,mydata.test,type="class")
mydata.test$predict.score=predict(ptree,mydata.test)

## Creating the confusion matrix
tabtest=with(mydata.test,table(`Personal Loan`,predict.class))
tabtest

TN_test = tabtest[1,1]
TP_test = tabtest[2,2]
FN_test = tabtest[2,1]
FP_test = tabtest[1,2]

test_acc = (TN_test+TP_test)/(TN_test+TP_test+FN_test+FP_test)
test_acc

test_sens = TP_test/(TP_test+FN_test)
test_sens


test_spec = TN_test/(TN_test+FP_test)
test_spec

df_results_train = data.frame(train_acc, train_sens, train_spec)
names(df_results_train) = c("ACC", "SENS", "SPEC")
df_results_test = data.frame(test_acc, test_sens, test_spec)
names(df_results_test) = c("ACC", "SENS", "SPEC")

?rbind
df_fin =rbind(df_fin, df_results_train, df_results_test)
row.names(df_fin) = c('tree_full_train', 'tree_full_test', 'tree_manual_prune_train', 'tree_manual_prune_test', 'cptree_train', 'cptree_test')
round(df_fin,3)

#remove predicted score and class before running other models
mydata.train$predict.class = NULL
mydata.train$predict.score = NULL
mydata.test$predict.class = NULL
mydata.test$predict.score = NULL



