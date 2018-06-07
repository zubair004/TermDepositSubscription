rm(list = setdiff(ls(),''))

# The classification goal is to predict if the client will subscribe (yes/no) a term deposit (variable y).

#imp libraries
library(ggplot2)
library(stringr)

#load the data 
df = read.csv('usecase-2.csv')

#Basic exploration on overall data set

#NA
sapply(df, function(x)sum(is.na(x)))# no missing values

#data type and length of unique values in variable
sapply(df, function(x)length(unique(x)))
sapply(df, class) # data types are correct

#over all summary of all numeric the variables
install.packages('devtools')
library(devtools)
install_github("ujjwalkarn/xda") #installing 'xda' using github 

library(xda)
num_sum = numSummary(df) #using this function we can get whole summary for all numeric variables(Ex. NA, central tendency, measure of dispersion)


###################################################### univariate analysis
#output
prop.table(table(df$output)) #no = 88.30%, yes = 11.69%

#Age
range(df$age)
ggplot(df, aes(age))+geom_density(aes(fill = T))
age = boxplot(df$age,
              main = "age of the customer",
              col = "orange",
              border = "brown",
              horizontal = TRUE
)
#Around 50% customers age lies between 35 to 50 years.
#Hypothesis-1: customer whose age is in between 25 to 45 and they have not taken any other loan they will accept the scheme.   

length(age$out)#There are some outliers are present which will deal at later stage
ggplot(df, aes(age)) + geom_histogram(binwidth = 1, bins = 50)#age is slightly right skewed.


#job
table(df$job)
ggplot(df,aes(job)) + geom_bar(fill = "steelblue", color ="steelblue") +
  theme_minimal()
names(df)

#marital
table(df$marital)
ggplot(df,aes(marital)) + geom_bar(fill = "steelblue", color ="steelblue") +
  theme_minimal()
names(df)

#education
table(df$education)# 70% of customers education is only till primary and secondary, so most probably they will be doing business. Generally these people are not inclined towards such investment schemes.
#Hypothesis-3: customer with qualification primary and secondary will not accept the scheme.
ggplot(df,aes(education)) + geom_bar(fill = "steelblue", color ="steelblue") +
  theme_minimal()
names(df)

#default
prop.table(table(df$default))#1.8% customers have credit default history. This is imbalance catagories, this may negatively effect our result.
ggplot(df,aes(default)) + geom_bar(fill = "steelblue", color ="steelblue") +
  theme_minimal()

#housing
prop.table(table(df$housing))#55.5% customer have taken the housing loan previously. 
#Hypothesis-4: Customers who have already taken the loan they will not accept our investment schemes, this we will check at later stage.

#loan
prop.table(table(df$loan))#16.02% have personnel loan

#campaign
ggplot(df, aes(campaign)) + geom_histogram(binwidth = 1, bins = 50)
#maximum people have contacted only once and twice. There are some customers who have contacted more 30 times, will check the reason for that.

#pdays
table(df$pdays)
#36954 customer have value '-1', means they have not contacted before
ggplot(df, aes(pdays)) + geom_histogram(binwidth = 10, bins = 50)#highly skewed variable 

#poutcome
prop.table(table(df$poutcome))
ggplot(df, aes(poutcome)) + geom_bar()
#only 3.3% customers accepted the previous scheme and 10.8% are didnt accept.81.74% customers are in 'unknown' catagory amy be they have not contacted for scheme.


################################################## Bivariate Analysis
#output-housing
# Chi-square Test of Independence - Categorical Data 
# We would like to statistically test if previos housing loans of the applicants have any association to current outcome. 

#Null Hypothesis: Customers’s previous housing loan and current outcome are Independent of each other.

prop.table(table(output = df$output, loan = df$housing),1) 

test1 = chisq.test(df$output,df$housing)
test1 #

#Drawing conclusion
# We can see the 63.4% of the customer who accepted this scheme have no previous housing loans. 
# This difference is statistically significant. Chi-square test of independence found probability of outcome to be dependent on housing loan with p-value of 2.2e-16 at 1 degrees of freedom. 
# Insight: Customer who are not involved in housing loan they are most likely to accept the loan. Our target customers should be independent of any loan burden.

#output-poutcome
prop.table(table(output = df$output, pre_contact_outcome = df$poutcome),1) 
prop.table(table(output = df$output, pre_contact_outcome = df$poutcome),2) 

#Insight: 64.72% of customer accepted this campaign out of 100% of the customer who had accepted the previous campaign.only 12.6% accepted whose status is failure in previous campaign 
#This gives the insight that customer who have accepted previous scheme are most likely to accept this scheme.

test2 = chisq.test(df$output,df$poutcome)
test2 # base on the p value it has significant relatioship with previous campaign.

#output-education
prop.table(table(output = df$output, Qualification = df$education),2) 
ggplot(data = df, aes(output, fill = education)) + geom_bar()

test3 = chisq.test(table(df$output,df$education))
test3 #

# Conclusion
# Based on the proportionate table I we proved the base hypothesis made while doing univariate analysis. 15% customers with tertiary education background accepted the scheme. Followed by 10.5% secondary and 8.6% primary.
# Based on p value, there is a significant relation ship between education and outcome

#output-education
prop.table(table(output = df$output, default = df$default),1) 
ggplot(data = df, aes(output, fill = default)) + geom_bar()

test4 = chisq.test(table(df$output,df$default))
test4 #

# Conclusion:
# By looking at the plot we can that the defaulted customers have not accepted the scheme.
# It has the significant relationship with the target. We should approach the customers who have clean credit history. 

######### output-age
ggplot(df, aes(output, age)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Target variable") + 
  ylab("age") + 
  ggtitle("age vs target variable")

#ANOVA to check the significance
test5 = aov(age ~ output,df)
summary(test5)

###Conclusion
# Based on ANOVA age has significant effect on probability of accepting the scheme. 
# We can see the boxplot the median age of the customer who have accepted the scheme is slightly less than who have not accepted.


#output - duration
ggplot(df, aes(x = output, y = duration, fill = output )) + geom_boxplot()
tmp = subset(df, df$duration <= 8)
table(tmp$output)

test5 = aov(age ~ output,df)
summary(test5)

#conclusion:
#this came out to be highly significant variable as the fact that call duration is '0' or less than 30sec are almost all are not accepted. 


###################### Multi variate Analysis

# correlation between all Numeric variables
library(ggcorrplot)
ggcorrplot(cor(df[, sapply(df, is.numeric)]), type = 'lower',lab = T)
# all the variables are not correlated with each other, lets check with the target variable

#correlation of all numeric variable with target variable
for (i in  which(sapply(df, is.numeric))) {
  print(names(df)[i])
  print(summary(aov(df[,i]~ df[,17], df)))
  cat('\n')
  
}

#This library gives the exact correlation %
library(ltm)
for (i in  which(sapply(df, is.numeric))) {
  print(names(df)[i])
  print(biserial.cor(df[,i], df$output))
  cat('\n')
  
}

#Conclusion:
# 1: All independent variables are not correlated except 'previous' and 'pdays' with 45%. That is acceptable. 
# 2: 'previous' and 'campaign' are not correlated with each other because in the 'previous' column around 36000 customer are '0', as they have contacted first time for this campaign. 
# 3: we can drop 'previous' as it is suspicious in point 1 and point 2 


############# Correlation between catagorical variable and target variable

for (i in  which(sapply(df, is.factor))) {
  print(names(df)[i])
  print(chisq.test(df[,i], df[,17]))
  cat('\n')
  
}

#We will get the exact % of correlation
library(lsr)
tmp = data.frame()
for (i in names(df[,sapply(df, is.factor)])) {
  for (j in names(df[,sapply(df, is.factor)])) {
    
    var_names = paste0(i,'-', j)
    
    Correlation = cramersV(df[,i], df[,j])
    
    tmp =rbind(tmp, data.frame(var_names = as.character(var_names),Correlation = as.numeric(Correlation)))  
  }
  tmp =rbind(tmp, data.frame(var_names = as.character(paste0(i,'-', 'Ends here')),Correlation = NA))
  # cat('\n')

}

############################ Divide the data into train and test before applying preprocessing techniques
df = read.csv('usecase-2.csv')
library(caTools)
tmp1 = sample.split(df$output,.8)
train = subset(df, tmp1 == T)
test = subset(df, tmp1==F)
rm(tmp1)

########################################## Outlier analysis
##Train data

################################# Age
outlier = boxplot(train$age)
length(outlier$out)#392

#extracting indexes of outlier
out_index = which(train$age %in% boxplot.stats(train$age)$out)
tmp = subset(train, row.names(train)%in% out_index)
prop.table(table(tmp$output))#only 13% of the outlier customer have accepted the scheme

#Based on business understanding I applied winsorizing technique which will correctly represent the outlier values 
train$age[out_index] = outlier$stats[3]+1.5*(outlier$stats[4]-outlier$stats[2])

##################################balance
outlier = boxplot(train$balance)
length(outlier$out)#3781

hist(train$balance) #95% values are '0', it will include the variance in the data and no discription is given about this variable so plan to drop

###################################campaign
outlier = boxplot(train$campaign)
length(outlier$out)#392

boxplot(train$campaign)
table(train$campaign)

#extracting indexes of outlier
out_index = which(train$campaign %in% boxplot.stats(train$campaign)$out)
tmp = subset(train, row.names(train)%in% out_index)
prop.table(table(tmp$output))#even though contacting them for more than 10 times only 6% of the outlier customer have accepted the scheme.

#Based on the data understanding I selected this standard bench mark value
train$campaign[out_index] = 12

############################### pdays
table(train$pdays)
ggplot(train, aes(pdays)) + geom_histogram(binwidth = 10, bins = 50)#highly skewed variable 

################################ previous
table(train$previous)
ggplot(train, aes(previous)) + geom_histogram(binwidth = 10, bins = 50)#highly skewed variable 


## Test data

################################# Age
outlier = boxplot(test$age)

#extracting indexes of outlier
out_index = which(test$age %in% boxplot.stats(test$age)$out)

#Based on business understanding I applied winsorizing technique which will correctly represent the outlier values 
test$age[out_index] = outlier$stats[3]+1.5*(outlier$stats[4]-outlier$stats[2])

################################# balance
outlier = boxplot(test$balance)
length(outlier$out)#3781

hist(test$balance) #95% values are '0', it will include the variance in the data and no discription is given about this variable so plan to drop

################################## campaign
outlier = boxplot(test$campaign)

boxplot(test$campaign)
table(test$campaign)

#extracting indexes of outlier
out_index = which(test$campaign %in% boxplot.stats(test$campaign)$out)
#Based on the data understanding I selected this standard bench mark value
test$campaign[out_index] = 12


#
train$duration = NULL
train$contact = NULL  #Although it is correlated with target variable but doesn't make sense to add in model, so removed based on data understanding
train$balance = NULL  #unit variance and highly skewed
train$pdays = NULL    #unit variance and highly skewed
train$previous = NULL #unit variance and highly skewed


test$duration = NULL
test$contact = NULL
test$balance = NULL
test$pdays = NULL
test$previous = NULL

#############################  Modelling


############################  XGboost model
library(xgboost)
library(dummies)
library(caret)
require(data.table)
library(randomForest)

#using one hot encoding 
labels <- train$output 
labels = ifelse(labels== 'yes',1,0)
ts_label <- test$output
ts_label = ifelse(ts_label== 'yes',1,0)

new_tr = as.matrix(dummy.data.frame(train[,!(names(train) %in% 'output')]))
new_ts = as.matrix(dummy.data.frame(test[,!(names(test) %in% 'output')]))

#convert factor to numeric 
labels <- as.numeric(as.character(labels))
ts_label <- as.numeric(as.character(ts_label))

#For xgboost, we'll use xgb.DMatrix to convert data table into a matrix (most recommended):
#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr, label = labels) 
dtest <- xgb.DMatrix(data = new_ts, label=ts_label)

set.seed(200)
params <- list(booster = "gbtree", objective = "multi:softmax", num_class = 2, eta = .4, gamma = 2, max_depth = 15, colsample_bytree=.8)

#first default - model training
xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 400, watchlist = list( val1 = dtest,train = dtrain), print.every.n = 10, eval_metric = "merror")

#model prediction
xgbpred <- predict(xgb1,dtest)

xgb1$evaluation_log[xgb1$best_iteration]

#Evaluation metric
confusionMatrix(xgbpred, ts_label, positive = '1')

#view variable importance plot
mat = xgb.importance (feature_names = colnames(new_tr), model = xgb1)
xgb.plot.importance(importance_matrix = mat[c(1:20)]) 

##Conclusion
# Developed the xgboost model with fine tuning. with all the variable excluding duration I'm able get the Accuracy: 90.85%, Sensitivity: 49% Specificity: 96.13%  
# Excluding redundant variable Accuracy: 88%, Sensitivity: 26.02% Specificity: 97%.Th
# This is not the desired result because we need more data to to feed the model.


############################ h2o
require(h2o)
h2o.init()

y = grep('output', names(train))
x = setdiff( seq(names(train)), y)

# Converting data to h2o environment
train.h = as.h2o(train)
test.h = as.h2o(test)

#model building
rf_h = h2o.randomForest(x,y,training_frame = train.h, ntrees = 1000)
prediction = as.data.frame(h2o.predict(rf_h, test.h))

confusionMatrix(prediction$predict, test$output)

#Conclusion
#received good performance compared to xgboost Accuracy: 89.6%, Sensitivity: 73.58%, Specificity: 91.75%.








