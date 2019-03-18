## group members : Chandrakanth Yela, Aravind Reddy Keesara, Harshavardhan Ganti
setwd("C:/Users/Chandrakanth/Desktop/Canvas/Semester-3/KDD/Project_Ross")
library(data.table)
library(ggplot2)
library(randomForest)

store <- read.csv("store.csv", header=TRUE) # Reading store.csv files
train <- read.csv("train.csv", header=TRUE) # Reading train.csv files
test <- read.csv("test.csv", header=TRUE)   # Reading test.csv files

train <- merge(train,store)                 # Merging the train with store            
test <- merge(test,store)                   # Merging the test with store 

names(train)
str(train)
summary(train)
head(train)

names(test)
str(test)
summary(test)

# Converting Date Attribute into Date Format for further analysis
i <- sapply(train, is.factor)                # =====train data==========#
train[i] <- lapply(train[i], as.character)   # converting factors into string
train$Date <- as.Date(train$Date)            # converting into date format for the column "date"

j <- sapply(test, is.factor)                 # =====test data==========#
test[j] <- lapply(test[j], as.character)     # converting factors into string
test$Date <- as.Date(test$Date)              # converting into date format for the column "date"


# checking for outliers using boxplot and distribution by hist
boxplot(train$Sales)
hist(train$Sales)                           

sapply(X = train, FUN = function(x) sum(is.na(x)))    # checking null values 
sapply(X = test, FUN = function(x) sum(is.na(x)))

test[is.na(test$Open), ]                              # test$open has 11 null values
test$Open[test$Store == 622]

train[is.na(train)] <- 0                              #updating the null values to 1
test[is.na(test)] <- 0

train <- train[order(as.Date(train$Date)),]           # Arranging the data by date 
test <- test[order(as.Date(test$Date)),]

#========================================================================================#
######################################## EDA analysis ####################################

# Analysis of attributes for further data preparation and transformation
sum(unique(test$Store) %in% unique(train$Store))
# Count of Unique values available in the data sets 856 unique in common with both       
sum(!(unique(train$Store) %in% unique(test$Store)))
# 259 not unique in both 

table(train$Open) / nrow(train) 
# ratio of open attribute in the train of the total train , 16.98933 are closed and 83.01067 are open             

table(test$Open) / nrow(test)            
# ratio of open attribute in the test dataset,of the total train , 14.59063 are closed and 85.40937 are open

table(train$Promo) / nrow(train) 
# available promos are 38.15145 and no promos 61.84855
table(test$Promo) / nrow(test) 
# available promos are 39.58333 and no promosare 60.41667

table(train$StateHoliday) / nrow(train)    
table(test$StateHoliday) / nrow(test) 
# a = public holiday, b = Easter holiday, c = Christmas, 0 = None
# For train data, 0:0.969475300 a:0.019917244 b:0.006576820 c:0.004030637
# For test data, 0:0.995619159 a:0.004380841


table(train$SchoolHoliday) / nrow(train)
# No holiday=0.8213533 Yes holiday= 0.1786467 

table(test$SchoolHoliday) / nrow(test)
# No holiday=0.5565129  Yes holiday=  0.4434871

table(train$StoreType)/nrow(train)
#a          b          c          d 
#0.54229465 0.01556219 0.13452496 0.30761820 

table(train$Assortment)/nrow(train)
#a           b           c 
#0.528352580 0.008153683 0.463493736 

# checking for outliers
boxplot(train$Sales)
#Outliers are described as values which are far away from the mean. 
#In this case some values are equally distributed from mean till a particular value. So we dont neglect them.
hist(train$Sales)

#Ploting the Sales for each Stores. Overall sales for each store is good, as u can see from the plot 
# it's difficult to observe any pattern, so lets take inti account other parameters
ggplot(train,aes(Store, Sales)) + geom_point()


# Ploting the no of customers for each day of week
# Average number of customers visitng the stiores on "sunday" is more than the week days
#But it is observed that sales are very less on "sunday"
# This means that even customers buy porducts on sunday, their value of sales may be low
ggplot(train,aes(DayOfWeek, Customers)) + geom_line(color="red")+geom_point()

#Promotions based on day of the week
# There are almost no promotions on "sunday". Where as promotions are given on weekdays to boost the sales.
#This might be contributing factor as to why the sales are less on "sunday" eventhough avg. of customers is high on that day

train$DayOfWeek <- as.factor(train$DayOfWeek)
boxplot(Sales ~ DayOfWeek, data = train, xlab="Day of week", ylab="Sales")

#boxplot for sales vs StateHoliday
boxplot(Sales ~ StateHoliday, data = train, xlab="Holidays", ylab="Sales")
# plot shows that sales are very low where there is a stateHoliday compared with no stateHoliday
# This clearly indicates that holidays affect sales.

##########################################################################################


#Understanding from the analysis above, below are the steps for filtering the data that helps us to build the model for the prediction
#Focusing on stores that are open
train <- train[ which(train$Open=='1'),]      
#test<- test[ which(test$Open=='1'),]
head(train)

#seperating out the elements of the date column for the train set
train$month <- format(as.Date(train$Date), "%m")
train$year <- format(as.Date(train$Date), "%y")
train$day <- format(as.Date(train$Date), "%d")

test$month <- format(as.Date(test$Date), "%m")
test$year <- format(as.Date(test$Date), "%y")
test$day <- format(as.Date(test$Date), "%d")

# Removing unwanted attributes store, dayOfWeek,Date,Customers, Open, CompetitionOpenSinceYear,PromoInterval
head(train[,-c(1,2,3,5,6,14,18)])
train<-train[,-c(1,2,3,5,6,14,18)]
head(train)
head(test)

# Removing unwanted attributes from test data set store 1,id 2,DayOFWeek,Date 4, CompetitionOpenSinceYear 13,PromoInterval 17
head(test[,-c(1,2,3,4,5,13,17)])
test_new<-test[,-c(1,2,3,4,5,13,17)]

# convetring StoreType Assortment into numerical
attribute.names <- names(train)[c(3,5,6)]
attribute.names <- c("StateHoliday","StoreType","Assortment")
for (f in attribute.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test_new[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test_new[[f]]  <- as.integer(factor(test_new[[f]],  levels=levels))
  }
}
head(train)

length(unique(train$Store))

train$day <- as.numeric(as.character(train$day))
train$month <- as.numeric(as.character(train$month))
train$year <- as.numeric(as.character(train$year))

test_new$day <- as.numeric(as.character(test_new$day))
test_new$month <- as.numeric(as.character(test_new$month))
test_new$year <- as.numeric(as.character(test_new$year))

#splitting train data into train 70% and test 30% 
dt = sort(sample(nrow(train), nrow(train)*.7))
train_pt1<-train[dt,]
test_pt1<-train[-dt,]
head(train_pt1)

#================================= Model ===========================

#===================Applying Random forest model =====================
randomf_model <- randomForest(train_pt1[,-c(1)], 
                              train_pt1$Sales,
                              mtry=5,
                              ntree=25,
                              sampsize=100000,
                              do.trace=TRUE)

randomf_model
print(randomf_model)
importance(randomf_model)
importance(randomf_model, type = 1)
importance(randomf_model, type = 2)
plot(randomf_model)
plot(importance(randomf_model), lty=2, pch=16)

pred <- predict(randomf_model, test_pt1[,-c(1)])
head(pred)


#===================Applying Linear Regression model =====================
linearR_model = lm(Sales~.,data=train_pt1)
summary(linearR_model)


sales_lr_pred = predict(linearR_model, newdata = test_pt1[,-c(1)])
write_csv(sales_lr_pred, "LR_out.csv")

#==========RMSE Function for calculating root mean square value
RMSE = function(pred, tes){
  sqrt(mean((pred - tes)^2))
}

#====== RMSE for Randomf_model prediction
RMSE(pred,test_pt1$Sales)
#====== RMSE for linearR_model prediction
RMSE(sales_lr_pred,test_pt1$Sales)

df_out <- test_pt1
df_out$Linear_Prediction <- sales_lr_pred
df_out$RandomFor_Prediction <- pred

write_csv(df_out, "result_output.csv")

#======================kaggle submission=================================
pred_kag_sub<-predict(randomf_model, test_new)
result<-data.frame(Id=test$Id,Sales=pred_kag_sub)
write_csv(result, "sub_kag.csv")

