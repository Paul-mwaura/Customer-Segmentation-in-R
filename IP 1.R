# Import the dataset.
install.packages("data.table")
library("data.table")

advert <- fread("http://bit.ly/IPAdvertisingData")
head(advert)
View(advert)


# Lets Identify missing data in your dataset 
# by using the function is.na() 
# ---
# 
colSums(is.na(advert))

# Removing these duplicated rows in the dataset or 
# showing these unique items and assigning to a variable unique_items below
# ---
#
unique_items <- advert[!duplicated(advert), ]

# Preview the unique values.
#
head(unique_items)

# Check for duplicated values.
#
dup_items <- advert[duplicated(advert)]

# Preview duplicated items.
#
dup_items

# Check for outliers.
#
numeric2 <- c(advert[, 1:4])
boxplot(numeric2)

# Univariate Analysis.
#
time_spent_on_site <- c(advert$`Daily Time Spent on Site`)
age <- c(advert$`Age`)
area_income <- c(advert$`Area Income`)
net_usage <- c(advert$`Daily Internet Usage`)
clicked_on_ad <- c(advert$`Clicked on Ad`)

# Barplots of Numerical columns.
#
hist(time_spent_on_site)
hist(age)
hist(area_income)
hist(net_usage)
hist(clicked_on_ad)

# Factor for the categorical Variables.
# ----
#
topic <- factor(c(advert$`Ad Topic Line`))
city <- factor(c(advert$City))
country <- factor(c(advert$Country))

summary(topic)
summary(city)
summary(Country)

# Bivariate Analysis.
#

# Determine the covariance
# --- 
# time_spent_on_site and clicked_on_ad
#
cov(time_spent_on_site, clicked_on_ad)

# --- 
# age and clicked_on_ad
#
cov(age, clicked_on_ad)

# --- 
# area_income and clicked_on_ad
#
cov(area_income, clicked_on_ad)

# --- 
# net_usage and clicked_on_ad
#
cov(net_usage, clicked_on_ad)


# Determine the correlation
# --- 
# time_spent_on_site and clicked_on_ad
#
cor(time_spent_on_site, clicked_on_ad)

# --- 
# age and clicked_on_ad
#
cor(age, clicked_on_ad)

# --- 
# area_income and clicked_on_ad
#
cor(area_income, clicked_on_ad)

# --- 
# net_usage and clicked_on_ad
#
cor(net_usage, clicked_on_ad)



# Bivariate data visualization
# ----
# 
# Scatterplot - time_spent_on_site
#

plot(clicked_on_ad, time_spent_on_site, xlab = "Clicked Ad", ylab = "Time spent on the site")


# Scatterplot - Age
#

plot(clicked_on_ad, age, xlab = "Clicked Ad", ylab = "Age")

# Scatterplot - area_income
#

plot(clicked_on_ad, area_income, xlab = "Clicked Ad", ylab = "Area Income")


# Scatterplot - net-usage
#

plot(clicked_on_ad, net_usage, xlab = "Clicked Ad", ylab = "Net Usage")


# Visualizing Relationships between Numerical Variables.
# ----
#
install.packages("devtools")
library(devtools)
install_github("ggobi/ggally")
library(ggpairs)
ggpairs(swiss, 
        lower=list(continuous="smooth", wrap=c(colour="blue")),
        diag=list(continuous="bar", wrap=c(colour="blue")))

num_df <- advert[, c("Age", "Area Income", "Daily Internet Usage")]
ggpairs(num_df)

# Visualizing Relationships between Categorical Variables.
# ----
#
cat_df <- advert[, c("Ad Topic Line", "City", "Country")]
ggpairs(cat_df,  cardinality_threshold = 1000)

### Label Encoding Categorical varibles.
atl <- as.numeric(factor(advert$`Ad Topic Line`))
city < as.numeric(factor(advert$`City`))
country <- as.numeric(factor(advert$`Country`))

# Merge the Categorica dataframes to the numeric dataframe.

df <- cbind(num_df,atl, city, country, clicked_on_ad)

df <- na.omit(df)

head(df)

# Implementing the Solution.
## Decision Trees
install.packages(("party"))
install.packages("caret")
install.packages("dplyr")
install.packages(("partykit"))

library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)
library(caret)

set.seed(120)
m <- rpart(clicked_on_ad ~ ., data = df,
           method = "class")

rpart.plot(m)

p <- predict(m, df, type = "class")
table(p, df$clicked_on_ad)
## We identified 488 people would click on the ads while 500 would not. 
# The program also misclassified 12 people would not click on the Ads but in reality the did.


## KNN Classifier.

# We define a normal function which will normalize the set of values according to its minimum value and maximum value.
normal <- function(x) (
  return( ((x - min(x)) /(max(x)-min(x))) )
)
normal(1:5)
knn_df <- as.data.frame(lapply(df[,-5], normal))
summary(knn_df)

# Lets now create test and train data sets

train <- df[1:130,]
test <- df[131:150,]
train_sp <- df[1:130,6]
test_sp <- df[131:150,6]


library(class)    
require(class)
cl= train_sp[,1,drop=True]

model <- knn(train= train,test=test, cl,k=13)
table(factor(model))
table(test_sp,model)


## Naive Bayes.
head(df)

# Splitting data into training and test data sets
# ---
# 
indxTrain <- createDataPartition(y = df$clicked_on_ad,p = 0.75,list = FALSE)
training <- df[indxTrain,]
testing <- df[-indxTrain,]

# Checking dimensions of the split
# ---
#
prop.table(table(df$clicked_on_ad)) * 100
prop.table(table(training$clicked_on_ad)) * 100
prop.table(table(testing$clicked_on_ad)) * 100

# Comparing the outcome of the training and testing phase
# ---
# Creating objects x which holds the predictor variables and y which holds the response variables
# ---
#
x = training[,-7]
y = training$clicked_on_ad

# Loading our inbuilt e1071 package that holds the Naive Bayes function.
# ---
# 
install.packages("e1071")
library(e1071)

# Now building our model 
# ---
# 
model = naiveBayes(y ~., data=df)

class(model)
summary(model)
print(model)
# Model Evalution
# ---
# Predicting our testing set
# 
Predict <- predict(model,newdata = testing )

Predict

# Confusion Matrix
cm = table(df$clicked_on_ad, Predict)
print(cm)


## Regression
head(df)

# Applying the lm() function.
df_lm <- lm(clicked_on_ad ~ ., df)

# Generating the anova table
anova(df_lm)

# Then performing our prediction 
pred2 <- predict(df_lm, df)


# Printing out our result
pred2

# Check for RMSE.
error <- pred2 - df$clicked_on_ad

rmse_xval <- sqrt(mean(error^2)) ## xval RMSE

rmse_xval

# We get an RMSE of 0.0429


## Support Vector Machine.
head(df)

# We check the structure of the dataframe through the function str()
# ---
# 
str(df)

# We are passing FALSE for not returning a list
# ---
# 
install.packages('caret')
library(caret)

indxTrain <- createDataPartition(y = df$clicked_on_ad,p = 0.75,list = FALSE)
training <- df[indxTrain,]
testing <- df[-indxTrain,]

# We check the dimensions of out training dataframe and testing dataframe
# ---
# 
dim(training); 
dim(testing);

# We then clean the data using the anyNA() method that checks for any null values.
# ---
#  
anyNA(df)

# Then check the summary of our data by using the summary() function
# ---
#  
summary(df)

# This should be a categorical variable. To convert these to categorical variables, we need to factorize them.
# The following code will convert the training data frameâs âV14â column to a factor variable.
# ---
# 
training[["clicked_on_ad"]] = factor(training[["clicked_on_ad"]])

# We are using setting number=10 and repeats =3
# ---
# 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(clicked_on_ad ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# We can then check the result of our train() model as shown below
# ---
# 
svm_Linear

# We can use the predict() method for predicting results as shown below. 
# We pass 2 arguements, our trained model and our testing data frame.
# ---
# 
test_pred <- predict(svm_Linear, newdata = testing)
test_pred

# Now checking for our accuracy of our model by using a confusion matrix 
# ---
# 
confusionMatrix(table(test_pred, testing$clicked_on_ad))

# We et an accuracy of 88.4% Using the Support Vector Machine Algorithm.