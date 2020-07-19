# Install packages.
#
install.packages("tidyverse")
install.packages("modelr")
install.packages("broom")

# Load Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

# Load data and preview the first 6 rows.
#
shop <- read.csv("D:/Moringa/Class/R/IP/IP Week 13/online_shoppers_intention.csv")

head(shop)
view(shop)



# Before you start data cleaning, you need to understand the structure of the data.
class(shop)



# We can see the dimensions of the weather dataframe using the dim() function.
dim(shop)



# We can see the dataframe column names using names() function.
names(shop)



# It will show a  summary of the object's internal structure.
str(shop)



# The glimpse() function shows the same information as str()
glimpse(shop)



# Check the top rows.
head(shop)



# Check Bottom rows of the dataframe.
tail(shop)



## Univariate Analysis
### Data Visualization


#### Histogram Plots of numeric variables.
# Administrative.
admin <- c(shop$Administrative)
hist(admin)



# Administrative Duration
admin_dur <- c(shop$Administrative_Duration)
hist(admin_dur)



# Information.
inform <- c(shop$Informational)
hist(inform)


# Information Duration.
inform_dur <- c(shop$Informational_Duration)
hist(inform_dur)



# Product Related.
prodrel <- c(shop$ProductRelated)
hist(prodrel)


# Product Duration
prodrel_dur <- c(shop$ProductRelated_Duration)
hist(prodrel_dur)

# Bounce Rates
Brates <- c(shop$BounceRates)
hist(Brates)


# Exit Rates
Erates <- c(shop$ExitRates)
hist(Erates)


Pvalues <- c(shop$PageValues)
hist(Pvalues)


sday <- c(shop$SpecialDay)
hist(sday)


region <- c(shop$Region, breaks=5)
hist(region)


## Bivariate Analysis.
# Administrative Against Region

# Plot Bounce Rates against Exit Rates

plot(Brates, Erates)




# Plot Month against Revenue
revenue <- c(shop$Revenue)
month <- c(shop$Month)

plot(month, revenue)




# Plot Month against Revenue
revenue <- c(shop$Revenue)
weekend <- c(shop$Weekend)

plot(weekend, revenue)



## Tidying data
# Find Null values
sum(is.na(shop))



# Check for Null values.
which(is.na(shop))



# Check for the data summary.
summary(shop)



### Dealing with missing values
# The complete.cases() function is used to see which rows have no missing values.
complete.cases(shop)




# Keeping only the complete cases.
# ----

shop[complete.cases(shop), ]



# Numerical data
num <- select_if(shop, is.numeric)
num


# Categorical data
cat <- select_if(shop, is.factor)
cat




## Checking and Dealing with Outliers
boxplot(num, horizontal=FALSE)



# We have created the following table below, and want to obtain the sum of Variables When the Revenue was either True or False.

sumif_table<-aggregate(. ~ Revenue, data=shop, sum)
sumif_table



# Filter where Revenue is True
Revenue <- shop %>% filter(Revenue=="TRUE")

Revenue



# Filter where Revenue is False
F_Revenue <- shop %>% filter(Revenue=="FALSE")

F_Revenue



### Label Encoding Categorical varibles.
month <- as.numeric(month)
revenue < as.numeric(revenue)
visitor <- as.numeric(c(shop$VisitorType))

# Merge the Categorica dataframes to the numeric dataframe.

df <- cbind(num, month, visitor, revenue)

head(df)

## Implementing the Solution

### K-Means Clustering

df <- na.omit(df)
head(df)

# Normalizing the dataset so that no particular attribute 
# has more impact on clustering algorithm than others.
# ---
# 
# we start by scaling the data using the R function scale() as follows
# ---
# 
df <- scale(df)
head(df)     

result <- kmeans(df, 4)
# Previewing the no. of records in each cluster
# 
result$size 

# Getting the value of cluster center datapoint value(4 centers for k=4)
# ---
# 
result$centers 

# Getting the cluster vector that shows the cluster where each record falls
# ---
# 
result$cluster

# The graph shows that we have got 4 clearly distinguishable clusters.

# Visualizing the  clustering results
# ---
# 
par(mfrow = c(12,13), mar = c(5,4,2,2))

# Plotting to see how Ozone and Solar.R data points have been distributed in clusters
# ---
#
plot(df[,1:10], col = result$cluster) 

# Verifying the results of clustering
# ---
# 
par(mfrow = c(2,2), mar = c(5,4,2,2))

# Plotting to see how Administrative and Administrative Duration data points have been distributed in clusters
plot(df[c(8,10)], col = result$cluster)

# Plotting to see how Revenue and Region data points have been distributed 
# originally as per "class" attribute in dataset
# ---
#


plot(df[c(1,2)], col = revenue)


# ---
# 
plot(df[c(3,4)], col = result$cluster)
plot(df[c(3,4)], col = revenue)

### Hierarchical Clustering.

df <- na.omit(df)

head(df)


# Before hierarchical clustering, we can compute some descriptive statistics
# ---
# 
desc_stats <- data.frame(
  Min = apply(df, 2, min),    # minimum
  Med = apply(df, 2, median), # median
  Mean = apply(df, 2, mean),  # mean
  SD = apply(df, 2, sd),      # Standard deviation
  Max = apply(df, 2, max)     # Maximum
)
desc_stats <- round(desc_stats, 2)
head(desc_stats)


# we start by scaling the data using the R function scale() as follows
# ---
# 
df <- scale(df)
head(df)



# First we use the dist() function to compute the Euclidean distance between observations, 
# d will be the first argument in the hclust() function dissimilarity matrix
# ---
#
d <- dist(df, method = "euclidean")

# We then hierarchical clustering using the Ward's method
# ---
# 
res.hc <- hclust(d, method = "ward.D2" )


# Plotting the Dendogram.

# Lastly, we plot the obtained dendrogram
# ---
# 
plot(res.hc, cex = 0.7, hang = -1)


# Average
# 
res.hc <- hclust(d, method = "average" )

# Lastly, we plot the obtained dendrogram
# ---
# 
plot(res.hc, cex = 0.7, hang = -1)

# 


# Median

res.hc <- hclust(d, method = "median" )

# Lastly, we plot the obtained dendrogram
# ---
# 
plot(res.hc, cex = 0.7, hang = -1)





### DBSCAN

# Importing the required package
# ---
# 
install.packages("dbscan")



# Loading the required library
# ---
# 
library("dbscan")


na.omit(df)

db <- dbscan(df, eps=0.9, MinPts=5)

# Printing out the clustering results
# ---
# 
print(db)

# With an eps of 0.9 and Min Points as 5 we get 7000 values as noise points.

# We also plot our clusters as shown
# ---
# The dataset and cluster method of dbscan is used to plot the clusters.
# 
na.omit(df)
hullplot(df,db$cluster)

