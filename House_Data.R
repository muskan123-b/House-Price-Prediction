#Setting the working directory
setwd("C:/Users/muska/Desktop/Minor Poject")

#Importing the dataset
df <- read.csv("C:/Users/muska/Desktop/Minor Poject/HouseData.csv")
View(df)

#Importing Libraries
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(caTools)

#Data Exploration
nrow(df) #No. of Rows
ncol(df) #No. of Columns

head(df)

str(df)

summary(df)

View(data.frame(sapply(df,class))) #To find Data Types of columns

#Checking Null values



#Data Preparation
require(dplyr)
df$date=NULL
df<-dplyr::mutate(df, date<-NULL, street = NULL, country = NULL, statezip = NULL, city = NULL, waterfront=NULL)

df$age_house <- as.integer(format(Sys.Date(), "%Y")) - df$yr_built #Finding the age of house
df$yr_built<-NULL
df$yr_renovated <- ifelse(df$yr_renovated > 0, 1, 0)
df$sqft_basement<- ifelse(df$sqft_basement > 0, 1, 0)

#Plotting boxplot to check for outliers
par(mfrow=c(2, 3))
boxplot(df$price, main="Price")
boxplot(df$bedrooms, main="Bedrooms")
boxplot(df$bathrooms, main="Bathrooms")
boxplot(df$sqft_living, main="Sqft_Living")
boxplot(df$sqft_above, main="Sqft_Above")
boxplot(df$age_house, main="Age of house")


plot(df$sqft_living,xlab = "Observation number", ylab = "Price [in $]")

#Outlier treatment
plot(df$price,xlab = "Observation number", ylab = "Price [in $]")
boxplot(df$price)

outlier_treat <- function(x){
   UC = quantile(x, p=0.99,na.rm=T)
   LC = quantile(x, p=0.01,na.rm=T)
   x=ifelse(x>UC,UC, x)
   x=ifelse(x<LC,LC, x)
   return(x)
 }
df = data.frame(apply(df, 2, FUN=outlier_treat)) #Applying Outlier func. to the dataset
plot(df$price,xlab = "Observation number", ylab = "Price [in $]")

#Plotting Correlation Matrix
cor(df)

require(ggcorrplot)
corr <- round(cor(df), 1) 
ggcorrplot(corr,
           type = "lower",
           lab = TRUE, 
           lab_size = 5,  
           colors = c("red", "white", "cyan4"),
           title="Correlogram of Housing Dataset", 
           ggtheme=theme_bw)


#Plotting Histogram
par(mfrow=c(2, 3))
hist(df$price, breaks = 10, col = "coral2", main = "Histogram for price", xlab = "Price")
hist(df$bedrooms, breaks = 5, col = "gold1", main = "Histogram for no. of bedrooms", xlab = "Bedrooms")
hist(df$bathrooms, breaks = 10, col = "light green", main = "Histogram for no. of bathrooms", xlab = "Bathrooms")
hist(df$sqft_living, breaks = 10, col = "sky blue", main = "Histogram for area of living", xlab = "Sqft_living")
hist(df$floors, breaks = 5, col = "orange", main = "Histogram for no. Floors", xlab = "Floors")
hist(df$age_house, breaks = 10, col = "pink", main = "Histogram for Age of house", xlab = "Age")

#Box plot for the Comparison between the basement and price of the house
sqftbase<-ifelse(df$sqft_basement > 0, "Yes", "No")
ggplot(data=df,aes(y=price,x=sqftbase, fill=sqftbase))+geom_boxplot()


#Scatter plot to see the relation between area and price of the house
ggplot(data=df,aes(y=sqft_living,x=price))+geom_point()+geom_smooth(method="lm",se=F)

#Scatter plot to see the relation between no. of bedrooms and price of the house
ggplot(df,aes(x=sqft_living,y=price,col=factor(bedrooms))) +geom_point() +geom_smooth(method="lm",se=F)+ labs(col="Bedrooms")

#Scatter plot to see the relation between condition and price of the house
ggplot(df,aes(x=sqft_living,y=price,col=factor(condition))) +geom_point() +geom_smooth(method="lm",se=F)+ labs(col="Condition")


# Splitting the dataset into the Training set and Test set
require(caTools)
sample.split(df$price, SplitRatio = 0.60)->split_index
training_set <- subset(df,split_index == TRUE)
test_set <- subset(df, split_index == FALSE)
nrow(test_set)
nrow(training_set)

#Model Fitting
lm(price~.-yr_renovated -condition ,data=training_set)->mod1
predict(mod1,test_set)->result
cbind(actual=test_set$price,predicted=result)->compare_result
View(compare_result)
as.data.frame(compare_result)->compare_result
compare_result$actual-compare_result$predicted->error
as.data.frame(error)->error
cbind(compare_result,error)->final
#View(final)
#View(compare_result)
#View(error)
summary(mod1)

df<-df[df$price!=0,]

count(df$price==0)

table(df$price==0)

missing_value_treatment = function(x){
   x[is.na(x)] = mean(x, na.rm=T)
   return(x)
}

nzmean <- function(x) {
   if (all(x==0)) 0 else mean(x[x!=0])
}
apply(df,1,nzmean)

plot(df$sqft_living,df$price)
abline(lm(price~sqft_living,data=df),col='red',lwd=2)
                                                                                                                                        
which(df$price==0)

colMeans(is.na(df))
sapply(df, sum(is.na(df)))


matrix_coef <- summary(lm(price~.-yr_renovated -condition ,data=training_set))$coefficients  # Extract coefficients in matrix
matrix_coef  
my_estimates <- matrix_coef[ , 1]                     # Matrix manipulation to extract estimates
as.data.frame(my_estimates)



ggplot(data=test_set,aes(y=result,x=price))+geom_point(colour='blue')+labs( y='Predicted Values',x='Actual Values')
