library(dplyr)
library(ggplot2)
library(caret)
install.packages("caretEnsemble")
library(caretEnsemble)
install.packages("VIM")
library(VIM)
library(gridExtra)


stores.df <- read.csv(paste("C:/Users/Muthu Govindhan/Desktop/bigmart/Train.csv", sep = ""))
View(stores.df)
attach(stores.df)
dim(stores.df)
summary(Item_Identifier)
head(stores.df)


#Transforming "low fat" and "LF" to "Low Fat"
index <- which(stores.df$Item_Fat_Content == "LF" | 
                 stores.df$Item_Fat_Content == "low fat")

stores.df[index, "Item_Fat_Content"] <- "Low Fat"

#Transforming "reg" to "Regular
index2 <- which(stores.df$Item_Fat_Content == "reg")

stores.df[index2, "Item_Fat_Content"] <- "Regular"

#Dropping Unused Levels
stores.df$Item_Fat_Content <- factor(stores.df$Item_Fat_Content)

#Using kNN imputation for missing values
big_mart_imputed <- kNN(stores.df)
big_mart_imputed <- big_mart_imputed %>% 
  select(Item_Identifier:Item_Outlet_Sales)

summary(big_mart_imputed)

table(big_mart_imputed$Outlet_Identifier, big_mart_imputed$Outlet_Size)
table(big_mart_imputed$Outlet_Identifier, big_mart_imputed$Outlet_Type)
table(stores.df$Outlet_Type, big_mart_imputed$Outlet_Size)

#Imputing Small for OUT010 Location
index3 <- which(big_mart_imputed$Outlet_Identifier == "OUT010")
big_mart_imputed[index3, "Outlet_Size"] <- "Small"
#Imputing Small for OUT017 Location
index4 <- which(big_mart_imputed$Outlet_Identifier == "OUT017")
big_mart_imputed[index4, "Outlet_Size"] <- "Small"
#Imputing Medium for OUT045 Location
index5 <- which(big_mart_imputed$Outlet_Identifier == "OUT045")
big_mart_imputed[index5, "Outlet_Size"] <- "Medium"
#Dropping Unused Levels for Outlet Size Variable
big_mart_imputed$Outlet_Size <- factor(big_mart_imputed$Outlet_Size)
summary(big_mart_imputed)

ggplot(big_mart_imputed, aes(x=Item_Outlet_Sales)) +
  geom_histogram(binwidth = 200) +
  labs(title = "Item Outlet Sales Histogram", 
       x = "Item Outlet Sales")
#Item Outlet Sales Histogram by Outlet Identifier
ggplot(big_mart_imputed, aes(x=Item_Outlet_Sales, 
                             fill = Outlet_Identifier)) +
  geom_histogram(binwidth = 200) +
  facet_wrap(~ Outlet_Identifier) +
  labs(title = "Item Outlet Sales Histogram", 
       x = "Item Outlet Sales")
#Sales by Outlet Identifier
ggplot(big_mart_imputed, aes(x = Outlet_Identifier,
                             y = Item_Outlet_Sales)) +
  geom_boxplot() +
  labs(title = "Sales by Outlet Identifier",
       x = "Outlet Identifier",
       y = "Item Outlet Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Item Outlet Sales by Item MRP and Outlet Identifier

ggplot(big_mart_imputed, aes(x = Item_MRP,
                             y = Item_Outlet_Sales)) +
  geom_bin2d() +
  facet_wrap(~ Outlet_Identifier) +
  labs(title = "Item Outlet Sales by Item MRP and Outlet Identifier",
       x = "Item MRP",
       y = "Item Outlet Sales")

#Median Sales by Location
big_mart_imputed %>%
  group_by(Outlet_Identifier) %>%
  summarize(median_sales = median(Item_Outlet_Sales)) %>%
  arrange(desc(median_sales))

cor(big_mart_imputed$Item_MRP, big_mart_imputed$Item_Outlet_Sales)

#Preparing Data For Machine Learning
big_mart_sub <- big_mart_imputed %>%
  select(-Item_Identifier, -Outlet_Identifier)

set.seed(366284)
inTrain <- createDataPartition(y = big_mart_sub$Item_Outlet_Sales, 
                               p = 0.7, list=FALSE)
train <- big_mart_sub[inTrain, ]
test <- big_mart_sub[-inTrain, ]

 

results <- resamples(models)
summary(results)


#GLMNET Ensemble
stack_glmnet <- caretStack(models, method = "glmnet", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE))
stack_glmnet
plot(stack_glmnet)
predictions_glmnet <- predict(stack_glmnet, test)
error <- predictions_glmnet - test$Item_Outlet_Sales
#calculating RMSE
sqrt(mean(error^2))
 #random 
stack_rf <- caretStack(models, method = "ranger", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE))
stack_rf
plot(stack_rf)
predictions_rf <- predict(stack_rf, test)  
error <- predictions_rf - test$Item_Outlet_Sales
#calculating RMSE
sqrt(mean(error^2))
 
#bagEarth

stack_bag <- caretStack(models, method = "bagEarth", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE))
stack_bag
plot(stack_bag)
predictions_bag <- predict(stack_bag, test)
error <- predictions_bag - test$Item_Outlet_Sales
sqrt(mean(error^2))


#conjoint analysis
Ross<-read.csv("C:/Users/Muthu Govindhan/Desktop/bigmart/Train.csv")#monthly data after counted
summary(Ross)
Ross<-Ross[Ross$Item_Outlet_Sales != 0,]
#Ross$CompetitionDistance<-as.integer(Ross[Ross$CompetitionDistance,]$CompetitionDistance)
#Ross<-Ross[!is.na(Ross$CompetitionDistance),]

#Ross$CDfactor<-0
#Ross[Ross$CompetitionDistance<=80000,]$CDfactor<-"d"
#Ross[Ross$CompetitionDistance<=6890,]$CDfactor<-"c"
#Ross[Ross$CompetitionDistance<=2330,]$CDfactor<-"b"
#Ross[Ross$CompetitionDistance<=710,]$CDfactor<-"a"

mylm1<-lm(Ross$Item_Outlet_Sales ~ Ross$Item_Weight ++ Ross$Item_MRP + Ross$Outlet_Identifier + Ross$Outlet_Size +Ross$Outlet_Location_Type + Ross$Outlet_Type,Ross)
#mylm1<-lm(Sales~StoreType+Assortment+as.factor(Sum.P111111111111111111romo.days),Ross)
summary(mylm1)
