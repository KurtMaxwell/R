## Predict sale price a property ##
## Using GAM and LASSO ##

## Install pacakges ##
install.packages('colorspace')
install.packages('lubridate')
install.packages("glmnet")
install.packages("mlbench")
install.packages("psych")
install.packages("tidytext")


## Run Libaries ##
library(caret)
library(glmnet)
library(mlbench)
library(psych)
library(tidytext)
library(dplyr)
library(mgcv)


## Read in data ##
train <- read.csv("C:/Users/kkusterer/Documents/MBD Semester 2/Statitical and Machine learning/Group_project2/Data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("C:/Users/kkusterer/Documents/MBD Semester 2/Statitical and Machine learning/Group_project2/Data/test.csv", stringsAsFactors = FALSE)


##Explore the data ##
dim(train)
summary(train)
str(train)

##Check for NA##
sum(is.na(train))/(nrow(train)*ncol(train)) # percentage data missing over the whole table = 0.0588
sum(is.na(test))/(nrow(test)*ncol(test)) # Percentage data missing over the whole table = 0.0599

##Divide the data into catergorical and into Numerical ##
## Check the number of columns related to each catergory ##
sum(sapply(train[,1:81], typeof) == "character") ## = 43
sum(sapply(train[,1:81], typeof) == "integer") ## = 38



#removing the IDs #
# In this way our test data reatin the labesl for our predictions ##
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA


### Split the catergorical and Numerical variables into seperate data sets ###

#Subset of numeric Variables ###
numericVars <- which(sapply(train, is.numeric)) # Indexing Vector for numeric variables
numericVarsName <- names(numericVars) # Save variable names 
cat('There are', length(numericVars), 'numeric variables')
numericVars


## Subset of Catergorical Variables ###
library("tangram")
categoricalVars <- which(sapply(train, is.character)) #Indexing vector for catergorical variables
categoricalVarNames <- names(categoricalVars) #saving names vector for use later on
cat('There are', length(categoricalVars), 'categorical variables')
categoricalVarNames



############### Find the variables most relevant to the dependant variable #######################

subset_catVars <- train[categoricalVars] # Create subset for CatVArs

## Function to find the most relevant variables ##
library("lsr")
corrcat <- function(x){
  treatment <- factor(as.numeric(factor(x)))
  outcome<-train$SalePrice
  anova <- aov(outcome ~ treatment)
  return(etaSquared(anova)[1])
}

#We apply our newly created function and select the top 10 variables
corr_categoricals <- sapply(subset_catVars,corrcat)
head(sort(corr_categoricals, decreasing = TRUE),10)
cat_selected <- sort(corr_categoricals, decreasing = TRUE)[0:10]


### Find the NA's in the selected features ##

for (col in names(train)){
  if (sum(is.na(train[,col] > 0))){
    print(paste(col,"has",sum(is.na(train[,col])),"number of missing values"))  
  }
}

## Replace NA's with instances of None as, It is fair to assume that if Qual/Finish/Type VAR is NA
## along with the Alley variable, then we can assume that the house does not have Basement/Pool/Alley/Garage



###############################Select a subset with all the numeric variables ######################

subset_numeric <- train[numericVars]
subset_numeric[is.na(subset_numeric)] <- 0

## Select the best Numerical Variables ##
cor_numVar <- cor(subset_numeric, use="pairwise.complete.obs") #correlations of all numeric variables


#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

#select only strong corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]


# Select the variables from the train dataset
selected_var <- c(names(cat_selected),colnames(cor_numVar))
train_subset <- train[selected_var]
test_subset <- test[selected_var]



############# Data preprocessing #############

#Checking that all variables are in fact Categorical
for (i in names(train_subset)){
  name <- names(train_subset[i])
  print (name)
  print(class(train_subset[, name]))
}
#For Test

for (i in names(test_subset)){
  name <- names(test_subset[i])
  print (name)
  print(class(test_subset[, name]))
}


#Replacing NA for catergorical cols in train_subset

for (col in list('Neighborhood','ExterQual','KitchenQual','BsmtQual','PoolQC','Alley','GarageFinish',
                 'Foundation','GarageType','HeatingQC')){
  
            train_subset[is.na(train_subset[,col]),col] <- "None"
            test_subset[is.na(test_subset[,col]),col] <- "None"
}

#Replacing NA for numerical cols in train_subset 

for (col in list('SalePrice','OverallQual','GrLivArea','GarageCars','GarageArea','TotalBsmtSF',
                 'X1stFlrSF','FullBath','TotRmsAbvGrd','YearBuilt','YearRemodAdd')){
            
            train_subset[is.na(train_subset[,col]),col] <- 0
            test_subset[is.na(test_subset[,col]),col] <- 0
}

# Check dataset for NA's 

sum(is.na(train_subset)) # 0 
sum(is.na(test_subset)) # 0


################## Dummy Encode Variables #########################   
#Train Set 
dmy <- dummyVars(" ~ .", data =train_subset)
train_dummy <- data.frame(predict(dmy, newdata = train_subset))
str(train_dummy) 
#Test set
test_dummy <- data.frame(predict(dmy, newdata = test_subset))
str(test_dummy) 


# Standardize all numerical variables in the basetable 
for (col in list('OverallQual','GrLivArea','GarageCars','GarageArea','TotalBsmtSF',
                 'X1stFlrSF','FullBath','TotRmsAbvGrd','YearBuilt','YearRemodAdd','SalePrice')){

          train_dummy[,col] <- scale(train_dummy[,col], center=T, scale=T)# sd = 1, mean = 0
}
str(train_dummy)


## Run for test ##
for (col in list('OverallQual','GrLivArea','GarageCars','GarageArea','TotalBsmtSF',
                 'X1stFlrSF','FullBath','TotRmsAbvGrd','YearBuilt','YearRemodAdd')){

  test_dummy[,col] <- scale(test_dummy[,col], center=T, scale=T) # sd = 1, mean = 0
}
str(test_dummy)

# Select statistically siginificant variables based on a linear regression summary

train_base <- select(train_dummy, NeighborhoodBlmngtn,NeighborhoodBlueste,NeighborhoodBrDale,NeighborhoodBrkSide,
                    NeighborhoodCollgCr, NeighborhoodEdwards,NeighborhoodGilbert,NeighborhoodIDOTRR, NeighborhoodMeadowV,
                    NeighborhoodMitchel,NeighborhoodNAmes,NeighborhoodNoRidge,NeighborhoodNPkVill,
                    NeighborhoodNWAmes,NeighborhoodOldTown,NeighborhoodSawyer,NeighborhoodSawyerW,
                    NeighborhoodStoneBr, NeighborhoodSWISU,ExterQualEx, KitchenQualEx,KitchenQualGd,BsmtQualEx,PoolQCEx,PoolQCGd,GarageFinishFin,
                    GarageType2Types,GarageTypeAttchd, OverallQual,GrLivArea,GarageCars,TotalBsmtSF,YearRemodAdd,SalePrice)

test_base <- select(test_dummy, NeighborhoodBlmngtn,NeighborhoodBlueste,NeighborhoodBrDale,NeighborhoodBrkSide,
                     NeighborhoodCollgCr, NeighborhoodEdwards,NeighborhoodGilbert,NeighborhoodIDOTRR, NeighborhoodMeadowV,
                     NeighborhoodMitchel,NeighborhoodNAmes,NeighborhoodNoRidge,NeighborhoodNPkVill,
                     NeighborhoodNWAmes,NeighborhoodOldTown,NeighborhoodSawyer,NeighborhoodSawyerW,
                     NeighborhoodStoneBr, NeighborhoodSWISU,ExterQualEx, KitchenQualEx,KitchenQualGd,BsmtQualEx,PoolQCEx,PoolQCGd,GarageFinishFin,
                     GarageType2Types,GarageTypeAttchd, OverallQual,GrLivArea,GarageCars,TotalBsmtSF,YearRemodAdd,SalePrice)


#Check classes of VAR in train_base
for (i in names(train_base)){
  name <- names(train_base[i])
  print (name)
  print(class(train_base[, name]))
}


for (i in names(test_base)){
  name <- names(test_base[i])
  print (name)
  print(class(test_base[, name]))
}

# Change class off some of the variables to numeric from matrix
for (col in list('OverallQual','GrLivArea','GarageCars','TotalBsmtSF','YearRemodAdd')){
  
            train_base[,col]<- as.numeric(train_base[,col])
            test_base[,col]<-as.numeric(test_base[,col])
}

str(train_base)
str(test_base)

### Shows us that Sales is left skewed ###
ggplot(train_base, aes(x = SalePrice)) + geom_density() 
ggplot(test_base, aes(x = SalePrice)) + geom_density() # Sales rice is Zero here thus we cannot get a plot 

?barplot
###### Running linear/Ridge/Lasso Regression ###########

# We can set custom controls using the caret package 
# the method is trainControl ##
custom <- trainControl(method = "repeatedCV",
                       number = 15,
                       repeats = 5,
                       verboseIter = T)

### Run a simple linear regression##
set.seed(123)
lm <- train(SalePrice ~ .,
            train_base,
            method = 'lm',
            trControl = custom)

# Run on test 
test_lm <- predict(lm, new = test_base)


# Check results on train 
lm$results
summary(lm)
plot(lm$finalModel)

# Check reults on test 
predicted_price_lm <- as.data.frame(test_lm)

### Ridge regression model ###
set.seed(1234)
ridge <- train(SalePrice ~ .,
               train_base,
               method = 'glmnet',
               tuneGrid= expand.grid(alpha = 0,
                                     lambda = seq(0.0001, 1, length = 5)),
               trControl = custom)

### Run test on Ridge ###
test_ridge <- predict(ridge, new = test_base)
predict_price_ridge <- as.data.frame(test_ridge)


#Checking ridge stats 
ridge$results
summary(ridge)

###plot results ###
plot(ridge)
ridge
plot(ridge$finalModel, xavr= 'lambda', label = T)
plot(ridge$finalModel, xvar= 'dev', label = T)
plot(varImp(ridge, scale = F)) #### We can say that there are too many variables in use ###


### Lasso Regression ###
lasso <- train(SalePrice ~ .,
               train_base,
               method = 'glmnet',
               tuneGrid= expand.grid(alpha = 1,
                                     lambda = seq(0.0001, 1, length = 5)),
               trControl = custom)




# run on test
test_lasso <- predict(lasso, new = test_base)
predict_price_lasso <- as.data.frame(test_lasso)


# Check resluts of lasso
lasso$results
summary(lasso)

####Plot lasso ###

plot(lasso)
plot(lasso$finalModel, xavr= 'lambda', label = T)
plot(lasso$finalModel, xvar= 'dev', label = T)
plot(varImp(lasso, scale = F))


### Running GAM Generalised Additive Models ###

gam_mod <- gam(SalePrice ~ s(OverallQual, bs = 'cr'), data = train_base)

## Running the model on test ##
# Running model with a  cubic regression on the variable in splice ##
gam_mod_test0 <- gam(SalePrice ~ s(OverallQual, bs = 'cr'), data = test_base)

# Running model with a  cubic regression on the variable in splice ##
gam_mod_test1 <- gam(SalePrice ~ s(GrLivArea, bs = 'cr' ), data = test_base) 
# Runing a GAM with a linear fit ## 
## Note : Splice is used wihen the relationship between IV and DV are non-linear ##
gam_mod_test2_linear <- gam(SalePrice ~ OverallQual + GrLivArea + GarageCars, data = test_base)
summary(gam_mod_test2_linear)

##Gam with splice added to each IV selected ###
## the default smoother for s is bs = 'tp'
gam_mod_test3_Splice <- gam(SalePrice ~ s(OverallQual) + s(GrLivArea) + s(GarageCars), data = test_base)

## Visualize
plot(gam_mod_test1, residuals = TRUE, pch = 1)
## Based on the graph I believe we are investigating a linear relationship

summary(gam_mod_test1)



gam_mod_test_df <- as.data.frame(gam_mod_test) ## doesnt work 

class(gam_mod_test)

## Plot GAM with smoothing of OverallQual variable ##

plot(gam_mod_test, residuals = TRUE, pch = 1)

gam_mod$var.summary
gam_mod$family

# For tesy stats 
gam_mod_test$coefficients


## Comparing testing using Anova ##

anova(gam_mod_test0,gam_mod_test1 , test = "Chisq")

## Inspection of Vars ##
summary(train$KitchenQual)
summary(train$SalePrice)





