# Here is my scratch work for doing the House Price Advanced Regression practice on Kaggle

# First I load the libraries I'll need, and then the data

library(tidyverse)
library(lubridate)

df <- read_csv("/PATH/train.csv")

# I always start by plotting some of the data   just to get a feel for it

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(data = df) +
  geom_histogram(mapping = aes(x = SalePrice))

ggplot(data = df) + 
  geom_bar(mapping = aes(x = HouseStyle))

ggplot(data = df) + 
  geom_bar(mapping = aes(x = OverallCond))

# the following messing around with dates is just to let R understand the dates better so I can see if there seems to be any monthly trend in the sales prices

df$Day <- 01

df <- df %>% 
  mutate(DateSold = make_date(YrSold, MoSold, Day))

head(df$DateSold)

ggplot(data = df) +
  geom_point(mapping = aes(x = DateSold, y = SalePrice))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sales prices seem to be related in some way to the time of the year, but it seems mostly due to the amount of sales increasing during the warmer months, so this might not be crucial to keep in the regression later but we'll see

df$MSZoning.f <- factor(df$MSZoning)
is.factor(df$MSZoning.f)

summary(lm(SalePrice ~ MSZoning.f, df))

df$MoSold.f <- factor(df$MoSold)
is.factor(df$MoSold.f)
summary(lm(SalePrice ~ MoSold.f, df))

# below is just some more fooling around with the variables

summary(lm(SalePrice ~ FullBath + BedroomAbvGr + GrLivArea + factor(CentralAir) + factor(GarageType) + GarageArea + YearBuilt + I(YearBuilt^2) + YearRemodAdd + factor(HouseStyle) + factor(MoSold) + factor(YrSold), df))

unique(df$BedroomAbvGr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now I start building the main model and comparing the models using the Adj R^2, I start with some things I would look for in buying a house, and then start to include some of the more interesting variables the dataset has to offer

summary(lm(SalePrice ~ GrLivArea, df))
# Adjusted R-squared:  0.5018 

summary(lm(SalePrice ~ GrLivArea  + BedroomAbvGr, df))
# Adjusted R-squared:  0.5571

summary(lm(SalePrice ~ GrLivArea + FullBath, df))
# Adjusted R-squared:  0.5231

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr, df))
# Adjusted R-squared:  0.5824 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Interestingly including the variable HalfBath seems to be of limited use

summary(lm(SalePrice ~ GrLivArea + FullBath + HalfBath, df))
# Adjusted R-squared:  0.523

summary(lm(SalePrice ~ GrLivArea + FullBath + HalfBath + BedroomAbvGr, df))
# Adjusted R-squared:  0.5825

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr, df))
# Adjusted R-squared:  0.6159 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + BsmtFinSF1, df))
# Adjusted R-squared:  0.6534 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual), df))
# Adjusted R-squared:  0.7044 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1, df))
# Adjusted R-squared:  0.7294 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now that I've included most of the details like sqare feet, number of bedreooms and bathrooms, etc, I wanted to focus on the overall condition of the house
# There are a number of variables which could help accomplich this, including OverallCond, and YearBuild so I looked through all of them to see which worked the best

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + factor(OverallCond), df))
# Adjusted R-squared:  0.7368 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt, df))
# Adjusted R-squared:  0.757 

# YearBuild is a clear winner, but I wonder if there is a difference between a house that is about 50 years old, and one that is closer to 100 years old?

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + I(YearBuilt^2), df))
# Adjusted R-squared:  0.7583 

# adding a squared term of YearBuild makes YearBuilt negative and the squared term is positive, 
# it makes sense from a theory perspective that really old houses might have some historical value or other factors that increase the price

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + I(YearBuilt^2) + YearRemodAdd, df))
# Adjusted R-squared:  0.7591 

# dropping the squared YearBuilt term lowers the Adj R^2 even with the YearRemod term added
summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt  + YearRemodAdd, df))
# Adjusted R-squared:  0.7586 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Here I experiment more with some of the details which might imact price

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + I(YearBuilt^2) + YearRemodAdd + factor(CentralAir), df))
# Adjusted R-squared:  0.7596 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + I(YearBuilt^2) + YearRemodAdd + GarageArea, df))
# Adjusted R-squared:  0.7707 

# Adding Garage area reduced the explanatory power of YearBuild^2

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea, df))
# Adjusted R-squared:  0.7706 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(GarageType), df))
# Adjusted R-squared:  0.7649 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofMatl), df))
# Adjusted R-squared:  0.816 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofStyle) , df))
# Adjusted R-squared:  0.7738 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofStyle) + factor(RoofMatl) , df))
# Adjusted R-squared:  0.8189 

# It looks like RoofMaterial is more important than Roof Style, I think I'll only include the former for the sake of parsimony 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofMatl) + factor(SaleCondition), df))
# Adjusted R-squared:  0.8231 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofMatl) + factor(SaleCondition) + factor(MoSold), df))
# Adjusted R-squared:  0.8234 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofMatl) + factor(SaleCondition) + factor(YrSold), df))
# Adjusted R-squared:  0.8232 

summary(lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofMatl) + factor(SaleCondition) + factor(MoSold) + factor(YrSold), df))
# Adjusted R-squared:  0.8235 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now that I have my model I'll save it to apply to the test data

reg <- lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + factor(KitchenQual) + BsmtFinSF1 + YearBuilt + YearRemodAdd + GarageArea + factor(RoofMatl) + factor(SaleCondition) + factor(MoSold) + factor(YrSold), df)

dfTest <- read_csv("/PATH/test.csv")

# The last snag was that some of the variables I had used to make the above model had NA values in the test data, so I removed them in final model

sum(is.na(c(dfTest$GrLivArea, dfTest$BedroomAbvGr, dfTest$KitchenAbvGr, dfTest$KitchenQual, dfTest$BsmtFinSF1, dfTest$YearBuilt, dfTest$YearRemodAdd, dfTest$GarageArea, dfTest$RoofMatl, dfTest$SaleCondition, dfTest$MoSold, dfTest$YrSold)))
sum(is.na(c(dfTest$KitchenAbvGr, dfTest$KitchenQual, dfTest$BsmtFinSF1, dfTest$YearBuilt, dfTest$YearRemodAdd, dfTest$GarageArea, dfTest$RoofMatl, dfTest$SaleCondition, dfTest$MoSold, dfTest$YrSold)))
sum(is.na(c(dfTest$KitchenQual, dfTest$BsmtFinSF1, dfTest$YearBuilt, dfTest$YearRemodAdd, dfTest$GarageArea, dfTest$RoofMatl, dfTest$SaleCondition, dfTest$MoSold, dfTest$YrSold)))
sum(is.na(c(dfTest$KitchenQual, dfTest$BsmtFinSF1, dfTest$GarageArea)))

reg <- lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + YearBuilt + YearRemodAdd + factor(RoofMatl) + factor(SaleCondition) + factor(MoSold) + factor(YrSold), df)

summary(reg)
# Adjusted R-squared:  0.7458 
# sadly the adj R^2 fell quite a bit

prediction <- as_tibble(predict(reg, newdata = dfTest))

prediction$SalePrice <- prediction$value
prediction$Id <- 1461:2919

prediction <- prediction %>%
  select(Id, SalePrice)

write_csv(prediction, "/PATH/prediction.csv")

sum(is.na(prediction))
# Now the predictions have no NA values