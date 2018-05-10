library(dplyr)

library(caret)

library(randomForest)

library(rpart)

data2017 = readxl::read_xlsx("data/_flat_2017.xlsx")

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa')
)


plot_geo(data2017, lon = ~WGS84_latitude, lat = ~WGS84_longitude) %>% 
  add_markers(data2017, size = ~`total_AV_pre-roll`) %>% 
  layout(geo = g)

data2017 = data2017 %>% 
  mutate(diff = SalePrice / TotalAV_17, 
         inRange = ifelse(diff > .9 & diff < 1.1, 1, 0), 
         Twp = as.factor(Twp), Grade = as.factor(Grade), 
         Cond = as.factor(Cond), 
         Nbrhd = as.factor(Nbrhd))

salesOnly = data2017 %>% 
  filter(!is.na(SalePrice)) %>% 
  select(diff, inRange, Twp, Acreage, Extensions, Improvements, Grade, Cond, 
         YrBuilt, DwellFinishedArea, Floors, Dwellings, 
         Bedrooms, Bathrooms, HeatType, KitchenSinks, TotalFixtures, 
         Twp, Grade, Cond, Nbrhd) %>% 
  na.omit() %>% 
  mutate(inRange = as.factor(inRange))

# Predicting Range Correct #

correctRangeTree = salesOnly %>% 
  select(-diff) %>% 
  party::ctree(inRange ~ ., data = .)

table(predict(correctRangeTree), salesOnly$inRange)

plot(correctRangeTree)

correctRangeForest = salesOnly %>% 
  select(-diff) %>%
  party::cforest(inRange ~ ., data = .)

table(predict(correctRangeForest), salesOnly$inRange)

varImpPlot(differenceForest)

svmTest = salesOnly %>% 
  select(-diff) %>%
  e1071::svm(inRange ~ ., data = .)

summary(svmTest)

pred = fitted(svmTest)

table(fitted(svmTest), salesOnly$inRange)

# Predicting Proportion #
  
differenceTree = rpart(diff ~ YrBuilt + DwellFinishedArea + Bedrooms + Bathrooms, 
                       data = salesOnly, method = "anova")

differenceForest = randomForest(diff ~ YrBuilt + DwellFinishedArea + Bedrooms + Bathrooms, 
                                data = salesOnly, mtry = 2)

varImpPlot(differenceForest)
