library(dplyr)

library(lme4)

# Step 1 

## Pick out trending factors

dat2017 = readxl::read_excel("data/20180411_flat_2017.xlsx")

dat2017 = dat2017 %>% 
  mutate(landFactorDec = land_trending_factor / 100, 
         landComputedFactor = computed_land_value * landFactorDec, 
         testLandValue = land_AV - landComputedFactor)

dat2016 = readxl::read_excel("data/20180411_flat_2016.xlsx")

dat2016Values = dat2016 %>% 
  select(LRSN, starts_with("computed"), ends_with("AV"), contains("trending"))


combined1617 = left_join(dat2017, dat2016Values, by = "LRSN")

combined1617 = combined1617 %>% 
  mutate(landFactorDec2 = land_trending_factor.x / 100, 
         landComputedFactor2 = computed_land_value.y * landFactorDec, 
         testLandValue2 = land_AV.x - landComputedFactor)


salesOnly2017 = dat2017 %>% 
  filter(!(is.na(sale_price))) %>% 
  mutate(saleAssessedRatioRoll = sale_price / total_AV_roll)

salesOnly2017 %>% 
  group_by(school_district) %>% 
  summarize(meanRatio = mean(saleAssessedRatioRoll))

salesOnly2017 %>% 
  group_by(city) %>% 
  summarize(meanRatio = mean(saleAssessedRatioRoll))

salesOnly2017 %>% 
  group_by(school_district) %>% 
  summarize(meanRatio = mean(saleAssessedRatioRoll))

salesOnly2017 %>%
  mutate(city = tolower(city)) %>% 
  group_by(city) %>% 
  summarize(meanRatio = mean(land_AV), 
            n = n())

salesOnly2017 %>% 
  group_by(school_district) %>% 
  summarize(meanRatio = mean(land_AV), 
            n = n())

files = c("data/20180411_flat_2015.xlsx", 
          "data/")

dat2015 = rio::import("data/20180411_flat_2015.xlsx") %>% 
  mutate(year = 2015)

dat2016 = rio::import("data/20180411_flat_2016.xlsx") %>% 
  mutate(year = 2016)

dat2017 = rio::import("data/20180411_flat_2017.xlsx") %>% 
  mutate(year = 2017)

combinedYears = bind_rows(dat2015, dat2016, dat2017)

combinedYearsSales = combinedYears %>% 
  filter(!(is.na(sale_price))) %>% 
  mutate(saleAssessedRatioRoll = sale_price / total_AV_roll)


combinedYearsSales %>% 
  group_by(school_district, year) %>% 
  summarize(meanRatio = mean(land_AV), 
            n = n())

dat2017 %>% 
  filter(school_district == "UNU") %>% 
  group_by(neighborhood) %>% 
  summarize(n = n())

combinedYearsSales %>% 
  group_by(school_district) %>% 
  summarize(meanRatio = mean(saleAssessedRatioRoll))

library(cluster)

dat2017$conditionNum = NA

dat2017$conditionNum[dat2017$condition == "VP"] = 1

dat2017$conditionNum[dat2017$condition == "P"] = 2

dat2017$conditionNum[dat2017$condition == "F"] = 3

dat2017$conditionNum[dat2017$condition == "AV"] = 4

dat2017$conditionNum[dat2017$condition == "G"] = 5

dat2017$conditionNum[dat2017$condition == "VG"] = 6

dat2017$conditionNum[dat2017$condition == "EX"] = 6

dat2017$centralBinary = ifelse(dat2017$central_air == "N", 0, 1)

clusterData = dat2017 %>% 
  filter(year_built > 0 & year_built < 2017) %>% 
  mutate(grade = as.numeric(grade), 
         year_built = round(year_built), 
         year_built = ifelse(year_built < 1000, 
                             year_built + 1000, year_built)) %>% 
  select(acreage, effective_front_feet, year_built, base_parcel_area, conditionNum,
         finished_dwelling_area, bedrooms, finished_rooms, bathrooms, grade, centralBinary,
         school_district, sale_price)

clus2UNU = clusterData %>% 
  filter(school_district == "UNU") %>% 
  select(-school_district, - sale_price) %>% 
  clara(., 2)

unuClusDat = clusterData %>% 
  filter(school_district == "UNU") %>% 
  mutate(clusterAssignment = clus2UNU$clustering)


clus2PHM = clusterData %>% 
  filter(school_district == "PHM") %>% 
  select(-school_district, - sale_price) %>% 
  clara(., 2)
  
clus3PHM = clusterData %>% 
  filter(school_district == "PHM") %>% 
  select(-school_district, - sale_price) %>% 
  clara(., 3)

clus4PHMBIC = clusterData %>% 
  filter(school_district == "PHM") %>% 
  select(-school_district, - sale_price) %>% 
  na.omit() %>% 
  mclustBIC(.)

clus4PHMEM6 = clusterData %>% 
  filter(school_district == "PHM") %>% 
  select(-school_district, - sale_price) %>% 
  na.omit() %>% 
  Mclust(., G = 6, modelNames = "EEV")

clus4PHMPAM = clusterData %>% 
  filter(school_district == "PHM") %>% 
  select(-school_district, - sale_price) %>% 
  na.omit() %>% 
  clara(., 4)


clus10PHM = clusterData %>% 
  filter(school_district == "PHM") %>% 
  select(-school_district, - sale_price) %>% 
  clara(., 10)

dat2017 %>% 
  filter(school_district == "PHM" & !(is.na(centralBinary))) %>% 
  select(sale_price, total_AV_roll) %>% 
  mutate(assignment = clus10PHM$clustering) %>% 
  filter(!(is.na(sale_price))) %>% 
  mutate(ratio = sale_price / total_AV_roll) %>% 
  group_by(assignment) %>% 
  summarize(mean = mean(ratio), 
            n = n())

phmDat