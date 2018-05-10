library(dplyr)

library(cluster)

library(purrr)

dat2017 = readxl::read_excel("data/20180411_flat_2017.xlsx")

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
         school_district, sale_price, LRSN) %>% 
  na.omit()

clusterData %>% 
  select(-school_district, -sale_price, -LRSN) %>% 
  cor() %>% 
  corrplot::corrplot.mixed(., order = "FPC")

test = clusterData %>% 
  select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
         -LRSN, -centralBinary) %>% 
  split(.$school_district) %>% 
  map(~clara(.x, 3)) 