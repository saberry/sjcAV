library(dplyr)

library(cluster)

library(purrr)

library(mclust)

library(factoextra)

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
         school_district, sale_price, LRSN)

clusterData %>% 
  select(-school_district, -sale_price, -LRSN) %>% 
  na.omit() %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(., hc.order = TRUE,
                         outline.col = "white",
                         ggtheme = ggplot2::theme_minimal,
                         colors = c("#6D9EC1", "white", "#E46726"))



# The school_district order is JG, MSC, NPU, PHM, SBCSC, & UNU

# clusterData %>%
#   filter(school_district == "JG") %>%
#   select(-sale_price, -LRSN, -school_district) %>%
#   na.omit() %>%
#   dist(.) %>%
#   fviz_dist(., show_labels = TRUE)

clusNums = clusterData %>% 
  dplyr::select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
                -LRSN, -centralBinary) %>% 
  na.omit() %>% 
  split(.$school_district) %>% 
  purrr::map(~eclust(.[!(names(.) %in% "school_district")], FUNcluster = "clara",
                     k.max = 6))

splitCluster = clusterData %>% 
  select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
         -LRSN, -centralBinary) %>% 
  na.omit() %>% 
  split(.$school_district)

clusNumTest = purrr::map2(splitCluster,
                          list(4, 4, 4, 4, 4, 4), 
                          ~clara(.x[!(names(.) %in% "school_district")], k = .y))




clusterAssignment = clusterData %>% 
  select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
         -centralBinary) %>% 
  na.omit() %>% 
  arrange(school_district) %>% 
  mutate(clusterAssignment = c(unlist(clusNumTest$JG$clustering), 
                               unlist(clusNumTest$MSC$clustering), 
                               unlist(clusNumTest$NPU$clustering), 
                               unlist(clusNumTest$PHM$clustering), 
                               unlist(clusNumTest$SBCSC$clustering), 
                               unlist(clusNumTest$UNU$clustering))) %>% 
  select(LRSN, clusterAssignment)

clusterAssignmentData = left_join(clusterData, clusterAssignment, by = "LRSN")

save(clusNumTest, clusterData, clusterAssignmentData, file = "clustering.RData")
