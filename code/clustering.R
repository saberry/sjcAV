# NOTE TO SETH #
# Cluster the big 3 school districts by township

# For Mishawaka, try to split between north and south of the river.

# Identify wacky values for further investigation.


library(dplyr)

library(cluster)

library(purrr)

library(mclust)

library(factoextra)

dat2017 = readxl::read_excel("data/_flat_2017.xlsx")

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
         finished_dwelling_area, bedrooms, finished_rooms, bathrooms, grade,
         school_district, township, sale_price, LRSN)

clusterData$schoolDistrictTownship = NA

clusterData$schoolDistrictTownship = ifelse(grepl("MSC|PHM|SBCSC", clusterData$school_district), 
                                            paste(clusterData$school_district, clusterData$township, sep = "_"), 
                                            clusterData$school_district)

clusterData %>% 
  select(-school_district, -sale_price, -LRSN, -township) %>% 
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
                -LRSN, -centralBinary, -township) %>% 
  na.omit() %>% 
  split(.$school_district) %>% 
  purrr::map(~eclust(.[!(names(.) %in% "school_district")], FUNcluster = "clara",
                     k.max = 8))

clusterData %>% 
  dplyr::select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
                -LRSN, -centralBinary, -township) %>% 
  na.omit() %>% 
  filter(school_district == "") %>%
  purrr::map(~fviz_nbclust(.[!(names(.) %in% "school_district")], clara, method = "silhouette"))


silTest = clusterData %>% 
  dplyr::select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
                -LRSN, -centralBinary, -township) %>% 
  na.omit() %>% 
  split(.$school_district) %>%
  purrr::map(~fviz_nbclust(.[!(names(.) %in% "school_district")], clara, method = "silhouette"))

gapTest = clusterData %>% 
  dplyr::select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
                -LRSN, -centralBinary, -township) %>% 
  na.omit() %>% 
  split(.$school_district) %>%
  purrr::map(~fviz_nbclust(.[!(names(.) %in% "school_district")], clara, method = "gap_stat", 
                           nboot = 10, k.max = 8))

elbowTest = clusterData %>% 
  dplyr::select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
                -LRSN, -centralBinary, -township) %>% 
  na.omit() %>% 
  split(.$school_district) %>%
  purrr::map(~fviz_nbclust(.[!(names(.) %in% "school_district")], clara, method = "wss", 
                           k.max = 8))


bigTest = clusterData %>% 
  dplyr::select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
                -LRSN, -centralBinary, -township) %>% 
  na.omit() %>% 
  split(.$school_district) %>%
  purrr::map(~NbClust(.[!(names(.) %in% "school_district")], method = "centroid", 
                           max.nc = 8, min.nc = 2, index = "all"))

splitCluster = clusterData %>% 
  select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
         -LRSN, -township) %>% 
  na.omit() %>% 
  split(.$school_district)

clusNumTest = purrr::map2(splitCluster,
                          list(2, 7, 3, 7, 7, 2), 
                          ~clara(.x[!(names(.) %in% "school_district")], k = .y))


splitCluster2 = clusterData %>% 
  select(-base_parcel_area, -grade, -finished_rooms, -sale_price, 
         -LRSN, -township) %>% 
  na.omit() %>% 
  split(.$schoolDistrictTownship)

clusNumTest2 = purrr::map2(splitCluster2,
                          list(3, 7, 3, 5, 3, 5, 5, 5, 5, 4, 5, 4, 3), 
                          ~clara(.x[!(names(.) %in% "school_district")], k = .y))


clusterAssignment = clusterData %>% 
  select(-base_parcel_area, -grade, -finished_rooms, -sale_price, -township) %>% 
  na.omit() %>% 
  arrange(school_district) %>% 
  mutate(clusterAssignment = c(unlist(clusNumTest$JG$clustering), 
                               unlist(clusNumTest$MSC$clustering), 
                               unlist(clusNumTest$NPU$clustering), 
                               unlist(clusNumTest$PHM$clustering), 
                               unlist(clusNumTest$SBCSC$clustering), 
                               unlist(clusNumTest$UNU$clustering)), 
         clusterAssignment2 = c(unlist(clusNumTest2$JG$clustering), 
                                unlist(clusNumTest2$MSC_010$clustering), 
                                unlist(clusNumTest2$NPU$clustering), 
                                unlist(clusNumTest2$PHM_005$clustering), 
                                unlist(clusNumTest2$PHM_008$clustering), 
                                unlist(clusNumTest2$PHM_010$clustering), 
                                unlist(clusNumTest2$SBCSC_001$clustering), 
                                unlist(clusNumTest2$SBCSC_002$clustering), 
                                unlist(clusNumTest2$SBCSC_003$clustering), 
                                unlist(clusNumTest2$SBCSC_004$clustering), 
                                unlist(clusNumTest2$SBCSC_011$clustering), 
                                unlist(clusNumTest2$SBCSC_013$clustering), 
                                unlist(clusNumTest2$UNU$clustering))) %>% 
  select(LRSN, clusterAssignment, clusterAssignment2)

clusterAssignmentData = left_join(clusterData, clusterAssignment, by = "LRSN")

avDat = dat2017 %>% select(LRSN, total_AV_roll)

clusterAssignmentData = left_join(clusterAssignmentData, avDat, by = "LRSN")

save(clusNumTest, clusterData, clusterAssignmentData, file = "clustering.RData")
