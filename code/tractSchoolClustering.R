########################################
### Tract & School District Clusters ###
########################################

validTractList = read.table("https://www2.census.gov/geo/maps/dc10map/tract/st18_in/c18141_st_joseph/DC10CT_C18141_CT2MS.txt", 
                            sep = ";", header = TRUE)

load("data/Flat2017Joined.RData")

dat2017 = Flat2017

rm(Flat2017)

validTractsData = dat2017[dat2017$tract %in% validTractList$NAME, ]

clusterData = dat2017 %>% 
  filter(year_built > 0 & year_built < 2017) %>% 
  mutate(grade = as.numeric(grade), 
         year_built = round(year_built), 
         year_built = ifelse(year_built < 1000, 
                             year_built + 1000, year_built)) %>% 
  select(acreage, year_built, finished_dwelling_area, 
         finished_rooms, school_district, township, tax_district, 
         tract, sale_price, LRSN)

validTractsData = clusterData[clusterData$tract %in% validTractList$NAME, ]

clusterNumFunction = function(dat, varName) {
  splitCluster = dat %>% 
    select(acreage, year_built, 
           finished_dwelling_area, finished_rooms, 
           one_of(varName)) %>% 
    na.omit() %>% 
    split(.[, varName])
  
  clusNumTest = purrr::map2(splitCluster,
                            list(rep(2, length(splitCluster))), 
                            ~cluster::clara(.x[!(names(.) %in% varName)], k = .y[[1]]))
  
  return(clusNumTest)
}

# debugonce(clusterNumFunction)


# school_district, township, tax_district

tracCluster = clusterNumFunction(validTractsData, "tract")

tracCluster = unlist(lapply(names(tracCluster), function(x) unlist(tracCluster[[x]]$clustering)))

schoolCluster = clusterNumFunction(clusterData, "school_district")

schoolCluster = unlist(lapply(names(schoolCluster), function(x) unlist(schoolCluster[[x]]$clustering)))

clusterAssignment = clusterData %>% 
  arrange(school_district) %>% 
  mutate(clusNum = schoolCluster, 
    schoolDistrictClusterNumber = paste(school_district, clusNum, sep = "_")) %>% 
  select(LRSN, schoolDistrictClusterNumber)

clusterAssignmentData = left_join(clusterData, clusterAssignment, by = "LRSN")

tractAssignment = validTractsData %>% 
  arrange(tract) %>% 
  mutate(clusNum = tracCluster, 
         tractClusterNumber = paste(tract, clusNum, sep = "_")) %>% 
  select(LRSN, tractClusterNumber)

tractAssignmentData = left_join(validTractsData, tractAssignment, by = "LRSN")

avDat = dat2017 %>% select(LRSN, total_AV_roll)

clusterAssignmentData = left_join(clusterAssignmentData, avDat, by = "LRSN") %>% 
  left_join(., tractAssignmentData)

save(clusterAssignmentData, file = "clustering.RData")
