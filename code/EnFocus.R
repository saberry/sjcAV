library(dplyr)
# library(plyr)
library(ggplot2)
library(lme4)

#Read in file

# Flat2017 = readxl::read_xlsx(path = "G:/My Drive/Research/Projects/EnFocus/Shared with Jack/20180601_flat_files/_flat_2017.xlsx")

saleData = readxl::read_excel("data/Copy of 2013-2017 SJC Ratio Study Residentioal Sales used.xlsx", sheet = "2017-2018")

Flat2017 = readxl::read_excel("data/_flat_2017.xlsx")

load("clustering.RData")

load("data/sjcShapeTract.RData")

shapeSmall = sjcShape %>% 
  select(PARCELSTAT, tract)

Flat2017 = left_join(Flat2017, saleData, by = c("PARCELSTAT" = "Parcel Number")) %>% 
  left_join(., shapeSmall, by = "PARCELSTAT")

clusterAssignmentData$grade = as.character(clusterAssignmentData$grade)

Flat2017 = left_join(Flat2017, clusterAssignmentData)

saleCoef = Flat2017 %>% 
  filter(!is.na(`Sale Price`)) %>% 
  mutate() %>% 
  lm(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1, data = .) %>% 
  coef()

salesOnly = Flat2017 %>% 
  filter(!is.na(`Sale Price`)) %>% 
  mutate(newAssessedValue = (land_AV_preRoll * saleCoef["land_AV_preRoll"]) + 
           (improvement_AV_preRoll * saleCoef["improvement_AV_preRoll"]), 
         saleAssessedDiff = `Sale Price` - newAssessedValue, 
         saleAssessedDiffPerc = (`Sale Price` - newAssessedValue) / `Sale Price`, 
         ratio = newAssessedValue / `Sale Price`, 
         medianRatio = median(ratio), 
         dispersion = ratio - medianRatio)

trueCODRaw = ((100/nrow(salesOnly)) * sum(abs(salesOnly$dispersion))) / salesOnly$medianRatio[1]

salesOnly = salesOnly[which(!is.na(salesOnly$clusterAssignment)), ] %>% 
  tidyr::unite(schoolCluster, school_district, clusterAssignment, remove = FALSE) %>% 
  tidyr::unite(schoolTownCluster, schoolDistrictTownship, clusterAssignment2, remove = FALSE)

districtCoefs = salesOnly %>% 
  split(., .$schoolCluster) %>% 
  purrr::map(~lm(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1, data = .x)) %>% 
  purrr::map(~coef(.))
  
splitStuff = salesOnly %>% 
  split(., .$schoolCluster)

salesOnly = map2_df(splitStuff, districtCoefs, ~ mutate(., newAssessedValueTest = (.$land_AV_preRoll * .y["land_AV_preRoll"]) + 
                                        (.$improvement_AV_preRoll * .y["improvement_AV_preRoll"])))

salesOnly = salesOnly %>% 
  mutate(saleAssessedDiff = `Sale Price` - newAssessedValueTest, 
       saleAssessedDiffPerc = (`Sale Price` - newAssessedValueTest) / `Sale Price`, 
       ratio = newAssessedValueTest / `Sale Price`, 
       medianRatio = median(ratio), 
       dispersion = ratio - medianRatio)

trueCODSchool = ((100/nrow(salesOnly)) * sum(abs(salesOnly$dispersion))) / salesOnly$medianRatio[1]

districtTownCoefs = salesOnly %>% 
  split(., .$schoolTownCluster) %>% 
  purrr::map(~lm(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1, data = .x)) %>% 
  purrr::map(~coef(.))

splitStuffTown = salesOnly %>% 
  split(., .$schoolTownCluster)

salesOnly = map2_df(splitStuffTown, districtTownCoefs, 
                    ~ mutate(., newAssessedValueSDTown = (.$land_AV_preRoll * .y["land_AV_preRoll"]) + 
                                                          (.$improvement_AV_preRoll * .y["improvement_AV_preRoll"])))

salesOnly = salesOnly %>% 
  mutate(saleAssessedDiffTown = `Sale Price` - newAssessedValueSDTown, 
         saleAssessedDiffPercTown = (`Sale Price` - newAssessedValueSDTown) / `Sale Price`, 
         ratioTown = newAssessedValueSDTown / `Sale Price`, 
         medianRatio = median(ratioTown, na.rm = TRUE), 
         dispersion = ratioTown - medianRatio)

trueCODSchoolTown = ((100/nrow(salesOnly)) * sum(abs(salesOnly$dispersion), na.rm = TRUE)) / salesOnly$medianRatio[1]


# Tax District

tractCoefs = salesOnly %>% 
  split(., .$tract) %>% 
  purrr::map(~lm(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1, data = .x)) %>% 
  purrr::map(~coef(.))

splitTract = salesOnly %>% 
  split(., .$tract)

salesOnly = map2_df(splitTract, tractCoefs, 
                    ~ mutate(., newAssessedValueTract = (.$land_AV_preRoll * .y["land_AV_preRoll"]) + 
                               (.$improvement_AV_preRoll * .y["improvement_AV_preRoll"])))

salesOnly = salesOnly %>% 
  mutate(saleAssessedDiffTract = `Sale Price` - newAssessedValueTract, 
         saleAssessedDiffPercTract = (`Sale Price` - newAssessedValueTract) / `Sale Price`, 
         ratioTract = newAssessedValueTract / `Sale Price`, 
         medianRatio = median(ratioTract, na.rm = TRUE), 
         dispersion = ratioTract - medianRatio)

trueCODTract = ((100/nrow(salesOnly)) * sum(abs(salesOnly$dispersion), na.rm = TRUE)) / salesOnly$medianRatio[1]



# Mixed Model Test

mixedModTest = lmer(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1 + (1|schoolTownCluster), data = salesOnly)



#Attempt to calcualte trending factors
#First, which neighborhoods had sufficient sales?

NbrhdSales2016 = Flat2016 %>% group_by(neighborhood) %>% 
  mutate(SaleNY = ifelse(!is.na(sale_price),1,0)) %>% 
  group_by(neighborhood) %>% 
  tally(SaleNY)

WithSales2016 = Flat2016 %>% filter(sale_price > 0) %>% 
  mutate(KeyRatio = sale_price / (computed_land_value + computed_improvement_value))

write.csv(WithSales2016,"G:/My Drive/Research/Projects/EnFocus/WithSales2016.csv",row.names = FALSE,na="")


#this yields infinite means for some reason
NbrhdTrend2016 = WithSales2016 %>% group_by(neighborhood) %>% 
  mutate(Trend2016 = mean(KeyRatio))

#Check Ratio of Sales per neighborhood
NbrhdSaleRatios2017 = Flat2017 %>% group_by(neighborhood) %>% 
  mutate(SaleNY = ifelse(!is.na(sale_price),1,0)) %>% 
  summarize(NumSales = sum(SaleNY),CountProperties = n()) %>% 
  mutate(NbrhdSaleRatio = NumSales/CountProperties)
write.csv(NbrhdSaleRatios2017,"G:/My Drive/Research/Projects/EnFocus/NbrhdSaleRatios2017.csv",row.names = FALSE,na="")
#Check Ratio of Sales per old neighborhood
OldNSaleRatios2017 = Flat2017 %>% group_by(old_neighborhood) %>% 
  mutate(SaleNY = ifelse(!is.na(sale_price),1,0)) %>% 
  summarize(NumSales = sum(SaleNY),CountProperties = n()) %>% 
  mutate(OldNSaleRatio = NumSales/CountProperties)
write.csv(OldNSaleRatios2017,"G:/My Drive/Research/Projects/EnFocus/OldSaleRatios2017.csv",row.names = FALSE,na="")
#Check on Trending from prior year
TrendPerNbrhd2017 = Flat2017 %>% group_by(neighborhood) %>% 
  mutate (Trend = ((total_AV_roll) / (`total_AV_pre-roll`) - 1)) %>% 
  summarize_at(vars(Trend),funs(mean))

TaxTown2017 = ddply(Flat2017,~tax_district, summarise, NumTwp = length(unique(township)))

TwpTax2017 = ddply(Flat2017,~township, summarise, NumTaxD = length(unique(tax_district)))

TownSchDist2017 = ddply(Flat2017,~township, summarise, NumSchD = length(unique(school_district)))

SchDist2017 = ddply(Flat2017,~school_district, summarise, 
                  NumTwp = length(unique(township)),NumTaxD = length(unique(tax_district)),NumNbrhd = length(unique(neighborhood)))

Neighborhoods2017 = ddply(Flat2017,~neighborhood, summarise, NumTaxD = length(unique(tax_district)))

TaxDistrict = Flat2017 %>% group_by(tax_district) %>% tally(tax_district)

Nbrhd2TaxD = Flat2017 %>%  group_by(neighborhood,tax_district) %>% tally()

SchTwpTax = Flat2017 %>%  group_by(school_district,township,tax_district) %>%  tally()


write.csv(Nbrhd2TaxD,"G:/My Drive/Research/Projects/EnFocus/NbrhdTaxDistrict.csv",row.names = FALSE,na="")

SaleRatios2017 %>% filter(CountProperties>1) %>%  ggplot() +
  geom_point(mapping = aes(x=NumSales,y=CountProperties, color = SaleRatio),
             position = "jitter")

Townships = Flat2017 %>% count(Twp)
WithSales = Flat2017 %>% filter(SalePrice > 0) %>% mutate(KeyRatio = TotalAV_17roll/SalePrice)

KeyRatios = WithSales %>% group_by(Nbrhd) %>% summarize_at(vars(KeyRatio),funs(min,mean,max))

KeyRatios2 = left_join(KeyRatios,SaleRatios2017,by = "Nbrhd")

KeyRatios2 %>% ggplot() +
  stat_summary(
    mapping = aes(x = NumSales,y)
  )

KeyRatios2 %>% summary()

write.csv(TrendPerNbrhd2017,"G:/My Drive/Research/Projects/EnFocus/TrendPerNbrhd2017.csv",row.names = FALSE,na="")

write.csv(TwpNbrhd2017,"G:/My Drive/Research/Projects/EnFocus/TwpNbrhd2017.csv",row.names = FALSE,na="")
