testFunc = function(salesOnly, fullData, minSale, type = c("true", "full"),
                    subVar, subVar2 = NULL) {
  
  library(dplyr)
  
  library(purrr)
  
  library(rlang)
  
  if(is_empty(subVar2) == FALSE) {
    newName = paste(subVar, subVar2, sep = "_")
    salesOnly[, newName] = paste(salesOnly[, subVar][[1]], salesOnly[, subVar2][[1]], sep = "_")
    fullData[, newName] = paste(fullData[, subVar][[1]], fullData[, subVar2][[1]], sep = "_")
    subVar = newName
  }
  
  salesOnly = as.data.frame(salesOnly)
  
  counts = salesOnly %>% 
    group_by_at(vars(one_of(subVar))) %>% 
    summarize_(stuff = ~n()) %>% 
    filter(stuff > minSale) %>% 
    filter(UQ(sym(subVar)) != "NULL", UQ(sym(subVar)) != "NA") %>%
    as.data.frame()
  
  salesOnlyPlus = salesOnly
  
  salesOnlyPlus$trendType[salesOnlyPlus[, subVar] %in% counts[, subVar]] = "true"
  
  salesOnlyPlus$trendType[!(salesOnlyPlus[, subVar] %in% counts[, subVar])] = "full"
  
  tractCoefs =  switch(type, 
                       full = salesOnlyPlus %>% 
                         split(., .[,subVar]) %>% 
                         purrr::map(~lm(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1, data = .x)) %>% 
                         purrr::map(~coef(.)), 
                       true = salesOnlyPlus %>% 
                         filter(trendType == "true") %>% 
                         split(., .[,subVar]) %>% 
                         purrr::map(~lm(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1, data = .x)) %>% 
                         purrr::map(~coef(.)))
  
  splitTract = switch(type, 
                      full = salesOnlyPlus %>% 
                        split(., .[,subVar]), 
                      true = salesOnlyPlus %>%
                        filter(trendType == "true") %>% 
                        split(., .[,subVar]))
  
  salesOnlyPlus =  switch(type, 
                          true = map2_df(splitTract, tractCoefs, 
                                         ~ mutate(., newAssessedValueTract = ifelse(trendType == "true", 
                                                                                    (.$land_AV_preRoll * .y["land_AV_preRoll"]) + 
                                                                                      (.$improvement_AV_preRoll * .y["improvement_AV_preRoll"]), 
                                                                                    (.$land_AV_preRoll * 1) + 
                                                                                      (.$improvement_AV_preRoll * 1)))), 
                          full = map2_df(splitTract, tractCoefs, 
                                         ~ mutate(., newAssessedValueTract = ifelse(trendType == "true", 
                                                                                    (.$land_AV_preRoll * .y["land_AV_preRoll"]) + 
                                                                                      (.$improvement_AV_preRoll * .y["improvement_AV_preRoll"]), 
                                                                                    (.$land_AV_preRoll * 1) + 
                                                                                      (.$improvement_AV_preRoll * 1)))))
  
  # map2_df(splitTract, tractCoefs, 
  #                          ~ mutate(., newAssessedValueTract = ifelse(trendType == "true", 
  #                                                                     (.$land_AV_preRoll * .y["land_AV_preRoll"]) + 
  #                                                                       (.$improvement_AV_preRoll * .y["improvement_AV_preRoll"]), 
  #                                   (.$land_AV_preRoll * 1) + 
  #                                     (.$improvement_AV_preRoll * 1))))

  salesOnlyPlus = salesOnlyPlus %>%
    mutate(saleAssessedDiffTract = `Sale Price` - newAssessedValueTract,
           saleAssessedDiffPercTract = (`Sale Price` - newAssessedValueTract) / `Sale Price`,
           ratioTract = newAssessedValueTract / `Sale Price`,
           medianRatio = median(ratioTract, na.rm = TRUE),
           dispersion = ratioTract - medianRatio)
  
  trueCODTract = ((100/nrow(salesOnlyPlus)) * sum(abs(salesOnlyPlus$dispersion), na.rm = TRUE)) / salesOnlyPlus$medianRatio[1]
  
  countPercentage = fullData[fullData[, subVar][[1]] %in% salesOnlyPlus[, subVar], ] %>% 
    group_by(UQ(sym(subVar))) %>% 
    summarize(n = n())
  
  countPercentage = sum(countPercentage$n) / nrow(fullData)
  
  summaryStat = rbind(trueCOD = trueCODTract, 
                      ratio = mean(salesOnlyPlus$medianRatio), 
                      proportion = countPercentage)
}
