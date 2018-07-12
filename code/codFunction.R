testFunc = function(salesOnly, minSale, subVar, subVar2 = NULL) {
  
  library(dplyr)
  
  library(purrr)
  
  if(is_empty(subVar2) == FALSE) {
    newName = paste(subVar, subVar2, sep = "_")
    salesOnly[, newName] = paste(salesOnly[, subVar][[1]], salesOnly[, subVar2][[1]], sep = "_")
    subVar = newName
  }
  
  salesOnly = as.data.frame(salesOnly)
  
  counts = salesOnly %>% 
    group_by_at(vars(one_of(subVar))) %>% 
    summarize_(stuff = ~n()) %>% 
    filter(stuff > minSale) %>% 
    as.data.frame()
  
  salesOnlyPlus = salesOnly[salesOnly[, subVar] %in% counts[, subVar], ]
    
  
  tractCoefs =  salesOnlyPlus %>% 
    split(., .[,subVar]) %>% 
    purrr::map(~lm(`Sale Price` ~ land_AV_preRoll + improvement_AV_preRoll -1, data = .x)) %>% 
    purrr::map(~coef(.))
  
  splitTract = salesOnlyPlus %>% 
    split(., .[,subVar])
  
  salesOnlyPlus = map2_df(splitTract, tractCoefs, 
                      ~ mutate(., newAssessedValueTract = (.$land_AV_preRoll * .y["land_AV_preRoll"]) + 
                                 (.$improvement_AV_preRoll * .y["improvement_AV_preRoll"])))
  
  salesOnlyPlus = salesOnlyPlus %>% 
    mutate(saleAssessedDiffTract = `Sale Price` - newAssessedValueTract, 
           saleAssessedDiffPercTract = (`Sale Price` - newAssessedValueTract) / `Sale Price`, 
           ratioTract = newAssessedValueTract / `Sale Price`, 
           medianRatio = median(ratioTract, na.rm = TRUE), 
           dispersion = ratioTract - medianRatio)
  
  trueCODTract = ((100/nrow(salesOnlyPlus)) * sum(abs(salesOnlyPlus$dispersion), na.rm = TRUE)) / salesOnlyPlus$medianRatio[1]
  
  rbind(trueCOD = trueCODTract, 
        ratio = mean(salesOnlyPlus$medianRatio))
  
  # return(trueCODTract)
}
