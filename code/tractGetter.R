library(rgdal)

sjcShape = sf::read_sf("data/parcelShape/parcel2017.shp")

sjcShape %>% 
  select(MAILINGADD, MAILINGCIT, MAILINGSTA, MAILINGZIP) %>% 
  write.csv(., "addresses.csv", row.names = FALSE, col.names = FALSE)

tractGetter = function(street, city, zip){
  
  Sys.sleep(.1)
  
  test = httr::GET(paste("https://geocoding.geo.census.gov/geocoder/geographies/address?street=", 
                         gsub("\\s", "\\+", street), "&city=", city, 
                         "&state=IN&zip=", zip, "&benchmark=9&vintage=910&format=json", 
                         sep = ""))
  out = tryCatch({
    tract = content(test)$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$BASENAME
    
    return(tract)
  }, 
  error = function(e) {
    return(NA)
  })
  
  return(out)
}

allTracts = mapply(tractGetter, sjcShape$MAILINGADD, sjcShape$MAILINGCIT, sjcShape$MAILINGZIP, 
                   SIMPLIFY = FALSE)

library(parallel)

cl = makeCluster(3)

clusterExport(cl, c("tractGetter", "sjcShape"))

clusterEvalQ(cl, library(httr))

stopCluster(cl)

allTracts = clusterMap(cl, tractGetter, sjcShape$MAILINGADD, sjcShape$MAILINGCIT, sjcShape$MAILINGZIP, 
                       SIMPLIFY = FALSE)

testTracts = data.frame(address = names(allTracts), 
                        tract = unlist(allTracts))

sjcShape = sjcShape %>% 
  mutate(tract = unlist(as.character(allTracts), use.names = FALSE))

save(sjcShape, file = "data/sjcShapeTract.RData")

tractGetter(sjcShape$MAILINGADD[1000], sjcShape$MAILINGCIT[1000], sjcShape$MAILINGZIP[1000])

tract = content(test)$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$TRACT




inShape = rgdal::readOGR("data/tl_2017_18_tract/tl_2017_18_tract.shp", 
                         layer = "tl_2017_18_tract")

inShape = inShape[inShape$COUNTYFP == 141,]

library(leaflet)

leaflet() %>% 
  addPolylines(data = inShape) %>% 
  addCircles(data = sjcShape, lng = ~INSIDE_X_D, lat = ~Y_Degrees)

library(tidycensus)



census_api_key("99f02d15745b04acaf40f60535ae45a70ecde2ac")

tidycensus::get_decennial(geography = "tract", variables = "P0050003",
                          state = "IN", county = "141", year = 2010)
