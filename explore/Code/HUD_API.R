###### HUD QUERY EXPLORE ####
# The HUD provides a ton of data on multifamily units they track
# The goal is to feed in areas near the RD and scrape areas out, but the API is not super cooperative rn

# map
# http://www.arcgis.com/home/webmap/viewer.html?url=https://services.arcgis.com/VTyQ9soqVukalItT/ArcGIS/rest/services/Multifamily_Properties_Assisted/FeatureServer&source=sd

# reference
# https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Multifamily_Properties_Assisted/FeatureServer/0/query?where=&objectIds=6&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=PCT_HISPANIC&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=html&token=
# https://developers.arcgis.com/rest/services-reference/query-map-service-layer-.htm
# https://services.arcgis.com/VTyQ9soqVukalItT/ArcGIS/rest/services/Multifamily_Properties_Assisted/FeatureServer/layers



library(jsonlite)
library(dplyr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/hud') 
}

# output_fields of interest:
# * : gives everything
# PCT_HISPANIC : % hispanic

url = 'https://services.arcgis.com/'
call = 'VTyQ9soqVukalItT/arcgis/rest/services/Multifamily_Properties_Assisted/FeatureServer/0/query?'
query1 = 'where=&objectIds='
query2 = '&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=json&token='

## manually loop across space of possible object IDs
for (id in 1:100000) {
  # Actual API Call
  raw_output <- GET(url = url, path = paste0(call,query1,id,query2))
  # Transform and Save
  text_output <- rawToChar(raw_output$content)
  api_output <- do.call(rbind, lapply(paste(text_output, collapse=""),jsonlite::fromJSON))
  saveRDS(api_output,file=paste0("rawMultifamily/",id,".Rdata"))
}


  




