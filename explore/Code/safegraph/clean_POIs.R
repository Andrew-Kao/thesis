###### Safegraph data -- POI cleaning ####

library(dplyr)
library(data.table)
library(rgdal)
library(sf)
library(raster)
library(rgeos)
library(stringr)
library(spatial)


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/safegraph')
  sdir <- '~/Dropbox/safegraph/coreplaces'
} else {
  setwd('/n/holyscratch01/dyang_lab/sltv/explore/Data/safegraph')
  sdir <- '/n/holyscratch01/dyang_lab/sltv/explore/Data/safegraph/coreplaces'
}

######### merge to instrument #########

# instrument data
instrument <- readRDS("../instrument/countyInstrumentCovariate.Rdata")
counties <- rgdal::readOGR("../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))
counties@data <- counties@data %>%
  mutate(stateCounty = paste0(STATE,COUNTY))

### need a spatial level county dataset
instrument <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(COUNTY = origcounty, STATE = origstate) %>%
  mutate(STATE = str_pad(STATE,3,side="right","0"), COUNTY = str_pad(COUNTY,4,side="right","0"),
         stateCounty = paste0(STATE,COUNTY)) %>%
  filter(!is.na(COUNTY) & !is.na(STATE))

# contour data
contours1 <- readRDS('../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# do for each of the coreplaces

for (cp in 1:5) {
  bdf <- fread(paste0(sdir,'/core_poi-part',cp,'.csv')) %>%
    filter(iso_country_code == 'US') %>%
    mutate(sector = floor(naics_code/10000)) %>%
    filter(sector == 51 | sector == 52 | sector == 61 | sector == 71 | sector == 72)

  # chunk for memory reasons
  for (j in 1:35) {
    print(paste0('Run: File ',cp,' row subset ', j))
    df <- bdf %>%
      filter(row_number() <= j * 10000, row_number() >= (j-1) * 10000 + 1)
    
    # new CRS system, use WKT
    # https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/ 
    coordinates(df) <- c('longitude','latitude')
    crs_nad83 <- CRS(SRS_string = "EPSG:4269")
    # wkt(crs_nad83)
    proj4string(df) <- crs_nad83
    
    ## need to match up data to counties and merge
    
    
    
    ### distances to contours 
    df_project <- spTransform(df, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
    
    contourdfDist <- gDistance(contours_project,df_project, byid = TRUE)
    contourdfMinDist <- apply(contourdfDist,1,FUN=min)  # 428 counties that intersect!
    saveRDS(contourdfMinDist,paste0('POI/contourPOIMinDist_',cp,'_',j,'.Rdata'))
    # stargazer(matrix(contourLEAMinDist,ncol=1), out="../../Output/Summary/ContourLEAMinDist.tex", title="Contour-LEA Minimum Distances", summary = TRUE)
    
    contours_poly <-SpatialPolygons(
      lapply(1:length(contours_project), 
             function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
    crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    contourdfIntersect <- gIntersects(contours_poly,df_project, byid = TRUE)
    contourdfIntersect[contourdfIntersect == "FALSE"] <- 0
    contourdfIntersect[contourdfIntersect == "TRUE"] <- 1
    contourdfInterAll <- apply(contourdfIntersect,1,FUN=sum)  
    saveRDS(contourdfInterAll,paste0('POI/contourPOIInterAll_',cp,'_',j,'.Rdata'))
    # stargazer(matrix(contourLEAInterAll,ncol=1), out="../../Output/Summary/ContourLEAInterAll.tex", title="LEA Within Contours", summary = TRUE)
    
    contourdfMinDist <- readRDS(paste0('POI/contourPOIMinDist_',cp,'_',j,'.Rdata'))
    contourdfInterAll <- readRDS(paste0('POI/contourPOIInterAll_',cp,'_',j,'.Rdata'))
    
    df@data <- df@data %>%
      mutate(minDist = contourdfMinDist, inside = contourdfInterAll)
    
    ### merge in county level data
    dfCountyLink <- over(df,counties,returnList = FALSE)
    df@data <- df@data %>%
      mutate(stateCounty = dfCountyLink$stateCounty)
    
    df2 <- merge(df, instrument, by = 'stateCounty', all.x = TRUE)
    saveRDS(df2, paste0('POI/POIReadyRaster_',cp,'_',j,'.Rdata'))
    
    
  }


}


# combine data
for (cp in 1:5) {
  for (j in 1:35) {
    df <- readRDS(paste0('POI/POIReadyRaster_',cp,'_',j,'.Rdata'))
    
    if (cp == 1 && j == 1) {
      bdf <- df
    } else {
      bdf <- rbind(tdf, df)
    }
  }
}

saveRDS(bdf,paste0('POI/POIReadyRaster.Rdata'))

# delete temp files for chunking
for (cp in 1:5) {
  for (j in 1:35) {
    file.remove(paste0('POI/contourPOIMinDist_',cp,'_',j,'.Rdata'))
    file.remove(paste0('POI/contourPOIInterAll_',cp,'_',j,'.Rdata'))
    file.remove(paste0('POI/POIReadyRaster_',cp,'_',j,'.Rdata'))
  }
}


######### cleaning data ########
bdf <- readRDS('POI/POIReadyRaster.Rdata')@data %>%
  filter(minDist < 100000) %>%
  mutate(school = ifelse(sub_category == "Elementary and Secondary Schools",1,0),
         hispanic_food = ifelse(str_detect(category_tags,"Argentinean Food") | 
                                str_detect(category_tags,"Cuban Food") |
                                str_detect(category_tags,"Latin American Food") |
                                str_detect(category_tags,"Mexican Food") |
                                str_detect(category_tags,"Peruvian Food") |
                                str_detect(category_tags,"Spanish Food") |
                                str_detect(category_tags,"Tapas"),1,0),
         hispanic_loc = ifelse(latinCheck(location_name) > 0 ,1,0))

write.csv(bdf,"POI/POI_cleaned.csv")

# see Language Schools

# check location names, brand_ids

### Check if name contains common Latin American words/food identifiers/countries
# substrings: taqueria, taco, empanada, huevo, pollo, burrito, arepa, pupusa, tamale, tortilla
# salsa, asado, lechon, mojo, ropa, vieja, chorizo, 
# countries: mexic, bolivia, chile, argentin, venezuela, beliz, costa rica, salvador
# guatemala, hondur, nicaragua, panama, brazil, colombia, ecuador, guyana, paragua, peru
# surinam, urugu, cuba, dominican, haiti, puerto, latin
# words: la, de, como, su, que, el, para, en, por, los, casa, caliente
latinCheck <- function(names, spec = 1) {
  
  output <- 0
  
  # spec:
  # == 1 : everythting
  # == 2 : no food
  
  patterns <- c('latin', 'mexic', 'bolivia', 'chile', 'argentin', 
                'venezuela', 'beliz', 'costa rica', 'salvador', 'guatemala', 'hondur',
                'nicaragua', 'panama', 'brazil', 'colombia', 'ecuador', 'guyana', 'paragua',
                'peru', 'surinam', 'urugu', 'cuba', 'dominican', 'haiti', 'puerto',
                '^la ', ' la ', '^de ', ' de ', '^como ', 'como ', '^su ', ' su ',
                ' que ', '^el ', ' el ', '^para ', ' para ', '^en ', ' en ',
                '^por ', ' por ', '^los ', ' los ', '^casa ', ' casa ',
                '^caliente ', ' caliente ')
  if (spec == 2) {
    patterns <- c(patterns,'taqueria', 'taco', 'empanada', 'huevo', 'pollo', 'burrito', 'arepa',
                  'pupusa', 'tamale', 'tortilla', 'salsa', 'asado', 'lechon', 'mojo', 'ropa',
                  'vieja', 'chorizo')
  }
  for (p in patterns) {
    output <- output + grepl(p, names, ignore.case=TRUE) 
  }
  
  
  return(output) 
}

