


### Wenchman analysis of SEAMAP data

### 4) Remove erroneous points
library(sp)
library(leaflet)
### 1) Import STAREC table
setwd("C:/Users/JohnF/OneDrive - Gulf of Mexico Fishery Mgmt Council/Documents/SEAMAP/Wenchman/public_seamap_csvs")
starec <- read.csv("STAREC.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
bgsrec <- read.csv("BGSREC.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

### 2) Make Spatial
## Georeference
starec$lat <- starec$S_LATD + (starec$S_LATM/60)
starec$long <- (starec$S_LOND + (starec$S_LONM/60)) * -1
starec$year <- as.numeric(as.character(substr(starec$START_DATE,1,4)))
starec$month<- as.numeric(as.character(substr(starec$START_DATE,6,7)))

### remove NA's for lat and long

starec <- subset(starec, lat!="NA")

#removing lat/ongs with na's
starec2 <-  starec[!(is.na(starec$lat)),]




starec2.coords <- data.frame(x=starec2$lon, y=starec2$lat)
coordinates(starec2.coords) <- ~ x + y
class(starec2.coords)
### Assign albers projection
proj4string(starec2.coords)  <- CRS("+proj=longlat +datum=WGS84")  ## for example
plot(starec2.coords, axes=TRUE) ## sanity check


library(leaflet)
leaflet(starec2.coords) %>% 
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>% 
  addCircleMarkers(color = "#FFFF00", radius=3)


#####################
##### merge starec2 with bgsrec

## subset to wenchman  and butter fishfirst
##peprilus burti ## butterfish
##Pristipomoides aquilonaris ## wenchman

### get species codes from bgsrec
Species <- data.frame(unique(bgsrec$SPEC_BGS))
Species2 <- Species[order(Species$unique.bgsrec.SPEC_BGS.), ]
Species2 <- data.frame(Species2)
### BGSREC to wide then merge?


## SUBSET TO AQUILO & BURTI
#df <- subset(bgsrec, SPEC_BGS=="AQUILO" | SPEC_BGS=="BURTI")
df <- subset(bgsrec, SPEC_BGS=="AQUILO" )
### merge df with starec2
df2 <- merge(starec2, df, by=c("CRUISEID","STATIONID", "CRUISE_NO"),
            all=TRUE)



# 
 library(reshape2)
df3 <- dcast(df2, CRUISEID + STATIONID + CRUISE_NO + lat + long  + START_DATE +
               END_DATE + DEPTH_ESTA + GEARS + TEMP_BOT + TEMP_SSURF + 
               TEMP_SAIR + VESSEL_SPD + FAUN_ZONE + STAT_ZONE +
               year + month + HAULVALUE
             ~ SPEC_BGS, 
             sum, value.var="CNT", fill=0)

df4 <-  df3[!(is.na(df3$lat)),]


POS <- subset(df4, AQUILO > 0)
NEG <- subset(df4, AQUILO == 0)

##############map the POS data


POS.coords <- data.frame(x=POS$lon, y=POS$lat)
coordinates(POS.coords) <- ~ x + y
class(POS.coords)
### Assign albers projection
proj4string(POS.coords)  <- CRS("+proj=longlat +datum=WGS84")  ## for example
plot(POS.coords, axes=TRUE) ## sanity check

NEG.coords <- data.frame(x=NEG$lon, y=NEG$lat)
coordinates(NEG.coords) <- ~ x + y
class(NEG.coords)
### Assign albers projection
proj4string(NEG.coords)  <- CRS("+proj=longlat +datum=WGS84")  ## for example
plot(NEG.coords, axes=TRUE) ## sanity check


library(leaflet)
leaflet(POS.coords) %>% 
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>% 
  addCircleMarkers(color = "#FFFF00", radius=3)


leaflet() %>% 
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>% 
  addCircleMarkers(data=NEG.coords,color = "#9370DB", radius=1, group="Zero") %>% 
  addCircleMarkers(data=POS.coords,color = "#FFFF00", radius=3, group="Wenchmen") %>% 
  addLayersControl(
    overlayGroups = c("Wenchmen", "Zero"),
    options = layersControlOptions(collapsed = FALSE)
  )




################### Not used###########################
             
             unique + GEARS +
#                 DECSLAT + DECSLON +
#                 DEPTH_ESTA +
#                 TIME_EMIL +
#                 GEAR_SIZE + 
#                 MESH_SIZE +
#                 MO_DAY_YR + 
#                 SURVEY +
#                 DEPTH_SSTA
#               ~ TAXONOMIC, sum, value.var="TOTCNT", fill=0) 


 

## Convert to SpatialPointsDataFrame
df.coords2 <- df.coords
df.coords3 <- SpatialPointsDataFrame(df.coords2, data=df.pos[,c(1,2,5)])
### Export object for backup.  Will need to test percent of points in final polygon
writeOGR( df.coords3, layer="level",
          "C:/EFHreview/Data/Large_environmental_database_kernel_density/shp_95_albers/test_points.shp",
          driver="ESRI Shapefile",
          overwrite_layer=TRUE)

m4 <- merge(m3, PENAEUSbgs, by=c("CRUISEID","STATIONID", "CRUISE_NO"),
            all.x=TRUE)
## Section 3:


##

## Improve the dataset
df <- data.frame(
  CRUISEID = m4$CRUISEID,
  STATIONID = m4$STATIONID,
  GEARS = m4$GEARS,
  DECSLAT = m4$DECSLAT,
  DECSLON = m4$DECSLON,
  CRUISE_NO = m4$CRUISE_NO,
  MO_DAY_YR = m4$MO_DAY_YR,
  YR = m4$YR,
  DEPTH_SSTA = m4$DEPTH_SSTA,
  DEPTH_ESTA = m4$DEPTH_ESTA,
  TEMP_SSURF = m4$TEMP_SSURF,
  TEMP_BOT = m4$TEMP_BOT,
  TEMPSURF = m4$TEMPSURF,
  TEMPMID = m4$TEMPMID,
  TEMPMAX = m4$TEMPMAX,
  SALSURF = m4$SALSURF,
  SALMID = m4$SALMID,
  SALMAX = m4$SALMAX,
  CHLORSURF = m4$CHLORSURF,
  CHLORMID = m4$CHLORMID,
  CHLORMAX = m4$CHLORMAX,
  OXYSURF = m4$OXYSURF,
  OXYMID = m4$OXYMID,
  OXYMAX = m4$OXYMAX,
  GENUS_BGS = m4$GENUS_BGS,
  SPEC_BGS = m4$SPEC_BGS,
  CNT = m4$CNT,
  CNTEXP = m4$CNTEXP
)