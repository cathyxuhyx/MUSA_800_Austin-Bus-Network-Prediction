#load packages
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(sf)
library(gridExtra)
library(RColorBrewer)
library(osmdata)
library(tidycensus)
library(areal)
library(viridis)
library(lubridate)
library(ggrepel)
library(rgeos)


mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = NA, fill=NA, size=2)
  )
}

# change the directory in order to load the data
agg <- read.csv('D:/Spring20/Practicum/data/MUSA Data - Stop Ridership Aggregated.csv')
Routes1801 <- st_read("D:/Spring20/Practicum/data/Jan2018/Routes.shp")
disagg <- read.csv('D:/Spring20/Practicum/data/MUSA Disagregated Data Sample 01-06-2020 to 01-10-2020.csv')
austin <- st_read('https://data.austintexas.gov/api/geospatial/3pzb-6mbr?method=export&format=GeoJSON')
serviceArea <- st_read('D:/Spring20/Practicum/data/June2018/Service_Area.shp')%>%
  st_transform(2278)
NewRoutes <- st_read('D:/Spring20/Practicum/data/NewRoutes.shp')
HighFreq <- st_read('D:/Spring20/Practicum/data/HighFrequency.shp')
Replaced <- st_read('D:/Spring20/Practicum/data/EliminatedReplacement.shp')
Eliminated <- st_read('D:/Spring20/Practicum/data/Eliminated.shp')
Routes2001 <- st_read('D:/Spring20/Practicum/data/Routes.shp')
#stops <- st_read('D:/Spring20/Practicum/data/Stops.shp')
stops <- st_read("D:/Spring20/Practicum/data/Stops.shp")%>%
  st_transform(2278)

Routes1801 <- Routes1801%>%
  mutate(capremap = "Before Cap Remap")

Routes2001 <- Routes2001%>%
  mutate(capremap = "After Cap Remap")%>%
  st_transform(2278)

#new scale function
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
serviceArea <- serviceArea%>%
  st_transform(2278)

stops <- stops%>%
  st_transform(2278)

austin <- austin%>%
  st_transform(2278)


counties <- counties%>%
  st_transform(2278)

cities <- cities%>%
  st_transform(2278)


counties <- subset(counties, COUNTY == "WILLIAMSON" | COUNTY == "TRAVIS")
cities <- subset(cities, MUNI_NM == "AUSTIN" | MUNI_NM == "JONESTOWN"|MUNI_NM == "LAGO VISTA"|MUNI_NM =="LEANDER"|MUNI_NM =="MANOR"|
                   MUNI_NM == "POINT VENTURE"|MUNI_NM =="SAN LEANNA"|MUNI_NM =="ROUND ROCK"|MUNI_NM =="VOLENTE")


#turn dataframe into spacitial object
agg_sf <- agg%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)%>%
  st_transform(2278)

disagg_sf <- disagg%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)%>%
  st_transform(2278)

stops_sf <- Stops%>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)%>%
  st_transform(2278)

#divide agg_sf data into before and after capremap
agg_before_sf <- agg_sf%>%
  filter(YEAR_ID == 2017 | (YEAR_ID == 2018 & MONTH_ID < 6))%>%
  group_by(STOP_ID)%>%
  summarise(avg_on = mean(AVERAGE_ON),
            avg_trips = mean(TRIPS))%>%
  mutate(ratio = avg_on / avg_trips)

agg_after_sf <- agg_sf%>%
  filter(YEAR_ID == 2019 | (YEAR_ID == 2018 & MONTH_ID > 5))%>%
  group_by(STOP_ID)%>%
  summarise(avg_on = mean(AVERAGE_ON),
            avg_trips = mean(TRIPS))%>%
  mutate(ratio = avg_on / avg_trips)%>%
  st_transform(2278)


#scatterplot
Before_scatter <- ggplot()+
  geom_point(data = agg_before_sf, aes(x=avg_trips, y=avg_on, color = STOP_ID))+
  labs(title = "Ridership vs. Number of Trips",
       subtitle = "Before CapRemap",
       xlab = "Average Ridership",
       ylab = "Average Trips")
After_scatter <-  ggplot()+
  geom_point(data = agg_after_sf, aes(x=avg_trips, y=avg_on, color = STOP_ID))+
  labs(title = "Ridership vs. Number of Trips",
       subtitle = "After CapRemap",
       xlab = "Average Ridership",
       ylab = "Average Trips")

Scatterplots <- grid.arrange(Before_scatter,After_scatter,ncol = 2)

#mapping the stops by ridership

#doesn't show clear patterns when plotting all the stops
ggplot()+
  geom_sf(mapping = aes(), data = austin)+
  geom_sf(mapping = aes(color = ratio), data = agg_before_sf)+
  scale_colour_gradient(low = "light blue", high = "darkblue")


#below 25% as low ridership whereas higher than 75% as high ridership
quantile(agg_before_sf$ratio)
quantile(agg_after_sf$ratio)


Low_before <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_before_sf, ratio<0.06505824), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Low ridership stops before CapRemap")
Low_after <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_after_sf, ratio<0.03767514), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Low ridership stops after CapRemap")

Low_Compare <- grid.arrange(Low_before, Low_after, ncol = 2)


High_before <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_before_sf, ratio>0.43755896), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Low ridership stops before CapRemap")

High_after <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_after_sf, ratio>0.41095547), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Low ridership stops after CapRemap")

High_Compare <- grid.arrange(High_before, High_after, ncol = 2)

Zero_before <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_before_sf, ratio==0), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Stops without Any Ridership before CapRemap")

Zero_after <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_after_sf, ratio==0), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Stops without Any Ridership before CapRemap")

Zero_Compare <- grid.arrange(Zero_before, Zero_after, ncol = 2)

ggplot()+
  geom_sf(data = Stops, aes(colour = STOP_ID))+
  geom_sf(data = Eliminated)
#overview map
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = Eliminated, aes(color = "Eliminated Routes"), lwd = 0.3, show.legend = "line")+
  geom_sf(data = Replaced, aes(color = "Eliminated but Replaced Routes"),lwd = 0.3, show.legend = "line")+
  geom_sf(data = HighFreq, aes(color = "High Frequency Routes"), lwd = 0.8, show.legend = "line")+
  geom_sf(data = NewRoutes, aes(color = "New Routes"),lwd = 0.8, show.legend = "line")+
  scale_colour_manual(values = c("Eliminated Routes" = "darkorange", "Eliminated but Replaced Routes" = "gold", "High Frequency Routes" = "dodgerblue", "New Routes" = "deeppink"),
                      guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid", "solid", "solid"), shape = c(NA, NA, NA, NA))))+
  labs(title = "Cap Remap Route Changes",
       subtitle = "City of Austin, June 2018")


#types of routes
#local
ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  #geom_sf(data = subset(Routes1801, ROUTETYPE == "Local"), color = "lightblue2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Local"), color = "lightblue2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "lightblue2", "After Cap Remap" = "lightblue2"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  #facet_grid(~capremap)+
  labs(title = "Local Routes Before and After Cap Remap")+
  mapTheme()

#HighFrequency

ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  #geom_sf(data = subset(Routes1801, ROUTETYPE == "High Frequency"), color = "dodgerblue",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "High Frequency"), color = "dodgerblue",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "dodgerblue", "After Cap Remap" = "dodgerblue"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  #facet_grid(~capremap)+
  labs(title = "High Frequency Routes Before and After Cap Remap")+
  mapTheme()

#major changes grid arrange
grid.arrange(local, highFrequency, ncol = 1)

#Crosstown

ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  #geom_sf(data = subset(Routes1801, ROUTETYPE == "Crosstown"), color = "greenyellow",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Crosstown"), color = "greenyellow",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "greenyellow", "After Cap Remap" = "greenyellow"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  #facet_grid(~capremap)+
  labs(title = "Crosstown Routes Before and After Cap Remap")+
  mapTheme()

#Feeder
ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  #geom_sf(data = subset(Routes1801, ROUTETYPE == "Feeder"), color = "lightcoral",lwd = 0.5, show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Feeder"), color = "lightcoral",lwd = 0.5, show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "lightcoral", "After Cap Remap" = "lightcoral"))+
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  #facet_grid(~capremap)+
  labs(title = "Feeder Routes Before and After Cap Remap")+
  mapTheme()


#Flyer
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Flyer"), color = "magenta2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Flyer"), color = "magenta2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "magenta2", "After Cap Remap" = "magenta2"),
                     # guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Flyer Routes Before and After Cap Remap")

#Express
ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  #geom_sf(data = subset(Routes1801, ROUTETYPE == "Express"), color = "red",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Express"), color = "red",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "red", "After Cap Remap" = "red"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  #facet_grid(~capremap)+
  labs(title = "Express Routes Before and After Cap Remap")+
  mapTheme()

#minor changes grid arrange

grid.arrange(crosstown, feeder, flyer, express, special,ncol =2)


#UT Shuttle
ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  #geom_sf(data = subset(Routes1801, ROUTETYPE == "UT Shuttle"), color = "orange",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "UT Shuttle"), color = "orange",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "orange", "After Cap Remap" = "orange"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  #facet_grid(~capremap)+
  labs(title = "UT Shuttle Before and After Cap Remap")+
  mapTheme()

#Special
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Special"), color = "seashell2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Special"), color = "seashell2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "seashell2", "After Cap Remap" = "seashell2"),
   #                   guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Speical Routes Before and After Cap Remap")

#Nigh Owl
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Night Owl"), color = "slategray2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Night Owl"), color = "slategray2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "slategray2", "After Cap Remap" = "slategray2"),
   #                   guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Nigh Owl Routes Before and After Cap Remap")

#no change grid arrange
grid.arrange(UTShuttle, special, nightowl, ncol = 1)

#####OSM#####
getOSM <- function(key,value){
  feature <- opq(bbox = 'Austin, Texas')%>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf ()
  if(is.null(feature$osm_points)){
    feature_poly <- feature$osm_polygons%>%
      select(osm_id,geometry)%>%
      st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
      st_transform(2278)
    return(feature_poly)
  } else {
  feature_pt <- feature$osm_points%>%
    select(osm_id,geometry)%>%
    st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
    st_transform(2278)
  return (feature_pt)
  }
}

#shop
shop <- opq(bbox = 'Austin, Texas')%>%
  add_osm_feature(key = 'shop') %>%
  osmdata_sf ()

shop_pt <- shop$osm_points

shop_pt <- shop_pt%>%
  select(osm_id,
         name,
         addr.city,
         geometry)%>%
  st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
  st_transform(2278)

#university
university <- opq(bbox = 'Austin, Texas')%>%
  add_osm_feature(key = 'amenity',value = 'university') %>%
  osmdata_sf ()

university <- university$osm_polygons%>%
  select(geometry)%>%
  st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
  st_transform(2278)

shop_pt <- shop_pt%>%
  select(osm_id,
         name,
         addr.city,
         geometry)%>%
  st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
  st_transform(2278)

#commercial
commercial <- getOSM('building', 'commercial')
#retail
retail <- getOSM('building', 'retail')
#supermarket
supermkt <- getOSM('building', 'supermarket')
#office
office <- getOSM('building', 'office')
#residential
residential <- getOSM('building','residential')
#bar
bar <- getOSM('amenity', 'bar')
#school
school <- getOSM('amenity', 'school')
#uni
university <- getOSM('amenity', 'university')
#parking
parking <- getOSM('amenity', 'parking')
#statium
stadium <- getOSM('building', 'stadium')
#trainstation
trainstation <- getOSM('building', 'train_station')

#####buffer#####
#1/8 mile
StopBuff3 <- stops%>%
  st_buffer(330)

#1/8 mile
StopBuff0 <- stops%>%
  st_buffer(660)

#1/4 mile
StopBuff <- stops%>%
  st_buffer(1320)

#1/2 mile
StopBuff2 <- stops%>%
  st_buffer(2640)


######spatial join#####
bufferInit <- function(Buffer, Points, Name){
  if(class(Points$geometry) == "sfc_POINT"){
  Init <- st_join(Buffer%>% select(STOP_ID), Points, join = st_contains)%>%
  group_by(STOP_ID)%>%
    summarize(count = n())%>%
    rename(!!Name := count)
  }else {
    Init <- st_join(Buffer%>% select(STOP_ID), Points, join = st_intersects)%>%
      group_by(STOP_ID)%>%
      summarize(count = n())%>%
      rename(!!Name := count)
  }
}


#1/4 mile buffer OSM data
CommercialInit <- bufferInit(StopBuff, commercial, 'commercial_count')
RetailInit <- bufferInit(StopBuff, retail, 'retail_count')
OfficeInit <- bufferInit(StopBuff, office, 'office_count')
ResidentialInit <- bufferInit(StopBuff, residential, 'residential_count')
SupermktInit <- bufferInit(StopBuff, supermkt, 'supermkt_count')
BarInit <- bufferInit(StopBuff, bar, 'bar_count')
UniInit <- bufferInit(StopBuff, university, 'university_count')
ParkingInit <- bufferInit(StopBuff, parking, 'parking_count')
SchoolInit <- bufferInit(StopBuff, school, 'school_count')
StationInit <- bufferInit(StopBuff, trainstation, 'station_count')
StadiumInit <- bufferInit(StopBuff, stadium, 'stadium_count')

#1/2 mile buffer OSM data
CommercialInit2 <- bufferInit(StopBuff2, commercial, 'commercial_count')
RetailInit2 <- bufferInit(StopBuff2, retail, 'retail_count')
OfficeInit2 <- bufferInit(StopBuff2, office, 'office_count')
ResidentialInit2 <- bufferInit(StopBuff2, residential, 'residential_count')
SupermktInit2 <- bufferInit(StopBuff2, supermkt, 'supermkt_count')
BarInit2 <- bufferInit(StopBuff2, bar, 'bar_count')
UniInit2 <- bufferInit(StopBuff2, university, 'university_count')
ParkingInit2 <- bufferInit(StopBuff2, parking, 'parking_count')
SchoolInit2 <- bufferInit(StopBuff2, school, 'school_count')
StationInit2 <- bufferInit(StopBuff2, trainstation, 'station_count')
StadiumInit2 <- bufferInit(StopBuff2, stadium, 'stadium_count')

#1/8 mile buffer OSM data
CommercialInit0 <- bufferInit(StopBuff0, commercial, 'commercial_count')
RetailInit0 <- bufferInit(StopBuff0, retail, 'retail_count')
OfficeInit0 <- bufferInit(StopBuff0, office, 'office_count')
ResidentialInit0 <- bufferInit(StopBuff0, residential, 'residential_count')
SupermktInit0 <- bufferInit(StopBuff0, supermkt, 'supermkt_count')
BarInit0 <- bufferInit(StopBuff0, bar, 'bar_count')
UniInit0 <- bufferInit(StopBuff0, university, 'university_count')
ParkingInit0 <- bufferInit(StopBuff0, parking, 'parking_count')
SchoolInit0 <- bufferInit(StopBuff0, school, 'school_count')
StationInit0 <- bufferInit(StopBuff0, trainstation, 'station_count')
StadiumInit0 <- bufferInit(StopBuff0, stadium, 'stadium_count')

#1/16 mile buffer OSM data
CommercialInit3 <- bufferInit(StopBuff3, commercial, 'commercial_count')
RetailInit3 <- bufferInit(StopBuff3, retail, 'retail_count')
OfficeInit3 <- bufferInit(StopBuff3, office, 'office_count')
ResidentialInit3 <- bufferInit(StopBuff3, residential, 'residential_count')
SupermktInit3 <- bufferInit(StopBuff3, supermkt, 'supermkt_count')
BarInit3 <- bufferInit(StopBuff3, bar, 'bar_count')
UniInit3 <- bufferInit(StopBuff3, university, 'university_count')
ParkingInit3 <- bufferInit(StopBuff3, parking, 'parking_count')
SchoolInit3 <- bufferInit(StopBuff3, school, 'school_count')
StationInit3 <- bufferInit(StopBuff3, trainstation, 'station_count')
StadiumInit3 <- bufferInit(StopBuff3, stadium, 'stadium_count')


#plot OSM
plotOSM <- function(OSM)
  ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL#,
                    #guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))
  ) +
  #geom_sf(data=parking,inherit.aes =FALSE,colour="#E4C054",
  #fill="#004529",alpha=.2,size=0.8)+
  geom_sf(data = stops, aes(color = UniInit$university_count),size = 0.8) +
  #geom_sf(data = SchoolInit)+
  mapTheme()

#####census#####
options(tigris_use_cache = TRUE)
v17 <- load_variables(2017, "acs5", cache = TRUE)

Hays <- get_acs(state = "48", county = "209", geography = "tract", 
                variables = "B01001_001", geometry = TRUE)
Travis <- get_acs(state = "48", county = "453", geography = "tract", 
                  variables = "B01001_001", geometry = TRUE)
Williamson <- get_acs(state = "48", county = "491", geography = "tract", 
                      variables = "B01001_001", geometry = TRUE) 

Travis_race <- get_acs(state = "48", county = "453", geography = "tract", 
                       variables = "B02001_002", geometry = TRUE)
Williamson_race <- get_acs(state = "48", county = "491", geography = "tract", 
                           variables = "B02001_002", geometry = TRUE) 

Travis_noveh <- get_acs(state = "48", county = "453", geography = "tract", 
                        variables = "B08014_002", geometry = TRUE)
Williamson_noveh <- get_acs(state = "48", county = "491", geography = "tract", 
                            variables = "B08014_002", geometry = TRUE)

Travis_oneveh <- get_acs(state = "48", county = "453", geography = "tract", 
                        variables = "B08014_003", geometry = TRUE)
Williamson_oneveh <- get_acs(state = "48", county = "491", geography = "tract", 
                            variables = "B08014_003", geometry = TRUE)

Travis_twoveh <- get_acs(state = "48", county = "453", geography = "tract", 
                         variables = "B08014_004", geometry = TRUE)
Williamson_twoveh <- get_acs(state = "48", county = "491", geography = "tract", 
                             variables = "B08014_004", geometry = TRUE)

Travis_threeveh <- get_acs(state = "48", county = "453", geography = "tract", 
                         variables = "B08014_005", geometry = TRUE)
Williamson_threeveh <- get_acs(state = "48", county = "491", geography = "tract", 
                             variables = "B08014_005", geometry = TRUE)

Travis_fourveh <- get_acs(state = "48", county = "453", geography = "tract", 
                           variables = "B08014_006", geometry = TRUE)
Williamson_fourveh <- get_acs(state = "48", county = "491", geography = "tract", 
                               variables = "B08014_006", geometry = TRUE)

Travis_fiveveh <- get_acs(state = "48", county = "453", geography = "tract", 
                          variables = "B08014_007", geometry = TRUE)
Williamson_fiveveh <- get_acs(state = "48", county = "491", geography = "tract", 
                              variables = "B08014_007", geometry = TRUE)

Travis_poverty <- get_acs(state = "48", county = "453", geography = "tract", 
                          variables = "B06012_002", geometry = TRUE)
Williamson_poverty <- get_acs(state = "48", county = "491", geography = "tract", 
                              variables = "B06012_002", geometry = TRUE)

#####buffer deomographics#####
#population
Population <- rbind(Travis, Williamson)%>%
  st_transform(2278)

Population_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = Population, sid = GEOID, weight = "sum",
                                  output = "sf", extensive = "estimate")
Population_buff$estimate<- round(Population_buff$estimate)

Population_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = Population, sid = GEOID, weight = "sum",
                                  output = "sf", extensive = "estimate")
Population_buff0$estimate <- round(Population_buff0$estimate)

Population_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = Population, sid = GEOID, weight = "sum",
                                  output = "sf", extensive = "estimate")
Population_buff2$estimate <- round(Population_buff2$estimate)

Population_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = Population, sid = GEOID, weight = "sum",
                                  output = "sf", extensive = "estimate")
Population_buff3$estimate<- round(Population_buff3$estimate)

#race
Race <- rbind(Travis_race, Williamson_race)%>%
  st_transform(2278)

Race_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = Race, sid = GEOID, weight = "sum",
                            output = "sf", extensive = "estimate")
Race_buff$estimate <- round(Race_buff$estimate)

Race_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = Race, sid = GEOID, weight = "sum",
                            output = "sf", extensive = "estimate")
Race_buff0$estimate <- round(Race_buff0$estimate)

Race_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = Race, sid = GEOID, weight = "sum",
                            output = "sf", extensive = "estimate")
Race_buff2$estimate <- round(Race_buff2$estimate)

Race_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = Race, sid = GEOID, weight = "sum",
                            output = "sf", extensive = "estimate")
Race_buff3$estimate <- round(Race_buff3$estimate)

#vehicle ownership
NoVeh <- rbind(Travis_noveh, Williamson_noveh)%>%
  st_transform(2278)

NoVeh_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = NoVeh, sid = GEOID, weight = "sum",
                             output = "sf", extensive = "estimate")
NoVeh_buff$estimate <- round(NoVeh_buff$estimate)

NoVeh_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = NoVeh, sid = GEOID, weight = "sum",
                             output = "sf", extensive = "estimate")
NoVeh_buff0$estimate <- round(NoVeh_buff0$estimate)

NoVeh_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = NoVeh, sid = GEOID, weight = "sum",
                             output = "sf", extensive = "estimate")
NoVeh_buff2$estimate <- round(NoVeh_buff2$estimate)

NoVeh_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = NoVeh, sid = GEOID, weight = "sum",
                             output = "sf", extensive = "estimate")
NoVeh_buff3$estimate <- round(NoVeh_buff3$estimate)

OneVeh <- rbind(Travis_oneveh, Williamson_oneveh)%>%
  st_transform(2278)

OneVeh_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = OneVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
OneVeh_buff$estimate <- round(OneVeh_buff$estimate)

OneVeh_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = OneVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
OneVeh_buff0$estimate <- round(OneVeh_buff0$estimate)

OneVeh_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = OneVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
OneVeh_buff2$estimate <- round(OneVeh_buff2$estimate)

OneVeh_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = OneVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
OneVeh_buff3$estimate <- round(OneVeh_buff3$estimate)

TwoVeh <- rbind(Travis_twoveh, Williamson_twoveh)%>%
  st_transform(2278)

TwoVeh_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = TwoVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
TwoVeh_buff$estimate <- round(TwoVeh_buff$estimate)

TwoVeh_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = TwoVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
TwoVeh_buff0$estimate <- round(TwoVeh_buff0$estimate)

TwoVeh_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = TwoVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
TwoVeh_buff2$estimate <- round(TwoVeh_buff2$estimate)

TwoVeh_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = TwoVeh, sid = GEOID, weight = "sum",
                              output = "sf", extensive = "estimate")
TwoVeh_buff3$estimate <- round(TwoVeh_buff3$estimate)

ThreeVeh <- rbind(Travis_threeveh, Williamson_threeveh)%>%
  st_transform(2278)

ThreeVeh_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = ThreeVeh, sid = GEOID, weight = "sum",
                                output = "sf", extensive = "estimate")
ThreeVeh_buff$estimate <- round(ThreeVeh_buff$estimate)

ThreeVeh_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = ThreeVeh, sid = GEOID, weight = "sum",
                                output = "sf", extensive = "estimate")
ThreeVeh_buff0$estimate <- round(ThreeVeh_buff0$estimate)

ThreeVeh_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = ThreeVeh, sid = GEOID, weight = "sum",
                                output = "sf", extensive = "estimate")
ThreeVeh_buff2$estimate <- round(ThreeVeh_buff2$estimate)

ThreeVeh_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = ThreeVeh, sid = GEOID, weight = "sum",
                                output = "sf", extensive = "estimate")
ThreeVeh_buff3$estimate <- round(ThreeVeh_buff3$estimate)

FourVeh <- rbind(Travis_fourveh, Williamson_fourveh)%>%
  st_transform(2278)

FourVeh_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = FourVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FourVeh_buff$estimate <- round(FourVeh_buff$estimate)

FourVeh_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = FourVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FourVeh_buff0$estimate <- round(FourVeh_buff0$estimate)

FourVeh_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = FourVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FourVeh_buff2$estimate <- round(FourVeh_buff2$estimate)

FourVeh_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = FourVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FourVeh_buff3$estimate <- round(FourVeh_buff3$estimate)


FiveVeh <- rbind(Travis_fiveveh, Williamson_fiveveh)%>%
  st_transform(2278)

FiveVeh_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = FiveVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FiveVeh_buff$estimate <- round(FiveVeh_buff$estimate)

FiveVeh_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = FiveVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FiveVeh_buff0$estimate <- round(FiveVeh_buff0$estimate)

FiveVeh_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = FiveVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FiveVeh_buff2$estimate <- round(FiveVeh_buff2$estimate)

FiveVeh_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = FiveVeh, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
FiveVeh_buff3$estimate <- round(FiveVeh_buff3$estimate)

#poverty
Poverty <- rbind(Travis_poverty, Williamson_poverty)%>%
  st_transform(2278)

Poverty_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = Poverty, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
Poverty_buff$estimate <- round(Poverty_buff$estimate)

Poverty_buff0 <- aw_interpolate(StopBuff0, tid = STOP_ID, source = Poverty, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
Poverty_buff0$estimate <- round(Poverty_buff0$estimate)

Poverty_buff2 <- aw_interpolate(StopBuff2, tid = STOP_ID, source = Poverty, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
Poverty_buff2$estimate <- round(Poverty_buff2$estimate)

Poverty_buff3 <- aw_interpolate(StopBuff3, tid = STOP_ID, source = Poverty, sid = GEOID, weight = "sum",
                               output = "sf", extensive = "estimate")
Poverty_buff3$estimate <- round(Poverty_buff3$estimate)

#####Time Lag#####
disagg$ACT_STOP_TIME <- as.character(disagg$ACT_STOP_TIME)

disagg <- disagg%>%
  mutate(interval60 = floor_date(mdy_hm(ACT_STOP_TIME), unit = "hour"),
         interval15 = floor_date(mdy_hm(ACT_STOP_TIME), unit = "15 mins"))

study.panel <- 
  expand.grid(interval60=unique(disagg$interval60), 
              STOP_ID = unique(disagg$STOP_ID))

disagg.panel <- disagg%>%
  right_join(study.panel)%>%
  group_by(interval60, STOP_ID)%>%
  summarize(avg_on = mean(PSGR_ON))

disagg.timelag <- 
  disagg.panel %>% 
  arrange(STOP_ID, interval60) %>% 
  mutate(lagHour = dplyr::lag(avg_on,1),
         lag2Hours = dplyr::lag(avg_on,2),
         lag3Hours = dplyr::lag(avg_on,3),
         lag4Hours = dplyr::lag(avg_on,4),
         lag12Hours = dplyr::lag(avg_on,12),
         lag1day = dplyr::lag(avg_on,24))


#Building Area Feature Engineering
#Create the polygon buffer function
bufferPoly <- function(Buffer, Polygons, Name){
  if(class(Polygons$geometry) == "sfc_POLYGON"){
    Poly <- st_join(Buffer%>% select(STOP_ID), Polygons, join = st_intersects)%>%
      group_by(STOP_ID)%>%
      summarize(area = sum(Total_area))%>%
      rename(!!Name := area)
  }
  #else {
  #  Poly <- st_join(Buffer%>% select(STOP_ID), Polygons, join = st_intersects)%>%
  #    group_by(STOP_ID)%>%
  #    summarize(count = n())%>%
  #    rename(!!Name := count)
  #}
}

#Import building footprint shapefile
Buildings <- 
  st_read("C:/Users/HanyongXu/Documents/Me/grad/Spring_2020/MUSA801/Data/building_footprints_2017/building_footprints_2017.shp")%>%
  st_transform(2278) 
#Import Stop shp
stops <-
  st_read("C:/Upenn/Practicum/Data/Shapefiles_-_JUNE_2018/Stops.shp") %>%
  st_transform(2278)

#Create columns of the num of floors and total building areas
Buildings$Floor <- round(Buildings$MAX_HEIGHT/10)
Buildings$Total_area <- Buildings$Floor * Buildings$SHAPE_AREA

AreaPoly <- bufferPoly(StopBuff, Buildings, 'building_area')
AreaPoly$geometry <- NULL

AreaPoly2 <- bufferPoly(StopBuff2, Buildings, 'building_area')

write.csv(AreaPoly, "C:/Upenn/Practicum/Data/Building_Area2.csv")

write.csv(AreaPoly2, "C:/Upenn/Practicum/Data/Building_Area2.csv")

?merge

Austin.sf <- merge(AreaPoly, data.2018, by= "STOP_ID", all.x=TRUE)

Austin.sf$geometry <- NULL
Austin.sf<- Austin.sf[-c(25158,25159,25160,25161,25162,25163,25164,35408,37475,37476,37477,37478,37479,37480,37481,39140,39153,39292,42526,42527,42528,42529,42530,42531,
                         42532,42533,42534,44292,44293,45249,45250,48882,48883,48915,48916), ]
which(is.na(Austin.sf$LONGITUDE))

Austin.sf <- 
  Austin.sf %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant") %>%
  st_transform(2278)

q5 <- function(variable) {as.factor(ntile(variable, 5))}
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

library(wesanderson)
palette5 <- wes_palette("Moonrise3", n = 5)
palette5 <- 
  mapTheme <- function(base_size = 12) {
    theme(
      text = element_text( color = "black"),
      plot.title = element_text(size = 14,colour = "black"),
      plot.subtitle=element_text(face="italic"),
      plot.caption=element_text(hjust=0),
      axis.ticks = element_blank(),
      panel.background = element_blank(),axis.title = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=2)
    )
  }

ggplot() +
  geom_sf(data = nhood, fill = "grey40") +
  geom_sf(data = Austin.sf, aes(colour = q5(building_area)), show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(Austin.sf,"building_area"),
                      name="Quintile\nBreaks") +
  labs(title="Building Area within 1/4 Mile Buffer from each Stop") +
  mapTheme()


#####Data Structure#####
#We use aggregated data to look at the average ridership on weekdays at individual stops
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea,NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA)))+
  geom_sf(data = subset(agg_after_sf, STOP_ID == 476), aes(color = "Stop 476"), size = 2, show.legend = "point")+
  scale_colour_manual(values = c("Stop 476" = "darkorange"),
                      guide = guide_legend("Aggregated Data Example"))+
  labs(title = "Aggregated Data Structure",
       subtitle = "Data from Capital Metro")+
  ggrepel::geom_label_repel(
    data = subset(agg_after_sf, STOP_ID == 476),aes(label = "Average Ridership = 33 \n Average Passing Buses = 55", geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 3)

#We use disaggregated data to investigate the average ridership on weekdays on different routes.
disagg_803 <- subset(disagg_sf, ROUTE == 803)%>%
  group_by(STOP_ID)%>%
  summarize(avg_on = mean(PSGR_ON),
            avg_load = mean(PSGR_LOAD))
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea,NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA)))+
  geom_sf(data = disagg_803, aes(color = "Stops on Route 803"), size = 2, show.legend = "point")+
  scale_colour_manual(values = c("Stops on Route 803" = "darkorange"),
                      guide = guide_legend("Disggregated Data Example"))+
  labs(title = "Disaggregated Data Structure",
       subtitle = "Data from Capital Metro")+
  geom_label_repel(
    data = subset(disagg_803, STOP_ID == 2606),aes(label = "Average On-board Passengers of Stop 2606 = 11 \n Route Type = Metro Rapid", geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    segment.color = "lightgrey",
    point.padding = 20)

ggplot()+
  #geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea,NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA)))+
  mapTheme()
 
  

ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = agg_after_sf, aes(color = "darkorange"))

#####Exploratory Analysis - Typology#####
final <- read.csv("D:/Spring20/Practicum/data/all_2.csv")

agg <- read.csv("D:/Spring20/Practicum/data/Final_Aggregate.csv")
agg_nhood <- read.csv("D:/Spring20/Practicum/data/Rider_nhood4.csv")

nhood <- st_read("https://data.austintexas.gov/resource/nz5f-3t2e.geojson")%>%
  st_set_crs(4326)%>%
  st_transform(2278)


agg_nhood_sf <- left_join(agg_nhood, nhood, by = "label" )%>%
  st_as_sf()
  

agg_sf <- agg%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)%>%
  st_transform(2278)

agg_sf19 <- agg_sf%>%
  filter(YEAR_ID == 2019)%>%
  group_by(STOP_ID)%>%
  summarize(avg_on = mean(AVERAGE_ON))

agg_sf19_nhood <- st_join(nhood, agg_sf19, join = st_contains)
  
agg_sf19_nhood <- agg_sf19_nhood%>%
  group_by(label.x)%>%
  summarize(avg_on = mean(avg_on))

agg_before_sf <- agg_sf%>%
  filter(agg_sf$Before == 1)%>%
  group_by(STOP_ID, MONTH_ID)%>%
  summarise(mean_on = mean(AVERAGE_ON))%>%
  as.data.frame()

agg_after_sf <- agg_sf%>%
  filter(agg_sf$Before == 0)%>%
  group_by(STOP_ID, MONTH_ID)%>%
  summarise(mean_on = mean(AVERAGE_ON))%>%
  as.data.frame()

agg_dif_sf <- left_join(agg_before_sf, agg_after_sf, by = c("STOP_ID", "MONTH_ID"))

schoolDist <- st_read("D:/Spring20/Practicum/data/Trustee.shp")%>%
  st_transform(2278)

agg_sf19_sch <- st_join(schoolDist, agg_sf19, join = st_contains)

agg_sf19_sch <- agg_sf19_sch%>%
  group_by(TRUSTEE)%>%
  summarize(avg_on = mean(avg_on))


UT <- st_read("D:/Spring20/Practicum/data/UTAustin/UT1.shp")%>%
  st_transform(2278)%>%
  select(geometry)

st_write(UT, "D:/Spring20/Practicum/data/UTAustin/UT.shp")

nhood_merge  <- st_read("D:/Spring20/Practicum/data/nhood_merge.shp")%>%
  st_as_sf(4326)%>%
  st_transform(2278)

CBD <- st_read("D:/Spring20/Practicum/data/CBD/CBD.shp")%>%
  st_transform(2278)

nhood_merge <- st_union(nhood)%>%
  st_as_sf()%>%
  st_transform(2278)

nhood_CBD <- st_difference(nhood_merge, CBD)

#ST_DIFFERENCE didnt work for UT
nhood_UT <- st_read("D:/Spring20/Practicum/data/nhood_UT.shp")%>%
  st_as_sf()%>%
  st_transform(2278)

ggplot()+
  geom_sf(data = nhood_UT)

agg_sf19_CBD <- st_join(CBD, agg_sf19, join = st_contains)%>%
  mutate(typology = "CBD")

agg_sf19_CBD <- agg_sf19_CBD%>%
  group_by(Id)%>%
  summarize(avg_on = mean(avg_on))%>%
  mutate(label = "CBD")

agg_sf19_oCBD <- st_join(nhood_CBD, agg_sf19, join = st_contains)%>%
  mutate(typology = "oCBD")%>%
  rename(geometry = x)

agg_sf19_oCBD <- agg_sf19_oCBD%>%
  group_by(Id)%>%
  summarize(avg_on = mean(avg_on))%>%
  mutate(label = "The Rest of Austin")

agg_CBD <- rbind(agg_sf19_CBD,agg_sf19_oCBD)
agg_CBD_typology <- rbind(agg_sf19_CBD,agg_sf19_oCBD)

agg_sf19_UT <- st_join(UT, agg_sf19, join = st_contains)%>%
  mutate(typology = "UT")

agg_sf19_UT <- agg_sf19_UT%>%
  mutate(Id = 0)%>%
  group_by(Id)%>%
  summarize(avg_on = mean(na.omit(avg_on)))%>%
  mutate(label = "UT Austin")

agg_sf19_oUT <- st_join(nhood_UT, agg_sf19, join = st_contains)%>%
  mutate(typology = "oUT")%>%
  select(STOP_ID,
         avg_on,
         typology,
         geometry)

ggplot()+
  geom_sf(data= agg_CBD, aes(fill = label))+
  geom_sf(data = nhood_UT)

ggplot()+
  geom_sf(data =agg_UT)
agg_sf19_oUT <- agg_sf19_oUT%>%
  mutate(Id = 0)%>%
  group_by(Id)%>%
  summarize(avg_on = mean(na.omit(avg_on)))%>%
  mutate(label = "The Rest of Austin")

agg_UT <- rbind(agg_sf19_UT,agg_sf19_oUT)
agg_UT_typology <- rbind(agg_sf19_UT,agg_sf19_oUT)

#Ridership by neighborhoods
ggplot() +
  geom_sf(data = agg_sf19_nhood, aes(fill = q5(avg_on))) +
  scale_fill_viridis(labels=qBr(agg_sf19_nhood,"avg_on"),
                     name="Ridership \nQuintile Breaks in 2019", discrete = TRUE) +
  #geom_sf(data = subset(nhood,nhood$label == "Downtown"), aes(color = "Downtown", fill = NA), lwd = 1.2)+
  #scale_color_manual(values = c("Downtown" = "purple"),
                    #guide = guide_legend(title = "", overide.aes = list(linetype = 2, shape = 15, lwd = 1)))+
  labs(title="Ridership by Neighborhoods in 2019") +
  mapTheme()

#Ridership by School Districts
ggplot() +
  geom_sf(data = agg_sf19_sch, aes(fill = avg_on)) +
  scale_fill_viridis(name="Ridership in 2019", discrete = FALSE) +
  labs(title="Ridership by School Districts in 2019") +
  mapTheme()


#UT vs. other areas
ggplot() +
  geom_sf(data = subset(agg_UT,label = "UT Austin"), aes(fill = as.factor(avg_on))) +
  geom_sf(data = subset(agg_UT,label = "The Rest of Austin"), aes(fill = as.factor(avg_on))) +
  scale_fill_manual(values = c("#440154", "#FDE725"), name = "Ridership in 2019\nThe Rest of Austin vs. UT Austin")+
  #guide = guide_legend(" ", override.aes = list(linetype = "blank", shape = NA))) +
  labs(title="Ridership in 2019 - The Rest of Austin VS. UT Austin") +
  mapTheme()

#CBD vs. other areas
ggplot() +
  geom_sf(data = subset(agg_CBD,label = "CBD"), aes(fill = as.factor(avg_on))) +
  geom_sf(data = subset(agg_CBD,label = "The Rest of Austin"), aes(fill = as.factor(avg_on))) +
  scale_fill_manual(values = c("#440154", "#FDE725"), name = "Ridership in 2019\nThe Rest of Austin vs. CBD")+
                    #guide = guide_legend(" ", override.aes = list(linetype = "blank", shape = NA))) +
  labs(title="Ridership in 2019 - The Rest of Austin VS. CBD") +
  mapTheme()

#landuse
landuse <- st_read("D:/Spring20/Practicum/data/landuse.shp")%>%
  st_transform(2278)

#schoolDist
schoolDist_sf <- final%>%
  select(STOP_ID,
         mean_on,
         TRUSTEE)%>%
  left_join(., schoolDist, by = "TRUSTEE")%>%
  st_as_sf()%>%
  st_transform(2278)%>%
  mutate(dif = agg_after_sf)
