library(dplyr)
library(network)
library(maps)
library(sf)

mob<-readxl::read_excel("mat_bin_125.xlsx")

#convert to dataframe
mat <- as.data.frame(mob)

#use stack to reorder columns
mob_df <- data.frame(start= mat$col,stack(mat,select=-col)) %>%
  mutate(end=ind,trips=values) %>%
  select(-ind,-values)%>%
  mutate(start=as.character(start),end=as.character(end)) %>% 
  glimpse()

#filter out gmsp
#census tracts of Sao Paulo metro area
muni_sp<-geobr::read_municipality(year=2018,simplified=T,code_muni="SP")
metro_sp_munis <- geobr::read_metro_area(year=2018)
metro_sp_munis <- subset(metro_sp_munis, name_metro == 'RM São Paulo')$code_muni

#extract data for gmsp
gmsp_df<-subset(muni_sp,code_muni %in% metro_sp_munis) 

mob_gmsp<-subset(mob_df, start %in% metro_sp_munis & end %in% metro_sp_munis ) %>%
  glimpse()

#mob_mat<-matrix(sapply(mob_gmsp$trips, as.numeric), nrow=39,byrow = TRUE) 

#match coordinates with network data
#first we need to calculate lat/long of centroids of shapefile  
gmsp_df$centroids <- st_transform(gmsp_df, 29101) %>% 
  st_centroid() %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

muni_sp$centroids <- st_transform(muni_sp, 29101) %>% 
  st_centroid() %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

# plot to check the location of centroid points
plot(st_geometry(gmsp_df))
plot(st_set_geometry(gmsp_df, 'centroids')[, 0], add = T, col = 'red', pch = 19)

plot(st_geometry(muni_sp))
plot(st_set_geometry(muni_sp, 'centroids')[, 0], add = T, col = 'red', pch = 19)

#now convert centroids to lat/long coordinates 
gmsp_coord<-st_coordinates(gmsp_df$centroids) 

sp_coord_cent<-st_coordinates(muni_sp$centroids) 

#create  two df, one containing the lat/long of start location and another for the end location
gmsp_coord_start<-cbind(gmsp_coord,gmsp_df[,c("code_muni")])  %>%
  mutate(start=code_muni) %>%
  mutate(start=as.character(start)) %>%
  select(-code_muni) %>%
  glimpse()

gmsp_coord_end<-cbind(gmsp_coord,gmsp_df[,c("code_muni")])  %>%
  mutate(end=code_muni) %>%
  mutate(end=as.character(end)) %>%
  glimpse()

#now merge these coordinate columns with the trip data
coord_start<- left_join(mob_gmsp,gmsp_coord_start[,c("X","Y","start")],by="start") %>%
  mutate(lon.start=X,lat.start=Y) %>%
  select(-geom,-X,-Y) %>%
  glimpse()

coord_end<- left_join(mob_gmsp,gmsp_coord_end[,c("X","Y","end")],by="end") %>%
  mutate(lon.end=X,lat.end=Y) %>%
  select(-geom,-X,-Y) %>%
  glimpse()

#create final df containing start lat-long and end lat-long
coord_gmsp<-left_join(coord_start,coord_end[,c("lon.end","lat.end","end","start")],
                      by=c("start","end")) %>%
  filter(trips==1) %>%
  glimpse()


#sp
sp_coord_merge<-cbind(sp_coord_cent,muni_sp[,c("code_muni")])  %>%
  mutate(start=code_muni) %>%
  mutate(start=as.character(start)) %>%
  mutate(end=code_muni) %>%
  mutate(end=as.character(end)) %>%
  select(-code_muni) %>%
  glimpse()

#now merge these coordinate columns with the trip data
sp_coord_start<- left_join(mob_df,sp_coord_merge[,c("X","Y","start")],by="start") %>%
  mutate(lon.start=X,lat.start=Y) %>%
  select(-geom,-X,-Y) %>%
  glimpse()

sp_coord_end<- left_join(mob_df,sp_coord_merge[,c("X","Y","end")],by="end") %>%
  mutate(lon.end=X,lat.end=Y) %>%
  select(-geom,-X,-Y) %>%
  glimpse()

#create final df containing start lat-long and end lat-long
coord_sp<-left_join(sp_coord_start,sp_coord_end[,c("lon.end","lat.end","end","start")],
                      by=c("start","end")) %>%
  filter(trips==1) %>%
  glimpse()

#plot mobility network on map 
network_plot_mob<- ggplot() +
  geom_sf(data =  gmsp_df, #muni_sp,
               show.legend = FALSE,alpha = 0.25,
               color = "grey") +
  geom_segment(data = coord_gmsp, #coord_sp, 
               aes(x = lon.start, xend = lon.end,
                   y = lat.start, yend = lat.end),
               color="red", 
               size = 0.25,alpha = 0.5) +
  geom_point(data = data.frame(sp_coord_cent), #gmsp_coord
             aes(X, Y), color = "black",shape=1) +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        ) +
  labs(title = "Municipality network as defined in the adjacency matrix",
       subtitle="Threshold = 150 trips")

network_plot_mob

#====now plot network based on nearest neighbour adjacency matrix====
#first convert list (nb) into a matrix

num.el <- sapply(nb, length)
# Generate the neighbouring dataframe
res <- as.data.frame(cbind(unlist(nb), rep(1:length(nb), num.el))) %>%
  rename(muni.start=V1,muni.end=V2) 

#now we need to link location id with code_muni
muni_sp_link <- muni_sp %>%
  select(code_muni) %>%
  mutate(muni.start=1:nrow(.)) %>%
  mutate(muni.end=1:nrow(.)) %>%
  mutate(start=as.character(code_muni)) %>%
  mutate(end=as.character(code_muni)) %>%
  select(-code_muni) %>%
  glimpse()

muni_sp_nb<-left_join(res,muni_sp_link[c("start","muni.start")],by="muni.start") %>%
  left_join(muni_sp_link[,c("end","muni.end")],by="muni.end") %>%
  select(-geom.x,-geom.y) %>%
  glimpse()


#filter out munis from gmsp
nb_gmsp<-subset(muni_sp_nb, start %in% metro_sp_munis & end %in% metro_sp_munis ) %>%
  glimpse()

#now add coordinates
coord_start_nb<- left_join(nb_gmsp,gmsp_coord_start[,c("X","Y","start")],by="start") %>%
  mutate(lon.start=X,lat.start=Y) %>%
  select(-geom,-X,-Y) %>%
  glimpse()

coord_end_nb<- left_join(nb_gmsp,gmsp_coord_end[,c("X","Y","end")],by="end") %>%
  mutate(lon.end=X,lat.end=Y) %>%
  select(-geom,-X,-Y) %>%
  glimpse()

#create final df containing start lat-long and end lat-long
coord_gmsp_nb<-left_join(coord_start_nb,coord_end_nb
                         [,c("lon.end","lat.end","end","start")],
                      by=c("start","end")) %>%
  glimpse()

#sp
coord_start_nb_sp<- left_join(muni_sp_nb,sp_coord_start[,c("lon.start","lat.start","start","end")],by=c("start","end")) %>%
  glimpse()

coord_end_nb_sp<- left_join(muni_sp_nb,sp_coord_end[,c("lon.end","lat.end","start","end")],by=c("start","end")) %>%
  glimpse()

#create final df containing start lat-long and end lat-long
coord_sp_nb<-left_join(coord_start_nb_sp,coord_end_nb_sp
                         [,c("lon.end","lat.end","end","start")],
                         by=c("start","end")) %>%
  glimpse()


#plot
#plot mobility network on map 
network_plot_nb<- ggplot() +
  geom_sf(data = muni_sp, #gmsp_df
          show.legend = FALSE,alpha = 0.25,
          color = "grey") +
  geom_segment(data = coord_sp_nb, #coord_gmsp_nb
               aes(x = lon.start, xend = lon.end,
                   y = lat.start, yend = lat.end),
               color="red", 
               size = 0.25,alpha = 0.5) +
  geom_point(data = data.frame(sp_coord_cent), #gmsp_coord
             aes(X, Y), color = "black",shape=1) +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
  ) +
  labs(title = "Municipality network as defined in the adjacency matrix",
       subtitle="Nearest neighour")

network_plot_nb

