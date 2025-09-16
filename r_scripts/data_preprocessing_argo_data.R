#######################################################################
#################### Real data application ############################
#######################################################################

## Argo floats data
######packages:
library(devtools)
# install.packages("argoFloats")
# install.packages("lubridate")
library("argoFloats")
library(oce)
library(ggplot2)
library(dplyr)
library(rlang)
# library(Hmisc)
library("ncdf4")
library(geosphere)

library(fields)
# library(ggpubr)
library(RColorBrewer)
library(geoR)
# library(latex2exp)
library(sp)
library(mapdata)
library(ggmap)
library(sf)

if (!dir.exists("raw_datasets/argo")){
  dir.create("raw_datasets/argo")
}

#library(maptools)
# setwd(args[1])
# ######Extracting the data:
# ## 1. Get worldwide float-profile index, saving to ~/data/argo by default.
indexAll <- getIndex(destdir="raw_datasets/argo")
index <- subset(indexAll,
                circle=list(longitude=-40,latitude=50,radius=2000))
index.y = subset(index, time=list(from="2020-01-01", to="2020-01-30"))
profiles = getProfiles(index.y,destdir="raw_datasets/argo")
argos = readProfiles(profiles,destdir="raw_datasets/argo")

argosClean = applyQC(argos)


num_loc = length(argosClean@data$argos)
lon <- c()
lat <- c()
pres <- c()
temp <- c()
sali <- c()

the_argo <- argosClean@data$argos[[1]]@data
len_argo <- length(argosClean@data$argos[[1]]@data$pressure)
lon <- cbind(lon,rep(argosClean@data$argos[[1]]@data$longitude[1],len_argo))
lat <- cbind(lat,rep(argosClean@data$argos[[1]]@data$latitude[1],len_argo))
pres <- cbind(pres, c(argosClean@data$argos[[1]]@data$pressureAdjusted) )
temp <- cbind(temp, c(argosClean@data$argos[[1]]@data$temperatureAdjusted))
sali <- cbind(sali, c(argosClean@data$argos[[1]]@data$salinityAdjusted))
sum = len_argo
for(i in 2:num_loc){
  the_argo <- argosClean@data$argos[[i]]@data
  len_argo <- length(argosClean@data$argos[[i]]@data$pressure)
  lon <- rbind(lon,matrix(rep(argosClean@data$argos[[i]]@data$longitude[1],len_argo),
                          nrow = len_argo, byrow = FALSE))
  lat <- rbind(lat, matrix(rep(argosClean@data$argos[[i]]@data$latitude[1],len_argo),
                           nrow = len_argo, byrow = FALSE))
  pres <- rbind(pres, matrix(c(argosClean@data$argos[[i]]@data$pressureAdjusted[,1]),
                             nrow = len_argo, byrow = FALSE) )
  temp <- rbind(temp, matrix(c(argosClean@data$argos[[i]]@data$temperatureAdjusted[,1]),
                             nrow = len_argo, byrow = FALSE))
  sali <- rbind(sali, matrix(c(argosClean@data$argos[[i]]@data$salinityAdjusted[,1]),
                             nrow = len_argo, byrow = FALSE))
  # print(len_argo)
  sum = sum + len_argo
}

df = data.frame(lon = lon, lat = lat, pres = pres , temp = temp, sali = sali )
df = na.omit(df)

write.csv(df, file="raw_datasets/argo_data_full.csv", row.names = F)
#

df = read.csv("raw_datasets/argo_data_full.csv", header = T)
# lon.lat = unique(df[,c(1,2)])
# dmat<-distm(lon.lat)/1000*.62
new_df = data.frame(matrix(ncol = 5, nrow = 0))
cols = c("lon","lat","mean_pres","mean_temp","mean_sali")
colnames(new_df) = cols



unique.lonlat = unique(df[,c(1,2)])
for(i in 1:dim(unique.lonlat)[1]){
  df1 = subset(df, subset=(lon==unique.lonlat$lon[i]&lat==unique.lonlat$lat[i]))
  df1$group = cut_number(df1$pres, 10)
  df1_new = df1 %>%
    group_by(group) %>%
    mutate(mean_pres = mean(pres), mean_temp = mean(temp),
           mean_sali = mean(sali)) %>%
    data.frame()
  
  df1_new = df1_new[!duplicated(df1_new$mean_pres), ]
  drops = c("pres","temp","sali", "group")
  df1_new = df1_new[ , !(names(df1_new) %in% drops)]
  new_df = rbind(new_df,df1_new)
}
new_df = new_df[sample(1:dim(new_df)[1]),]
# print(head(new_df))
write.csv(new_df, file="raw_datasets/argo_data_subset.csv", row.names = F)

normalize = function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
new_df$lon = normalize(new_df$lon)
new_df$lat = normalize(new_df$lat)
new_df$mean_pres = normalize(new_df$mean_pres)

index = sample(1:dim(new_df)[1],5000)
train_df = new_df[index,c(1,2,3,4)]
test_df_locs = new_df[-index,c(1,2,3)]
test_df_data = new_df[-index,c(4)]
new_df1 <- new_df
# print(head(new_df1))
# write.csv(new_df1, file="raw_datasets/argo3D.csv", row.names = F)
write.csv(test_df_locs, file="raw_datasets/test_locs_argo.csv", row.names = F)
write.csv(test_df_data, file="raw_datasets/test_data_argo.csv", row.names = F)


## train-test data for single location
new_df = read.csv("raw_datasets/argo_data_subset.csv", header = T)
new_df = new_df[,c(1,2,3,4)]
unique.lonlat = unique(new_df[,c(1,2)])
min_lon = min(unique.lonlat$lon)
max_lon = max(unique.lonlat$lon)

min_lat = min(unique.lonlat$lat)
max_lat = max(unique.lonlat$lat)

#### choice of 4 points for testing 
excluded_locs = data.frame(lon = c(-60.05200,-29.99300,-33.61000 ),
                           lat = c(40.43000,61.80200,50.00200 ))

train_df = new_df %>%
  filter(!(lon %in% excluded_locs$lon & lat %in% excluded_locs$lat))
test_df = new_df %>%
  filter(lon %in% excluded_locs$lon & lat %in% excluded_locs$lat)
normalize = function(x,mini,maxi, na.rm = TRUE) {
  return((x- mini) /(maxi-mini))
}
standardize = function(x,mn,var, na.rm = TRUE) return((x-mn)/var)

train_df$lon = normalize(train_df$lon,min(new_df$lon),max(new_df$lon))
train_df$lat = normalize(train_df$lat,min(new_df$lat),max(new_df$lat))
train_df$mean_pres = normalize(train_df$mean_pres,min(new_df$mean_pres),
                               max(new_df$mean_pres))
train_df$mean_temp = standardize(train_df$mean_temp, mean(new_df$mean_temp),
                                 sd(new_df$mean_temp))

test_df$lon = normalize(test_df$lon,min(new_df$lon),max(new_df$lon))
test_df$lat = normalize(test_df$lat,min(new_df$lat),max(new_df$lat))
test_df$mean_pres = normalize(test_df$mean_pres,min(new_df$mean_pres),
                               max(new_df$mean_pres))
test_df$mean_temp = standardize(test_df$mean_temp, mean(new_df$mean_temp),
                                 sd(new_df$mean_temp))
test.lonlat = unique(test_df[,c(1,2)])
test_locs = data.frame(matrix(nrow = 3000, ncol =3))
colnames(test_locs) = c("s1","s2","s3")
test_locs$s1 = c(rep(test.lonlat$lon[1],1000),rep(test.lonlat$lon[2],1000),
                 rep(test.lonlat$lon[3],1000))
test_locs$s2 = c(rep(test.lonlat$lat[1],1000),rep(test.lonlat$lat[2],1000),
                 rep(test.lonlat$lat[3],1000))
test_locs$s3 = c(seq(0,1,length.out = 1000),seq(0,1,length.out = 1000),
                 seq(0,1,length.out = 1000))

write.csv(train_df, file="raw_datasets/argo3D.csv", row.names = F)
write.csv(test_locs, 
          file="raw_datasets/test_locs_single-coordinate.csv", row.names = F)
write.csv(test_df, 
          file="raw_datasets/test_data_single-coordinate.csv", row.names = F)




#### test data with pressure levels 0.1, 0.5, 0.9
library(spData)
lon = seq(min_lon,max_lon, length.out = 50)
lat = seq(min_lat,max_lat, length.out = 50)

d1 = expand.grid(lon = lon, lat = lat)
pts = st_as_sf(d1, coords=1:2, crs=4326)
ii = is.na(as.numeric(st_intersects(pts, world)))
d1$is_oscen = ii
d1 = subset(d1, is_oscen == TRUE)

test_df = data.frame(lon = c(d1$lon,d1$lon,d1$lon), 
                     lat = c(d1$lat, d1$lat, d1$lat),
                     pres = c(rep(0.1,dim(d1)[1]),rep(0.5,dim(d1)[1]),rep(0.9,dim(d1)[1])))
test_df$lon = normalize(test_df$lon,min(new_df$lon),max(new_df$lon))
test_df$lat = normalize(test_df$lat,min(new_df$lat),max(new_df$lat))
write.csv(test_df[,c(1,2,3)], 
          file="raw_datasets/test_locs_single-pres.csv", row.names = F)                                                                                                 
