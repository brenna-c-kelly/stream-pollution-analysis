
## stream data prep
##  TRI-polluted streams

library(sf)
library(tmap)
library(units)
library(rgdal)
library(tidyr)
library(dplyr)
library(foreign)
library(stringr)
library(ggplot2)
library(viridis)
library(tidycensus)
getwd()

#load("data/nhd_tri.RData")
str(nhd_tri)

nhd_streams <- st_read("data/WBD_Subwatershed.shp") # HUC data
#   ^for non-polluted streams and length

### getting administrative unit geometry
counties <- get_decennial(geography = "county",
                          variables = c("P5_001N"),
                          geometry = TRUE,
                          year = 2020) %>%
  spread(variable, value) %>%
  group_by(GEOID) %>%
  select(-c(P5_001N)) %>%
  mutate(state = substr(GEOID, start = 0, stop = 2)) %>%
  filter(!state %in% c("02", "15", "72")) # excluding noncontiguous

names(counties) <- tolower(names(counties))


list.files("NHD Micro Results/NHDMicroResults_OnsiteCore_1988/NHDMicroResults_OnsiteCore_1988.csv")


# three things are needed
#   • stream geometry (polluted and non-polluted)
#       Source:
#   • stream flow and length (flow approximates depth/width)
#       Source: NHDPlus National Data (EPA)
#       https://www.epa.gov/waterdata/nhdplus-national-data
#   • stream toxicity
#       Source:

# stream geometry
nhd_streams <- st_read("data/WBD_Subwatershed.shp")


# stream toxicity
read.csv("NHD Micro Results/NHDMicroResults_OnsiteCore_1988/NHDMicroResults_OnsiteCore_1988.csv")
list.files("NHD Micro Results/")
tox_files <- list.files("NHD Micro Results")
st_layers("NHD Micro Results/NHDMicroResults_OnsiteCore_1988")
st_layers(paste0("NHD Micro Results/", "NHDMicroResults_OnsiteCore_", 1988, "/", tox_files[1]))

streams_all <- st_read(paste0(getwd(), "/NHD Micro Results/", tox_files[1], "/", tox_files[1], ".shp"))
streams_all$year <- NA
streams_all <- streams_all[0, ]
#streams <- st_read(paste0("NHD Micro Results/", tox_files[2], "/", tox_files[2], ".shp"))

names(streams)
for(i in 1:length(tox_files)) {
  streams <- st_read(paste0(getwd(), "/NHD Micro Results/", tox_files[i], "/", tox_files[i], ".shp"))
  names(streams) <- tolower(names(streams))
  streams$year <- substr(tox_files[i], start = nchar(tox_files[i]) - 3, stop = nchar(tox_files[i]))
  streams_all <- rbind(streams_all, streams)
  #assign(paste0("streams_", i) , streams)
}

head(streams_all)
streams_all$toxconc <- as.numeric(streams_all$toxconc)

streams_2019 <- streams_all %>%
  filter(year == 2019)

# reproject
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
streams_2019 <- st_as_sf(streams_2019)
streams_2019 <- st_transform(streams_2020, aea)



streams_2019$toxconc_log <- log(streams_2019$toxconc)
tm_shape(streams_2019) +
  tm_lines(col = "toxconc_log", palette = "-plasma", style = "cont", midpoint = NA)

#   • stream flow and length
#...

flow <- st_read("data/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb",
                "NHDFlowline_Network")

nhd <- st_read("/Users/brenna/Documents/School/Thesis/stream-pollution-analysis/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb",
               "NHDFlowline_Network")

names(streams_all)
head(tri_streams)

#nhd <- nhd %>% #first prepare to merge with polluted streams, then filer
#  filter(FTYPE %in% c("StreamRiver", "Coastline")) #only natural water segments

#tm_shape(nhd) +
#  tm_lines(col = "LENGTHKM")

#all streams, including non-polluted
#flow <- subset(nhd, select = -c(60:138))
summary(flow$QC_MA) #stream flow
summary(flow$VC_MA) #stream velocity
#flow$VC_MA[flow$VC_MA < 0 ] <- NA
#summary(flow$LENGTHKM) #stream length

max(streams_all$comid)

#polluted streams
#tri_streams <- st_read("/Users/brenna/Documents/School/Thesis/Data/NHD Micro Results/NHDMicroResults_OnsiteCore_2020")
#names(tri_streams)
names(streams_all)
# reach codes should be 14 characters long
streams_all$reachcode <- str_pad(streams_all$reachcode, 14, pad = "0")
# comid should be 9 characters long
streams_all$comid <- str_pad(streams_all$comid, 9, pad = "0")

# drop geometry before join
tri_streams_nogeom <- st_drop_geometry(tri_streams)
summary(tri_streams$TOXCONC)
nhd_tri <- merge(flow, tri_streams_nogeom, by = "reachcode", all = T) #adding toxicity to stream data

nhd_tri$unit <- substr(nhd_tri$REACHCODE, start = 1, stop = 2)
nhd_tri$FTYPE <- as.factor(nhd_tri$FTYPE)
table(nhd_tri$TOXCONC > 0)
nhd_tri$polluted[!is.na(nhd_tri$TOXCONC)] <- 1
nhd_tri$polluted[is.na(nhd_tri$TOXCONC)] <- 0
nhd_tri$polluted <- as.factor(nhd_tri$polluted)
ftable(nhd_tri$FTYPE, nhd_tri$polluted)
prop.table(ftable(nhd_tri$FTYPE, nhd_tri$polluted))

aggregate(nhd_tri$VC_MA, by = list(nhd_tri$polluted), FUN = mean, na.rm = TRUE)
ftable(nhd_tri$polluted, is.na(nhd_tri$VC_MA))
23888 / (118701 + 23888) # 17 percent of polluted streams don't have velocity
304579 / (304579 + 2314915) # 12 percent of non-polluted streams don't have velocity
prop.table(table(is.na(nhd_tri$VC_MA))) # 12 percent of streams (total) don't have velocity

ftable(nhd_tri$polluted, is.na(nhd_tri$QC))
#take all streams without 
ftable(is.na(nhd_tri$QC_MA), nhd_tri$FTYPE)
    # it's mostly artificial paths and coastline that don't have velocity
ftable(is.na(flow$VC_MA))
ftable(is.na(nhd_tri$VC_MA))

#nhd_tri_test <- nhd_tri %>%
#  filter(!unit %in% c("03"))



table(is.na(flow$VC_MA))
table(is.na(test$TOXCONC))
names(tri_streams)
# %>%
  #filter(TOXCONC > 0) %>%
  #filter(is.na(VC_MA))
summary(flow$LENGTHKM)
table(nhd_tri$unit)

test <- nhd_tri %>%
  filter(unit == "03")
test$missing[is.na(test$VC_MA)] <- 0
test$missing[is.na(test$missing)] <- 1
test$missing <- as.factor(test$missing)
summary(nhd_tri)
head(tx_tri)
test$TOXCONC <- as.numeric(test$TOXCONC) #making toxconc numeric
test$TOXCONC <- ifelse(is.na(test$TOXCONC), 0, test$TOXCONC) #if missing, toxconc is 0
table(nhd$FTYPE)
test <- st_as_sf(test)
test$TOXCONC_log <- log(test$TOXCONC)

tm_shape(test) +
  tm_lines(col = "missing", midpoint = NA)
tm_shape(test) +
  tm_lines(col = "TOXCONC", palette = "YlGnBu", midpoint = NA)


### cleaning
# filter non-continental
nhd_tri <- nhd_tri %>%
  filter(!unit %in% c("20", "19", "21")) #HI, AK, Caribbean
# cleaning toxicity
hist(nhd_tri$TOXCONC)
nhd_tri$TOXCONC <- as.numeric(nhd_tri$TOXCONC)
nhd_tri$TOXCONC <- ifelse(is.na(nhd_tri$TOXCONC), 0, nhd_tri$TOXCONC) # if no pollution, tox conc = 0



# proj4
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
counties <- st_transform(counties, crs = st_crs(aea))
nhd_tri <- st_transform(nhd_tri, crs = st_crs(aea))
names(counties) <- tolower(names(counties))
counties$state <- substr(counties$geoid, start = 1, stop = 2)
table(counties$state)

counties <- counties %>%
  filter(!state %in% c("72", "15", "02")) #puerto rico, hawaii, alaska

tm_shape(counties) +
  tm_polygons(col = "total_pop", lwd = 0, palette = "YlGnBu")

remove(flow, nhd, tri_streams, tri_streams_nogeom) #make room please

counties_test <- st_zm(counties, drop = TRUE) # dropping z dimensions
nhd_tri_test <- st_zm(nhd_tri, drop = TRUE)
#counties_nhd <- st_join(nhd_tri_test, counties_test, join = st_intersects) #Identifies if x and y geometry share any space
str(counties_nhd)

counties_nhd <- st_intersection(nhd_tri_test, counties_test)

length(unique(counties_nhd$REACHCODE))

counties_nhd$length_bycounty <- st_length(counties_nhd)

#spot check
longest <- counties_nhd %>%
  filter(LENGTHKM == max(counties_nhd$LENGTHKM))
tm_shape(longest) +
  tm_lines(col = "length_bycounty")
#confirm that all stream segments have a county
table(is.na(counties_nhd$total_pop))

ftable(counties_nhd$geoid == "31005")

##    Weighting
#aggregate just with mean
no_wt <- aggregate(counties_nhd$TOXCONC, by = list(counties_nhd$geoid), FUN = mean, na.rm = TRUE)

#weighted by length
counties_nhd$length_bycounty <- drop_units(counties_nhd$length_bycounty)
wt_km <- counties_nhd %>%
  group_by(geoid) %>%
  summarise(geoid_sum = sum(length_bycounty), 
            toxicity_mean_km = weighted.mean(TOXCONC, length_bycounty))

#weighted by flow
wt_flow <- counties_nhd %>%
  group_by(geoid) %>%
  summarise(geoid_sum = sum(QC_MA), 
            toxicity_mean_flow = weighted.mean(TOXCONC, QC_MA))

#weighted by length and flow
counties_nhd$flow_length <- counties_nhd$QC_MA * counties_nhd$length_bycounty
wt_flow_length <- counties_nhd %>%
  group_by(geoid) %>%
  summarise(geoid_sum = sum(flow_length), 
            toxicity_km_flow = weighted.mean(TOXCONC, flow_length))

summary(no_wt$x) #no weights
summary(wt_km$toxicity_mean_km) #weighted by length
summary(wt_flow$toxicity_mean_flow) #weighted by flow
summary(wt_flow_length$toxicity_km_flow) #weighted by length and flow

#########

plot(density(log(wt_flow$toxicity_mean_flow)), col = "blue", lwd = 1.5, main = "Log-transformed toxicity")
lines(density(log(wt_km$toxicity_mean_km)), col = "red", lwd = 1.5)
lines(density(log(wt_flow_length$toxicity_km_flow)), col = "purple", lwd = 1.5)
lines(density(log(no_wt$x)), lwd = 1.5, main = "mean")


no_wt_density <- density(log(no_wt$x))
wt_flow_length_density <- density(log(wt_flow_km$toxicity_km_flow))

all_marg <- data.frame(x = seq(min(no_wt$x), max(no_wt$x), length.out = 512))
all_marg$x <- approx(wt_flow_length_density$x, wt_flow_length_density$y, 
                      no_wt_density$x, 
                          yleft = 0, yright = 0)$y
marg <- all_marg
marg <- approx(wt_flow_length_density$x, wt_flow_length_density$y, 
               no_wt_density$y, 
                          yleft = 0, yright = 0)
## Joint density
marg$joint <- pmin(marg$x, marg$y)
marg_df <- data.frame(marg$x, marg$y, marg$joint)
names(marg_df) <- c("x", "y", "joint")

## Credibility
(sum(marg$joint)/sum(marg$x) + 
    sum(marg$joint)/sum(marg$y))/2
# 0.6289097
## suspect:
plot(marg$joint)
plot(marg$x)
plot(marg$y)
lines(marg$joint, col = "red")

##### pick up here

test_wts <- wt_flow_length
test_wts$toxicity_nozeroes <- test_wts$toxicity_mean_flow
test_wts$toxicity_nozeroes[test_wts$toxicity_nozeroes == 0] <- NA
test_wts <- test_wts %>%
  filter(!is.na(toxicity_nozeroes))

plot(density(test_wts$toxicity_nozeroes), col = "purple", lwd = 1.5)

summary(wt_flow_length$toxicity_mean_flow)
summary(no_wt$x)
from <- 0
to <- 53450
wts_density <- density(wt_flow_length$toxicity_mean_flow, from = from, to = to)
nowts_density <- density(no_wt$x, from = from, to = to)

wt_flow_length_ <- wt_flow_length %>%
  filter(geoid != "31005")

library(approx)
library(mc2d)
test <- approx(no_wt$x, wt_flow_length_$toxicity_mean_flow, method = "linear")

joint_mean_both <- pmin(test$x, test$y)
(sum(joint_mean_both)/sum(test$x) + sum(joint_mean_both)/sum(test$y))/2

# 49% overlap between mean and km*flow weights
joint_mean_km <- pmin(no_wt$x, wt_km$toxicity_mean_km)
(sum(joint_mean_km)/sum(no_wt$x) + sum(joint_mean_km)/sum(wt_km$toxicity_mean_km))/2
# 86% overlap between mean and km weight
joint_mean_flow <- pmin(no_wt$x, wt_flow$toxicity_mean_flow)
(sum(joint_mean_flow)/sum(wt_flow$toxicity_mean_flow) + sum(joint_mean_flow)/sum(no_wt$x))/2
# 23% overlap between mean and flow weight **CHECK
joint_both_flow <- pmin(wt_km$toxicity_mean_km, wt_flow$toxicity_mean_flow)
(sum(joint_both_flow)/sum(wt_km$toxicity_mean_km) + sum(joint_both_flow)/sum(wt_flow$toxicity_mean_flow))/2

joint_mean_both <- pmin(no_wt$x, wt_flow_length$toxicity_mean_flow)
(sum(joint_mean_both)/sum(no_wt$x) + sum(joint_mean_both)/sum(wt_flow_length$toxicity_mean_flow))/2 

plot(density(log(wt_flow_length$toxicity_mean_flow)), col = "purple", lwd = 1.5)
lines(density(log(wt_flow$toxicity_mean_flow)), col = "blue", lwd = 1.5)

setdiff(counties$geoid, test$geoid)

###   Maps
wt_flow_length <- st_drop_geometry(wt_flow_length)
test1 <- merge(counties, wt_flow_length, by = "geoid")
test1$toxicity_mean_flow[test1$toxicity_mean_flow == 0] <- NA
test1$log_toxicity <- log(test1$toxicity_mean_flow)
#test1$log_toxicity[test1$toxicity_mean_flow == 1e-100] <- NA
summary(test1$log_toxicity)
flow_length_map <- tm_shape(test1) +
  tm_polygons(col = "log_toxicity", style = "cont", title = "Flow, length weighted", lwd = 0,
              legend.is.portrait = FALSE, palette = "-RdYlGn") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)

wt_flow <- st_drop_geometry(wt_flow)
test2 <- merge(counties, wt_flow, by = "geoid")
test2$log_toxicity <- log(test2$toxicity_mean_flow)
test2$log_toxicity[test2$toxicity_mean_flow == 1e-100] <- NA
flow_map <- tm_shape(test2) +
  tm_polygons(col = "log_toxicity", style = "cont", title = "Flow weighted", lwd = 0,
              legend.is.portrait = FALSE, palette = "-RdYlGn") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)

test3 <- merge(counties, no_wt, by.x = "geoid", by.y = "Group.1")
test3$log_toxicity <- log(test3$x)
test3$log_toxicity[test3$x == 1e-100] <- NA
mean_map <- tm_shape(test3) +
  tm_polygons(col = "log_toxicity", style = "cont", title = "Mean, no weights", lwd = 0,
              legend.is.portrait = FALSE, palette = "-RdYlGn") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)

wt_km <- st_drop_geometry(wt_km)
test4 <- merge(counties, wt_km, by = "geoid")
test4$log_toxicity <- log(test4$toxicity_mean_km)
test4$log_toxicity[test4$toxicity_mean_km == 1e-100] <- NA
length_map <- tm_shape(test4) +
  tm_polygons(col = "log_toxicity", style = "cont", title = "Length weighted", lwd = 0,
              legend.is.portrait = FALSE, palette = "-RdYlGn") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)

current.mode <- tmap_mode("plot")
tmap_arrange(mean_map, length_map, flow_map, flow_length_map,
             ncol = 2, nrow = 2)
tmap_mode(current.mode)


### BLACK/PURPLE: 42.65% overlap between mean and flow weight
# define limits of a common grid, adding a buffer so that tails aren't cut off
lower <- min(c(no_wt$x, wt_flow_length$toxicity_mean_flow)) - 1 
upper <- max(c(no_wt$x, wt_flow_length$toxicity_mean_flow)) + 1
# generate kernel densities
da <- density(no_wt$x, from=lower, to=upper)
db <- density(wt_flow_length$toxicity_mean_flow, from=lower, to=upper)
d <- data.frame(x=da$x, a=da$y, b=db$y)
# calculate intersection densities
d$w <- pmin(d$a, d$b)
# integrate areas under curves
library(sfsmisc)
total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
intersection <- integrate.xy(d$x, d$w)
# compute overlap coefficient
overlap <- 2 * intersection / total
overlap

library(dplyr)

roane <- wt_flow_length %>%
  filter(geoid == "54087")
head(roane)

names(weights)

st_write(counties_nhd, "counties_nhd.shp")
st_write(weights, "weights.shp")

weights_test <- st_read("/Users/brenna/Documents/School/GEOG 6960/thesis weights")
names(weights_test)

summary(weights)

weights <- cbind(no_wt, wt_km, wt_flow, wt_flow_length)

str(no_wt)
summary(no_wt$x) #no weights
summary(wt_km$toxicity_mean_km) #weighted by length
summary(wt_flow$toxicity_mean_flow) #weighted by flow
summary(wt_flow_length$toxicity_km_flow) #weighted by length and flow




