
# distance from population-weighted centroids to streams


library(sf)
#library(mapview)

trls = trails
trls$id = 1:nrow(trls) # make sure to have unique id to trace selected features later
brew = st_transform(breweries, st_crs(trls)) # make sure crs is the same

brew_w_nearest_trail = st_join(brew, trls, join = st_nearest_feature)

dists = st_distance(brew, trls[brew_w_nearest_trail$id, ], by_element = TRUE)
