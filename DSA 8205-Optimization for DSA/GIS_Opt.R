#======================================== The relevant libraries =========================================================
#library(ggplot2)
theme_set(theme_bw())
library("sf")
library(igraph)
library("rnaturalearth")
library("rnaturalearthdata")
#library("rgeos")
library("ggspatial")
library("ggrepel")
library("tidyverse")
library(geosphere) 



#----------------------------------------------------simple graph--------------------------------------------------------#
g <- make_graph(edges =  c(1,2, 1, 3, 2,3,2,4,3,5,4,5), directed = TRUE)

E(g)$capacity <- c(15, 10, 25, 20, 30, 10)

print(data.frame(as_edgelist(g), capacity = E(g)$capacity))

#-------plot the network

plot(g, 
     edge.label = E(g)$capacity, 
     vertex.size = 30,
     edge.arrow.size = 0.5,
     main = "Network"
       )

#--------max flow and min cut

source <- 1
sink <- 5

max_flow <- max_flow(g, source = source, target = sink, capacity = E(g)$capacity)

cat("Maximum flow:",  max_flow$value, "\n")
#cat("Flow through each edge:\n")

#print(max_flow$flow)


min_cut <- min_cut(g, source = source, target = sink, capacity = E(g)$capacity)
cat("Min cut value:",  min_cut, "\n")

#================================= World data as dataframe ===============================================================

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#=============================== visualization  using ggplot2 ============================================================

world_map <- ggplot(data =  world) + geom_sf()

suppressWarnings(print(world_map))

#=========================== Creating dataset for longitude and latitude =================================================

lonlat <- read.table(textConnection(
  "lon, lat
12.1589441,49.0234716
14.3228984,48.2866839
 4.7538607,52.3056529
 6.5137089,53.2003132
 5.0578695,51.5867659
13.2326326,41.6497845
 9.3116100,45.5896400
 5.2929623,45.5871020
-1.2881266,51.6244096
-1.7317462,51.5647743
"),header=TRUE,strip.white = TRUE, sep=",")

#====================================== names of the places ==============================================================

nname = c("Name1","Name2","Name3","Name4","Name5","Name6","Name7","Name8","Name9","Name10")

#============================================resultant data frame ========================================================

v = data.frame(ids  = 1:10, name = nname, x  = lonlat$lon, y  = lonlat$lat)


#====================================== creating map for the dataset created =============================================

select_map <- ggplot(data = world) +   geom_sf(color="blue") + geom_point(data = v, aes(x = x, y = y)) +
  coord_sf(xlim = c(-2, 15), ylim = c(40, 55), expand = FALSE)
print(select_map)

#===================   converting our data v to an sf object for mapping =================================================

mst_data <- st_as_sf(v, coords = c("x", "y"), remove = FALSE, crs = 4326, agr = "constant")

#===== the above can also be done as follows===================

#================================ adding the points representing the cities ============================================

select_map + geom_sf(data = mst_data, color="red") + coord_sf(xlim = c(-2, 15), ylim = c(40, 55), expand = FALSE)



#======================= Adding the names of the coordinates ============================================================


select_map  +   geom_sf(data = mst_data) + geom_label_repel(data = mst_data, aes(x = x, y = y, label = name), 
                                                            color = "red", fontface = "bold", size = 3, force = 5)+ 
  coord_sf(xlim = c(-2, 15), ylim = c(40, 55), expand = FALSE) + xlab("Latitude") +ylab("Longitude")




#===============The distance matrix between nodes (MST) =================================================================


Distance <- distm(lonlat, lonlat, fun=distVincentyEllipsoid) # in meters


#======================Transform the matrix to an edge (list) and build the MST =========================================


Matrix_list <- function(Distance) {
  n = dim(Distance)[1]
  k <- 1
  e <- matrix(ncol = 3,nrow = n*(n-1)/2)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      e[k,] = c(i,j,Distance[i,j])
      k<-k+1
    }
  }
  return(e)
}

Edge_distance = Matrix_list(Distance/1000)


#======================================Before building an igraph ========================================================



net <- graph_from_data_frame(Edge_distance[,1:2],directed = FALSE, vertices = v)
E(net)$weight <- Edge_distance[,3] # Important use edge weight rather than net$weight
mst <- mst(net)

par(mfrow=c(1,2), mar=c(0,1,0.75,0)) 
plot(net,vertex.label=NA)
plot(mst, vertex.shape="none",edge.label=round(E(mst)$weight))


#========================================= Drawing line segments ========================================================


##### ======================introducing the minimum spanning tree (mst) lines as a function  ============================ 



mst_lines <- function(mst,lonlat) {
  me = get.edges(mst,1:ecount(mst))
  R = data.frame(lon=NULL,lat=NULL,group=NULL)
  for (k in 1:ecount(mst)) {
    A = lonlat[me[k,],]
    A$group = k
    R <- rbind(R,A)
  }
  rownames(R) <- NULL
  return(R)
}

R = mst_lines(mst, lonlat)

#===================================== displaying the map, mst and vertices =============================================

select_map1 <- ggplot(data = world) 


range <- (apply(lonlat,2,max) - apply(lonlat,2,min))*.10
xlimits = c(min(lonlat$lon)-range[1],max(lonlat$lon)+range[1]) 
ylimits = c(min(lonlat$lat)-range[2],max(lonlat$lat)+range[2]) 


select_map1 + coord_map(xlim = xlimits, ylim = ylimits)+
  geom_path(aes(x = lon, y = lat, group=group), data = R, colour = 'red', size = 3)+
  geom_point(data = v, aes(x = x, y = y), color="gold", size=10, alpha=0.5)+
  geom_label_repel(data = v, aes(x = x, y = y, label=name),size=4, 
                   point.padding = unit(0.5, "lines")) +   geom_sf(data = mst_data) + 
  xlab("Latitude") +ylab("Longitude")



#======Try playing with the code to take the above minimum spanning tree to the map select_map already drawn above

#==== Use the data shared to plot its map and find minimum spanning tree (mst), shortest path and the maximum flow

#====think of developing the results into an app that may be used to advise on the movement of goods services
  #you may have a hypothetical weights if not given in the data.  The code above also shows how weights can be
   #be computed.  


