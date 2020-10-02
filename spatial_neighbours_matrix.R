#creating a spatial neighbours matrix for RINLA
library(INLA)

#read sf object containing the municiaplities of Sao Paulo 
polyg<-geobr::read_municipality(year=2018,simplified=F,code_muni="SP")
#polyg<-readRDS("polygon_tracts.rds")
#polyg<-readRDS("aggregated_gmsp_census_17sept.rds")

# builds a neighbours list based on regions with contiguous boundaries, that is sharing one or more boundary point.
nb<-spdep::poly2nb(polyg)
head(nb)

# Output spatial neighbours for INLA
spdep::nb2INLA("map_muni.adj", nb)
g <- INLA::inla.read.graph(filename = "map_muni.adj")

#plot graph for spatial matrix
image(inla.graph2matrix(g),xlab="",ylab="")  
inla.spy(g,factor=1/10)

#save as rds file
saveRDS(g,"sp_muni_spatial_matrix.rds")


mat<-inla.graph2matrix(g)
?read.graph

