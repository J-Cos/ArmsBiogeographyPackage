#' Get biographic variables
#'
#' This function accepts a dataframe with columns of 'ARMS' 'Site_Longitude' and 'Site_Latitude'
#' and returns a dataframe with the seamount area and isolation associated with each ARMS unit. 
#'
#' @param dat inout dataframe
#' @param shallowOceanBuffer the radius of the circle within which to calculate the proportion of shallow ocean (metres). default 50km
#' @param shallowDepth the depth of ocean up to which it is considered shallow, default 200m
#' @return A dataframe containing the ARMs identifier, area, and distance. Also contains the LatLong of the closest point on the continental shelf and the id of that continental shelf (eurasian (1) or australasian (2))
#' @export

GetBiogeographicVariablesForARMS<- function(dat, shallowOceanBuffer=50000, shallowDepth=200) {

    # 1. Packages and path to project directory ######
        require(geosphere)
        require(igraph)
        require(raster)
        require(sp)
        require(tidyverse)


    #hardcoded parameters
        NumberShelves=2 #this is hardcoded as there is no other sensible number given 
        #the area of the world we are looking at with the bathymetry data.
        
    ##load data
    print("Data Loading...")
        #format point data from input
            d<-dat %>%
                dplyr::select(ARMS, Site_Longitude, Site_Latitude) %>%
                dplyr::group_by(ARMS) %>%
                dplyr::summarise_all(mean) %>%
                dplyr::mutate(Site_Longitude= ifelse(test=Site_Longitude<0, yes=Site_Longitude+ 360, no=Site_Longitude)) %>%
                as.data.frame
            rownames(d)<-d$ARMS    
            d<-d[,c(2,3)] %>% as.matrix
            print("LatLongs extracted from input")

        # Load ETOPO data
            load(file="data/etopo.rda")
            print("Bathymetry data loaded")


    #areas
    print("Calculating shallow sea areas...")

        #get shallow seas
            shallowseas<-etopo>-shallowDepth & etopo<0
            anyvalue<-etopo<100000
        #make arms into spatial points
            p <- sp::SpatialPoints(d, proj4string=raster::crs(etopo))   
            print("LatLongs converted to SpatialPoints")

        #loop over all points and get area based on buffer
            proportionshallow_df<-data.frame(ARMS=rownames(d), proportionshallow=NA)
            for (ARMS in 1:length(p)){
                b<-raster::buffer(p[ARMS], shallowOceanBuffer, proj4string=crs(etopo))
                m_shallow <- raster::mask(shallowseas, b)
                m_all<-raster::mask(anyvalue, b)
                proportionshallow_df[ARMS,2] <- cellStats(m_shallow, "sum") / cellStats(m_all, "sum")
                print(paste0("Area for ARMS ", ARMS, " calculated"))

            }
            print("Shallow sea area calculation complete")

    #distances
        print("Calculating isolation...")

        # Change raster from longitude display (-180, 180) to (0, 360) to ensure distances work correctly
            x1 <- raster::crop(etopo, raster::extent(raster::extent(etopo)[1], raster::extent(etopo)[1]+180, raster::extent(etopo)[3], raster::extent(etopo)[4])) # crop to extents and zero meridian line
            x2 <- raster::crop(etopo, raster::extent(raster::extent(etopo)[1]+180, raster::extent(etopo)[2], raster::extent(etopo)[3], raster::extent(etopo)[4])) # crop to extents and zero meridian line
            extent(x1) <- c(raster::extent(etopo)[1]+360, raster::extent(etopo)[1]+180+360,  raster::extent(etopo)[3],  raster::extent(etopo)[4])
            etopo360 <- raster::merge(x1, x2)
            print("Bathymetry data prepared for distance calculations")

        #get continental shelf polygons
            shallowPolys <- raster::rasterToPolygons(raster::clump(etopo360>-shallowDepth & etopo360<0), dissolve = TRUE)
            polygonsAreas<-sapply(slot(shallowPolys, "polygons"), slot, "area")
            shelfAreas<-polygonsAreas %>% sort %>% tail(NumberShelves)
            shelfPolys<-shallowPolys[polygonsAreas %in% shelfAreas,]
            print("Continental shelf polygons generated")

        #get distance
            distances<-geosphere::dist2Line(p, shelfPolys)
            distances[,1]<-distances[,1]/1000
            print("Isolation calculation complete")

    #MERGE THIS INTO DATA
        new_dat<-proportionshallow_df %>% 
            cbind(., distances)
        colnames(new_dat)<-c("ARMS", "proportionshallow", "distance(km)", "NearestShelf_Longitude", "NearestShelf_Latitude", "ShelfMatch")
    
    return(new_dat)
}


