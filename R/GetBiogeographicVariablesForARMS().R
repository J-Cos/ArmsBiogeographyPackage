#' Get biographic variables
#'
#' This function accepts a dataframe with columns of 'ARMS' 'Site_Longitude' and 'Site_Latitude'
#' and returns a dataframe with the seamount area, isolation and stressor values associated with each ARMS unit. 
#'
#' @param dat input dataframe with three columns titles: ARMS, Site_Longitude, Site_Latitude
#' @param shallowOceanBuffer the radius of the circle within which to calculate the proportion of shallow ocean (metres). Default 50km
#' @param shallowDepth the depth of ocean up to which it is considered shallow, Default 200m
#' @param stressRadius the radius of the circle from which the nearest stress value is drawn, default (metres). Default 30km
#' @return A dataframe containing the ARMs identifier, area, and distance. Also contains the LatLong of the closest point on the continental shelf and the id of that continental shelf (eurasian (1) or australasian (2))
#' @export

GetBiogeographicVariablesForARMS<- function(dat, shallowOceanBuffer=50000, shallowDepth=200, stressRadius=30000) {

    # 1. Packages and path to project directory ######
        require(geosphere)
        require(igraph)
        require(raster)
        require(rgdal)
        require(rgeos)
        require(sf)
        require(sp)
        require(tidyverse)

    #functions
    extract_values <- function(data, allreefs, max.radius) {

        # If allreefs is sf, convert it to SpatialPolygonsDataFrame
        if (is(allreefs, "sf")) {
            cat("Converting allreefs to SpatialPolygonsDataFrame...")
            allreefs <- sf::as_Spatial(allreefs)
            cat("done\n")
        }
        if (!is(allreefs, "SpatialPolygonsDataFrame")) stop("allreefs must be of class sf or SpatialPolygonsDataFrame")

        # Read CRS for allreefs
        prj4 <- sp::proj4string(allreefs)

        cat("CRS for allreefs is\n", prj4, "\n")

        # Convert points to spatialPoints. Assuming input data are in WGS84 (EPSG: 4326)
        points <- sp::SpatialPoints(data[, c(1, 2)],
            proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
        )


        # Change CRS for points
        points_prj4 <- sp::spTransform(points, sp::CRS(prj4))

        # Extract allreefs centroids
        allreefs_centroids <- rgeos::gCentroid(allreefs, byid = T)

        # Find nearest neighboring allreefs for each point
        nn <- RANN::nn2(sp::coordinates(allreefs_centroids),
            sp::coordinates(points_prj4),
            k = 1,
            searchtype = "radius",
            radius = max.radius
        )


        # Initializing output dataframe
        # LEGEND:
        # "score", Climate: composite score
        # "scorecn", Climate: connectivity
        # "scorecy", Climate: Cyclone Risk
        # "scorepfc", Climate: Thermal future
        # "scoreth", Climate: Thermal history
        # "scoretr", Climate: Recent stress
        # "grav_NC", Fishing: Market Pressure
        # "sediment", Pollution: Sedimentation
        # "nutrient", Pollution: Nutrients
        # "pop_count", Coastal Development: Human Population
        # "num_ports", Industrial Development: Ports
        # "reef_value", Tourism: Reef Value
        out.data <- matrix(NA, nrow = nrow(nn$nn.idx), ncol = 13)
        colnames(out.data) <- c(
            "score",
            "scorecn",
            "scorecy",
            "scorepfc",
            "scoreth",
            "scoretr",
            "grav_NC",
            "sediment",
            "nutrient",
            "pop_count",
            "num_ports",
            "reef_value",
            "cumul_score"
        )
        out.data <- as.data.frame(out.data)

        # Loop on points to fill values into output dataframe
        for (i in 1:nrow(nn$nn.idx)) {
            if (nn$nn.idx[i, 1] == 0) next
            out.data[i, ] <- as.list(allreefs@data[
            nn$nn.idx[i, 1],
            c(
                "score",
                "scorecn",
                "scorecy",
                "scorepfc",
                "scoreth",
                "scoretr",
                "grav_NC",
                "sediment",
                "nutrient",
                "pop_count",
                "num_ports",
                "reef_value",
                "cumul_score"
            )
            ])
        }
        out.data <- cbind(data, out.data)
        return(out.data)
    }

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
            distances[,1]<-round(distances[,1]/1000, digits=-1)
            print("Isolation calculation complete")

    #stress
        print("Extracting stress...")
        footprint<-extract_values(data=d, allreefs, max.radius = stressRadius) %>%
            dplyr::select(-c(Site_Longitude, Site_Latitude))
        print("Stress extraction complete")


    #MERGE THIS INTO DATA
        new_dat<-proportionshallow_df %>% 
            cbind(., distances) %>%
            cbind(., footprint)
        colnames(new_dat)[1:6]<-c("ARMS", "proportionshallow", "distance(km)", "NearestShelf_Longitude", "NearestShelf_Latitude", "ShelfMatch")
        
    
    return(new_dat)
}


