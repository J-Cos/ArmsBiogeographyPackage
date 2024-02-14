#' Get biographic variables
#'
#' This function accepts a dataframe with columns of 'ARMS' 'Site_Longitude' and 'Site_Latitude'
#' and returns a dataframe with the seamount area, isolation and stressor values associated with each ARMS unit. 
#'
#' @param dat input dataframe with three columns titles: ARMS, Site_Longitude, Site_Latitude
#' @param shallowOceanBuffer the radius of the circle within which to calculate the proportion of shallow ocean (metres). Default 50km
#' @param shallowDepth the depth of ocean up to which it is considered shallow, Default 200m
#' @param stressRadius the radius of the circle from which the nearest stress value is drawn, default (metres). Default 30km
#' @param NumberShelves the number of seperate continental shelves to allow. Default =2 as this picks up australasia and eurasia - for mot locations in the indo-pacific. Need to be more cautiious in the arabian sea, red sea and east african coast due to very deep water coasts in (roughly) Oman, Somalia and Mozambique. Dependent on the ShallowDepth parameter and the bathymetry map resolution this can result in the eurasian costline being split into more pieces. Inspect the map output by the function to view the contientental shelves used in the isolation calculation.
#' @return A list. The first item is a dataframe containing the ARMs identifier, area, and distance. Also contains the LatLong of the closest point on the continental shelf and the id of that continental shelf (ordered by size). The second item is a map of those continental shelves used in the isolation calculation.
#' @export

GetBiogeographicVariablesForARMS<- function(dat, shallowOceanBuffer=50000, shallowDepth=200, NumberShelves=2, stressRadius=30000) {

    require(tidyverse)

    #functions
    extract_values <- function(data, allreefs, max_radius) {

        s <- sf::st_as_sf(x = data,                         
                coords = c("Site_Longitude", "Site_Latitude"),
                crs =  "EPSG:4326")
        s<-sf::st_transform(s,  sf::st_crs(allreefs))

        nearest<-sf::st_join(x=s, y=allreefs, join = sf::st_nearest_feature)  %>% 
            as.data.frame %>%
            as_tibble

        nearestpoly = sf::st_nearest_feature(s,allreefs)
        dist = sf::st_distance(s, allreefs[nearestpoly,], by_element=TRUE) %>%
            as.vector


        nearest [dist<max_radius,]<-NA

        output<-nearest %>% 
            dplyr::select(c(
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
                ))

        output<-cbind(rownames(d), output)

        return(output)
    }


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
            d<-d[,c(2,3)] #%>% as.matrix
            print("LatLongs extracted from input")

        #load etopo 
            fpath <- system.file("extdata", "etopo.tif", package="ArmsBiogeographyPackage")
            etopo<-terra::rast(fpath)
    #areas
    print("Calculating shallow sea areas...")

        #get shallow seas
            shallowseas<-etopo>-shallowDepth & etopo<0
            anyvalue<-etopo<100000
        #make arms into spatial points
            #p <- sp::SpatialPoints(d, proj4string=raster::crs(etopo))   
            p<-terra::vect(d, geom=c("Site_Longitude", "Site_Latitude"), crs="EPSG:4326")
            p<-terra::project(p, terra::crs(etopo))
            print("LatLongs converted to SpatVector")

        #loop over all points and get area based on buffer
            proportionshallow_df<-data.frame(ARMS=rownames(d), proportionshallow=NA)
            for (ARMS in 1:length(p)){
                b<-terra::buffer(p[ARMS], shallowOceanBuffer)
                m_shallow <- terra::mask(shallowseas, b)
                m_all<-terra::mask(anyvalue, b)
                proportionshallow_df[ARMS,2] <- terra::global(m_shallow, "sum", na.rm=TRUE) /  terra::global(m_all, "sum", na.rm=TRUE)
                print(paste0("Area for ARMS ", ARMS, " calculated"))

            }
            print("Shallow sea area calculation complete")

    #distances
        print("Calculating isolation...")

        # Change raster from longitude display (-180, 180) to (0, 360) to ensure distances work correctly
            x1 <- terra::crop(etopo, terra::ext(terra::ext(etopo)[1], terra::ext(etopo)[1]+180, terra::ext(etopo)[3], terra::ext(etopo)[4])) # crop to extents and zero meridian line
            x2 <- terra::crop(etopo, terra::ext(terra::ext(etopo)[1]+180, terra::ext(etopo)[2], terra::ext(etopo)[3], terra::ext(etopo)[4])) # crop to extents and zero meridian line
            terra::ext(x1) <- c(terra::ext(etopo)[1]+360, terra::ext(etopo)[1]+180+360,  terra::ext(etopo)[3],  terra::ext(etopo)[4])
            etopo360 <- terra::merge(x1, x2)
            print("Bathymetry data prepared for distance calculations")

        #get continental shelf polygons
            m <- c( -Inf, -shallowDepth, 0,
                    -shallowDepth, 0, 1,
                    0, Inf, 0)
            rclmat <- matrix(m, ncol=3, byrow=TRUE)
            shallowPolys<-terra::classify(etopo360, rclmat) %>% 
                terra::patches(zeroAsNA=TRUE) %>%
                terra::as.polygons( dissolve=TRUE)

            polygonsAreas<-terra::expanse(shallowPolys, unit="km")
            shallowPolys$expanse<-polygonsAreas

            shelfAreas<-polygonsAreas %>% sort %>% tail(NumberShelves)
            shelfPolys<-shallowPolys[polygonsAreas %in% shelfAreas,]

            terra::plot(shelfPolys, "expanse")
            map<-recordPlot()

            print("Continental shelf polygons generated")

        #get distance
            
            library(raster)
            s<-as(p, "Spatial")
            line<-as(shelfPolys, "Spatial")

            distances<-geosphere::dist2Line(p=s, line=line)

            distances[,1]<-round(distances[,1]/1000, digits=-1)
            print("Isolation calculation complete")

    #stress
        print("Extracting stress...")
        footprint<-extract_values(data=d, allreefs, max_radius = stressRadius)
        print("Stress extraction complete")


    #MERGE THIS INTO DATA
        new_dat<-proportionshallow_df %>% 
            cbind(., distances) %>%
            cbind(., footprint)
        colnames(new_dat)[1:6]<-c("ARMS", "proportionshallow", "distance(km)", "NearestShelf_Longitude", "NearestShelf_Latitude", "ShelfMatch")
        
    
    return(new_dat)
}


