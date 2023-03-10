    # 1. Packages and path to project directory ######
        path<-"/home/j/Dropbox/MacrobialMediation_Paper" #provide the path to the project folder

        require(raster)
        require(tidyverse)

         rasterOptions(tolerance = 10)

        # Load ETOPO data
            etopo1 <- raster::raster(file.path(path, "Data", "exportImage.tiff")) %>%
                    raster::aggregate(fact=3)
            etopo2 <- raster::raster(file.path(path, "Data", "exportImage(1).tiff")) %>%
                    raster::aggregate(fact=3)
            etopo3 <- raster::raster(file.path(path, "Data", "exportImage(2).tiff")) %>%
                    raster::aggregate(fact=3)
            etopo<-raster::merge(etopo1, etopo2, etopo3, tolerance=0.2)

        usethis::use_data(etopo, overwrite=TRUE)