    # 1. Packages and path to project directory ######
        path<-"/home/j/Dropbox/MacrobialMediation_Paper" #provide the path to the project folder

        require(terra)
        require(tidyverse)

        # Load ETOPO data
            etopo1 <- terra::rast(file.path(path, "Data", "exportImage.tiff")) %>%
                    terra::aggregate(fact=3)
            etopo2 <- terra::rast(file.path(path, "Data", "exportImage(1).tiff")) %>%
                    terra::aggregate(fact=3)
            etopo3 <- terra::rast(file.path(path, "Data", "exportImage(2).tiff")) %>%
                    terra::aggregate(fact=3)
            etopo4 <- terra::rast(file.path(path, "Data", "exportImage(3).tiff")) %>%
                    terra::aggregate(fact=3)
            etopo5 <- terra::rast(file.path(path, "Data", "exportImage(4).tiff")) %>%
                    terra::aggregate(fact=3)
            etopo6 <- terra::rast(file.path(path, "Data", "exportImage(5).tiff")) %>%
                    terra::aggregate(fact=3)
            etopo<-terra::merge(etopo1, etopo2, etopo3,etopo4, etopo5, etopo6, filename="inst/extdata/etopo.tif")

        usethis::use_data(etopo, overwrite=TRUE)