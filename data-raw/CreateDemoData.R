DemoData <- data.frame( "ARMS"=c("ARMSA", "ARMSB", "ARMSC", "ARMSD"), 
                        "Site_Longitude"=c(-150, -160, 140, 80), 
                        "Site_Latitude"=c(20, 30, 0, 0))
usethis::use_data(DemoData, overwrite=TRUE)