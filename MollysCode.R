devtools::install_github("J-Cos/ArmsBiogeographyPackage")
library(ArmsBiogeographyPackage)
library(tidyverse)


filepath<-"~/Downloads/ARMS_SeqSample_MetaData.csv"

dat<-read.csv(filepath) %>%
    as_tibble %>%   
    dplyr::select(Unit, lat, lon) %>%
    filter(lat!="") %>%
    filter(lat!="?") %>%
    mutate(lat=as.double(lat), lon=as.double(lon)) %>% 
    rename(Site_Longitude=lon, Site_Latitude=lat, ARMS=Unit) %>% 
    GetBiogeographicVariablesForARMS

read.csv(filepath) %>%
    left_join(., dat, c("Unit"="ARMS")) %>%
    write.csv(str_replace(filepath, ".csv", "_WithBiogegraphy.csv"))

