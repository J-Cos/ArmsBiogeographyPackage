# ArmsBiogeographyPackage
 A package to obtain seamount biogeographic variables  from lat long data.

## How to use:
#### Install and load the package
    devtools::install_github("J-Cos/ArmsBiogeographyPackage")
    library(ArmsBiogeographyPackage)

#### Get biogeographic variables for your data: isolation (distance to continental shelf) and area of seamount (proportion of a circle of given radius)
    BiogData<-GetBiogeographicVariablesForARMS(dat)
