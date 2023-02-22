# ArmsBiogeographyPackage
 A package to obtain seamount biogeographic variables  from lat long data.

## How to use:
#### Install and load the package
    devtools::install_github("J-Cos/ArmsBiogeographyPackage")
    library(ArmsBiogeographyPackage)

#### Convert your data to phyloseq object
    BiogData<-GetBiogeographicVariablesForARMS(dat)
