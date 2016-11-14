library(dplyr)
library(xts)
library(lubridate)

setwd("C:/Users/Lenovo T440s/Desktop/Thesis/2_October/Raw_Data")

malar<-read.csv('ICEMRSurveillanceDat_DATA_2016-10-24_1705.csv')
clinics<- read.csv('Macha_Clinic_Locs_WGS_1984_UTM_Zone_35S.csv')


malar$pos_rdt<-(malar[,5] + malar[,6])
# get only choma district
malar <- filter(malar, redcap_data_access_group == "1__choma_district")

# get a glue variable
clinics$glue<-0

date<- seq.Date(as.Date("2012-04-16",  format = "%Y-%m-%d"), 
             as.Date(last(malar$wk_start_date),  format = "%Y-%m-%d"),
         by = "day")

date <- as.data.frame(date)
date$glue <- 0

# join all the dates between 2012-04 and the most recent 2016 date with the 
# elevation, proximity to water, and catchment area
joined <- full_join(clinics, date, by = "glue")

malar$date <- as.Date(malar$wk_start_date)

# join by both date and ID
joined <- left_join(joined, malar, by = c("date", "hcid"))
View(joined)

#######################################
# upload the drought index raster files
#install.packages("geoR")
#install.packages("rgdal")
#install.packages("maptools")
#install.packages("raster")

library(geoR)
library(rgdal)
library(maptools) # read shapepoints shapefile
library(raster) # extract from raster


# get the clinic locations
setwd("C:/Users/Lenovo T440s/Desktop/Thesis/2_October/ArcGIS/Clinics")
clinics_shp<-readShapePoints("Macha_HC_3_Project.shp")
plot(clinics_shp)
View(clinics_shp)

# get the rasters
setwd("C:/Users/Lenovo T440s/Desktop/Thesis/2_October/ArcGIS")

## the drought index
files <-list.files(path = "AFDM_data/drought_index_vcpct--VIC_DERIVED", pattern='*.asc',full.names=TRUE)
files_s<-stack(files)

# extract the raster values at clinics_shp points
drought_i<-extract(files_s, clinics_shp)
drought_i <- as.data.frame(drought_i)

# transpose, add columns labeling each clinic
drought_i_t<-as.data.frame(t(drought_i))

drought_i_t$c101<- "101"
drought_i_t$c102<- "102"
drought_i_t$c103<- "103"
drought_i_t$c104<- "104"
drought_i_t$c105<- "105"
drought_i_t$c106<- "106"
drought_i_t$c107<- "107"
drought_i_t$c108<- "108"
drought_i_t$c109<- "109"
drought_i_t$c110<- "110"
drought_i_t$c111<- "111"
drought_i_t$c112<- "112"
drought_i_t$c113<- "113"
drought_i_t$c114<- "114"


# add dates to each row
date2<- seq.Date(as.Date("2012-01-01",  format = "%Y-%m-%d"), 
                as.Date("2016-10-05",  format = "%Y-%m-%d"),
                by = "day")


line <- stack(drought_i_t[,1:14])
line$hcid <- stack(drought_i_t[,15:28])
line$date <- date2




## precipitation

files2<-list.files(path = "AFDM_data/prec--3B42RT_BC", pattern='*.asc',full.names=TRUE)
files2<- stack(files2) # stack the raster files, in order to analyze them as a group

prec<-extract(files2, clinics_shp)

prec_t<-as.data.frame(t(prec))

line$prec <- stack(prec_t)

# clean the data so far
full_line <- line[,c(1,4)]
colnames(full_line)<- c("drought_i", "date")
a <- line[,3]
full_line$hcid <- as.numeric(a[,1])
b <- line[,5]
full_line$prec <- b[,1]



## NDVI

files3 <-list.files(path = "AFDM_data/ndvi30--MOD09_NDVI_MA", pattern='*.asc',full.names=TRUE)
files3<- stack(files3) # stack the raster files, in order to analyze them as a group

ndvi<-extract(files3, clinics_shp)

ndvi_t<-as.data.frame(t(ndvi))

ndvi_t$c101<- "101"
ndvi_t$c102<- "102"
ndvi_t$c103<- "103"
ndvi_t$c104<- "104"
ndvi_t$c105<- "105"
ndvi_t$c106<- "106"
ndvi_t$c107<- "107"
ndvi_t$c108<- "108"
ndvi_t$c109<- "109"
ndvi_t$c110<- "110"
ndvi_t$c111<- "111"
ndvi_t$c112<- "112"
ndvi_t$c113<- "113"
ndvi_t$c114<- "114"


# add dates to each row
date3<- seq.Date(as.Date("2012-01-01",  format = "%Y-%m-%d"), 
                 as.Date("2016-07-17",  format = "%Y-%m-%d"),
                 by = "day")


ndvi_line <- stack(ndvi_t[,1:14])
ndvi_line$hcid <- stack(ndvi_t[,15:28])
ndvi_line$date <- date3


ndvi_line2 <- ndvi_line[,c("values","date")]
d<-ndvi_line[,3]
ndvi_line2$hcid <- as.numeric(d[,1])
colnames(ndvi_line2)<- c("ndvi", "date", "hcid")



# join ndvi to the data
full_line2<- left_join(full_line, ndvi_line2, by = c("date", "hcid"))



## Temperature Max
files4 <-list.files(path = "AFDM_data/tmax--GFS_ANALYSIS_BC", pattern='*.asc',full.names=TRUE)
files4<- stack(files4) # stack the raster files, in order to analyze them as a group

tmax<-extract(files4, clinics_shp)

tmax_t<-as.data.frame(t(tmax))
e<-stack(tmax_t)

full_line2$tmax <- e[,1]



## Temperature Min


files5 <-list.files(path = "AFDM_data/tmin--GFS_ANALYSIS_BC", pattern='*.asc',full.names=TRUE)
files5<- stack(files5) # stack the raster files, in order to analyze them as a group

tmin<-extract(files5, clinics_shp)

tmin_t<-as.data.frame(t(tmin))

f<-stack(tmin_t)

full_line2$tmin <- f[,1]

## Join the final datasets
dim(joined)
dim(full_line2)

final_join <- left_join(joined, full_line2, by = c("date", "hcid"))

dim(final_join)
View(final_join)

###
# save the file

write.csv(final_join, file.path("C:/Users/Lenovo T440s/Desktop/Thesis/2_October/Analyzed_Data/final_join.csv"))





