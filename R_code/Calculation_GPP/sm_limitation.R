##############################run splash by the soil data from GSED
library(ncdf4)
library(raster)

BDgif<-raster("BD1.tif")
print(BDgif)
bb <- extent(-180, 180, -90, 90)
extent(BDgif) <- bb
BDgif1<- setExtent(BDgif, bb, keepres=TRUE)
print(BDgif1)
pts <- read.csv("site_location_lon_lat.csv", stringsAsFactors = FALSE)
site_point <- cbind(pts$Longitude, pts$Latitude)
BDsite<- raster::extract(BDgif1,site_point,method = 'bilinear')
BDsite<-data.frame(BDsite)
write.csv(BDsite*0.01,"BD1_site.csv")
#########################################we need to run splsh to get the soil moisture
####
sand<-read.csv("SAND1_site.csv")
clay<-read.csv("CLAY1_site.csv")
gravel<-read.csv("GRAV1_site.csv")
bd<-read.csv("BD1_site.csv")
OC<-read.csv("OC1_site.csv")
OM<-(OC$OCsite)*1.72#####Organic matter (%) = Total organic carbon (%) x 1.72##this transformation is same as David splashtool  
soildata<-cbind(sand,clay$CLAYsite,OM,gravel$GRAVsite,bd$BDsite)
soildata$depth <- 0.3
names(soildata)<-c("","sand","clay","OM","gravel","bd","depth")
write.csv(soildata,"soildata_splash.csv")
############################################################precipitation from CHELSA 1.2 1979-2013
path_netcdf <- "/Volumes/rd619/home/SPLASH"
##path_netcdf <- "/Volumes/rd619/home/rh"
setwd(path_netcdf)

files_list <- list.files(pattern = glob2rx("*tif"))

files_list <- sort(files_list)
split <- strsplit(files_list, "_") 
# split the name 
# turn the characters in numeric
split <- as.numeric(sapply(split, function(x) x <- sub(".tif", "", x[3])))
# not you can sort, by using order, that gives the original filenames, ordered on the numeric part of the filename
myFiles.correct.order <- files_list[order(split)]
# This is for one coordinate, let'S say: lon=4.5198, lat=4.5198
# If you have many coordenates, just need an outter loop including the function below
pts <- read.csv("site_location_lon_lat.csv", stringsAsFactors = FALSE)
site_point <- cbind(pts$Longitude, pts$Latitude)
#site_point <- cbind(113.8,4)
# A loop is enough. However there are better functions (e.g. apply family)
for (i in 1:length(myFiles.correct.order)) {
  if (i == 1) {
    pretest <- raster(myFiles.correct.order[i])
    bb <- extent(-180, 180, -90, 90)
    extent(pretest) <- bb
    r <- setExtent(pretest, bb, keepres=TRUE)
    data_set_wfdei <- stack(r)
    variable_data <- as.numeric(raster::extract(data_set_wfdei,site_point,method = 'bilinear')) # Please verify if you need to convert the values
    print("iu")
  } else {
    pretest <- raster(myFiles.correct.order[i])
    bb <- extent(-180, 180, -90, 90)
    extent(pretest) <- bb
    r <- setExtent(pretest, bb, keepres=TRUE)
    data_set_wfdei <- stack(r)
    variable_data <- c(variable_data,as.numeric(raster::extract(data_set_wfdei,site_point,method = 'bilinear')))  # Please verify if you need to convert the values
  }
}


# And now you have your time series
pre_time_series <- data.frame(variable_data)
write.csv(pre_time_series,"pre_chelsa_noindex1.csv")
precdata<-read.csv("pre_chelsa_noindex1.csv")

####The data is 17549(site)*12(month)
precedit<-precdata$variable_data
precform<-split(as.data.frame(precedit), rep(1:12, each = 17549))
precform<-as.data.frame(precform)
site<-read.csv("site_location_lon_lat.csv",header=T)
precmerge<-cbind(site,precform)
write.csv(precmerge,"prec_chelsa_modify.csv")
#######
###check the data 
test2<-raster("CHELSA_prec_02_V1.2_land.tif")##
plot(test2)
print(test2)
pts <- read.csv("site_location_lon_lat.csv", stringsAsFactors = FALSE)
site_point <- cbind(pts$Longitude, pts$Latitude)
bb <- extent(-180, 180, -90, 90)
extent(test2) <- bb
r <- setExtent(test2, bb, keepres=TRUE)
data_set_wfdei <- stack(r)
variable_data <- as.numeric(raster::extract(data_set_wfdei,site_point,method = 'bilinear'))
variable_data<-data.frame(variable_data)
################################################################run splash
############# run splash point########################################
###sw_in Incoming shortwave solar radiation (W m-2), timeseries object of monthly or daily averages.
# tc Air temperature (°C), same timestep as sw_in
# pn Precipitation (mm), same timestep as sw_in

library(devtools)
install_github("dsval/rsplash")
library(rsplash)
library(xts)
library(zoo)
global_sites <- read.table(file = "site_location_lon_lat.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


# run splash
Result_sw_in <- read.csv(file = "srad_chelsa_modify.csv",header = T,sep = ",", stringsAsFactors = FALSE)
##unit:(kJ m-2)
sw<-Result_sw_in[,4:15]
sw<-data.frame(sw)
Result_sw_in<-sw*1000/3600/24
Result_sw_in<-t(Result_sw_in)
Result_sw_indata<-data.frame(Result_sw_in)
Result_sw_indata$date<-seq(as.Date("2000-01-01"), by = "month", length = 12)
Result_sw_inlist<-as.list(Result_sw_indata)
Result_sw_inlist.ts <- xts(Result_sw_inlist$X1, order.by=as.POSIXct(Result_sw_inlist$date))
Result_sw_inlist.ts
##unit:(W m-2)
## SSR [W/m2] = SSR [J/m^2] / (3600 seconds)
Result_tc <- read.csv(file = "Tg_CHELSE.csv",header = T, sep = ",", stringsAsFactors = FALSE)
Result_tc<-Result_tc[,-1]
Result_tc<-t(Result_tc)
Result_tc<-data.frame(Result_tc)
Result_tc$date<-seq(as.Date("2000-01-01"), by = "month", length = 12)
Result_tclist<-as.list(Result_tc)
Result_tclist.ts <- xts(Result_tclist$X1, order.by=as.POSIXct(Result_tclist$date))
Result_tclist.ts

Result_pn <- read.csv(file = "prec_chelsa_modify.csv",header = T, sep = ",", stringsAsFactors = FALSE)
pndata<-Result_pn[,4:15]
Result_pn<-t(pndata)
Result_pn<-data.frame(Result_pn)
Result_pn$date<-seq(as.Date("2000-01-01"), by = "month", length = 12)
Result_pnlist<-as.list(Result_pn)
Result_pnlist.ts <- xts(Result_pnlist$X1, order.by=as.POSIXct(Result_pnlist$date))
Result_pnlist.ts

elv<-read.csv("elevation_site.csv")
eledata<-elv[,2:13]
elemean<-rowMeans(eledata, na.rm = TRUE)
elemean<-data.frame(elemean)
soil<-read.csv("soildata_splash.csv")
soil<-soil[,3:8]

soil1<-t(soil)

soildata<-as.data.frame(soil1)
soildata[is.na(soildata)] <- 0
soil_list<- as.list(soildata)

#####################################should delay the NA for the data set
Result_sw_in1 <- read.csv(file = "srad_chelsa_modify.csv",header = T,sep = ",", stringsAsFactors = FALSE)
Result_tc1 <- read.csv(file = "Tg_CHELSE.csv",header = T, sep = ",", stringsAsFactors = FALSE)
Result_pn1 <- read.csv(file = "prec_chelsa_modify.csv",header = T, sep = ",", stringsAsFactors = FALSE)
globaldata<-data.frame(cbind(Result_sw_in1,Result_tc1[,-1],Result_pn1[,4:15],elemean,soil))
globaldata<-na.omit(globaldata)
soildata<-globaldata[,41:46]
soildata1<-t(soildata)

soildata1<-as.data.frame(soildata1)
colnames (soildata1)<-NULL 
rownames (soildata1)<-NULL

soil_list1<- as.list(soildata1)
##class(soildata1[,1])


sw1<-globaldata[,4:15]
sw1<-data.frame(sw1)
Result_sw_in1<-sw1*1000/3600/24
Result_sw_in1<-t(Result_sw_in1)
Result_sw_indata1<-data.frame(Result_sw_in1)
Result_sw_indata1$date<-seq(as.Date("2000-01-01"), by = "month", length = 12)
Result_sw_inlist1<-as.list(Result_sw_indata1)
Result_sw_inlist.ts1 <- xts(Result_sw_inlist1$X81, order.by=as.POSIXct(Result_sw_inlist1$date))
Result_sw_inlist.ts1

Result_tc1<-globaldata[,16:27]
Result_tc1<-t(Result_tc1)
Result_tc1<-data.frame(Result_tc1)
Result_tc1$date<-seq(as.Date("2000-01-01"), by = "month", length = 12)
Result_tclist1<-as.list(Result_tc1)
Result_tclist.ts1 <- xts(Result_tclist1$X81, order.by=as.POSIXct(Result_tclist1$date))
Result_tclist.ts1

pndata1<-globaldata[,28:39]
Result_pn1<-t(pndata1)
Result_pn1<-data.frame(Result_pn1)
Result_pn1$date<-seq(as.Date("2000-01-01"), by = "month", length = 12)
Result_pnlist1<-as.list(Result_pn1)
Result_pnlist.ts1 <- xts(Result_pnlist1$X81, order.by=as.POSIXct(Result_pnlist1$date))
Result_pnlist.ts1


runglobal<-rsplash::splash.point(sw_in=Result_sw_inlist.ts1[,1],		# shortwave radiation W/m2
                                 tc=Result_tclist.ts1[,1],		# air temperature C
                                 pn=Result_pnlist.ts1[,1],		# precipitation mm
                                 lat=globaldata$Latitude[1],	# latitude deg
                                 elev=globaldata$elemean[1],	# elevation masl
                                 slop=0,	# slope deg
                                 asp=0,	# aspect deg
                                 soil_data=soil_list1[[1]], 			# soil data: sand,clay,om,grvel in %, bulkdens g/cm3
                                 Au=0,	# upslope area m2
                                 resolution=250  			# resolution pixel dem used to get Au
)
##where the grid is 0.5degree. for GPP calculation I do not consider  the soil moisture. 
runglobalsite<-c()

for (i in 1:10715) {
  Result_sw_inlist1[[i]] <- xts(Result_sw_inlist1[[i]], order.by=as.POSIXct(Result_sw_inlist$date))
  Result_sw_inlist1[[i]]
  Result_pnlist1[[i]] <- xts(Result_pnlist1[[i]], order.by=as.POSIXct(Result_pnlist$date))
  Result_pnlist1[[i]]
  Result_tclist1[[i]] <- xts(Result_tclist1[[i]], order.by=as.POSIXct(Result_tclist$date))
  Result_tclist1[[i]]
  runglobalsite[[i]]<-rsplash::splash.point(sw_in=Result_sw_inlist1[[i]],		# shortwave radiation W/m2
                                            tc=Result_tclist1[[i]],		# air temperature C
                                            pn=Result_pnlist1[[i]],		# precipitation mm
                                            lat=globaldata$Latitude[i],	# latitude deg
                                            elev=globaldata$elemean[i],	# elevation masl
                                            slop=0,	# slope deg
                                            asp=0,	# aspect deg
                                            soil_data=soil_list1[[i]], 			# soil data: sand,clay,om,grvel in %, bulkdens g/cm3
                                            Au=0,	# upslope area m2
                                            resolution=250  			# resolution pixel dem used to get Au
  )
}
runglobalsite[[1]]
############################################save the output from splash
######wn
soilmoisture<-c()
for (i in  1:10715) {
  soilmoisture[[i]]<-runglobalsite[[i]]$wn
  soilmoisture[i]<-mean(soilmoisture[[i]])
  
}
soilmoisturedata<-data.frame(soilmoisture)
soilmoisturedata<-t(soilmoisturedata)
write.csv(soilmoisturedata,"soilmoisture_wn_splash.csv")

#####sm_lim
smlim<-c()
for (i in  1:10715) {
  smlim[[i]]<-runglobalsite[[i]]$sm_lim
  smlim[i]<-mean(smlim[[i]])
  
}
smlimdata<-data.frame(smlim)
smlimdata<-t(smlimdata)
write.csv(smlimdata,"soil moisture limitations_smlim_splash.csv")

##test<-mean(runglobal$wn)
smforGPP<-cbind(globaldata$X,smlimdata)
write.csv(smforGPP,"soil moisture limitations_smlim_withindex_splash.csv")
