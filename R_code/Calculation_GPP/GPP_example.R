
#######################example of calculate soil moisture limited GPP
library(dplyr)
library(rpmodel)

tc<-read.csv("tc_monthmean.csv")###this is growth temperature
ppfd<-read.csv("ppfd_monthmean.csv")

vpd<-read.csv("vpd_monthmean.csv")

co<-read.csv("co2_monthmean.csv")

fapar<-read.csv("fapar_monthmean.csv")

patm<-read.csv("pressure_site.csv")
patm<-patm$pressure
elv<-read.csv("elevation_site.csv")
elv<-elv$leftJoinDf.elevation
###################################################run Rpmodel
##tc Temperature, relevant for photosynthesis (deg C)
#vpd Vapour pressure deficit (Pa)
#co2 Atmospheric CO2 concentration (ppm)
#fapar (Optional) Fraction of absorbed photosynthetically active radiation (unitless, defaults to NA)
#ppfd Incident photosynthetic photon flux density (mol m-2 d-1, defaults to NA). Note
# that the units of ppfd (per area and per time) determine the units of outputs
# lue, gpp, vcmax, and rd. For example, if ppfd is provided in units of mol m-2
# month-1, then respective output variables are returned as per unit months.
# patm Atmospheric pressure (Pa). When provided, overrides elv, otherwise patm is
# calculated using standard atmosphere (101325 Pa), corrected for elevation (argument elv), using the function patm.
# elv Elevation above sea-level (m.a.s.l.). Is used only for calculating atmospheric
# pressure (using standard atmosphere (101325 Pa), corrected for elevation (argument elv), using the function patm), if argument patm is not provided. If
# argument patm is provided, elv is overridden.
#################
#tc$tc[is.nan(tc$tc)]<-0
vpd[vpd==0]<-NA

#ppfd$ppfd[is.nan(ppfd$ppfd)]<-0
site<-read.csv("site_location_lon_lat.csv")
site$id=1:17549
pmodeldata<-data.frame(cbind(site,tc, vpd,co,fapar,ppfd,patm,elv))
smdata<-read.csv("soil moisture limitations_smlim_withindex_splash.csv")
smdata<-smdata[,-1]
names(smdata)<-c("id","smlim")
pmodeldata1<-left_join(smdata, pmodeldata, 'id'="id")
pmodeldata1<-na.omit(pmodeldata1)
colSums(pmodeldata==0)
pmodeldata1$fapar[pmodeldata1$fapar==0]<-NA
pmodeldata1<-na.omit(pmodeldata1)
###################replace the NaN as 0 in tc, vpd 
model<-rpmodel(
  tc=pmodeldata1$tc,
  vpd=pmodeldata1$vpd,
  co2=pmodeldata1$co,
  fapar=pmodeldata1$fapar,
  ppfd=pmodeldata1$ppfd,
  patm=pmodeldata1$patm,
  elv=pmodeldata1$elv,
  kphio =  0.049977,
  beta = 146,
  soilm =pmodeldata1$smlim,
  meanalpha = 1,
  apar_soilm = 0,
  bpar_soilm =  0.7330,
  c4 = FALSE,
  method_optci = "prentice14",
  method_jmaxlim = "wang17",
  do_ftemp_kphio = TRUE,
  do_soilmstress = TRUE,
  returnvar = NULL,
  verbose = FALSE
)
modeldata<-data.frame(model)
write.csv(modeldata,"GPP_withsmlim.csv")
