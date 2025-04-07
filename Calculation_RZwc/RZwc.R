prec_sites <- read.csv("Wfdei_2001_2016.csv",header=TRUE,row.names = 1)
ET <- read.csv("ext_pts_Out_mod16_monthly.csv",header=TRUE,row.names = 1)
colnames(prec_sites)[1] <- 'date'


#install.packages("dplyr")
library(dplyr)
get_month <- function(series) {
  # this performs
  # decomposition
  i <- ts(series, frequency = 12) # we want to capture a 12-month pattern
  d <- decompose(i, type = c("additive"), filter = NULL) # additive approach is always a good practice
  seasonality <- as.vector(d$figure)
  seasonality <- replace(seasonality, seasonality >= 0, 1) # this highlights the rainy season
  seasonality <- replace(seasonality, seasonality < 0, 0) # this highlights the dry season
  seasonality <- with(as.data.frame(seasonality), ifelse(seasonality == 1 & dplyr::lag(seasonality, n = 1, default = seasonality[12]) == 0, 1, 0)) # this masks the start of the rainy season
  rainy_month <- max(match(1,seasonality))
  return(rainy_month)
}

get_rainy_month <- function(series) {
  r <- list()
  for (i in colnames(series)[2:length(colnames(series))]) { 
    d <- subset(series, select = c('date', i))
    r[[i]] <- get_month(d)
  }
  r
}

##  Water deficit calculation 
prec_sites_monthly <- prec_sites
subset_et <- ET
rainy_month <- get_rainy_month(prec_sites_monthly)

rainy_month_reset <- rainy_month
do_hydrological_calc <- TRUE
use_runoff_regression <- FALSE 
use_runoff_data <- FALSE

sr <- matrix(ncol = (ncol(prec_sites_monthly) - 1), nrow = nrow(prec_sites_monthly)) 
sr <- data.frame(sr)
names(sr) <- names(subset_et[2:ncol(subset_et)])
for (i in 2:ncol(prec_sites_monthly)) {
  for (j in 1:nrow(prec_sites_monthly)) {
    if (use_runoff_regression) {
      runoff_site <- prec_sites_monthly[j,i] * runoff_results$runoff_slope[(i - 1)] # Estimating runoff based on adhoc regression
      if (runoff_site < 0) { runoff_site <- 0 }
    } else {
      if (use_runoff_data) {
        runoff_site <- subset_runoff[j,i]
      } else {
        runoff_site <- 0
      }
    }
    
    if (rainy_month[i - 1] == 1 | !(do_hydrological_calc)) {
      day_reset <- 13 # 13 is the first month of the year
      if ((j %% day_reset) == 0) { # The day_reset (= 13) means to reset the water balance at the beginning of each year
        sr[j,(i - 1)] <- max(subset_et[j,i] + runoff_site - prec_sites_monthly[j,i] , 0) # + subset_runoff[j,i]
      } else {
        sr[j,(i - 1)] <- max(subset_et[j,i] + runoff_site - prec_sites_monthly[j,i]   + sr[j - 1,(i - 1)], 0) # + subset_runoff[j,i]
      }
    } else {
      day_reset <- as.numeric(rainy_month_reset[i - 1])
      if ((j == day_reset | j %% (day_reset + 12) == 0)) { # The day_reset here comes from the previous loop calculation
        sr[j,(i - 1)] <- max(subset_et[j,i] + runoff_site - prec_sites_monthly[j,i] , 0) # + subset_runoff[j,i]
      } else {
        sr[j,(i - 1)] <- max(subset_et[j,i] + runoff_site - prec_sites_monthly[j,i]   + sr[j - 1,(i - 1)], 0) # + subset_runoff[j,i]
      }
    }
  }
}
write.csv(sr,"sr_2001_2016_right_monthly.csv")

rzwc_site <- data.frame(apply(sr[,2:17550],2,max))

write.csv(rzwc_site,"sr_2001_2016_right_root_zone_water_capacity.csv")

sr<-read.csv("sr_2001_2016_right_monthly.csv")
# A quick test to check if precipitation and ET values are reliable
# We are going to run through a selection of points that Ruijie has done
# In general, as rule of thumb, we expect the ET in dry sites to be around 500 mm (+/- 300 mm)
# and in the Wet areas around 1000 mm (+/- 300 mm) - Of course there are many exceptions
# Climate, Site Nr, Lon, Lat, Country
sites_eval <- rbind.data.frame(
  list("Dry", 7327, 115.6, 39.8, "China"),
  list("Dry", 13174, 90.06667, 43.65000, "China"),
  list("Dry", 14938, 110.19750, 39.49583, "China"),
  list("Dry", 9878, 146.2559, -26.61445, "Australia"),
  list("Dry", 10153, 139.6351, -35.33317, "Australia"),
  list("Wet", 74, 106.5, 22.0, "Vietnam"),
  list("Wet", 3151, -48.40000, -23.00000, "Brazil"),
  list("Wet", 3157, -48.30000, -22.50000, "Brazil"),
  list("Wet", 839, -67.1, 1.9, "Colombia"),
  list("Wet", 14230, -43.59106, -19.27936, "Brazil")
)
names(sites_eval) <- c("climate", "id", "lon", "lat", "country")

sites_eval_func <- function(x, data) {
  point_test <- sites_eval$id + 1 # Adding '1' because the first column is the dates
  aggregate(data[,point_test], by = list(format(as.Date(prec_sites$date),'%Y')), FUN = sum)
}
#results_prec_list <- apply(sites_eval, 1, sites_eval_func, data = prec_sites)
#results_et_list <- apply(sites_eval, 1, sites_eval_func, data = subset_et)
#results_sr_list <- apply(sites_eval, 1, sites_eval_func, data = sr)
#aggregate(prec_sites[,point_test], by = list(format(as.Date(prec_sites$date),'%Y')), FUN = sum)

sites_eval_func(sites_eval$id, prec_sites)
sites_eval_func(sites_eval$id, subset_et)
sites_eval_func(sites_eval$id, sr)

point_test <- 3151 + 1
rzwc_point <- max(sr[,point_test])
sr_point <- sr[,point_test]
water_av_no_correction <- sr_point/rzwc_point

plot(sr_point, type="l")

plot(subset_et[,point_test], type="l")

##check the rainy month
library(zoo)
library(tsbox)
which(sapply(rainy_month, function(x) "1" %in% x))  
site3432<-data.frame(cbind(prec_sites$date,prec_sites$site3432))
h3432 <- xts(site3432$X2, as.Date(site3432$X1, format="%Y-%m-%d")) 
nowTS3432 <-ts_ts(h3432)
plot.ts(nowTS3432,ylab = "Precipitation of site3432",xlab="Time series")

###################################
site<-read.csv("site_location_lon_lat.csv",header=T)
site$id<-1:17549
##function
sites_test_func <- function(x, data) {
  site_test <- site$id # Adding '1' because the first column is the dates
  aggregate(data[,site_test], by = list(format(as.Date(prec_sites$date),'%Y')), FUN = sum)
}

site_sr<-sites_test_func(site$id, sr)
write.csv(site_sr,"sr_2001_2016_right_annual_nouse.csv")

##chack data with the site point
site_sr$V7329

##
