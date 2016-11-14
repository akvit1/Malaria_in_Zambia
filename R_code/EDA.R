library(dplyr)
library(xts)
library(lubridate)
library(zoo)

setwd("C:/Users/Lenovo T440s/Desktop/Thesis/2_October/Analyzed_Data")

malar<- read.csv("final_join.csv")
malar$tmean <- rowMeans(cbind(malar$tmax, malar$tmin), na.rm = TRUE)

malar$tmeanC<- malar$tmean -273.15
malar$date <- as.Date(malar$date)

View(malar)


# do some EDA
par(mfrow=c(1,1))

plot(malar$date, malar$drought_i, type = "l", col= "red")

# plot drought index for all 14 clinics
par(mfrow=c(4,4))
for (i in 101:114){
    malar %>% filter(hcid == i) %>%
        #group_by(hcid) %>%
        
        with(plot(date, drought_i, type = "l", col= "red",
                  main = i))
    
    
}


malar$drought_i/max(malar$drought_i, na.rm = TRUE)



# plot all the parameters for one clinic
par(mfrow=c(1,1))

malar %>% filter(hcid == 101) %>%
with(plot(date, drought_i/max(drought_i, na.rm = TRUE), type = "l", col= "red",
              main = "101"))


malar %>% filter(hcid == 101) %>%
with(lines(date, prec/max(prec, na.rm = TRUE), type = "l", col= "blue"))

malar %>% filter(hcid == 101) %>%
    with(lines(date, ndvi/max(ndvi, na.rm = TRUE), type = "l", col= "green"))

malar %>% filter(hcid == 101) %>%
    with(lines(date, tmeanC/max(tmeanC, na.rm = TRUE), type = "l", col= "purple"))

malar %>% filter(hcid == 101) %>%
    with(lines(date, pos_rdt/max(pos_rdt, na.rm = TRUE), type = "p", col= "brown"))

legend("topright", c("drought_i", "rain", "ndvi", "temp", "RDT"), pch=c(1,1), bty="n",
       col=c("red","blue", "green", "purple", "brown"))



# plot the average for all the clinics
all_clinics<- malar %>% group_by(date) %>% 
    summarise(prec = round(mean(prec), digits = 2),
              pos_rdt = round(mean(pos_rdt), digits = 2),
              tmeanC = round(mean(tmeanC), digits = 2),
              ndvi = round(mean(ndvi), digits = 2),
              drought_i = round(mean(drought_i), digits = 2)
              
    )


View(all_clinics)
# plot all the parameters for ALL CLINICS on a weekly scale
par(mfrow=c(1,1))

all_clinics  %>%
    with(plot(date, drought_i/max(drought_i, na.rm = TRUE), type = "l", col= "red",
              main = "All Clinics Malaria and Weather Factors Average"))


all_clinics  %>%
    with(lines(date, prec/max(prec, na.rm = TRUE), type = "l", col= "blue"))

all_clinics  %>%
    with(lines(date, ndvi/max(ndvi, na.rm = TRUE), type = "l", col= "green"))

all_clinics  %>%
    with(lines(date, tmeanC/max(tmeanC, na.rm = TRUE), type = "l", col= "purple"))

all_clinics  %>%
    with(lines(date, pos_rdt/max(pos_rdt, na.rm = TRUE), type = "p", col= "brown"))

legend("topright", c("drought_i", "rain", "ndvi", "temp", "RDT"), pch=c(1,1), bty="n",
       col=c("red","blue", "green", "purple", "brown"))



# create month year dates, aggregate things to weekly levels. 
malar$date2 <- as.yearmon(malar$date)

dim(malar)

View(malar)

# get only the needed data

malar2<- malar[,c(2:9, 11, 15, 16, 44:52)]

malar2[,10]<-as.numeric(malar2[,10])
malar2[,11]<-as.numeric(malar2[,12])
malar2[,12]<-as.numeric(malar2[,12])


View(malar2)

# get only the weekly malaria outcomes
a<- filter(malar2, !is.na(rdtp_u5))

# this shows the WEEK START DATES for each week with malaria incidence
week_malar <- select(a, name, hcid, date, rdtp_u5, rdtp_a5, pos_rdt)
week_malar$date2<- week_malar$date + 6 # malaria values are on sunday for the previous week

View(week_malar)

# get weekly numbers for hcid 101
filt <- filter(malar2, hcid == 101)
filt <- as.xts(filt[,c(1, 3:8, 13:19)], order.by = filt$date)
weekly <- apply.weekly(filt, mean, na.rm = TRUE)
weekly$prec <- apply.weekly(filt$prec, sum, na.rm = TRUE)
# get the weekly numbers for the rest of the clinics
for (i in 102:114){
    filt <- filter(malar2, hcid == i)

filt <- as.xts(filt[,c(1, 3:8, 13:19)], order.by = filt$date)

count <- apply.weekly(filt, mean, na.rm = TRUE)
count$prec <- apply.weekly(filt$prec, sum, na.rm = TRUE)

weekly<- rbind(count, weekly)

}

# combine the weekly malaria cases with the weekly environmental factors

weekly2<- as.data.frame(weekly)
# get the dates you want to merge by
weekly2$date2 <- as.Date(rownames(weekly2))


# join
weekly_final<-inner_join(weekly2, week_malar, by = c("date2", "hcid"))
weekly_final$yearmon <- as.yearmon(weekly_final$date2)

dim(week_malar)
dim(weekly_final)

View(weekly_final)


# plot all the parameters for one clinic at WEEKLY SCALE
par(mfrow=c(1,1))

weekly_final %>% filter(hcid == 101) %>%
    with(plot(date, drought_i/max(drought_i, na.rm = TRUE), type = "l", col= "red",
              main = "101"))


weekly_final %>% filter(hcid == 101) %>%
    with(lines(date2, prec/max(prec, na.rm = TRUE), type = "l", col= "blue"))

weekly_final %>% filter(hcid == 101) %>%
    with(lines(date2, ndvi/max(ndvi, na.rm = TRUE), type = "l", col= "green"))

weekly_final %>% filter(hcid == 101) %>%
    with(lines(date2, tmeanC/max(tmeanC, na.rm = TRUE), type = "l", col= "purple"))

weekly_final %>% filter(hcid == 101) %>%
    with(lines(date2, pos_rdt/max(pos_rdt, na.rm = TRUE), type = "l", col= "brown"))

legend("topright", c("drought_i", "rain", "ndvi", "temp", "RDT"), pch=c(1,1), bty="n",
       col=c("red","blue", "green", "purple", "brown"))


# plot the average for all the clinics over time
all_clinics<- weekly_final %>% group_by(date2) %>% 
    summarise(prec = round(mean(prec), digits = 2),
              pos_rdt = round(mean(pos_rdt), digits = 2),
              tmeanC = round(mean(tmeanC), digits = 2),
              tmax = round(mean(tmax), digits = 2),
              tmin= round(mean(tmin), digits = 2),
              ndvi = round(mean(ndvi), digits = 2),
              drought_i = round(mean(drought_i), digits = 2)
              
              )

View(all_clinics)
# plot all the parameters for ALL CLINICS on a weekly scale
par(mfrow=c(1,1))

all_clinics  %>%
    with(plot(date2, drought_i/max(drought_i, na.rm = TRUE), type = "l", col= "red",
              main = "All Clinics Malaria and Weather Factors Average"))


all_clinics  %>%
    with(lines(date2, prec/max(prec, na.rm = TRUE), type = "l", col= "blue"))

all_clinics  %>%
    with(lines(date2, ndvi/max(ndvi, na.rm = TRUE), type = "l", col= "green"))

all_clinics  %>%
    with(lines(date2, tmeanC/max(tmeanC, na.rm = TRUE), type = "l", col= "purple"))

all_clinics  %>%
    with(lines(date2, pos_rdt/max(pos_rdt, na.rm = TRUE), type = "l", col= "brown"))

legend("topright", c("drought_i", "rain", "ndvi", "temp", "RDT"), pch=c(1,1), bty="n",
       col=c("red","blue", "green", "purple", "brown"))


write.csv(weekly_final, "weekly_final.csv")

# disaggregate data that is bigger than weekly to weekly? How?


#################################
# plot just the sums of malaria over time, to see how it associates with elevation,
# proximity from water, and geographic location. 

all_times<- weekly_final %>% group_by(hcid) %>% 
    summarise(UTMX = round(mean(UTMX), digits = 2),
              UTMY = round(mean(UTMY), digits = 2),
              elev = round(mean(elev), digits = 2),
              closest_river = round(mean(closest_river), digits = 2),
              catchment_area = round(mean(catchment_area), digits = 2),
              pos_rdt = round(sum(pos_rdt, na.rm = TRUE), digits = 2)
              
    )

# look at the univariate regressions of static variables
all_times %>%
with(plot( elev, pos_rdt, type = "p", col= "red",
     main = "Malaria vs Elevation"))

View(all_times)

s1 <- glm.nb(pos_rdt ~ elev, data = all_times)
summary(s1)
s2 <- glm.nb(pos_rdt ~ closest_river, data = all_times)
summary(s2)
s3 <- glm.nb(pos_rdt ~ catchment_area, data = all_times)
summary(s3)
s4 <- glm.nb(pos_rdt ~ UTMX, data = all_times)
summary(s4)
s5 <- glm.nb(pos_rdt ~ UTMY, data = all_times)
summary(s5)

plot(all_times$closest_river, all_times$pos_rdt)
abline(lm(all_times$pos_rdt~ all_times$closest_river))
#pos_rdt and elev, p = 0.3119
#pos_rdt and closest_river, p = 0.0605
#pos_rdt and catchment_area, p = 0.822
#pos_rdt and UTMX, p = 0.1514
#pos_rdt and UTMY, p = 0.403

    


# start some regressions

# From Mufaro;s Paper:

#All variables were introduced separately as independent variables in the univariate adjusted
#regression models, adjusted for seasonality and health facility. Variables with a p-value <0.1 in
#the univariate adjusted regression models were considered as potential candidates for the multivariable
#selection process.

library(gee)
library(MASS)
library(AER)
library(geeM)


chilala <- filter(weekly_final, hcid == "101")

# negative binomial regression
m1 <- glm.nb(pos_rdt ~ prec, yearmon, data = chilala)
summary(m1)

# do a poisson regression
m2<- glm(pos_rdt ~ prec, family = poisson, data = chilala)
summary(m2)
# looks like there is lots of overdispersion
dispersiontest(m2,trafo=1)




# a gee + negative binomial model. Have to specify a theta, not sure
# how exactly to do that. 

m3<- geem(pos_rdt ~ prec, id = hcid, family = negative.binomial(1), data = weekly_final)

summary(m3)
# Robust Z = -0.982319, p value 0.46, add a lag and see what happens.


# a plot to test out exactly how the lags work, for precipitation
all_clinics  %>%
    with(plot(date2, pos_rdt*8, type = "l", col= "red"))

all_clinics  %>%
    with(lines(date2, prec, type = "l", col= "green"))

all_clinics  %>%
    with(lines(date2+7*10, prec, type = "l", col= "blue"))


# looks like 10 weeks is the best lag for precipitation

# temp
all_clinics  %>%
    with(plot(date2, pos_rdt/max(pos_rdt,na.rm = TRUE), type = "l", col= "red"))

all_clinics  %>%
    with(lines(date2+7*0, (tmin-273.15)/max((tmin-273.15), na.rm = TRUE), type = "l", col= "green"))

all_clinics  %>%
    with(lines(date2, (tmax-273.15)/max((tmax-273.15), na.rm = TRUE), type = "l", col= "blue"))

# not sure about temperature association

# ndvi
all_clinics  %>%
    with(plot(date2, pos_rdt/max(pos_rdt,na.rm = TRUE), type = "l", col= "red"))

all_clinics  %>%
    with(lines(date2+7*6, ndvi/max(ndvi, na.rm = TRUE), type = "l", col= "green"))

# NDVI associated at a 6 week lag

# drought index 
all_clinics  %>%
    with(plot(date2, pos_rdt/max(pos_rdt,na.rm = TRUE), type = "l", col= "red"))

all_clinics  %>%
    with(lines(date2+7*10, drought_i/max(drought_i, na.rm = TRUE), type = "l", col= "green"))

# drought index associated at a 10 week lag, similar to rainfall. 


###################################################
# More univariate negative binomial regressions, with lags. 


# a gee + negative binomial model. Have to specify a theta, not sure
# how exactly to do that. 

# Write a lag function, lag NDVI and precipitation by 10 weeks, drought by
# 6 weeks. 

a<-weekly_final %>% group_by(hcid) %>% mutate(lprec = lag(prec, n = 10L))
a<-a %>% group_by(hcid) %>% mutate(lndvi = lag(ndvi, n = 10L))
a<-a %>% group_by(hcid) %>% mutate(ldrought_i = lag(drought_i, n = 6L))

a<-a %>% filter(!is.na(lprec))
a<-a %>% filter(!is.na(lndvi))
a<- as.data.frame(a)
a$year <- as.numeric(substring(a$date2,1,4))


# add a pre-post drought index
mutant <- a %>% mutate(drought = ifelse(date <"2015-10-01", 0, 1))
a$drought <- mutant$drought


# a plot to test out exactly how the lags work, for precipitation, for the first clinic
a  %>% filter(hcid == 101)%>% 
    with(plot(date2, pos_rdt*8, type = "l", col= "red"))

a  %>% filter(hcid == 101)%>% 
    with(lines(date2, prec, type = "l", col= "green"))

a  %>% filter(hcid == 101)%>% 
    with(lines(date2, lprec, type = "l", col= "blue"))



# lagged precipitation
m3<- geem(pos_rdt ~ lprec, id = hcid, family = negative.binomial(1), data = a)
summary(m3)

# lagged ndvi
m4<- geem(pos_rdt ~ lndvi, id = hcid, family = negative.binomial(1), data = a)
summary(m4)

# lagged drought index. 
m5<- geem(pos_rdt ~ ldrought_i, id = hcid, family = negative.binomial(1), data = a)
summary(m5)

# ndvi, precipitation, temperature, and drought index
f1 <- pos_rdt ~ lprec + lndvi + ldrought_i+ tmeanC

m5<- geem(f1, id = hcid, family = negative.binomial(1), data = a)
summary(m5)


# not all of these work, not sure why. 

# look at the static variables
# elevation
s6<- geem(pos_rdt ~ elev, id = hcid, family = negative.binomial(1), data = a)
summary(s6)
# closest river
s7<- geem(pos_rdt ~ closest_river, id = hcid, family = negative.binomial(1), data = a)
summary(s7)
# catchment area
s8<- geem(pos_rdt ~ catchment_area, id = hcid, family = negative.binomial(1), data = a)
summary(s8)
# UTMX
s9<- geem(pos_rdt ~ UTMX, id = hcid, family = negative.binomial(1), data = a)
summary(s9)
# UTMY
s10<- geem(pos_rdt ~ UTMY, id = hcid, family = negative.binomial(1), data = a)
summary(s10)


# adding a year variable throws an error. 

# ndvi, precipitation, temperature, and drought index
f2 <- pos_rdt ~ lprec + lndvi + ldrought_i + tmeanC + closest_river +  yearmon + drought
  

m6<- geem(f2, id = hcid, family = negative.binomial(1), data = a)
summary(m6)

# model without the drought index
f3 <- pos_rdt ~ lprec + lndvi + ldrought_i + tmeanC + closest_river +  yearmon


m7<- geem(f3, id = hcid, family = negative.binomial(1), data = a)
summary(m7)




