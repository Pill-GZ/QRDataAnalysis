library(plyr)


########## Read in the data set ##########
data = read.csv("C:/Users/gaozheng/Documents/yellow_tripdata_2015-12_sample100.csv")

tripdata = data
names(tripdata)
# head(tripdata$tpep_pickup_datetime)

### convert the time attributes into time format
tripdata$tpep_pickup_datetime = strptime(as.character(tripdata$tpep_pickup_datetime),"%Y-%m-%d %H:%M:%S")
tripdata$tpep_dropoff_datetime = strptime(as.character(tripdata$tpep_dropoff_datetime),"%Y-%m-%d %H:%M:%S")

### payment_type should be factors
tripdata$payment_type = as.factor(tripdata$payment_type)

data = tripdata
tripdata = data

########## Now clean the data set ##########


### Reasonable locations
selectIndexPickup = tripdata$pickup_longitude<0
selectIndexDropoff = tripdata$dropoff_longitude<0
selectIndex = selectIndexPickup&selectIndexDropoff
tripdata = data[selectIndex,]
rm(list = c("selectIndexPickup","selectIndexDropoff"))

### Restrict locations to NYC
# filter pickup locations
attach(tripdata)
# above the sea
selectIndexPickup1 = pickup_latitude > 40.543
# right of NJ
selectIndexPickup2 = pickup_longitude > -74.052
# left of Long Island
selectIndexPickup3 = pickup_longitude < -73.721
# below hudson river x = (-73.926, 40.919) y = (-74.052, 40.671)
selectIndexPickup4 = cbind(pickup_longitude+74.052,pickup_latitude-40.671)%*%c(1, -(74.052-73.926)/(40.919-40.671))>0
# below NY state x = (-73.926, 40.919) y = (-73.750, 40.876)
selectIndexPickup5 = cbind(pickup_longitude+74.052,pickup_latitude-40.671)%*%c(1, -(73.750-73.926)/(40.919-40.876))>0
detach(tripdata)
selectIndex = selectIndexPickup1&selectIndexPickup2&selectIndexPickup3&selectIndexPickup4&selectIndexPickup5
sum(selectIndex)
tripdata = tripdata[selectIndex,]
rm(list = c("selectIndexPickup5","selectIndexPickup4","selectIndexPickup3","selectIndexPickup2","selectIndexPickup1"))

# filter dropoff locations
attach(tripdata)
# above the sea
selectIndexDropoff1 = dropoff_latitude > 40.543
# right of NJ
selectIndexDropoff2 = dropoff_longitude > -74.052
# left of Long Island
selectIndexDropoff3 = dropoff_longitude < -73.721
# below hudson river x = (-73.926, 40.919) y = (-74.052, 40.671)
selectIndexDropoff4 = cbind(dropoff_longitude+74.052,dropoff_latitude-40.671)%*%c(1, -(74.052-73.926)/(40.919-40.671))>0
# below NY state x = (-73.926, 40.919) y = (-73.750, 40.876)
selectIndexDropoff5 = cbind(dropoff_longitude+73.926,dropoff_latitude-40.919)%*%c(1, (73.926-73.750)/(40.876-40.919))>0
detach(tripdata)
selectIndex = selectIndexDropoff1&selectIndexDropoff2&selectIndexDropoff3&selectIndexDropoff4&selectIndexDropoff5
sum(selectIndex)
tripdata = tripdata[selectIndex,]
rm(list = c("selectIndexDropoff5","selectIndexDropoff4","selectIndexDropoff3","selectIndexDropoff2","selectIndexDropoff1"))

### Long/Lat should be non-zeros
# plot(tripdata$dropoff_longitude,tripdata$dropoff_latitude)
#plotIndex = sample(1:dim(tripdata)[1],10000)
plotIndex = 1:dim(tripdata)[1]

plot(tripdata[plotIndex,]$pickup_longitude,tripdata[plotIndex,]$pickup_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.919),pch='.')

plot(tripdata[plotIndex,]$dropoff_longitude,tripdata[plotIndex,]$dropoff_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.919), pch='.')

# quantile(tripdata$pickup_longitude,probs = c(0.00005,0.99995))
# quantile(tripdata$pickup_latitude,probs = c(0.00005,0.99995))
# quantile(tripdata$dropoff_longitude,probs = c(0.00005,0.99995))
# quantile(tripdata$dropoff_latitude,probs = c(0.00005,0.99995))

### and some other data wrangling

### Fare should be non-negative (and reasonable)
boxplot(tripdata$fare_amount)
summary(tripdata$fare_amount)
tripdata = tripdata[tripdata$fare_amount>=0,]

### Distance should be positive (and reasonable)
boxplot(tripdata$trip_distance)
summary(tripdata$trip_distance)
sum(tripdata$trip_distance==0)
tripdata = tripdata[(tripdata$trip_distance>0)&(tripdata$trip_distance<60),]
summary(tripdata[tripdata$trip_distance==0,]$fare_amount)

### Remove trips above 3 hours and zero
tripdata$duration = (as.numeric(tripdata$tpep_dropoff_datetime)-as.numeric(tripdata$tpep_pickup_datetime))
boxplot(tripdata$duration)
summary(tripdata$duration)
sum(tripdata$duration==0)
tripdata = tripdata[(tripdata$duration>0)&(tripdata$duration<10800),]

### Remove non-movers (already removed in distance filter) and Flash Gordons (150 mph)
boxplot(tripdata$trip_distance/tripdata$duration*60*60)
summary(tripdata$trip_distance/tripdata$duration*60*60)
tripdata = tripdata[tripdata$trip_distance/tripdata$duration*60*60<150,]

### take a look at duration VS distance ##########
plot(tripdata$duration,tripdata$trip_distance,pch='.',cex=2)
# boxplot(tripdata$duration~tripdata$trip_distance)

### take a look at duration VS passenger count ############
boxplot(tripdata$duration~tripdata$passenger_count)


### take a look at duration VS pickup/dropoff neighbourhood ################

neighbourhoods = rbind(UpperManhatten = c(-73.956000,40.839804),
                       UpperWest = c(-73.970271,40.800212),
                       UpperEast = c(-73.948886,40.782980),
                       Midtown = c(-73.984610,40.754453),
                       Downtown = c(-74.000081,40.732514),
                       BrooklynHeights = c(-73.985675,40.680464),
                       NorthBrooklyn = c(-73.944259,40.713988),
                       EastQueens = c(-73.931738,40.756407),
                       LaGuadia = c(-73.844981,40.759204),
                       JFKAirport = c(-73.776780,40.652341),
                       Bronx = c(-73.906457,40.832033))
pickupNH = apply(tripdata[,c(6,7)],1,function(x){which.min(apply((t(neighbourhoods)-x)^2,2,sum))})
dropoffNH = apply(tripdata[,c(10,11)],1,function(x){which.min(apply((t(neighbourhoods)-x)^2,2,sum))})

plot(tripdata$pickup_longitude,tripdata$pickup_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.919),col=pickupNH,pch='.')
plot(tripdata$dropoff_longitude,tripdata$dropoff_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.919),col=pickupNH,pch='.')

points(neighbourhoods[,1],neighbourhoods[,2],pch='+')

### Get rid of Bronx ###################
tripdata = tripdata[(dropoffNH!=11)&(pickupNH!=11),]
pickupNHnew = pickupNH[(dropoffNH!=11)&(pickupNH!=11)]
dropoffNHnew = dropoffNH[(dropoffNH!=11)&(pickupNH!=11)]
pickupNH = pickupNHnew
dropoffNH = dropoffNHnew


####### The pick-up / drop-off locations colored by the other location neighborhood #############
colorMapPartition = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
colorMapPartition = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")
colorMapPartition = sample(colorMapPartition,10)
par(mar=c(0,0,0,0))
par(mfrow=c(1,2))
plot(tripdata$pickup_longitude,tripdata$pickup_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.9),col=colorMapPartition[pickupNHnew],pch='.',cex=3,
     xlab = F, ylab = F, axes = F)
legend("bottomleft", inset = .05,col = colorMapPartition,
       legend = row.names(neighbourhoods)[-11],
       cex = 0.7, pch = 15, merge=FALSE, bty = 'n' )
plot(tripdata$dropoff_longitude,tripdata$dropoff_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.9),col=colorMapPartition[pickupNHnew],pch='.',cex=3,
     xlab = F, ylab = F, axes = F)

plot(tripdata$pickup_longitude,tripdata$pickup_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.9),col=colorMapPartition[dropoffNH],pch='.',cex=3,
     xlab = F, ylab = F, axes = F)
legend("bottomleft", inset = .05,col = colorMapPartition,
       legend = row.names(neighbourhoods)[-11],
       cex = 0.7, pch = 15, merge=FALSE, bty = 'n' )
plot(tripdata$dropoff_longitude,tripdata$dropoff_latitude,
     xlim = c(-74.052,-73.721), ylim = c(40.543,40.9),col=colorMapPartition[dropoffNH],pch='.',cex=3,
     xlab = F, ylab = F, axes = F)

par(mfrow=c(1,1))
par(mar=c(3,4,1,1))
boxplot(tripdata$duration~pickupNH,names=row.names(neighbourhoods)[-11])
boxplot(tripdata$duration~dropoffNH,names=row.names(neighbourhoods)[-11])


### dist, duration, speed VS origin and destination combination ####
boxplot(tripdata$duration~pickupNH*dropoffNH,outline=F)

colorWeek2 = c("#ffffd4","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04")
colorGradient = c("#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac","#053061")
colorGradient2 = c("#a50026","#d73027","#f46d43","#fdae61","#fee08b","#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837")
par(mfrow=c(3,1))
par(mar=c(5,5,2,5))
par(xpd=TRUE)

image(-with(tripdata, tapply(trip_distance, list("pick up"=pickupNH, "drop off"=dropoffNH), mean)),
      col=c(colorGradient),axes = FALSE, ann = FALSE)
text(x=1.3,y = 1.15, labels = "(a) trip distance", pos = 2, xpd = TRUE)
text(x = 1.1, par("usr")[3]-0.225, labels = "Pick-up location", pos = 2, xpd = TRUE)
text(y = 1.08, par("usr")[3]-0.175, labels = "Drop-off location", srt = 90, pos = 2, xpd = TRUE)
text(x = (1:10-1)/8.8, par("usr")[3]-0.02, labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
text(y = (1:10-1)/8.8, par("usr")[3], labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
legend("right", inset = -0.15,col = colorGradient,
       legend = c("  Far",rep("",8),"  Near"),
       pt.cex = 3, pch = 15, merge=FALSE, bty = 'n' )

image(1-with(tripdata, tapply(duration, list("pick up"=pickupNH, "drop off"=dropoffNH), mean)),
      col=c(colorGradient),axes = FALSE, ann = FALSE)
text(x=1.3,y = 1.15, labels = "(b) trip duration", pos = 2, xpd = TRUE)
text(x = 1.1, par("usr")[3]-0.225, labels = "Pick-up location", pos = 2, xpd = TRUE)
text(y = 1.08, par("usr")[3]-0.175, labels = "Drop-off location", srt = 90, pos = 2, xpd = TRUE)
text(x = (1:10-1)/8.8, par("usr")[3]-0.02, labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
text(y = (1:10-1)/8.8, par("usr")[3], labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
legend("right", inset = -0.15,col = colorGradient,
       legend = c("  Long",rep("",8),"  Short"),
       pt.cex = 3, pch = 15, merge=FALSE, bty = 'n' )

image(with(tripdata, tapply(trip_distance/duration, list("pick up"=pickupNH, "drop off"=dropoffNH), mean)),
      col=c(colorGradient2),axes = FALSE, ann = FALSE)
text(x=1.3,y = 1.15, labels = "(c) average trip speed", pos = 2, xpd = TRUE)
text(x = 1.1, par("usr")[3]-0.225, labels = "Pick-up location", pos = 2, xpd = TRUE)
text(y = 1.08, par("usr")[3]-0.175, labels = "Drop-off location", srt = 90, pos = 2, xpd = TRUE)
text(x = (1:10-1)/8.8, par("usr")[3]-0.02, labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
text(y = (1:10-1)/8.8, par("usr")[3], labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
legend("right", inset = -0.15,col = colorGradient2,
       legend = c("  Slow",rep("",8),"  Fast"),
       pt.cex = 3, pch = 15, merge=FALSE, bty = 'n' )

par(mfrow=c(1,1))
par(mar=c(3,4,1,1))



### duration versus weekdays ##########
tripdata$weekdays = weekdays(tripdata$tpep_pickup_datetime)
tripdata$weekdays = factor(tripdata$weekdays,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
boxplot(tripdata$duration~tripdata$weekdays,outline=F)

tripdata$pickupHours = as.factor(format(tripdata$tpep_pickup_datetime+60*30,"%H"))
boxplot((tripdata$duration)~tripdata$pickupHours,outline=F)



### A set of three plots that shows trip distance, duration, and average speed over the week #############

par(mfrow = c(3,1))
par(mar = c(3,4,1,1))

colorWeek = c("#f1eef6","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#034e7b")
boxplot(tripdata$duration/60~tripdata$weekdays*tripdata$pickupHours,
        outline=F,col=colorWeek,
        ylab='Trip duration (minutes)', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*7+0.5, cex.axis = 1,labels=F)
axis(1, at = 1:24*7-3, labels = 1:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1)
legend("topleft", inset = .05,col = colorWeek,
       legend = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
       cex = 1, pch = 15, merge=FALSE, bty = 'n' )
legend("topright", inset = .05,
       legend = c("(a) trip duration"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

boxplot(tripdata$trip_distance~tripdata$weekdays*tripdata$pickupHours,
        outline=F,col=colorWeek,
        ylab='Trip distance (Miles)', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*7+0.5, cex.axis = 1,labels=F)
axis(1, at = 1:24*7-3, labels = 1:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1)
#legend("topright", inset = .05,col = colorWeek,
#       legend = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
#       cex = 1, pch = 15, merge=FALSE, bty = 'n' )
legend("topright", inset = .05,
       legend = c("(b) trip distance"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )


### speed vs hour*weekday
boxplot((tripdata$trip_distance/tripdata$duration*3600)~tripdata$weekdays*tripdata$pickupHours,
        outline=F,col=colorWeek,
        ylab='Average trip speed (Mph)', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*7+0.5, cex.axis = 1,labels=F)
axis(1, at = 1:24*7-3, labels = 1:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1)
#legend("topright", inset = .05,col = colorWeek,
#       legend = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
#       cex = 1, pch = 15, merge=FALSE, bty = 'n' )
legend("topright", inset = .05,
       legend = c("(c) average trip speed"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )


par(mfrow = c(1,1))
