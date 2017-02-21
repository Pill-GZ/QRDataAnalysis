library(MASS)

################ Modelling average raod speed over time #################

###


tripCount = with(tripdata,tapply(rep(1,length(pickupNH)),list("pick up" = pickupNH, "drop off" = dropoffNH),sum))+3

hourSplit = c(3:24)
tripCountByTime = array(0,dim=c(10,10,length(hourSplit)))
for (i in 1:length(hourSplit)) {
  indexByTime = (as.numeric(tripdata$pickupHours) > hourSplit[i]-3) & (as.numeric(tripdata$pickupHours) <= hourSplit[i])
  
  tripCountByTime[,,i] = with(tripdata[indexByTime,],
                         tapply(
                           rep(1,length(pickupNH[indexByTime])),
                           list("pick up" = pickupNH[indexByTime], "drop off" =
                                  dropoffNH[indexByTime]),
                           sum
                         ))
  tripCountByTime[,,i][is.na(tripCountByTime[,,i])] <- 0
  tripCountByTime[,,i] = tripCountByTime[,,i]+1
  #image(
  #  log(tripCountByTime[,,i]/tripCount),
  #  col = c(colorGradient),axes = FALSE, ann = FALSE
  #)
}

varMatrix = matrix(0,10,10)
for(i in 1:10){
  for(j in 1:10){
    varMatrix[i,j] = sd(tripCountByTime[i,j,])
  }
}


par(mar=c(5,5,2,4))
image(
  log(tripCountByTime[,,7-2]/tripCount),
  col = c(colorGradient),axes = FALSE, ann = FALSE
)
text(x = 1.2,y = 0.6, labels = "rel.\n freq.", pos = 1, xpd = TRUE,cex=0.8)
text(x = 1.1, par("usr")[3]-0.225, labels = "Pick up location", pos = 2, xpd = TRUE)
text(y = 1.0, par("usr")[3]-0.225, labels = "Drop off location", srt = 90, pos = 2, xpd = TRUE)
text(x = (1:10-1)/8.8, par("usr")[3]-0.02, labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
text(y = (1:10-1)/8.8, par("usr")[3], labels = row.names(neighbourhoods)[-11], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
legend("right", inset = -0.15,col = colorGradient,
       legend = c("  Less",rep("",8),"  More"),
       pt.cex = 3, pch = 15, merge=FALSE, bty = 'n', xpd = TRUE)

daySplit = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

for (i in daySplit) {
  indexByTime = (tripdata$weekdays==i)
  
  tripCountByTime = with(tripdata[indexByTime,],
                         tapply(
                           rep(1,length(pickupNH[indexByTime])),
                           list("pick up" = pickupNH[indexByTime], "drop off" =
                                  dropoffNH[indexByTime]),
                           sum
                         ))
  tripCountByTime[is.na(tripCountByTime)] <- 0
  tripCountByTime = tripCountByTime + 1
  #image(
  #  log(tripCountByTime/tripCount),
  #  col = c(colorGradient),axes = FALSE, ann = FALSE
  #)
}

############ Variance Stablization #################

tripdata$speed = tripdata$trip_distance/tripdata$duration*3600
attach(tripdata)

# Do a Box-Cox transformation to stablize variance
par(mar=c(4,4,1,1))
test <- boxcox(speed~weekdays*pickupHours)
lambda <- test$x[which(test$y==max(test$y))]
lambda <- 0.1414141

# Check the boxplots again
boxplot(speed^lambda~weekdays*pickupHours,outline=F,col=colorWeek,
        ylab='Box-Cox Transformed speed', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*7+0.5, cex.axis = 1,labels=F)
axis(1, at = 1:24*7-3, labels = 1:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1)
legend("topright", inset = .05,
       legend = c("(c) average trip speed"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

# Actually we prefer the log transform, which is easier to interpret
boxplot(log(speed)~weekdays*pickupHours,outline=F,col=colorWeek,
        ylab='Box-Cox Transformed speed', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*7+0.5, cex.axis = 0.8,labels=F)
axis(1, at = 1:24*7-3, labels = 1:24, cex.axis = 0.8,tick = F)
axis(2, cex.axis = 1)
legend("topright", inset = .05,
       legend = c("average trip speed"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

# Finalize the transformation
detach(tripdata)
tripdata$speed <- log(tripdata$speed)

################### Model fitting #########################
attach(tripdata)
##### day-of-the-week #####

modelfit.weekdays <- lm(speed~weekdays)
summary(modelfit.weekdays)
BIC(modelfit.weekdays)
# 167831.5

weekend.indicator <- (weekdays=="Saturday")|(weekdays=="Sunday")
modelfit.weekend <- lm(speed~weekend.indicator)
summary(modelfit.weekend)
BIC(modelfit.weekend)
# 168669

sunday <- (weekdays=="Sunday")
saturday <- (weekdays=="Saturday")
weekends <- factor(saturday+2*sunday)
levels(weekends) <- c("weekdays","Saturday","Sunday")
modelfit.weekends <- lm(speed~weekends)
summary(modelfit.weekends)
BIC(modelfit.weekends)
# 168132.2

# We prefer the full model, and will build upon that.

##### time-of-the-day #####

modelfit.day.hour <- lm(speed~weekdays+pickupHours)
summary(modelfit.day.hour)
BIC(modelfit.day.hour)
# 152109

modelfit.dayhour <- lm(speed~weekdays*pickupHours)
summary(modelfit.dayhour)
beta<-modelfit.dayhour$coefficients
BIC(modelfit.dayhour)
# 148474.4

# Check fitted values
nHours = length(levels(pickupHours))
nWeekdays = length(levels(weekdays))

test = data.frame(cbind(weekdays=rep(levels(weekdays),each=nHours),pickupHours=rep(levels(pickupHours),nWeekdays)))
fitted.values = predict(modelfit.dayhour,test)

add.alpha <- function(COLORS, ALPHA){
  if(missing(ALPHA)) stop("provide a value for alpha between 0 and 1")
  RGB <- col2rgb(COLORS, alpha=TRUE)
  RGB[4,] <- round(RGB[4,]*ALPHA)
  NEW.COLORS <- rgb(RGB[1,], RGB[2,], RGB[3,], RGB[4,], maxColorValue = 255)
  return(NEW.COLORS)
}

image(1:(24*7),1:4,matrix(0,24*7,4),col = add.alpha("white",0.5),ann=F, axes=F,add=T)

for(i in 1:nWeekdays){
  j = 1:nHours
  lines(i+(j-1)*nWeekdays, fitted.values[j+(i-1)*nHours],
        pch=16,col="white",lwd=4)
}
for(i in 1:nWeekdays){
  j = 1:nHours
  lines(i+(j-1)*nWeekdays, fitted.values[j+(i-1)*nHours],
        pch=16,col=colorWeek[i],lwd=2)
}
mtext(side = 1,text = 'Hours',line = 2)
legend("bottomleft", inset = .05,col = colorWeek,
       legend = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
       cex = 0.8, pch = 15, merge=FALSE, bty = 'n' )


######### Residual Plots #############

par(mfrow=c(3,1))

boxplot(modelfit.dayhour$residuals~pickupHours,outline=T,pch='.',ylim=c(-2.5,2.5))
mtext(side = 1,text = 'Hours',line = 2.25)
mtext(side = 2,text = 'Residuals',line = 2)
legend("topright", inset = .05,
       legend = c("(a) by pick up hour"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

plot(modelfit.dayhour$residuals~log(trip_distance),pch='.',ylim=c(-2.5,2.5),xlim=c(-3,3),ann = F)
points(log(trip_distance[(pickupNH==10)|(dropoffNH==10)]),
       modelfit.dayhour$residuals[(pickupNH==10)|(dropoffNH==10)],pch='.',col=colorGradient[1])
points(log(trip_distance[(pickupNH==9)|(dropoffNH==9)]),
       modelfit.dayhour$residuals[(pickupNH==9)|(dropoffNH==9)],pch='.',col=colorGradient[3])
mtext(side = 1,text = 'Log trip distance',line = 2.25)
mtext(side = 2,text = 'Residuals',line = 2)
legend("topright", inset = .05,
       legend = c("(b) against trip distance"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )
legend("bottomright", inset = .05,
       legend = c("Trip to/from JFK","Trip to/from LGA"),
       pt.cex=2,cex = 1, pch = 15, col = colorGradient[c(1,3)],merge=FALSE, bty = 'n' )

boxplot(modelfit.dayhour$residuals~dropoffNH,
        pch='.',ylim=c(-2.5,2.5), xaxt = 'n', ann = FALSE)
axis(1, at = 1:10, labels = row.names(neighbourhoods)[-11],srt = 45, cex.axis = 0.9,tick = F)
mtext(side = 2,text = 'Residuals',line = 2)
legend("topright", inset = .05,
       legend = c("(c) by drop off location"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

##### Adding log(trip_distance) to the model #####


modelfit.dayhourlogd = lm(speed~weekdays*pickupHours+log(trip_distance))
BIC(modelfit.dayhourlogd)
# 106652.9

par(mfrow=c(3,1))

boxplot(modelfit.dayhourlogd$residuals~pickupHours,outline=T,pch='.',ylim=c(-2.5,2.5))
mtext(side = 1,text = 'Hours',line = 2)
mtext(side = 2,text = 'Residuals',line = 2)

plot(modelfit.dayhourlogd$residuals~log(trip_distance),pch='.',ylim=c(-2.5,2.5),xlim=c(-3,3),ann = F)
points(log(trip_distance[(pickupNH==10)|(dropoffNH==10)]),
       modelfit.dayhourlogd$residuals[(pickupNH==10)|(dropoffNH==10)],pch='.',col=4)
points(log(trip_distance[(pickupNH==9)|(dropoffNH==9)]),
       modelfit.dayhourlogd$residuals[(pickupNH==9)|(dropoffNH==9)],pch='.',col=3)
mtext(side = 1,text = 'Log trip distance',line = 2)
mtext(side = 2,text = 'Residuals',line = 2)

boxplot(modelfit.dayhourlogd$residuals~pickupNH*dropoffNH,
        pch='.',ylim=c(-2.5,2.5),ann = F,axes = F)
#boxplot(modelfit.dayhourlogd$residuals~pickupNH,
#        pch='.',ylim=c(-2.5,2.5),ann = F)
#boxplot(modelfit.dayhourlogd$residuals~dropoffNH,
#        pch='.',ylim=c(-2.5,2.5),ann = F)


##### Adding neighborhood to the model #####

modelfit.dayhourlogdOriDest = lm(speed~weekdays*pickupHours+log(trip_distance)+
                                 as.factor(pickupNH)+as.factor(dropoffNH))
BIC(modelfit.dayhourlogdOriDest)
# 90763.39

modelfit.dayhourlogdRoute = lm(speed~weekdays*pickupHours+log(trip_distance)+
                                 as.factor(pickupNH)*as.factor(dropoffNH))
BIC(modelfit.dayhourlogdRoute)
# 89683.83

# Check fitted values

test2 = data.frame(cbind(weekdays = rep(levels(weekdays),each=nHours),
                         pickupHours = rep(levels(pickupHours),nWeekdays),
                         trip_distance = log(median(trip_distance)),
                         pickupNH = factor(5,levels = as.character(1:10)),
                         dropoffNH = factor(5,levels = as.character(1:10))
                         ))
test2$trip_distance = as.numeric(test2$trip_distance)
fitted.values2 = predict(modelfit.dayhourlogdRoute,test2)

par(mfrow=c(1,1))
boxplot(speed~weekdays*pickupHours,outline=F,col=colorWeek,
        ylab='Box-Cox Transformed speed', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*7+0.5, cex.axis = 0.8,labels=F)
axis(1, at = 1:24*7-3, labels = 1:24, cex.axis = 0.8,tick = F)
axis(2, cex.axis = 1)
legend("topright", inset = .05,
       legend = c("average trip speed"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

for(i in 1:nWeekdays){
  j = 1:nHours
  lines(i+(j-1)*nWeekdays, fitted.values2[j+(i-1)*nHours],
        pch=16,col="white",lwd=4)
}
for(i in 1:nWeekdays){
  j = 1:nHours
  lines(i+(j-1)*nWeekdays, fitted.values2[j+(i-1)*nHours],
        pch=16,col=colorWeek[i],lwd=2)
}
mtext(side = 1,text = 'Hours',line = 2)
legend("bottomleft", inset = .05,col = colorWeek,
       legend = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
       cex = 0.8, pch = 15, merge=FALSE, bty = 'n' )



# residual plots 

par(mfrow=c(3,1))

boxplot(modelfit.dayhour$residuals~pickupHours,outline=T,pch='.',ylim=c(-2.5,2.5))
mtext(side = 1,text = 'Hours',line = 2.25)
mtext(side = 2,text = 'Residuals',line = 2)
legend("topright", inset = .05,
       legend = c("(a) by pick up hour"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

plot(modelfit.dayhourlogdRoute$residuals~log(trip_distance),pch='.',ylim=c(-2.5,2.5),xlim=c(-3,3),ann = F)
points(log(trip_distance[(pickupNH==10)|(dropoffNH==10)]),
       modelfit.dayhourlogdRoute$residuals[(pickupNH==10)|(dropoffNH==10)],pch='.',col=colorGradient[1])
points(log(trip_distance[(pickupNH==9)|(dropoffNH==9)]),
       modelfit.dayhourlogdRoute$residuals[(pickupNH==9)|(dropoffNH==9)],pch='.',col=colorGradient[3])
mtext(side = 1,text = 'Log trip distance',line = 2.25)
mtext(side = 2,text = 'Residuals',line = 2)
legend("topright", inset = .05,
       legend = c("(b) against trip distance"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )
legend("bottomright", inset = .05,
       legend = c("Trip to/from JFK","Trip to/from LGA"),
       pt.cex=2,cex = 1, pch = 15, col = colorGradient[c(1,3)],merge=FALSE, bty = 'n' )

boxplot(modelfit.dayhourlogdRoute$residuals~dropoffNH,
        pch='.',ylim=c(-2.5,2.5), xaxt = 'n', ann = FALSE)
axis(1, at = 1:10, labels = row.names(neighbourhoods)[-11],srt = 45, cex.axis = 0.9,tick = F)
mtext(side = 2,text = 'Residuals',line = 2)
legend("topright", inset = .05,
       legend = c("(c) by drop off location"),
       cex = 1, pch = NA, merge=FALSE, bty = 'n' )

######### Extract the time effect ###########

par(mfrow=c(1,1))
timeEffectMonday = data.frame(summary(modelfit.dayhourlogdRoute)$coefficients[8:30,])
timeEffectMondayNaive = data.frame(summary(modelfit.dayhour)$coefficients[8:30,])
# Naive model
with(timeEffectMondayNaive,
     plot(2:24,Estimate,type='b',
          ylim=c(min(Estimate)-1.96*max(Std..Error),max(Estimate)+1.96*max(Std..Error)),
          lwd = 2, col = rgb(239/256,101/256,72/256,1),ann =F,xaxt='n',yaxt='n',axes=F)
     )
with(timeEffectMondayNaive, polygon(x = c(2:24,24:2),
                               y = c(Estimate-2.58*Std..Error,rev(Estimate+2.58*Std..Error)),
                               col=rgb(239/256,101/256,72/256,0.3), border=NA)
)
with(timeEffectMondayNaive, polygon(x = c(2:24,24:2),
                               y = c(Estimate-1.96*Std..Error,rev(Estimate+1.96*Std..Error)),
                               col=rgb(239/256,101/256,72/256,0.3), border=NA)
)
with(timeEffectMondayNaive,
     lines(2:24,Estimate,type='b',
          lwd = 2, col = rgb(239/256,101/256,72/256,1),ann =F,xaxt='n',yaxt='n')
)
# Full model
with(timeEffectMonday, polygon(x = c(2:24,24:2),
                               y = c(Estimate-2.58*Std..Error,rev(Estimate+2.58*Std..Error)),
                               col=rgb(116/256,169/256,207/256,0.3), border=NA)
)
with(timeEffectMonday, polygon(x = c(2:24,24:2),
                               y = c(Estimate-1.96*Std..Error,rev(Estimate+1.96*Std..Error)),
                               col=rgb(116/256,169/256,207/256,0.3), border=NA)
)
with(timeEffectMonday,
     lines(2:24,Estimate,type='b',
           lwd = 2, col = rgb(3/256,78/256,123/256,1),ann =F,xaxt='n',yaxt='n')
)

text(x = 1,y = 0.6, labels = "log(speed)", pos = 1, xpd = TRUE,cex=1)
text(x = 24, y = -0.85, labels = "Hours", pos = 2, xpd = TRUE)
axis(1, at = 1:24+0.5, cex.axis = 1,labels=F,lwd=2)
axis(1, at = 2:24, labels = 2:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1,lwd=2)
legend("topright", inset = .05,
       legend = c("Naive model","","Full model"),
       pt.cex = 2, pch = 15, merge=FALSE, bty = 'n',
       col = c(rgb(239/256,101/256,72/256,1),"white",rgb(3/256,78/256,123/256,1)))


