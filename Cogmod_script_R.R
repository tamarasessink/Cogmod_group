library(dplyr)
library(ggplot2)
attach(mtcars)

######### 1A #########
# Copy data into new dataframe in case something goes wrong
no_error_trials <- keyPressDataWithLaneDeviation
# Select only non-error trials
no_error_trials <- no_error_trials[no_error_trials$typingErrorMadeOnTrial == 0, ]  

# Split data into dualDial and dualSteer
dualDial <- no_error_trials[no_error_trials$partOfExperiment == "dualDialFocus", ]
dualSteer <- no_error_trials[no_error_trials$partOfExperiment == "dualSteerFocus", ]

# Calculate total dialing time
dialingtime_dial <- dualDial[dualDial$phoneNrLengthAfterKeyPress == 11, ] 
dialingtime_steer <- dualSteer[dualSteer$phoneNrLengthAfterKeyPress == 11, ] 

# Calculate mean dialing time
mean_dial_pp <- aggregate(dialingtime_dial$timeRelativeToTrialStart, list(dialingtime_dial$pp), mean)
mean_steer_pp <- aggregate(dialingtime_steer$timeRelativeToTrialStart, list(dialingtime_steer$pp), mean)

# Grand Mean
grand_mean_dial = mean(mean_dial_pp$x)/1000
grand_mean_steer = mean(mean_steer_pp$x)/1000

# Grand sd
grand_sd_dial = sd(mean_dial_pp$x)
grand_sd_steer = sd(mean_steer_pp$x)

# Grand SE
grand_se_dial = grand_sd_dial/(sqrt(nrow(mean_dial_pp)))
grand_se_steer = grand_sd_steer/(sqrt(nrow(mean_steer_pp)))

######### 1B #########
lanedev_df <- keyPressDataWithLaneDeviation

# Split conditions
devDial <- dualDial[dualDial$partOfExperiment == "dualDialFocus", ]
devSteer <- dualSteer[dualSteer$partOfExperiment == "dualSteerFocus", ]

# Apply absolute value
devDial$lanePosition <- abs(devDial$lanePosition)
devSteer$lanePosition <- abs(devSteer$lanePosition)

# Grand Mean
grand_mean_dialdev = mean(devDial$lanePosition)
grand_mean_steerdev = mean(devSteer$lanePosition)

# Grand sd
grand_sd_dialdev = sd(devDial$lanePosition)
grand_sd_steerdev = sd(devSteer$lanePosition)

# Grand SE
grand_se_dialdev = grand_sd_dialdev/(sqrt(nrow(devDial)))
grand_se_steerdev = grand_sd_steerdev/(sqrt(nrow(devDial)))

######## 1C ########

# Calculate avg per participant per digit
dial <- aggregate(devDial$lanePosition, list(devDial$phoneNrLengthAfterKeyPress,devDial$pp), mean)
steer <- aggregate(devSteer$lanePosition, list(devSteer$phoneNrLengthAfterKeyPress,devSteer$pp), mean)

dial2 <- aggregate(devDial$timeRelativeToTrialStart/1000, list(devDial$phoneNrLengthAfterKeyPress,devDial$pp), mean) 
steer2 <- aggregate(devSteer$timeRelativeToTrialStart/1000, list(devSteer$phoneNrLengthAfterKeyPress,devSteer$pp), mean)

# Avg across participant
dial_acrosspart <- aggregate(dial$x, list(dial$Group.1), mean) # y
steer_acrosspart <- aggregate(steer$x, list(steer$Group.1), mean) # y
dial2_sd <- aggregate(dial$x, list(dial$Group.1), sd) 
steer2_sd <- aggregate(steer$x, list(steer$Group.1), sd) 

dial2 <- aggregate(dial2$x, list(dial2$Group.1), mean) # x
steer2 <- aggregate(steer2$x, list(steer2$Group.1), mean) # x

# Plot lateral deviation over dialingTime
plot(dial2$x,dial_acrosspart$x,col="blue",type="b",ylim = c(0,2), xlim = c(0,10), xlab='dialingTime (s)', ylab='Lateral deviation (m)')
arrows(x0=dial2$x, y0=dial_acrosspart$x-dial2_sd$x, x1=dial2$x, y1=dial_acrosspart$x+dial2_sd$x, code=3, angle=90, length=0.005)

par(new=T)
plot(steer2$x,steer_acrosspart$x,col="green", type="b",axes=F, xlab='', ylab='')
legend("topleft", legend=c("Dial", "Steer"),
       col=c("blue", "green"), lty=1:2, cex=0.8)

arrows(x0=steer2$x, y0=steer_acrosspart$x-steer2_sd$x, x1=steer2$x, y1=steer_acrosspart$x+steer2_sd$x, code=3, angle=90, length=0.005)

## ggplot
vec <- rep(c(0),each=13)
vec1 <- rep(c(1),each=13)
dial_df <- data.frame(dial2$x,dial_acrosspart$x,vec)
steer_df <- data.frame(steer2$x,steer_acrosspart$x,vec1)
names(dial_df) <- names(steer_df) 
bothdfs <- rbind(dial_df, steer_df)

# SE
se_dial = dial2_sd$x/(sqrt(nrow(dial2_sd)))
se_steer = steer2_sd$x/(sqrt(nrow(steer2_sd)))

bothse <- rbind(se_dial, se_steer)


ggplot(data = bothdfs, mapping = aes(x = steer2.x, y = steer_acrosspart.x, colour=vec1, group = vec1)) + geom_line() + geom_point() + 
  geom_errorbar(aes(ymin=steer_acrosspart.x-bothse, ymax=steer_acrosspart.x+bothse), width=.1) +
  geom_line() +
  geom_point() + theme(legend.position=c(1,0))   

# 1D
# People in the steering focused condition have no lane deviation increase between digit 6-7 which means that they did take a break from dialing to focus on steering. 
# In the dialing focused condition, the lane deviation did increase a lot between digit 6-7 which means that the average participant did not take a break from dialing. 

# 2A

# Select data between trial time of 15000 and 18000
drift1518 <- tableOfDriftValuesCalibration[tableOfDriftValuesCalibration$trialTime < 18000,]
drift1518 <- drift1518[drift1518$trialTime > 15000,]

# Plot data
ggplot(data = drift1518, mapping = aes(x = trialTime/1000, y = posX, colour=trial, group = trial))+ 
  geom_line() + theme(legend.position=c(1,0)) 

#2B
library(reshape2)
allData <- as.data.frame(matrix(nrow = 20, ncol =61))
for (x in 1:20)
  {
    simulate <- rnorm(61, 0.0, 0.13)
    simulate <- cumsum(simulate)
    allData[x, ] <- simulate
}

transposedData <- data.frame(t(allData))
transposedData$ID <- seq(0,3000, 50)

transposedData <- melt(transposedData, id.vars="ID")

simulatePlot <- ggplot(transposedData, aes(x=as.numeric(ID), y= value, colour = variable)) + 
  xlab('Trial Time(ms)') + ylab('Lateral Position (m)') + theme(legend.position = "none") + geom_line()
simulatePlot

3. 
singletask <- subset(notypingerrors, partOfExperiment == "singleDialing2")
View(singletask)
keypressespp <- aggregate(singletask$timeRelativeToTrialStart, list("key"= singletask$phoneNrLengthAfterKeyPress, "pp" = singletask$pp), mean)
keypresses <- aggregate(keypressespp$x, list("key"= keypressespp$key),mean)
View(keypressespp)

#sum of interval per key
sumlistintervalperkey <- c()
sumlistintervalperkey <- keypresses$x
print(sumlistintervalperkey)
intervalperkey <- c()

# calculate the time for each key seperate
for (number in 1:length(keypresses$key)-1)
{
  answer <- sumlistintervalperkey[number+1] - sumlistintervalperkey[number]
  intervalperkey[number] <- answer
}
# mean of keypresses
round(mean(intervalperkey),digits=0)
