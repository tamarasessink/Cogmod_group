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
grand_mean_dial = mean(mean_dial_pp$x)
grand_mean_steer = mean(mean_steer_pp$x)

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
dial <- aggregate(devDial$lanePosition, list(devDial$Event2,devDial$pp), mean)
steer <- aggregate(devSteer$lanePosition, list(devSteer$Event2,devSteer$pp), mean)

dial2 <- aggregate(devDial$timeRelativeToTrialStart/1000, list(devDial$Event2,devDial$pp), mean) 
steer2 <- aggregate(devSteer$timeRelativeToTrialStart/1000, list(devSteer$Event2,devSteer$pp), mean)

# Avg across participant
dial_acrosspart <- aggregate(dial$x, list(dial$Group.1), mean) 
steer_acrosspart <- aggregate(steer$x, list(steer$Group.1), mean) 
dial2_sd <- aggregate(dial$x, list(dial$Group.1), sd) 
steer2_sd <- aggregate(steer$x, list(steer$Group.1), sd) 

dial2 <- aggregate(dial2$x, list(dial2$Group.1), mean) 
steer2 <- aggregate(steer2$x, list(steer2$Group.1), mean) 


# sort order
dial3 <- sort(dial2$x, decreasing = FALSE, na.last = TRUE)
steer3 <- sort(steer2$x, decreasing = FALSE, na.last = TRUE)

# Plot lateral deviation over dialingTime
plot(dial2$x,dial_acrosspart$x,col="blue",type="p",ylim=0:1, xlab='dialingTime (s)', ylab='Lateral deviation (m)')
par(new=T)
plot(steer2$x,steer_acrosspart$x,col="green", type="p",axes=F, xlab='', ylab='')

arrows(x0=dial2$x, y0=dial_acrosspart$x-dial2_sd$x, x1=dial2$x, y1=dial_acrosspart$x+dial2_sd$x, code=3, angle=90, length=0.005)
arrows(x0=steer2$x, y0=steer_acrosspart$x-steer2_sd$x, x1=steer2$x, y1=steer_acrosspart$x+steer2_sd$x, code=3, angle=90, length=0.005)

## ggplot
# line
ggplot(dial3) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.5) +
  geom_linerange( aes(x=name, ymin=value-sd, ymax=value+sd), colour="orange", alpha=0.9, size=1.3)
