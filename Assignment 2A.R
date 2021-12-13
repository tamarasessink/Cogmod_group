tableResponses <- read.table("C:\\Users\\Tamara\\Downloads\\Lab assignment 2\\NeuralResponses - kopie.csv",header = FALSE)
View(tableResponses)

simualted_noise <- list()

for (x in 1:12)
{
  noise <- data.frame()
  for (y in 1:nrow(tableResponses))
  {
    noise <- rbind(noise, rnorm(100, 0, 1))
  }
  
  simualted_noise[[x]] <- tableResponses + noise
}

# with x you can call a certain participant
View(simualted_noise[[1]])

# RDM neural data
RDM_neural_data <- list()
RDM_neural_data <- 1 - cor(t(tableResponses), method = c("pearson"))
View(RDM_neural_data)

# RDM data participants
RDM_participants <- list()

for(i in 1:12){
  RDM_participants[[i]] <- 1 - cor(t(simualted_noise[[i]]), method = c("pearson"))
}

# melting a dataframe, to compare all elements with each other
neural_data <- melt(RDM_neural_data)
View(neural_data)
# single example subject thus those one of 1-12
participant5 <- melt(RDM_participants[[5]])
View(participant5)

# heatmap neural data
plot1 <- ggplot(neural_data, aes(x = Var1,
                  y = Var2,
                  fill = value))+ geom_tile()+ scale_fill_distiller(palette = "Spectral")+ ggtitle("RDM Original data")+ labs(fill = "Dissimilarity")
plot(plot1)

# heatmap participant data
plot2 <- ggplot(participant5, aes(x = Var1, 
                                  y = Var2, 
                                  fill = value))+ geom_tile()+ scale_fill_distiller(palette = "Spectral")+ ggtitle("RDM data of single particiant with noise")+ labs(fill = "Dissimilarity")
plot(plot2)

# average of all the 12 participants
participant1 <- melt(RDM_participants[[1]])
View(participant1)
meanparticipants <- participant1
for (par in 1:12)
{
  participant <- melt(RDM_participants[[i]])
  
  for (j in 2:length(participant))
  {
    meanparticipants$value[j] <- (participant$value[j] + meanparticipants$value[j]) / 2
  }
  
}

# Heatmap for average
plot3 <- ggplot(meanparticipants, aes(x = Var1, 
                                  y = Var2, 
                                  fill = value))+ geom_tile()+ scale_fill_distiller(palette = "Spectral")+ ggtitle("RDM average of the participants with noise")+ labs(fill = "Dissimilarity")
plot(plot3)
