# Data extraction for sim 100 project:

extractData <- function() {
  
  ######
  # Stan
  ######
  
  # List all CSV files in the folder
  file_list <- list.files(path = paste0("fits/model", modelNr, "/"), pattern = "sim", full.names = T)
  
  # Initialize an empty dataframe to store the extracted data
  simM <- data.frame()
  # Loop through each file
  for (file in file_list) {
    data <- read.csv(file)
    extracted_data <- data[, "m"] 
    simM <- rbind(simM, extracted_data)
  }
  colnames(simM) <- varsNames[1:nrow(data)]
  
  simM <- simM %>% 
    mutate_all(function(x) {
      mean_val <- mean(x, na.rm = TRUE)
      std_dev <- sd(x, na.rm = TRUE)
      filtered_x <- ifelse(abs(x - mean_val) <= stdIn * std_dev, x, NA)
      return(filtered_x)
    }) %>%
    drop_na() %>% 
    pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'm') 
  simM$Model = "Stan"
  
  NOPars = nrow(data)
  print(paste0("Removed ", (NOPars*100-nrow(simM))/NOPars, " Stan estimates out of 100"))
  
  simTR <- data.frame()
  
  # Loop through each file
  for (file in file_list) {
    data <- read.csv(file)
    extracted_data <- data[, c("time", "rhat")] 
    simTR <- rbind(simTR, extracted_data)
  }
  
  simTR <- unique(simTR)
  
  print(paste0("Stan had mean convergence time: ", mean(simTR$time, na.rm = T),
               " minutes (sd = ", round(sd(simTR$time, na.rm = T),2), ")", 
               ", and median Rhat ", round(median(simTR$rhat, na.rm = T),3),
               " (sd = ", round(sd(simTR$rhat, na.rm = T),2), ")"))
  
  simTR <- simTR %>%
    pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'm')
  simTR$Model = "Stan"
  
  simM <- rbind(simM, simTR)
  
  ######
  # Mplus
  ######
  
  # List all CSV files in the folder
  file_list <- list.files(path = paste0("mplus/model_", modelNr, "/sim100/"), pattern = "sim", full.names = T)
  
  # Initialize an empty dataframe to store the extracted data
  simP <- data.frame()
  # Loop through each file
  for (file in file_list) {
    data <- read.csv(file)
    extracted_data <- data[, "est"] 
    simP <- rbind(simP, extracted_data)
  }
  colnames(simP) <- varsNames[1:nrow(data)]
  
  simP <- simP %>% 
    mutate_all(function(x) {
      mean_val <- mean(x, na.rm = TRUE)
      std_dev <- sd(x, na.rm = TRUE)
      filtered_x <- ifelse(abs(x - mean_val) <= stdIn * std_dev, x, NA)
      return(filtered_x)
    }) %>%
    drop_na() %>% 
    pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'm') 
  simP$Model = "Mplus"
  
  print(paste0("Removed ", (NOPars*100-nrow(simP))/NOPars, " Mplus estimates out of 100"))
  
  simTR <- data.frame()
  # Loop through each file
  for (file in file_list) {
    data <- read.csv(file)
    extracted_data <- data[, c("mplus_time", "rhatMean")] 
    simTR <- rbind(simTR, extracted_data)
  }
  
  colnames(simTR) <- c("time", "rhat")
  
  simTR <- unique(simTR)
  
  print(paste0("Mplus had mean convergence time: ", mean(simTR$time, na.rm = T)," minutes (sd = ", round(sd(simTR$time, na.rm = T),3), ")", ", and mean Rhat ", round(mean(simTR$rhat, na.rm = T),4)," (sd = ", round(sd(simTR$rhat, na.rm = T),3), ")"))
  
  simTR <- simTR %>% 
    pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'm') 
  simTR$Model = "Mplus"
  
  simP <- rbind(simP, simTR)
  
  gt <- data.frame(parameter = c(varsNames[1:nrow(data)], "time", "rhat"), 
                   m = c(gamma[1:nrow(data)], NA, NA), 
                   Model = "Ground Truth")
  # save to global env <<-
  simM <<- rbind(simM, simP, gt)
}