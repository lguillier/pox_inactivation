##### This R script enables to estimate the value of D from all raw kinetics data previously collected

#### Packages ####
library(ggplot2)

#### Load all raw kinetics data ####
load(file="Kinetics_data_pox.RData")


#### Estimation of D ####
##### Function to automatize the procedure #####
f_estimate_Dvalues = function(dk) {
  ## This function enables to automatized the estimation of D by fitting linear inactivation model for each kinetic
  ## INTPUT
  ##  dk (data.frame) data frame with at least the following columns
  ##    $ Kinetic_key (factor/character): ID of the kinetic
  ##    $ time_min (numeric): experimental time points (in minutes)
  ##    $ y (numeric): measured of the viral concentration in log10 scale
  ## OUTPUT
  ##    - output (list)
  ##      $ regplot (list) list of de linear regression plot for each kinetic, e.g.:
  ##          $ K102 (ggplot2 object) regression plot for the kinetic K102
  ##          $ K103 (ggplot2 object) regression plot for the kinetic K103
  ##          ...
  ##      $ Dvalues_tab (dataframe) estimated values of log10(D) for each kinetic
  ##          $ Kinetic_key (character): ID of the kinetic
  ##          $ log10Dvalues (numeric): estimated value of log10(D)
  
  # Total number of kinetics in the data frame dk
  nb_kinetics = length(levels(dk$Kinetic_key)) 
  
  ## Initialize the output objects
  # Dvalues_tab
  Dvalues_tab <- data.frame(Kinetic_key = levels(dk$Kinetic_key),
                            #y_variable = rep(NA, nb_kinetics),
                            nb_points = rep(NA, nb_kinetics),
                            Dvalues = rep(NA, nb_kinetics),
                            Dvalues_stderr = rep(NA, nb_kinetics),
                            Dvalues_CV = rep(NA, nb_kinetics))
  # regplot
  regplot <- list()
  
  # Automatic estimation of D
  for (i in 1:nb_kinetics) { # for each kinetic
    # extract the kinetic key
    Kinetic_i <- levels(dk$Kinetic_key)[i]
    
    # console check
    writeLines(paste("kinetic", Kinetic_i))
    
    # extract the data associated with the considered kinetic
    tab_Kinetic_i <- subset(dk, Kinetic_key == Kinetic_i)
    
    # total number of available points associated with the considered kinetic
    nb_points_i <- sum(!is.na(tab_Kinetic_i$y))
    
    # save the kinetic keys and number of points in the output data frame
    Dvalues_tab$Kinetic_key[i] <- Kinetic_i
    Dvalues_tab$nb_points[i] <- nb_points_i
    
    # save the regression plot associated with the considered kinetic
    regplot[[Kinetic_i]] <- ggplot(data = tab_Kinetic_i,
                                   mapping = aes(x = time_min, y = y)) +
      geom_point(shape = 18, size=3) +
      geom_smooth(formula = y~x, method = lm, colour = "black") +
      theme(axis.ticks=element_blank(),
            axis.title = element_text(face="bold", size=14),
            panel.background=element_rect(fill="white"),
            panel.grid=element_line(colour="grey")) +
      labs(title = paste("Kinetic key:", Kinetic_i)) +
      xlab("time (minutes)") + ylab("log10 reduction")
    
    
    ## Estimation of D for each kinetic by fitting log-linear regression model on data
    skip_to_next <- FALSE
    tryCatch({
      model_i <- nls(data = tab_Kinetic_i,
                     formula = y ~ b - (time_min/D_value),
                     start = list(b = tab_Kinetic_i$y[1],
                                  D_value = -1/lm(data = tab_Kinetic_i, 
                                                  formula = y ~ time_min)$coefficients[2]),
                     algorithm = "port")
      
      # extract the estimated values of D and its standard error
      Dvalues_i <- summary(model_i)$coefficients[2,1]
      Dvalues_stderr_i <- summary(model_i)$coefficients[2,2]
      
      # save estimated values in the output data frame Dvalues_tab
      Dvalues_tab$Dvalues[i] <- abs(Dvalues_i)
      Dvalues_tab$Dvalues_stderr[i] <- Dvalues_stderr_i
      Dvalues_tab$Dvalues_CV[i] <- Dvalues_stderr_i / abs(Dvalues_i)
    }, error = function(e) { skip_to_next <<- TRUE})
    
    if(skip_to_next) { next }  

  }
  
  ## Prepare the output object (regression plots and data frame of estimated values)
  output <- list("regplot" = regplot,
                 "Dvalues_tab" = Dvalues_tab)
  
  return(output)
}

##### Estimation of D: automatic run #####
D <- f_estimate_Dvalues(dk)


##### Processing non-convergence cases #####

#### k036 ####
key <- "k036"
row_id <- which(D$Dvalues_tab$Kinetic_key == key)
d <- subset(dk, Kinetic_key==key)
#D$regplot$K039
#-1/lm(data = d, formula = y ~ time_hours)$coefficients[2] 
model_nls <- nls(data = d, formula = y ~ b - (time_min/D_value),
                 start = list(b = 0,
                              D_value = 5),
                 algorithm = "port")
summary(model_nls)
D$Dvalues_tab[row_id,]$Dvalues <- summary(model_nls)$coefficients[2,1]
D$Dvalues_tab[row_id,]$Dvalues_stderr <- summary(model_nls)$coefficients[2,2]
D$Dvalues_tab[row_id,]$Dvalues_CV <- summary(model_nls)$coefficients[2,2] / abs(summary(model_nls)$coefficients[2,1])



#### k039 ####
key <- "k039"
row_id <- which(D$Dvalues_tab$Kinetic_key == key)
d <- subset(dk, Kinetic_key==key)
#D$regplot$K039
#-1/lm(data = d, formula = y ~ time_hours)$coefficients[2] 
model_nls <- nls(data = d, formula = y ~ b - (time_min/D_value),
                 start = list(b = 0,
                              D_value = 5),
                 algorithm = "port")
summary(model_nls)
D$Dvalues_tab[row_id,]$Dvalues <- summary(model_nls)$coefficients[2,1]
D$Dvalues_tab[row_id,]$Dvalues_stderr <- summary(model_nls)$coefficients[2,2]
D$Dvalues_tab[row_id,]$Dvalues_CV <- summary(model_nls)$coefficients[2,2] / abs(summary(model_nls)$coefficients[2,1])

#### k040 ####
key <- "k040"
row_id <- which(D$Dvalues_tab$Kinetic_key == key)
d <- subset(dk, Kinetic_key==key)
#D$regplot$K039
#-1/lm(data = d, formula = y ~ time_hours)$coefficients[2] 
model_nls <- nls(data = d, formula = y ~ b - (time_min/D_value),
                 start = list(b = 0,
                              D_value = 5),
                 algorithm = "port")
summary(model_nls)
D$Dvalues_tab[row_id,]$Dvalues <- summary(model_nls)$coefficients[2,1]
D$Dvalues_tab[row_id,]$Dvalues_stderr <- summary(model_nls)$coefficients[2,2]
D$Dvalues_tab[row_id,]$Dvalues_CV <- summary(model_nls)$coefficients[2,2] / abs(summary(model_nls)$coefficients[2,1])


#### Calculate log10(D) ####
D$Dvalues_tab <- mutate(D$Dvalues_tab, log10D = log10(Dvalues))



# Clear the environment #
rm(d, model_nls, key, row_id, f_estimate_Dvalues)



# Save data with estimated values of D
save.image("Dvalues_data_pox.RData")
