# Commonly used functions:

# Melting VersaMax data to give a single dataframe of all plates and zeroing the data
# Dependencies: 
library(stringr)
library(reshape2)

# Function:
versa <- function(raw_date){
  exp_data <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(exp_data) <- c('Plate', 'Row', 'Column', 'Time', 'OD')
  data <- raw_date
  data <- data[,c(1:14)]
  for (row in 1:nrow(data)) {
    if(data[row, 1] == 'Plate:'){
      # Isolating the time from the plate data
        # if (nchar(as.character(data[row,2])) == 9){
      plate <- as.character(data[row,2])
      plate_split <- unlist(strsplit(plate, '[_]'))
      plate_name <- plate_split[[1]]
      time <- plate_split[[2]]
      # Extracting rows that contain data
      min = row + 2
      max = row + 9
      plate <- data[c(min:max), -c(1,2)]
      # Labeling plate data to represent the wells
      rownames(plate) <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
      colnames(plate) <- c(1:12)
      # Creating dataframe for export
      plate <- melt(as.matrix(plate))
      colnames(plate) <- c('Row', 'Column', 'OD')
      plate$Time <- time
      plate$Plate <- plate_name
      exp_data <- rbind(exp_data, plate)
    }
  }
  exp_data$OD <- as.numeric(as.character(exp_data$OD))
  return(exp_data)
}

# Zero the data based on a time zero plate reading
# 'input' is the un-zeroed data outputted from either versa or Envission
# Requires the following columns: 'Plate', 'Column', 'Row', 'OD', 'Time'
zero <- function(input){
  # Subsetting the values for each time zero based on well and plate
  zero <- subset(input, Time == 0)
  zero <- subset(zero, select = c(Plate, Row, Column, OD, Time))
  zero <- zero %>% rename(OD_Zero = OD)
  zero <- subset(zero, select = -Time)
  exp_data <- merge(input, zero, by = c('Plate', 'Row', 'Column'))
  exp_data$OD <- round(exp_data$OD - exp_data$OD_Zero, 3)
  exp_data <- subset(exp_data, select = -OD_Zero)
  return(exp_data)
}

# Finding the midlog for a number of different strains
# Dependencies: 
library(dplyr)
library(tidyr)
# 'input' is the dataframe merged with a key file, therefore containing all the information about your assay
# 'factors' are the unique variables that you want to calculate the midlog for, i.e. strain, supplement etc
  # Needs to include your concentrations or variable that you want to calculate midlog from
  # Factors can't have a '_' in their name
# 'concentrations' is the name of the column that contains your concentrations
# 'control' is the name you've given your control variables, meaning the variable to calculate midlog from
# Required columns: 'OD', 'Concentration', 'Time', 'Columns for each of your factors'.
# 'midlogs' is a dataframe containing all the midlog information for each of your different factors

midlog <- function(input, factors = '',  control = 'Control', midlogs){
  # Only considering the controls for the midlog
  df_control <- subset(input, Concentration == control)
  # Subsetting the unique factors that the midlog needs to be calculated for
  factors_uniq <- subset(df_control, select = factors)
  factors_uniq <- unique(factors_uniq)
  midlogs_df <- data.frame(matrix(ncol = length(factors) + 1, nrow = 0))
  colnames(midlogs_df) <- c(factors, 'Time')
  # Calcuating midlog for each factor
  for (row in 1:nrow(factors_uniq)){
    factor_row <- data.frame(factors_uniq[row, ])
    colnames(factor_row) <- factors
    df_factor <- merge(df_control, factor_row, by = factors)
    midlog_factor <- merge(midlogs, factor_row, by = factors)
    times <- unique(df_factor$Time)
    means_df <- data.frame(matrix(ncol = length(factors) + 2, nrow = 0))
    colnames(means_df) <- c(factors, 'Mean', 'Midlog')
    # Calculate the mean for each time point for each factor
    for (time in times){
      df_time <- subset(df_factor, Time == time)
      mean_od <- mean(df_time$OD)
      mean_df <- data.frame(Mean = mean_od, Time = time)
      mean_df <- cbind(factor_row, mean_df)
      means_df <- rbind(mean_df, means_df)
    }
    # Determine the midlog using the inputted data frame. Finds closest value
    means_df <- subset(means_df, Mean > 0.3)
    if (nrow(means_df) > 0){
      means_df$Midlog <- as.numeric(as.character(means_df$Time))
      midlog <- means_df[which.min(abs(midlog_factor$Midlog - means_df$Mean)),]
      midlog <- data.frame(factor_row, Time = midlog$Midlog)
      midlogs_df <- rbind(midlogs_df, midlog)
    }
    else{
      print(paste(unite(factors, sep = '_'), ' did not grow passed 0.3'))
    }
  }
  # # midlogs_df <- midlogs_df %>% rename(Time = Midlog)
  return(midlogs_df)
}

# res_mid calculates the residual grwoth from the output from midlog_conc
# Input required the follwoing columns: 'OD' 'Concentration', factors as defined in the 
res_mid <- function (input, factors = '', midlogs, control = 'Control'){
  # Isolating data from the midlog time points for each factor
  df_midlog <- merge(input, midlogs, by = c(factors, 'Time'))
  # Determining the unique factors
  factors_uniq <- subset(df_midlog, select = factors)
  factors_uniq <- unique(factors_uniq)
  # Creating a data frame to add all the means and standard deviations to
  mean_sd <- data.frame(matrix(nrow = 0, ncol = length(factors) + 4))
  colnames(mean_sd) <- c(factors, 'Mean', 'SD', 'Concentration', 'Unit')
  for (row in 1:nrow(factors_uniq)){
    # Subsetting data based on the factors
    factor <- data.frame(factors_uniq[row, ])
    colnames(factor) <- factors
    df_factor <- merge(df_midlog, factor, by = factors)
    # Calculating the mean and standard deviation of the control for the factor
    df_control <- subset(df_factor, Concentration == control)
    mean_control <- mean(df_control$OD)
    sd_control <- sd(df_control$OD)
    # Isolating the unique concentrations for each factor
    concentrations <- subset(df_factor, Concentration != control)
    concentrations <- unique(concentrations$Concentration)
    # Calculating the mean and standard deviation for each concentration
    # Mean and standard deviation calculated as a percentage of the treated over the untreated calculated above
    for (conc in concentrations){
      df_conc <- subset(df_factor, Concentration == conc)
      unit <- as.character(unique(df_conc$Unit))
      if (length(unit) < 2){
        mean <- mean(df_conc$OD)/mean_control * 100
        sd <- sqrt(sd(df_conc$OD)^2 + sd_control^2)/mean_control * 100
        mean <- round(mean, digits = 1)
        sd <- round(sd, digits = 1)
        conc_data <- data.frame(factor, mean, sd, conc, unit)
        colnames(conc_data) <- c(factors, 'Mean', 'SD', 'Concentration', 'Unit')
        conc_data$Concentration <- as.numeric(as.character(conc_data$Concentration))
        conc_data$Concentration <- round(conc_data$Concentration, digits = 3)
        mean_sd <- rbind(mean_sd, conc_data)
      }
      else{
        error <- paste("please check your plate map for multiple units at:", factor, conc, sep = ' ')
        shinyalert('Too many units!', error, type = 'error')
        return(error)
        break
      }
    }
  }
  return(mean_sd)
}

# Function to fit the curve for a dose response using the raw OD values
# Requires zeroed OD values
# Required the following columns: 'OD', 'Concentration', factors
# Dependencies
library(drc)

dose_curve_mid <- function(input, factors_midlog = '', factors_all = '', midlogs, control = 'Control', model = 'LL.4'){
  # Isolating data from the midlog time points for each factor
  df_midlog <- merge(input, midlogs, by = c(factors_midlog, 'Time'))
  # Determining the unique factors
  factors_uniq <- subset(df_midlog, select = factors_all)
  factors_uniq <- unique(factors_uniq)
  # Creating a dataframe fro all the curves
  curves_all <- data.frame(matrix(nrow = 0, ncol = length(factors_all) + 2))
  colnames(curves_all) <- c(factors_all, 'Concentration', 'Curve')
  for (row in 1:nrow(factors_uniq)){
    # Subsetting data based on the factors
    factor <- data.frame(factors_uniq[row, ])
    colnames(factor) <- factors_all
    df_factor <- merge(df_midlog, factor, by = factors_all)
    df_control <- subset(df_factor, Concentration == control)
    control_mean <- mean(df_control$OD)
    df_curve <- subset(df_factor, Concentration != control)
    df_curve$Concentration <- as.numeric(as.character(df_curve$Concentration))
    # Modeling the curve usng drc
    model_fun <- match.fun(model)
    curve <- plot(drm(OD ~ Concentration, data = df_curve,
                      fct = model_fun()))
    colnames(curve) <- c('Concentration', 'Curve')
    curve <- cbind(curve, factor)
    curve$Curve <- resid_mean(curve$Curve, control_mean)
    curves_all <- rbind(curves_all, curve)
  }
  return(curves_all)
}

# Function to label x axis text as a deletion
del <- function(gene = ''){
  bquote(italic(.(gene))*Delta)
}

# Function to render a single bar, bar graph using plotly
# Requires a data frame with columns Label, Mean, SD
{
  bar_graph_single <- function(data){
   g <-  ggplot(data, aes(x = Label, y = Mean, ymin = Mean - SD, ymax  = Mean + SD)) +
      geom_errorbar(stat = 'identity') +
      geom_bar(stat = 'identity', color = input$barout, fill = input$barcol)
   return(g)
  }
}

# Function to calculate residual growth mean

resid_mean <- function(treated_mean, control_mean){
  treated_mean/control_mean * 100
}

resid_sd <- function(treated_sd, control_sd, control_mean){
  sqrt(treated_sd^2 + control_sd^2)/control_mean * 100
}