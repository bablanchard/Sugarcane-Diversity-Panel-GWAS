library(readxl)
library(car)
library(dplyr)
library(nlme)
library(lme4)
library(Matrix)
library(ggplot2)
library(tidyr)
library(stringr)

#=============QC of the data===========
#This block will read in the master data and examine it to determine if any further cleaning needs to be performed
#it will also make conversions where necessary

#read master
df <- read.csv("C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/Dissertation Projects/Diversity Panel/BB_master_1.1.csv", header = TRUE)
df$BW <- as.numeric(df$BW)
df$NumStalkbu <- as.numeric(df$NumStalkbu)
df$Brix <- as.numeric(df$Brix)
df$TRS <- as.numeric(df$TRS)
df$Fiber <- as.numeric(df$Fiber)
df$Moisture <- as.numeric(df$Moisture)
df$Pol..Reading <- as.numeric(df$Pol..Reading)
df$Sucrose.. <- as.numeric(df$Sucrose..)
df$Purity <- as.numeric(df$Purity)
df$Pop <- as.numeric(df$Pop)
df$Dia <- as.numeric(df$Dia)

#calcualte SW
df$SW <- df$BW / df$NumStalkbu

# Convert plot area from square feet to hectares
plot_area_hectares <- 60 * 0.000009290304

# Calculate population (stalks per hectare)
df$Population <- df$Pop / plot_area_hectares
df$Population <- round(df$Population, 0)
# Convert SW column from pounds to kilograms
df$SW_kg <- df$SW * 0.45359237

# Calculate megagrams per hectare
df$CYMg_per_ha <- df$Population * df$SW_kg / 1000

# Convert TRS column from pounds per ton to kilograms per megagram
df$TRS_kg_per_Mg <- df$TRS * 0.45359237 / 0.90718474

#Take out one genotype
df <- df[df$Geno != "GI 6", ]


# Loop through every value where Rep is NA
for (i in which(is.na(df$Rep))) {
  # Check the corresponding Location value
  if (df$Location[i] == "Light") {
    # If Location is Light, assign Rep as 1
    df$Rep[i] <- ifelse(is.na(df$Rep[i]), 1, df$Rep[i])
  } else if (df$Location[i] == "Heavy") {
    # If Location is Heavy, assign Rep as 2
    df$Rep[i] <- ifelse(is.na(df$Rep[i]), 2, df$Rep[i])
  }
}

# Subset the df_filtered dataframe to remove all observations with NA or 0 values in the TRS column
TRSPC <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "0", ]
TRS1R <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "1", ]
TRS2R <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "2", ]
TRS3R <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "3", ]

CYPC <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "0", ]
CY1R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "1", ]
CY2R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "2", ]
CY3R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "3", ]

#=============Remake Empty Dataframes to collect BLUPs and relevant VCs=========
# Define datasets and their corresponding predicted variables
# Create empty data frames to store results
DataOutput <- data.frame(matrix(vector(),260,1, dimnames=list(c(), c("Genotype"))))
DataVarComp <- data.frame(VC = c("Geno", "Geno:Year", "Geno:Location", 
                                 "Geno:Year:Location", "Rep:(Year:Location)", "Residual", "Heritability"))
#fill empty dataframe with 1-300 so that the cbind will work later on
DataOutput$Genotype <- unique(df[,5]) #fill in Entry numbers

#===========Define linear mixed models for each trait===========
# Define the model with fixed and random effects
TRSPC <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                        (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                      data = TRSPC)


TRS1R <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
              data = TRS1R)

TRS2R <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
              data = TRS2R)

TRS3R <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Rep:(Year:Location)),
              data = TRS3R)

CYPC <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
               (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                    data = CYPC)

CY1R <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
               (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
             data = CY1R)

CY2R <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
               (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
             data = CY2R)

CY3R <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Rep:(Year:Location)),
             data = CY3R)

#============Use LMMs to extract BLUPs for each Cross overall and for each trait==========
# List of model objects
models <- list(TRSPC, TRS1R, TRS2R, TRS3R, CYPC, CY1R, CY2R, CY3R)
model_names <- c("TRSPC", "TRS1R", "TRS2R", "TRS3R", "CYPC", "CY1R", "CY2R", "CY3R")  # Names of the models



# Loop through each model and extract overall BLUPs for each trait
for (i in seq_along(models)) {
  model <- models[[i]]
  model_name <- model_names[i]
  
  # Extract the predicted variable name from the model
  predicted_variable <- all.vars(formula(model))[1]
  
  # Extract BLUPs from the model
  blups <- ranef(model)$Geno
  
  # Loop through each unique value of Cross
  for (geno_value in unique(DataOutput$Genotype)) {
    # Check if the cross_value exists in the BLUPs
    if (as.character(geno_value) %in% rownames(blups)) {
      # Extract BLUP for the current cross_value from the model
      blup <- blups[as.character(geno_value), "(Intercept)"]
      
      # Extract the predicted values from the model
      predicted_values <- predict(model, newdata = DataOutput, re.form = NA)
      
      # Calculate the overall mean of the predicted variable
      overall_mean <- mean(predicted_values, na.rm = TRUE)
      # Add the BLUP to the overall mean value of the predicted variable
      adjusted_blup <- blup + overall_mean
    } else {
      # If cross_value does not exist in the BLUPs, assign NA to BLUP
      adjusted_blup <- NA
    }
    
    # Add adjusted BLUP to new column in DataOutput
    DataOutput[[paste0(model_name, "_BLUP")]][DataOutput$Genotype == geno_value] <- adjusted_blup
  }
}
getwd()
# Assuming DataOutput is your dataframe
write.csv(DataOutput, file = "DataOutputTRSCYBlupspercrop.csv", row.names = FALSE)
#==============Use LMMs to extract VCs from each model and input into DataVarComp dataframe=======
# Loop through each model and extract relevant VCs for heritability
for (i in seq_along(models)) {
  model <- models[[i]]
  model_name <- model_names[i]
  
  # Extract variance components using VarCorr function
  var_components <- VarCorr(model)
  
  # Find the variance component for each model
  for (j in seq_len(nrow(DataVarComp))) {
    vc_name <- as.character(DataVarComp$VC[j])
    
    # Check if the variance component exists in var_components
    if (vc_name %in% names(var_components)) {
      # Extract the variance component value
      vc_value <- var_components[[vc_name]]
    } else {
      # If variance component does not exist, assign NA
      vc_value <- NA
    }
    
    # Create a new column in DataVarComp with model_name as the column name
    DataVarComp[[model_name]][j] <- vc_value
  }
}


# Loop through each model and calculate residuals
for (i in seq_along(models)) {
  model <- models[[i]]
  model_name <- model_names[i]
  
  # Extract variance components using VarCorr function
  var_components <- VarCorr(model)
  
  # Find the residual error for each model
  residual_error <- sigma(model)^2
  
  # Fill in the residual error for the corresponding model in DataVarComp
  DataVarComp[[model_name]][DataVarComp$VC == "Residual"] <- residual_error
}

# Loop through each row of the dataframe to change NAs to 0s
for (row_index in 1:nrow(DataVarComp)) {
  # Check if the current row is the row where VC is Heritability
  if (DataVarComp$VC[row_index] == "Heritability") {
    next  # Skip to the next iteration if the row is for Heritability
  } else {
    # Replace NA values with 0 in the current row
    DataVarComp[row_index, is.na(DataVarComp[row_index, ])] <- 0
  }
}



#extract values from the dataframe to calculate the broad sense heritability for each trait
for (col_index in 2:ncol(DataVarComp)) {
  if (is.na(DataVarComp[nrow(DataVarComp), col_index])) {
    # Extract values from the first and ninth rows of the current column
    value_row1 <- as.numeric(DataVarComp[1, col_index])
    value_row2 <- as.numeric(DataVarComp[2, col_index])
    value_row3 <- as.numeric(DataVarComp[3, col_index])
    value_row4 <- as.numeric(DataVarComp[4, col_index])
    value_row5 <- as.numeric(DataVarComp[5, col_index])
    value_row6 <- as.numeric(DataVarComp[6, col_index])
    value_row7 <- as.numeric(DataVarComp[7, col_index])
    
    
    # Print the extracted values for debugging
    cat("Values extracted for calculation in column", col_index, ":\n")
    cat("Row 1:", value_row1, "\n")
    cat("Row 2:", value_row2, "\n")
    cat("Row 3:", value_row3, "\n")
    cat("Row 4:", value_row4, "\n")
    cat("Row 5:", value_row5, "\n")
    cat("Row 6:", value_row6, "\n")
    cat("Row 7:", value_row7, "\n")
   
    
    # Calculate heritability by dividing the value in Row1 by the value in Row9
    heritability <- value_row1 / (value_row1 + value_row2/2 + value_row3/2 + value_row4/4 +
                                    value_row6/8)
    
    # Print the calculated heritability for debugging
    cat("Calculated heritability in column", col_index, ":", heritability, "\n")
    
    # Fill in the calculated heritability value in the corresponding cell
    DataVarComp[nrow(DataVarComp), col_index] <- heritability
    
    # Print the updated value in the corresponding cell for debugging
    cat("Updated value in column", col_index, ":", DataVarComp[nrow(DataVarComp), col_index], "\n\n")
  }
}

# Define a function to convert numbers from scientific notation to decimal notation with 9 decimal points
format_decimal <- function(x) {
  format(as.numeric(x), scientific = FALSE, digits = 9)
}

# Apply the format_decimal function to all elements in the columns after the first one
DataVarComp[, -1] <- lapply(DataVarComp[, -1], format_decimal)



#=============Correlate BLUPs======================
data_subset <- DataOutput[, c(6:9)]
data_subset <- data_subset[complete.cases(data_subset), ]

library(psych)

pairs.panels(data_subset,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


