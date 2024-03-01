library(readxl)
library("PerformanceAnalytics")
library(car)
library(dplyr)
library(nlme)
library(lme4)
library(Matrix)
library(ggplot2)

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

# Write the dataframe to a CSV file
write.csv(df, file = "BB_Masterprocessed_1.2.csv", row.names = TRUE)

#==============make datasets and quickly look at the spread============
# Subset the df_filtered dataframe to remove all observations with NA or 0 values in the TRS column
TRS_kg_per_Mg <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0,]
CYMg_per_ha <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0,]
Population <- df[complete.cases(df$Population) & df$Population != 0,]
Dia <- df[complete.cases(df$Dia) & df$Dia != 0,]
Brix <- df[complete.cases(df$Brix) & df$Brix != 0,]
Fiber <- df[complete.cases(df$Fiber) & df$Fiber != 0,]
SW_kg <- df[complete.cases(df$SW_kg) & df$SW_kg != 0,]
Sucrose.. <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0,]
Moisture <- df[complete.cases(df$Moisture) & df$Moisture != 0,]
Pol..Reading <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0,]
Purity <- df[complete.cases(df$Purity) & df$Purity != 0,]

hist(TRS_kg_per_Mg$TRS_kg_per_Mg)
hist(CYMg_per_ha$CYMg_per_ha)
hist(Population$Population)
hist(Dia$Dia)
hist(Brix$Brix)
hist(SW_kg$SW_kg)
hist(Sucrose..$Sucrose..)
hist(Moisture$Moisture)
hist(Pol..Reading$Pol..Reading)
hist(Purity$Purity)

boxplot(TRS_kg_per_Mg$TRS_kg_per_Mg)
boxplot(CYMg_per_ha$CYMg_per_ha)
boxplot(Population$Population)
boxplot(Dia$Dia)
boxplot(Brix$Brix)
boxplot(SW_kg$SW_kg)
boxplot(Sucrose..$Sucrose..)
boxplot(Moisture$Moisture)
boxplot(Pol..Reading$Pol..Reading)
boxplot(Purity$Purity)

table(TRS_kg_per_Mg$Geno)
table(TRS_kg_per_Mg$TestID)
table(TRS_kg_per_Mg$Rep)
table(TRS_kg_per_Mg$Crop)
table(TRS_kg_per_Mg$Year)

# Assuming "Geno" is the column name containing the unique values
unique_genos <- unique(TRS_kg_per_Mg$Geno)
num_unique_genos <- length(unique_genos)
print(num_unique_genos)


table(TRS_kg_per_Mg$Cross, TRS_kg_per_Mg$Location)
table(TRS_kg_per_Mg$Crop, TRS_kg_per_Mg$Location, TRS_kg_per_Mg$PlantYear)
table(TRS_kg_per_Mg$Cross, TRS_kg_per_Mg$Location, TRS_kg_per_Mg$PlantYear)
table(TRS_kg_per_Mg$Cross, TRS_kg_per_Mg$Location, TRS_kg_per_Mg$PlantYear, TRS_kg_per_Mg$Crop)

#=============== phenotypic correlation between traits===============
sapply(df[, c(8:14, 20, 22:25)], class)
df[, c(8:14, 20, 22:25)] <- lapply(df[, c(8:14, 20, 22:25)], as.numeric)
round(cor(df[, c(8:14, 20, 22:25)], use = "pairwise"), 2)
data_subset <- df[, c(8:14, 20, 22:25)]
#install.packages("PerformanceAnalytics")
chart.Correlation(as.matrix(na.omit(data_subset)), histogram = TRUE, pch = 1)

#===========TRS Sample Model and Outputs==============================
# 'TRS' is the response variable, and 'TRS' is the predictor variable
print(table(df$Geno))
print(table(TRS_kg_per_Mg$Year))
print(table(TRS_kg_per_Mg$Location))
print(table(TRS_kg_per_Mg$Crop))
print(table(TRS_kg_per_Mg$Geno, TRS_kg_per_Mg$Location))
print(table(TRS_kg_per_Mg$Year, TRS_kg_per_Mg$Rep))
# Loop through every value where Rep is NA
for (i in which(is.na(TRS_kg_per_Mg$Rep))) {
  # Check the corresponding Location value
  if (TRS_kg_per_Mg$Location[i] == "Light") {
    # If Location is Light, assign Rep as 1
    TRS_kg_per_Mg$Rep[i] <- ifelse(is.na(TRS_kg_per_Mg$Rep[i]), 1, TRS_kg_per_Mg$Rep[i])
  } else if (TRS_kg_per_Mg$Location[i] == "Heavy") {
    # If Location is Heavy, assign Rep as 2
    TRS_kg_per_Mg$Rep[i] <- ifelse(is.na(TRS_kg_per_Mg$Rep[i]), 2, TRS_kg_per_Mg$Rep[i])
  }
}
# Calculate the number of observations per Cross
cross_counts <- table(TRS_kg_per_Mg$Geno)
# Filter out zero counts (if any)
non_zero_counts <- cross_counts[cross_counts > 0]
# Calculate the harmonic mean of the number of observations per Cross
TRSharmonic_mean <- 1 / mean(1 / non_zero_counts)
# Print the results
cat("Number of observations per Cross:\n")
print(cross_counts)
cat("\nHarmonic mean of the number of observations per Cross:", TRSharmonic_mean, "\n")

# Define the model with fixed and random effects
model <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) +
                (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
              data = TRS_kg_per_Mg)

# Fit the model
model_fit <- summary(model)

# Obtain variance components
var_components <- VarCorr(model)

# Obtain BLUPs (Best Linear Unbiased Predictors)
blups <- ranef(model)

# Print results
print(model_fit)
print(var_components)
print(blups)


# Assuming var_components is a list with 16 components

# Extract variance components using VarCorr function
variances <- sapply(VarCorr(model), function(x) attr(x, "stddev")^2)

# Create a data frame for plotting
plot_data <- data.frame(Component = names(variances), Variance = variances)

# Create a stacked bar plot
ggplot(plot_data, aes(x = "", y = Variance, fill = Component)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for horizontal bars
  labs(x = "", y = "Variance", fill = "Component") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order for better readability


# Extract variance components using VarCorr function
var_components <- VarCorr(model)

# Find the variance component for the intercept of 'Cross' and 'Cross:PlantYear'
vc_cross <- as.numeric(var_components$Geno)
v
c_cross_plantyear <- as.numeric(var_components$`Geno:Year`)
vc_cross_Location <- as.numeric(var_components$`Geno:Location`)
vc_cross_Crop <- as.numeric(var_components$`Geno:Crop`)
vc_cross_plantyear_location <- as.numeric(var_components$`Geno:Year:Location`)
vc_cross_crop_plantyear <- as.numeric(var_components$`Geno:Crop:Year`)
vc_cross_crop_location <- as.numeric(var_components$`Geno:Crop:Location`)
vc_cross_crop_location_plantyear <- as.numeric(var_components$`Geno:Crop:Location:Year`)
vc_residual <- as.numeric(sigma(model)^2)


# Calculate broad sense heritability
broad_heritability <- vc_cross / (vc_cross + vc_cross_plantyear/2 + vc_cross_Location/2 + vc_cross_Crop/4 +
                                    vc_cross_plantyear_location/3 + vc_cross_crop_plantyear/7 + vc_cross_crop_location/7 +
                                    vc_cross_crop_location_plantyear/10 + vc_residual/8.7226)

# Print broad sense heritability
print(broad_heritability)


#=============Remake Empty Dataframes to collect BLUPs and relevant VCs=========
# Define datasets and their corresponding predicted variables
# Create empty data frames to store results
DataOutput <- data.frame(matrix(vector(),260,1, dimnames=list(c(), c("Genotype"))))
DataVarComp <- data.frame(VC = c("Geno", "Geno:Year", "Geno:Location", 
                                 "Geno:Crop", "Geno:Year:Location", 
                                 "Geno:Crop:Year", "Geno:Crop:Location", 
                                 "Geno:Crop:Location:Year", "Residual", "Heritability"))
#fill empty dataframe with 1-300 so that the cbind will work later on
DataOutput$Genotype <- unique(df[,5]) #fill in Entry numbers
#===========Define linear mixed models for each trait===========
# Define the model with fixed and random effects
TRS_kg_per_Mg <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) +
                (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
              data = TRS_kg_per_Mg)

TRSvar_components <- VarCorr(TRS_kg_per_Mg)
TRSmodel_fit <- summary(TRS_kg_per_Mg)


CYMg_per_ha <- lmer(CYMg_per_ha ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = CYMg_per_ha)

CYvar_components <- VarCorr(CYMg_per_ha)
CYmodel_fit <- summary(CYMg_per_ha)


Population <- lmer(Population ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Population)

Populationvar_components <- VarCorr(Population)
summary(Population)

Dia <- lmer(Dia ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Dia)

Diavar_components <- VarCorr(Dia)
Diamodel_fit <- summary(Dia)


Brix <- lmer(Brix ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Brix)

Brixvar_components <- VarCorr(Brix)
summary(Brix)

Fiber <- lmer(Fiber ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Fiber)

Fibervar_components <- VarCorr(Fiber)
summary(Fiber)


SW_kg <- lmer(SW_kg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = SW_kg)

SW_kgvar_components <- VarCorr(SW_kg)
summary(SW_kg)


Sucrose.. <- lmer(Sucrose.. ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Sucrose..)

Sucrose..var_components <- VarCorr(Sucrose..)
summary(Sucrose..)

Moisture <- lmer(Moisture ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Moisture)

Pol..Reading <- lmer(Pol..Reading ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Pol..Reading)

Purity <- lmer(Purity ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = Purity)

#============Use LMMs to extract BLUPs for each Cross overall and for each trait==========
# List of model objects
models <- list(TRS_kg_per_Mg, CYMg_per_ha, Population, Dia, Brix, Fiber, SW_kg, Sucrose.., Moisture, Pol..Reading, Purity)
model_names <- c("TRS_kg_per_Mg", "CYMg_per_ha", "Population", "Dia", "Brix", "Fiber", "SW_kg", "Sucrose..", "Moisture", "Pol..Reading", "Purity")  # Names of the models



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

# Assuming DataOutput is your dataframe
write.csv(DataOutput, file = "DataOutputOverallBlups.csv", row.names = FALSE)

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
    value_row8 <- as.numeric(DataVarComp[8, col_index])
    value_row9 <- as.numeric(DataVarComp[9, col_index])
    
    # Print the extracted values for debugging
    cat("Values extracted for calculation in column", col_index, ":\n")
    cat("Row 1:", value_row1, "\n")
    cat("Row 2:", value_row2, "\n")
    cat("Row 3:", value_row3, "\n")
    cat("Row 4:", value_row4, "\n")
    cat("Row 5:", value_row5, "\n")
    cat("Row 6:", value_row6, "\n")
    cat("Row 7:", value_row7, "\n")
    cat("Row 8:", value_row8, "\n")
    cat("Row 9:", value_row9, "\n")
    
    # Calculate heritability by dividing the value in Row1 by the value in Row9
    heritability <- value_row1 / (value_row1 + value_row2/2 + value_row3/2 + value_row4/4 +
                                    value_row5/3 + value_row6/7 + value_row7/7 + value_row8/10 +
                                    value_row9/8.7226)
    
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





#=============create complete dataset that contains all VCs with traits melted in a trait column==========
# Initialize the combined_var_components dataframe
combined_var_components <- data.frame(Model = character(),
                                      VC = character(),
                                      Value = numeric(),
                                      SD = numeric(),
                                      stringsAsFactors = FALSE)

# Loop through each model
for (i in seq_along(models)) {
  model <- models[[i]]
  model_name <- model_names[i]
  
  # Extract variance components using VarCorr function
  var_components <- VarCorr(model)
  
  # Extract variance components manually and create a dataframe
  var_df <- data.frame(
    Model = model_name,
    VC = names(var_components),
    Value = unlist(var_components),
    SD = sapply(var_components, function(x) sqrt(x[1, 1]))
  )
  
  # Combine variance components for all models
  combined_var_components <- rbind(combined_var_components, var_df)
}

# Calculate LowerBoundCI
combined_var_components$LowerBoundCI <- combined_var_components$Value - (1.96 * combined_var_components$SD)
combined_var_components$UpperBoundCI <- combined_var_components$Value + (1.96 * combined_var_components$SD)
# Create a new column 'Significant' in combined_var_components dataframe
combined_var_components$Significant <- ifelse(combined_var_components$LowerBoundCI > 0, "Yes", "No")


# Convert VC column to factor to ensure correct order in the plot
combined_var_components$VC <- factor(combined_var_components$VC, levels = unique(combined_var_components$VC))



#==========create various stacked bar plots to display the portions of VCs in each trait model=======
# Create stacked bar plot to display all model VCs
ggplot(combined_var_components, aes(x = Model, y = Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Variance Components of All Models", x = "Model", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sort the dataframe by Model and VC columns
combined_var_components <- combined_var_components[order(combined_var_components$Model, combined_var_components$VC),]

# Perform scaling transformation within each Model group
combined_var_components <- within(combined_var_components, {
  scaled_Value <- ave(Value, Model, FUN = function(x) scale(x, center = TRUE, scale = TRUE))
})

# Plot the scaled values with modified aesthetics
ggplot(combined_var_components, aes(x = Model, y = scaled_Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = bquote(bold("Variance Components of All Models")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Scaled Value")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))



# Filter combined_var_components to include only TRS, CY, and Height models
filtered_var_components <- combined_var_components[combined_var_components$Model %in% c("TRS_kg_per_Mgmodel", "Brix", "Fiber", "Sucrose..", "Purity", "Pol..Reading"), ]

# Create stacked bar plot
ggplot(filtered_var_components, aes(x = Model, y = scaled_Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = bquote(bold("Variance Components of CY, Height, and Dia")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Scaled Value")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Create stacked bar plot with labels within the stacks
ggplot(filtered_var_components, aes(x = Model, y = scaled_Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(label = VC), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Variance Components of TRS and CY", x = "Model", y = "Scaled Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#For just TRS
filtered_var_components <- combined_var_components[combined_var_components$Model %in% c("TRS_kg_per_Mgmodel"), ]

ggplot(filtered_var_components, aes(x = Model, y = Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(label = VC), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  scale_fill_manual(values = ifelse(filtered_var_components$VC %in% c("Geno:Year", "Geno:Crop", "Geno:Crop:Location:Year", "Rep:(Crop:(Year:Location))",
                                                                      "Crop:Year:Location", "Crop:Year", "Crop:Location", "Rep:(Year:Location)", "Location"), "black", scales::hue_pal()(length(unique(filtered_var_components$VC))))) +
  labs(title = bquote(bold("Variance Components of TRS")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Variance")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

#For just Brix
filtered_var_components <- combined_var_components[combined_var_components$Model %in% c("Brix"), ]
ggplot(filtered_var_components, aes(x = Model, y = Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(label = VC), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(title = bquote(bold("Variance Components of Brix")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Variance")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

#For just Fiber
filtered_var_components <- combined_var_components[combined_var_components$Model %in% c("Fiber"), ]
ggplot(filtered_var_components, aes(x = Model, y = Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(label = VC), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(title = bquote(bold("Variance Components of Fiber")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Variance")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

#For just CY
filtered_var_components <- combined_var_components[combined_var_components$Model %in% c("CYMg_per_ha"), ]
ggplot(filtered_var_components, aes(x = Model, y = Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(label = VC), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  scale_fill_manual(values = ifelse(filtered_var_components$VC %in% c("Crop:Location", "Crop:Year", "Year:Location", "Rep:(Year:Location)", "Cross:PlantYear", 
                                                                      "Geno:Crop:Location:Year", "Geno:Crop:Year", "Crop", "Location", 
                                                                      "Geno:Crop:Location", "Geno:Year"), "black", scales::hue_pal()(length(unique(filtered_var_components$VC))))) +
  labs(title = bquote(bold("Variance Components of CY")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Variance")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

#For just Height
filtered_var_components <- combined_var_components[combined_var_components$Model %in% c("Height"), ]
ggplot(filtered_var_components, aes(x = Model, y = Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(label = VC), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  scale_fill_manual(values = ifelse(filtered_var_components$VC %in% c("PlantYear:Location", "PlantYear", "Location",
                                                                      "Crop:Location", "Crop:PlantYear", "Cross:Location", "Cross:Crop", 
                                                                      "Cross:Crop:Location:PlantYear", "Cross:Crop:PlantYear"), "black", scales::hue_pal()(length(unique(filtered_var_components$VC))))) +
  labs(title = bquote(bold("Variance Components of Height")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Variance")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

#For just Dia
filtered_var_components <- combined_var_components[combined_var_components$Model %in% c("Dia"), ]
ggplot(filtered_var_components, aes(x = Model, y = Value, fill = VC)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(label = VC), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(title = bquote(bold("Variance Components of Diameter")),
       x = bquote(bold("Model")), 
       y = bquote(bold("Variance")),
       fill = bquote(bold("Variance Components"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
#==========using the DataVarComp dataframe to calculate GCV, PCV, CV=========
# Create a new row with the desired values
new_row <- c("OverallMean", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # Adjust for the number of columns in your dataframe
# Bind the new row to the dataframe
DataVarComp <- rbind(DataVarComp, new_row)
# Iterate through each model and its corresponding name to get overall mean
for (i in seq_along(models)) {
  # Extract the model name and object
  model_name <- model_names[i]
  model <- models[[i]]
  
  # Extract overall mean for the trait from the model
  overall_mean <- fixef(model)["(Intercept)"]
  
  # Update DataVarComp with the overall mean for the corresponding trait
  DataVarComp[DataVarComp$VC == "OverallMean", model_name] <- overall_mean
}



new_row <- c("GCV", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # Adjust for the number of columns in your dataframe
DataVarComp <- rbind(DataVarComp, new_row)
#extract values from the dataframe to calculate the genetic coefficient of Variation
for (col_index in 2:ncol(DataVarComp)) {
  if (is.na(DataVarComp[nrow(DataVarComp), col_index])) {
    # Extract values from the first and ninth rows of the current column
    value_row1 <- as.numeric(DataVarComp[1, col_index])
    value_row11 <- as.numeric(DataVarComp[11, col_index])
    
    
    # Print the extracted values for debugging
    cat("Values extracted for calculation in column", col_index, ":\n")
    cat("Row 1:", value_row1, "\n")
    cat("Row 11:", value_row2, "\n")
    
    
    # Calculate GCV by dividing the value in Row1 by the value in Row9
    GCV <- ((sqrt(value_row1)) / (value_row11)) * 100
    
    # Print the calculated GCV for debugging
    cat("Calculated GCV in column", col_index, ":", GCV, "\n")
    
    # Fill in the calculated GCV value in the corresponding cell
    DataVarComp[nrow(DataVarComp), col_index] <- GCV
    
    # Print the updated value in the corresponding cell for debugging
    cat("Updated value in column", col_index, ":", DataVarComp[nrow(DataVarComp), col_index], "\n\n")
  }
}
new_row <- c("PCV", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # Adjust for the number of columns in your dataframe
DataVarComp <- rbind(DataVarComp, new_row)
#extract values from the dataframe to calculate the phenotypic coefficient of variation
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
    value_row8 <- as.numeric(DataVarComp[8, col_index])
    value_row9 <- as.numeric(DataVarComp[9, col_index])
    value_row11 <- as.numeric(DataVarComp[11, col_index])
    
    # Print the extracted values for debugging
    cat("Values extracted for calculation in column", col_index, ":\n")
    cat("Row 1:", value_row1, "\n")
    cat("Row 2:", value_row2, "\n")
    cat("Row 3:", value_row3, "\n")
    cat("Row 4:", value_row4, "\n")
    cat("Row 5:", value_row5, "\n")
    cat("Row 6:", value_row6, "\n")
    cat("Row 7:", value_row7, "\n")
    cat("Row 8:", value_row8, "\n")
    cat("Row 9:", value_row9, "\n")
    cat("Row 11:", value_row11, "\n")
    
    
    # Calculate PCV by dividing the value in Row1 by the value in Row9
    PCV <- ((sqrt(value_row1 + value_row2/2 + value_row3/2 + value_row4/4 +
                    value_row5/3 + value_row6/7 + value_row7/7 + value_row8/10 +
                    value_row9/8.7226))/value_row11) * 100
    
    # Print the calculated PCV for debugging
    cat("Calculated PCV in column", col_index, ":", PCV, "\n")
    
    # Fill in the calculated PCV value in the corresponding cell
    DataVarComp[nrow(DataVarComp), col_index] <- PCV
    
    # Print the updated value in the corresponding cell for debugging
    cat("Updated value in column", col_index, ":", DataVarComp[nrow(DataVarComp), col_index], "\n\n")
  }
}

new_row <- c("SD", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # Adjust for the number of columns in your dataframe
DataVarComp <- rbind(DataVarComp, new_row)
# Loop through each model and collect the standard deviation in a row in DataVarComp
for (i in seq_along(models)) {
  # Extract the residuals from the model
  response_variable <- residuals(models[[i]])
  
  # Calculate the standard deviation of the response variable
  model_sd <- sd(response_variable, na.rm = TRUE)
  
  # Get the model name
  model_name <- model_names[i]
  
  # Find the column index in DataVarComp dataframe corresponding to the model name
  col_index <- which(names(DataVarComp) == model_name)
  
  # If the column index is found, update the cell in the "SD" row of DataVarComp
  if (length(col_index) > 0) {
    DataVarComp[DataVarComp$VC == "SD", col_index] <- model_sd
  } else {
    # Print a warning if the column index is not found
    warning(sprintf("Column index not found for model %s", model_name))
  }
}


new_row <- c("CV", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # Adjust for the number of columns in your dataframe
DataVarComp <- rbind(DataVarComp, new_row)
# Find the row index where DataVarComp$VC == "CV"
cv_row_index <- which(DataVarComp$VC == "CV")
# Get the column names where the values are NA in the "CV" row
cv_columns <- colnames(DataVarComp)[is.na(DataVarComp[cv_row_index,])]

# Convert columns in cv_columns to numeric
DataVarComp[, cv_columns] <- apply(DataVarComp[, cv_columns], 2, as.numeric)

# Loop through each column where CV needs to be calculated
for (cv_column in cv_columns) {
  # Get the column index for SD and OverallMean
  col_sd <- which(DataVarComp$VC == "SD")
  col_mean <- which(DataVarComp$VC == "OverallMean")
  
  # Calculate the coefficient of variation (CV)
  cv <- (DataVarComp[col_sd, cv_column] / DataVarComp[col_mean, cv_column]) * 100
  
  # Update the corresponding cell in the "CV" row of DataVarComp
  DataVarComp[cv_row_index, cv_column] <- cv
}

# Define a function to convert numbers from scientific notation to decimal notation with 9 decimal points
format_decimal <- function(x) {
  format(as.numeric(x), scientific = FALSE, digits = 9)
}

# Apply the format_decimal function to all elements in the columns after the first one
DataVarComp[, -1] <- lapply(DataVarComp[, -1], format_decimal)
#============make a visual of the many diagnostics created with DataVarComp=============
# Filter DataVarComp for rows where VC is Heritability, GCV, PCV, or CV
filtered_data <- DataVarComp[DataVarComp$VC %in% c("Heritability", "GCV", "PCV", "CV"), ]


# Transpose the filtered_data
flipped_data <- t(filtered_data[, -1])  # Exclude the first column (VC) before transposing
colnames(flipped_data)[colnames(flipped_data) == "10"] <- "Heritability"
colnames(flipped_data)[colnames(flipped_data) == "12"] <- "GCV"
colnames(flipped_data)[colnames(flipped_data) == "13"] <- "PCV"
colnames(flipped_data)[colnames(flipped_data) == "15"] <- "CV"


str(flipped_data)

# Convert to dataframe
flipped_data <- as.data.frame(flipped_data, stringsAsFactors = FALSE)

# Change all columns to numeric
flipped_data[] <- lapply(flipped_data, as.numeric)
# Create the bar plot using ggplot2
# Create the bar plot using ggplot2
ggplot(data = NULL, aes(x = rownames(flipped_data), y = flipped_data$CV, fill = rownames(flipped_data))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(flipped_data$CV, 2)), vjust = -0.5) +  # Add text labels with values rounded to 2 decimal places
  labs(title = "Coefficient of Variation",
       x = "Traits",
       y = "CV") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)  # Remove the legend


# Assuming DataVarComp is your dataframe
# Convert all columns after the first to numeric (excluding the first column)
DataVarComp[, -1] <- sapply(DataVarComp[, -1], as.numeric)

# Round numeric columns to three decimal places
DataVarComp[, -1] <- round(DataVarComp[, -1], 3)