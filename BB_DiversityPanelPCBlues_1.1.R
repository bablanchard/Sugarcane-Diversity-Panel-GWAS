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
df$Pop <- as.numeric(df$Pop)
df$Dia <- as.numeric(df$Dia)
df$Crop <- factor(df$Crop)
df$Year <- factor(df$Year)
df$Purity <- as.numeric(df$Purity)


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
TRSPC <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "0", ]
CYPC <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "0", ]
PopPC <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "0", ]
DiaPC <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "0", ]
BrixPC <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "0", ]
FiberPC <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "0", ]
SW_kgPC <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "0", ]
Sucrose..PC <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "0", ]
MoisturePC <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "0", ]
Pol..ReadingPC <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "0", ]
PurityPC <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "0", ]


# Assuming "Geno" is the column name containing the unique values
unique_genos <- unique(TRS_kg_per_Mg$Geno)
num_unique_genos <- length(unique_genos)
print(num_unique_genos)

#===========Define linear mixed models for each trait===========
# Define the model with fixed and random effects
TRS_kg_per_Mg <- lmer(TRS_kg_per_Mg ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                        (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                      data = TRSPC)

TRSvar_components <- VarCorr(TRS_kg_per_Mg)
TRSmodel_fit <- summary(TRS_kg_per_Mg)


CYMg_per_ha <- lmer(CYMg_per_ha ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                      (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                    data = CYPC)

CYvar_components <- VarCorr(CYMg_per_ha)
CYmodel_fit <- summary(CYMg_per_ha)


Population <- lmer(Population ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                     (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                   data = PopPC)

Populationvar_components <- VarCorr(Population)
summary(Population)

Dia <- lmer(Dia ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
              (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
            data = DiaPC)

Diavar_components <- VarCorr(Dia)
Diamodel_fit <- summary(Dia)


Brix <- lmer(Brix ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
               (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
             data = BrixPC)

Brixvar_components <- VarCorr(Brix)
summary(Brix)

Fiber <- lmer(Fiber ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
              data = FiberPC)

Fibervar_components <- VarCorr(Fiber)
summary(Fiber)


SW_kg <- lmer(SW_kg ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
              data = SW_kgPC)

SW_kgvar_components <- VarCorr(SW_kg)
summary(SW_kg)


Sucrose.. <- lmer(Sucrose.. ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                    (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                  data = Sucrose..PC)

Sucrose..var_components <- VarCorr(Sucrose..)
summary(Sucrose..)

Moisture <- lmer(Moisture ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                   (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                 data = MoisturePC)

Pol..Reading <- lmer(Pol..Reading ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                       (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                     data = Pol..ReadingPC)

Purity <- lmer(Purity ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                 (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
               data = PurityPC)

#=================Extract Overall BLUEs for each trait of interest for ratoon crops========
# List of model fits
model_fits <- list(
  TRS_kg_per_Mg = TRS_kg_per_Mg,
  CYMg_per_ha = CYMg_per_ha,
  Population = Population,
  Dia = Dia,
  Brix = Brix,
  Fiber = Fiber,
  SW_kg = SW_kg,
  Sucrose.. = Sucrose..,
  Moisture = Moisture,
  Pol..Reading = Pol..Reading,
  Purity = Purity
)

# Empty dataframe to store fixed effects
fixed_effects_df <- data.frame(Geno = unique(df$Geno), stringsAsFactors = FALSE)

# Loop through each model fit
for (trait_name in names(model_fits)) {
  model_fit <- model_fits[[trait_name]]
  
  # Extract fixed effects
  fixed_effects <- fixef(model_fit)
  
  # Filter fixed effects for 'Geno'
  geno_fixed_effects <- fixed_effects[grep("^Geno", names(fixed_effects))]
  
  # Remove "Geno" from the left side of each value in geno_fixed_effects
  names(geno_fixed_effects) <- gsub("^Geno", "", names(geno_fixed_effects))
  
  # Add overall_mean to geno_fixed_effects
  overall_mean <- fixed_effects["(Intercept)"]
  geno_fixed_effects <- geno_fixed_effects + overall_mean
  
  # Create a dataframe with Geno and the calculated estimate
  df_trait <- data.frame(Geno = unique(df$Geno), Estimate = ifelse(unique(df$Geno) %in% names(geno_fixed_effects), geno_fixed_effects[unique(df$Geno)], NA))
  
  # Rename the column to the trait name
  colnames(df_trait)[colnames(df_trait) == "Estimate"] <- trait_name
  
  # Merge with fixed_effects_df
  fixed_effects_df <- merge(fixed_effects_df, df_trait, by = "Geno", all.x = TRUE)
}

# Now fixed_effects_df contains Geno and the fixed effects for each trait
# Assuming DataOutput is your dataframe
write.csv(fixed_effects_df, file = "DataOutputPCBlues.csv", row.names = FALSE)
getwd()
