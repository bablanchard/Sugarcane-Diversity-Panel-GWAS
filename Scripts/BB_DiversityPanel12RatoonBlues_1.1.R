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
TRS_kg_per_Mg <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0,]
TRSLight <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Location == "Light", ]
TRSHeavy <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Location == "Heavy", ]
TRSPC <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "0", ]
TRS1R <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "1", ]
TRS2R <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "2", ]
TRS3R <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & df$Crop == "3", ]
TRS12R <- df[complete.cases(df$TRS_kg_per_Mg) & df$TRS_kg_per_Mg != 0 & (df$Crop == "1" | df$Crop == "2"), ]

CYMg_per_ha <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0,]
CYLight <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Location == "Light", ]
CYHeavy <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Location == "Heavy", ]
CYPC <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "0", ]
CY1R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "1", ]
CY2R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "2", ]
CY3R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "3", ]
CY12R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & (df$Crop == "1" | df$Crop == "2"), ]

Population <- df[complete.cases(df$Population) & df$Population != 0,]
PopLight <- df[complete.cases(df$Population) & df$Population != 0 & df$Location == "Light", ]
PopHeavy <- df[complete.cases(df$Population) & df$Population != 0 & df$Location == "Heavy", ]
PopPC <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "0", ]
Pop1R <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "1", ]
Pop2R <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "2", ]
Pop3R <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "3", ]
Pop12R <- df[complete.cases(df$Population) & df$Population != 0 & (df$Crop == "1" | df$Crop == "2"), ]

Dia <- df[complete.cases(df$Dia) & df$Dia != 0,]
DiaLight <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Location == "Light", ]
DiaHeavy <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Location == "Heavy", ]
DiaPC <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "0", ]
Dia1R <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "1", ]
Dia2R <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "2", ]
Dia3R <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "3", ]
Dia12R <- df[complete.cases(df$Dia) & df$Dia != 0 & (df$Crop == "1" | df$Crop == "2"), ]

Brix <- df[complete.cases(df$Brix) & df$Brix != 0,]
BrixLight <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Location == "Light", ]
BrixHeavy <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Location == "Heavy", ]
BrixPC <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "0", ]
Brix1R <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "1", ]
Brix2R <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "2", ]
Brix3R <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "3", ]
Brix12R <- df[complete.cases(df$Brix) & df$Brix != 0 & (df$Crop == "1" | df$Crop == "2"), ]


Fiber <- df[complete.cases(df$Fiber) & df$Fiber != 0,]
FiberLight <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Location == "Light", ]
FiberHeavy <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Location == "Heavy", ]
FiberPC <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "0", ]
Fiber1R <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "1", ]
Fiber2R <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "2", ]
Fiber3R <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "3", ]
Fiber12R <- df[complete.cases(df$Fiber) & df$Fiber != 0 & (df$Crop == "1" | df$Crop == "2"), ]


SW_kg <- df[complete.cases(df$SW_kg) & df$SW_kg != 0,]
SW_kgLight <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Location == "Light", ]
SW_kgHeavy <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Location == "Heavy", ]
SW_kgPC <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "0", ]
SW_kg1R <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "1", ]
SW_kg2R <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "2", ]
SW_kg3R <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "3", ]
SW_kg12R <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & (df$Crop == "1" | df$Crop == "2"), ]

Sucrose.. <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0,]
Sucrose..Light <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Location == "Light", ]
Sucrose..Heavy <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Location == "Heavy", ]
Sucrose..PC <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "0", ]
Sucrose..1R <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "1", ]
Sucrose..2R <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "2", ]
Sucrose..3R <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "3", ]
Sucrose..12R <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & (df$Crop == "1" | df$Crop == "2"), ]


Moisture <- df[complete.cases(df$Moisture) & df$Moisture != 0,]
MoistureLight <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Location == "Light", ]
MoistureHeavy <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Location == "Heavy", ]
MoisturePC <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "0", ]
Moisture1R <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "1", ]
Moisture2R <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "2", ]
Moisture3R <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "3", ]
Moisture12R <- df[complete.cases(df$Moisture) & df$Moisture != 0 & (df$Crop == "1" | df$Crop == "2"), ]

Pol..Reading <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0,]
Pol..ReadingLight <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Location == "Light", ]
Pol..ReadingHeavy <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Location == "Heavy", ]
Pol..ReadingPC <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "0", ]
Pol..Reading1R <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "1", ]
Pol..Reading2R <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "2", ]
Pol..Reading3R <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "3", ]
Pol..Reading12R <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & (df$Crop == "1" | df$Crop == "2"), ]

Purity <- df[complete.cases(df$Purity) & df$Purity != 0,]
PurityLight <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Location == "Light", ]
PurityHeavy <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Location == "Heavy", ]
PurityPC <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "0", ]
Purity1R <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "1", ]
Purity2R <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "2", ]
Purity3R <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "3", ]
Purity12R <- df[complete.cases(df$Purity) & df$Purity != 0 & (df$Crop == "1" | df$Crop == "2"), ]


# Assuming "Geno" is the column name containing the unique values
unique_genos <- unique(TRS_kg_per_Mg$Geno)
num_unique_genos <- length(unique_genos)
print(num_unique_genos)

#===========Define linear mixed models for each trait===========
# Define the model with fixed and random effects
TRS_kg_per_Mg <- lmer(TRS_kg_per_Mg ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                        (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                      data = TRS12R)

TRSvar_components <- VarCorr(TRS_kg_per_Mg)
TRSmodel_fit <- summary(TRS_kg_per_Mg)


CYMg_per_ha <- lmer(CYMg_per_ha ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                      (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                    data = CY12R)

CYvar_components <- VarCorr(CYMg_per_ha)
CYmodel_fit <- summary(CYMg_per_ha)


Population <- lmer(Population ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                     (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                   data = Pop12R)

Populationvar_components <- VarCorr(Population)
summary(Population)

Dia <- lmer(Dia ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
              (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
            data = Dia12R)

Diavar_components <- VarCorr(Dia)
Diamodel_fit <- summary(Dia)


Brix <- lmer(Brix ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
               (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
             data = Brix12R)

Brixvar_components <- VarCorr(Brix)
summary(Brix)

Fiber <- lmer(Fiber ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
              data = Fiber12R)

Fibervar_components <- VarCorr(Fiber)
summary(Fiber)


SW_kg <- lmer(SW_kg ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
              data = SW_kg12R)

SW_kgvar_components <- VarCorr(SW_kg)
summary(SW_kg)


Sucrose.. <- lmer(Sucrose.. ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                    (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                  data = Sucrose..12R)

Sucrose..var_components <- VarCorr(Sucrose..)
summary(Sucrose..)

Moisture <- lmer(Moisture ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                   (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                 data = Moisture12R)

Pol..Reading <- lmer(Pol..Reading ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                       (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
                     data = Pol..Reading12R)

Purity <- lmer(Purity ~ 1 + Geno + (1|Year) + (1|Location) + (1|Rep:(Year:Location)) + 
                 (1|Year:Location) + (1|Geno:Year) + (1|Geno:Location) + (1|Geno:Year:Location),
               data = Purity12R)

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
write.csv(fixed_effects_df, file = "DataOutputRatoonBlues.csv", row.names = FALSE)
