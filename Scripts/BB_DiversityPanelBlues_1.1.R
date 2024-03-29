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

CYMg_per_ha <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0,]
CYLight <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Location == "Light", ]
CYHeavy <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Location == "Heavy", ]
CYPC <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "0", ]
CY1R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "1", ]
CY2R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "2", ]
CY3R <- df[complete.cases(df$CYMg_per_ha) & df$CYMg_per_ha != 0 & df$Crop == "3", ]

Population <- df[complete.cases(df$Population) & df$Population != 0,]
PopLight <- df[complete.cases(df$Population) & df$Population != 0 & df$Location == "Light", ]
PopHeavy <- df[complete.cases(df$Population) & df$Population != 0 & df$Location == "Heavy", ]
PopPC <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "0", ]
Pop1R <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "1", ]
Pop2R <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "2", ]
Pop3R <- df[complete.cases(df$Population) & df$Population != 0 & df$Crop == "3", ]

Dia <- df[complete.cases(df$Dia) & df$Dia != 0,]
DiaLight <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Location == "Light", ]
DiaHeavy <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Location == "Heavy", ]
DiaPC <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "0", ]
Dia1R <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "1", ]
Dia2R <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "2", ]
Dia3R <- df[complete.cases(df$Dia) & df$Dia != 0 & df$Crop == "3", ]

Brix <- df[complete.cases(df$Brix) & df$Brix != 0,]
BrixLight <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Location == "Light", ]
BrixHeavy <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Location == "Heavy", ]
BrixPC <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "0", ]
Brix1R <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "1", ]
Brix2R <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "2", ]
Brix3R <- df[complete.cases(df$Brix) & df$Brix != 0 & df$Crop == "3", ]


Fiber <- df[complete.cases(df$Fiber) & df$Fiber != 0,]
FiberLight <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Location == "Light", ]
FiberHeavy <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Location == "Heavy", ]
FiberPC <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "0", ]
Fiber1R <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "1", ]
Fiber2R <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "2", ]
Fiber3R <- df[complete.cases(df$Fiber) & df$Fiber != 0 & df$Crop == "3", ]


SW_kg <- df[complete.cases(df$SW_kg) & df$SW_kg != 0,]
SW_kgLight <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Location == "Light", ]
SW_kgHeavy <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Location == "Heavy", ]
SW_kgPC <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "0", ]
SW_kg1R <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "1", ]
SW_kg2R <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "2", ]
SW_kg3R <- df[complete.cases(df$SW_kg) & df$SW_kg != 0 & df$Crop == "3", ]

Sucrose.. <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0,]
Sucrose..Light <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Location == "Light", ]
Sucrose..Heavy <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Location == "Heavy", ]
Sucrose..PC <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "0", ]
Sucrose..1R <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "1", ]
Sucrose..2R <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "2", ]
Sucrose..3R <- df[complete.cases(df$Sucrose..) & df$Sucrose.. != 0 & df$Crop == "3", ]


Moisture <- df[complete.cases(df$Moisture) & df$Moisture != 0,]
MoistureLight <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Location == "Light", ]
MoistureHeavy <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Location == "Heavy", ]
MoisturePC <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "0", ]
Moisture1R <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "1", ]
Moisture2R <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "2", ]
Moisture3R <- df[complete.cases(df$Moisture) & df$Moisture != 0 & df$Crop == "3", ]

Pol..Reading <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0,]
Pol..ReadingLight <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Location == "Light", ]
Pol..ReadingHeavy <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Location == "Heavy", ]
Pol..ReadingPC <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "0", ]
Pol..Reading1R <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "1", ]
Pol..Reading2R <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "2", ]
Pol..Reading3R <- df[complete.cases(df$Pol..Reading) & df$Pol..Reading != 0 & df$Crop == "3", ]

Purity <- df[complete.cases(df$Purity) & df$Purity != 0,]
PurityLight <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Location == "Light", ]
PurityHeavy <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Location == "Heavy", ]
PurityPC <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "0", ]
Purity1R <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "1", ]
Purity2R <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "2", ]
Purity3R <- df[complete.cases(df$Purity) & df$Purity != 0 & df$Crop == "3", ]

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

overalllist <- list(TRS_kg_per_Mg = TRS_kg_per_Mg, CYMg_per_ha = CYMg_per_ha, Population = Population, Dia = Dia, Brix = Brix, SW_kg = SW_kg,
                    Sucrose.. = Sucrose.., Moisture = Moisture, Pol..Reading = Pol..Reading, Purity = Purity)  # Add more datasets as needed

locationlist <- list(TRSLight = TRSLight, TRSHeavy = TRSHeavy, CYLight = CYLight, CYHeavy = CYHeavy, PopLight = PopLight, PopHeavy = PopHeavy,
                     DiaLight = DiaLight, DiaHeavy = DiaHeavy, BrixLight = BrixLight, BrixHeavy = BrixHeavy, SW_kgLight = SW_kgLight, 
                     SW_kgHeavy = SW_kgHeavy, Sucrose..Light = Sucrose..Light, Sucrose..Heavy = Sucrose..Heavy, 
                     MoistureLight = MoistureLight, MoistureHeavy = MoistureHeavy, Pol..ReadingLight = Pol..ReadingLight, 
                     Pol..ReadingHeavy = Pol..ReadingHeavy, PurityLight = PurityLight, PurityHeavy = PurityHeavy)

croplist <- list(TRSPC = TRSPC, TRS1R = TRS1R, TRS2R = TRS2R, TRS3R = TRS3R,
                 CYPC = CYPC, CY1R = CY1R, CY2R = CY2R, CY3R = CY3R,
                 PopPC = PopPC, Pop1R = Pop1R, Pop2R = Pop2R, Pop3R = Pop3R,
                 DiaPC = DiaPC, Dia1R = Dia1R, Dia2R = Dia2R, Dia3R = Dia3R,
                 BrixPC = BrixPC, Brix1R = Brix1R, Brix2R = Brix2R, Brix3R = Brix3R,
                 SW_kgPC = SW_kgPC, SW_kg1R = SW_kg1R, SW_kg2R = SW_kg2R, SW_kg3R = SW_kg3R, 
                 Sucrose..PC = Sucrose..PC, Sucrose..1R = Sucrose..1R, Sucrose..2R = Sucrose..2R, Sucrose..3R = Sucrose..3R, 
                 MoisturePC = MoisturePC, Moisture1R = Moisture1R, Moisture2R = Moisture2R, Moisture3R = Moisture3R, 
                 Pol..ReadingPC = Pol..ReadingPC, Pol..Reading1R = Pol..Reading1R, Pol..Reading2R = Pol..Reading2R, Pol..Reading3R = Pol..Reading3R, 
                 PurityPC = PurityPC, Purity1R = Purity1R, Purity2R = Purity2R, Purity3R = Purity3R) 


#===========TRS Sample Model and Outputs==============================
# 'TRS' is the response variable, and 'TRS' is the predictor variable
print(table(df$Geno))
print(table(TRS_kg_per_Mg$Year))
print(table(TRS_kg_per_Mg$Location))
print(table(TRS_kg_per_Mg$Crop))
print(table(TRS_kg_per_Mg$Geno, TRS_kg_per_Mg$Crop))
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
model <- lmer(TRS_kg_per_Mg ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + 
                (1|Rep:(Year:Location)) + (1|Rep:(Crop:(Year:Location))) + 
                (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) + (1|Geno:Location) + 
                (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
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


# Extract fixed effects
fixed_effects <- fixef(model)

# Extract the overall mean (intercept)
overall_mean <- fixed_effects["(Intercept)"]

# Filter fixed effects for 'Geno'
geno_fixed_effects <- fixed_effects[grep("^Geno", names(fixed_effects))]

# Create a dataframe from the names and values of fixed_effects for 'Geno'
geno_fixed_effects_df <- data.frame(
  Effect = names(geno_fixed_effects),
  Value = as.vector(geno_fixed_effects),
  stringsAsFactors = FALSE
)

# Add overall_mean to geno_fixed_effects_df$Value
geno_fixed_effects_df$Value <- geno_fixed_effects_df$Value + overall_mean

# Remove "Geno" from the left side of each value in geno_fixed_effects_df$Effect
geno_fixed_effects_df$Effect <- gsub("^Geno", "", geno_fixed_effects_df$Effect)

# Merge DataOutput and geno_fixed_effects_df by Geno
merged_data <- merge(DataOutput, geno_fixed_effects_df, by.x = "Geno", by.y = "Effect", all.x = TRUE)

# Create a new column TRSALL and assign values from Value where Geno matches
merged_data$TRSALL <- ifelse(!is.na(merged_data$Value), merged_data$Value, NA)

DataOutput <- merged_data[c("Geno", "TRSALL")]

#===========Define linear mixed models for each trait===========
# Define the model with fixed and random effects
TRS_kg_per_Mg <- lmer(TRS_kg_per_Mg ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = TRS_kg_per_Mg)

TRSvar_components <- VarCorr(TRS_kg_per_Mg)
TRSmodel_fit <- summary(TRS_kg_per_Mg)


CYMg_per_ha <- lmer(CYMg_per_ha ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                      (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                      (1|Crop:Year:Location) + (1|Geno:Year) +
                      (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                      (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                    data = CYMg_per_ha)

CYvar_components <- VarCorr(CYMg_per_ha)
CYmodel_fit <- summary(CYMg_per_ha)


Population <- lmer(Population ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                     (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                     (1|Crop:Year:Location) + (1|Geno:Year) +
                     (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                     (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                   data = Population)

Populationvar_components <- VarCorr(Population)
summary(Population)

Dia <- lmer(Dia ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
              (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
              (1|Crop:Year:Location) + (1|Geno:Year) +
              (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
              (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
            data = Dia)

Diavar_components <- VarCorr(Dia)
Diamodel_fit <- summary(Dia)


Brix <- lmer(Brix ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
               (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
               (1|Crop:Year:Location) + (1|Geno:Year) +
               (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
               (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
             data = Brix)

Brixvar_components <- VarCorr(Brix)
summary(Brix)

Fiber <- lmer(Fiber ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) +
                (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
              data = Fiber)

Fibervar_components <- VarCorr(Fiber)
summary(Fiber)


SW_kg <- lmer(SW_kg ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) +
                (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
              data = SW_kg)

SW_kgvar_components <- VarCorr(SW_kg)
summary(SW_kg)


Sucrose.. <- lmer(Sucrose.. ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                    (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                    (1|Crop:Year:Location) + (1|Geno:Year) +
                    (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                    (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                  data = Sucrose..)

Sucrose..var_components <- VarCorr(Sucrose..)
summary(Sucrose..)

Moisture <- lmer(Moisture ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                   (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                   (1|Crop:Year:Location) + (1|Geno:Year) +
                   (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                   (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                 data = Moisture)

Pol..Reading <- lmer(Pol..Reading ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                       (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                       (1|Crop:Year:Location) + (1|Geno:Year) +
                       (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                       (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                     data = Pol..Reading)

Purity <- lmer(Purity ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                 (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                 (1|Crop:Year:Location) + (1|Geno:Year) +
                 (1|Geno:Location) + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                 (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
               data = Purity)
#===============collect overall blues for CY==============
# Define the model with fixed and random effects
CYMg_per_ha <- lmer(CYMg_per_ha ~ 1 + Geno + (1|Year) + (1|Location) + (1|Crop) + 
                (1|Rep:(Year:Location)) + (1|Rep:(Crop:(Year:Location))) + 
                (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) + (1|Geno:Location) + 
                (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
              data = CYMg_per_ha)


# Fit the model
model_fit <- summary(CYMg_per_ha)

# Extract fixed effects
fixed_effects <- fixef(CYMg_per_ha)

# Extract the overall mean (intercept)
overall_mean <- fixed_effects["(Intercept)"]

# Filter fixed effects for 'Geno'
geno_fixed_effects <- fixed_effects[grep("^Geno", names(fixed_effects))]

# Create a dataframe from the names and values of fixed_effects for 'Geno'
geno_fixed_effects_df <- data.frame(
  Effect = names(geno_fixed_effects),
  Value = as.vector(geno_fixed_effects),
  stringsAsFactors = FALSE
)

# Add overall_mean to geno_fixed_effects_df$Value
geno_fixed_effects_df$Value <- geno_fixed_effects_df$Value + overall_mean

# Remove "Geno" from the left side of each value in geno_fixed_effects_df$Effect
geno_fixed_effects_df$Effect <- gsub("^Geno", "", geno_fixed_effects_df$Effect)

# Merge DataOutput and geno_fixed_effects_df by Geno
merged_data <- merge(DataOutput, geno_fixed_effects_df, by.x = "Geno", by.y = "Effect", all.x = TRUE)

# Create a new column TRSALL and assign values from Value where Geno matches
merged_data$CYALL <- ifelse(!is.na(merged_data$Value), merged_data$Value, NA)

DataOutput <- merged_data[c("Geno", "TRSALL", "CYALL")]

#=================Extract Overall BLUEs for each trait of interest for DataOutput========
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
write.csv(fixed_effects_df, file = "DataOutputOverallBlues.csv", row.names = FALSE)




