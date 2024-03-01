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

#===========Define linear mixed models for each trait===========
# Define the model with fixed and random effects
TRS_kg_per_Mg <- lmer(TRS_kg_per_Mg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                        (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                        (1|Crop:Year:Location) + (1|Geno:Year) +
                        Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                        (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                      data = TRS_kg_per_Mg)

TRSvar_components <- VarCorr(TRS_kg_per_Mg)
TRSmodel_fit <- summary(TRS_kg_per_Mg)


CYMg_per_ha <- lmer(CYMg_per_ha ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                      (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                      (1|Crop:Year:Location) + (1|Geno:Year) +
                      Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                      (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                    data = CYMg_per_ha)

CYvar_components <- VarCorr(CYMg_per_ha)
CYmodel_fit <- summary(CYMg_per_ha)


Population <- lmer(Population ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                     (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                     (1|Crop:Year:Location) + (1|Geno:Year) +
                     Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                     (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                   data = Population)

Populationvar_components <- VarCorr(Population)
summary(Population)

Dia <- lmer(Dia ~ 1 + (1|Geno) + (1|Location) + (1|Rep:(Year:Location)) + 
              (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
              (1|Crop:Year:Location) + (1|Geno:Year) +
              Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
              (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
            data = Dia)

Diavar_components <- VarCorr(Dia)
Diamodel_fit <- summary(Dia)


Brix <- lmer(Brix ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
               (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
               (1|Crop:Year:Location) + (1|Geno:Year) +
               Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
               (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
             data = Brix)

Brixvar_components <- VarCorr(Brix)
summary(Brix)

Fiber <- lmer(Fiber ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) +
                Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
              data = Fiber)

Fibervar_components <- VarCorr(Fiber)
summary(Fiber)


SW_kg <- lmer(SW_kg ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                (1|Crop:Year:Location) + (1|Geno:Year) +
                Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
              data = SW_kg)

SW_kgvar_components <- VarCorr(SW_kg)
summary(SW_kg)


Sucrose.. <- lmer(Sucrose.. ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                    (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                    (1|Crop:Year:Location) + (1|Geno:Year) +
                    Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                    (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                  data = Sucrose..)

Sucrose..var_components <- VarCorr(Sucrose..)
summary(Sucrose..)

Moisture <- lmer(Moisture ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                   (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                   (1|Crop:Year:Location) + (1|Geno:Year) +
                   Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                   (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                 data = Moisture)

Pol..Reading <- lmer(Pol..Reading ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                       (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                       (1|Crop:Year:Location) + (1|Geno:Year) +
                       Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                       (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
                     data = Pol..Reading)

Purity <- lmer(Purity ~ 1 + (1|Geno) + (1|Year) + (1|Location) + (1|Crop) + (1|Rep:(Year:Location)) + 
                 (1|Rep:(Crop:(Year:Location))) + (1|Year:Location) + (1|Crop:Year) + (1|Crop:Location) + 
                 (1|Crop:Year:Location) + (1|Geno:Year) +
                 Geno:Location + (1|Geno:Crop) + (1|Geno:Year:Location) + (1|Geno:Crop:Year) + 
                 (1|Geno:Crop:Location) + (1|Geno:Crop:Location:Year),
               data = Purity)

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

# Initialize an empty list to store results
fixed_effects_list <- list()

# Loop through each model fit
for (trait_name in names(model_fits)) {
  model_fit <- model_fits[[trait_name]]
  
  # Extract fixed effects
  fixed_effects <- fixef(model_fit)
  
  # Filter fixed effects for 'Geno'
  geno_fixed_effects <- fixed_effects[grep("^Geno", names(fixed_effects))]
  
  # Extract genotype and location
  Genotype <- gsub("^Geno'([^']+).*", "\\1", names(geno_fixed_effects))
  
  # Add overall mean to geno_fixed_effects
  overall_mean <- fixed_effects["(Intercept)"]
  geno_fixed_effects <- geno_fixed_effects + overall_mean
  
  # Store geno_fixed_effects in a data frame
  df_trait <- data.frame(Genotype = Genotype, Estimate = geno_fixed_effects, stringsAsFactors = FALSE)
  
  # Store df_trait in the fixed_effects_list
  fixed_effects_list[[trait_name]] <- df_trait
}

# Merge the data frames in the fixed_effects_list by 'Genotype'
fixed_effects_df <- Reduce(function(x, y) merge(x, y, by = "Genotype", all = TRUE), fixed_effects_list)

# Rename the 'Estimate' columns to trait names
colnames(fixed_effects_df)[-1] <- names(model_fits)

# If there are NAs in the data frame, replace them with 0
fixed_effects_df[is.na(fixed_effects_df)] <- 0

# Now, fixed_effects_df contains all fixed effects for each Genotype across all traits

# Splitting Genotype column into Geno and Location
fixed_effects_df <- within(fixed_effects_df, {
  Genotype <- strsplit(as.character(Genotype), ":", fixed = TRUE)
  Geno <- sapply(Genotype, `[`, 1)
  Location <- sapply(Genotype, `[`, 2)
})


# Remove the original Genotype column if it's no longer needed, remove Geno, Location at the front of values, 
#and move the columns to the front
fixed_effects_df <- fixed_effects_df[, !(names(fixed_effects_df) == "Genotype")]
fixed_effects_df$Geno <- gsub("^Geno", "", fixed_effects_df$Geno)
fixed_effects_df$Location <- gsub("^Location", "", fixed_effects_df$Location)
fixed_effects_df <- fixed_effects_df[, c("Geno", "Location", setdiff(names(fixed_effects_df), c("Geno", "Location")))]


# Assuming DataOutput is your dataframe
write.csv(fixed_effects_df, file = "DataOutputBluesperLocation.csv", row.names = FALSE)