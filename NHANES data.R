# ==============================================================================
# DATA IMPORT AND NHANES INTERFACE
# ==============================================================================
library(haven)          # Import and export data from SPSS, Stata, and SAS (.xpt)
library(nhanesA)        # Official API to search and download CDC NHANES data
library(readr)          # High-speed reading of flat files (CSV, TSV, TXT)

# ==============================================================================
# DATA WRANGLING AND TIDYVERSE CORE
# ==============================================================================
library(dplyr)          # A grammar for data manipulation and transformation
library(tidyr)          # Tools for tidying data (pivoting, nesting, unnesting)
library(purrr)          # Functional programming toolkit (map/reduce operations)
library(ggplot2)        # Advanced data visualization based on the Grammar of Graphics
library(DataExplorer)   # Automated EDA (missing data, distributions, correlations, etc.)
# ==============================================================================
# 
# The proposal focuses on the 2011-2018 cycles to increase statistical power[cite: 1].
# G: 2011-2012 | H: 2013-2014 | I: 2015-2016 | J: 2017-2018
cycles <- c("G", "H", "I", "J") 

get_clinical_data <- function(suffix) {
  
  message(paste(">>> Fetching Metadata for Cycle:", suffix))
  
  # --- 1. DOWNLOAD TABLES ---
  demo <- nhanes(paste0("DEMO_", suffix))    # Demographics (PIR, Gender, Age, Weights)
  mcq  <- nhanes(paste0("MCQ_", suffix))     # Medical Conditions (Heart Failure)
  nutr <- nhanes(paste0("DR1TOT_", suffix))  # Total Nutrition (Sodium, Protein)
  bmx  <- nhanes(paste0("BMX_", suffix))     # Body measures (BMI)
  bpx  <- nhanes(paste0("BPX_", suffix))     # Blood Pressure (Measured)
  diq  <- nhanes(paste0("DIQ_", suffix))     # Diabetes
  pro <- nhanes(paste0("DR1IFF_", suffix))   # Individual Foods (for future dietary pattern analysis)
  med <- nhanes(paste0("RXQ_RX_", suffix))   # Prescription Medications (for future confounder adjustment)
  

  # --- 2. SELECT & CLEAN VARIABLES ---

  # 2.1 Demographics and Socioeconomics[cite: 1]
  d_clean <- demo %>% 
    select(SEQN, 
           Age = RIDAGEYR, 
           Gender = RIAGENDR, 
           Poverty_Index_Ratio = INDFMPIR, 
           Education = DMDEDUC2,  # Added education level as per the proposal
           Weight_MEC = WTMEC2YR,
          Strata = SDMVSTRA,
          PSU = SDMVPSU)

  # 2.2 Outcome Variable: Heart Failure
  # MCQ160B: 1 = Yes, 2 = No
  m_clean <- mcq %>% 
    select(SEQN, HeartFailure = MCQ160B)

  # 2.3 Nutritional Predictors[cite: 1]
  nutr_clean <- nutr %>% 
    select(SEQN, 
           Sodium_mg = DR1TSODI, 
           Protein_g = DR1TPROT, 
           Energy_kcal = DR1TKCAL,
          ) # Useful to adjust the ratio for total energy intake[cite: 1]

  # 2.4 Clinical Variables and Comorbidities
  b_clean <- bmx %>% select(SEQN, BMI = BMXBMI)
  
  diq_clean <- diq %>% 
    select(SEQN, Diabetes = DIQ010)

  # Blood Pressure: Mean of the 3 systolic measurements (Measured Hypertension)[cite: 1]
  bpx_clean <- bpx %>% 
    rowwise() %>%
    mutate(Mean_Systolic = mean(c(BPXSY1, BPXSY2, BPXSY3), na.rm = TRUE),
           Mean_Diastolic = mean(c(BPXDI1, BPXDI2, BPXDI3), na.rm = TRUE)) %>%
    select(SEQN, Mean_Systolic, Mean_Diastolic)
  
  # Protein origin
    
  pro_clean <- pro %>%
  group_by(SEQN) %>%
  summarise(
    Total_Animal_Protein_g = sum(if_else(substr(as.character(DR1IFDCD), 1, 1) %in% c("1","2","3"),
                                         as.numeric(DR1IPROT), 0), # Codes starting with 1, 2, and 3 are animal-based foods (1 - Milk and milk products, 2 - Meat, poultry, fish, and mixtures, 3 - Eggs)
                                 na.rm = TRUE),
    Total_Plant_Protein_g  = sum(if_else(substr(as.character(DR1IFDCD), 1, 1) %in% c("4","5","6","7"),
                                         as.numeric(DR1IPROT), 0), # Codes starting with 4, 5, 6 and 7 are plant-based foods (4 - Legumes, nuts, and seeds, 5 - Grain products, 6 - Fruits, 7 - Vegetables)
                                 na.rm = TRUE),
    .groups = "drop"
  )
  
  # Medication use for heart failure (for future confounder adjustment
  # 1. Define the target codes (ensure all variations are included)
hf_icd_codes <- c("I50", "I50.9", "I50.1", "I50.2", "I50.3", "I50.4", "I50.9P")

# 2. Collapse the data to one row per participant (SEQN)
med_clean <- med %>%
  rename_with(toupper) %>%
  group_by(SEQN) %>%
  summarise(
    # Use any() to check across all medications the person takes
    HF_Medication = as.factor(any(
      if_any(
        any_of(c("RXDRSC1", "RXDRSC2", "RXDRSC3")), 
        ~ trimws(.x) %in% hf_icd_codes
      ), 
      na.rm = TRUE
    )),
    .groups = "drop"
  )
 
  # ---  MERGE ---
  
  tabular_data_NHANES <- d_clean %>%
    left_join(m_clean, by="SEQN") %>%
    left_join(nutr_clean, by="SEQN") %>%
    left_join(b_clean, by="SEQN") %>%
    left_join(diq_clean, by="SEQN") %>%
    left_join(bpx_clean, by="SEQN") %>%
    left_join(pro_clean, by="SEQN") %>%
    left_join(med_clean, by="SEQN") %>%
    mutate(
      Cycle = suffix
    )
  
  return(tabular_data_NHANES)
}

# A. Download and Combine G, H, I, J cycles
all_clinical <- map_df(cycles, get_clinical_data)

# B. FILTER TARGET POPULATION
# Crucial: General adult population[cite: 1]
target_population <- all_clinical %>%
    filter(Age >= 18) # Adults only

message(paste("Total Clinical Records Found:", nrow(all_clinical)))
message(paste("Target Population (Adults):", nrow(target_population)))


# Clean dataset (remove NA's in Eligibility, Sodium, Protein, and Heart Failure for future analyses)

df_clean <- target_population %>%
  filter(
    !is.na(Sodium_mg),                       # Remove records with missing Sodium
    !is.na(Protein_g),                         # Remove records with missing Protein
    !is.na(HeartFailure)                      # Remove records with missing Heart Failure status
  )
  
create_report(df_clean, output_file = "EDA_Report_Mortality.html", output_dir = "reports")

# Save the combined dataset for future analysis
saveRDS(df_clean, file = "df_clean.rds")

#Review the WTMEC2YR variable for representativeness and potential weighting in future analyses