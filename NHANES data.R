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

  

  # --- 2. SELECT & CLEAN VARIABLES ---

  # 2.1 Demographics and Socioeconomics[cite: 1]
  d_clean <- demo %>% 
    select(SEQN, 
           Age = RIDAGEYR, 
           Gender = RIAGENDR, 
           Poverty_Index_Ratio = INDFMPIR, 
           Education = DMDEDUC2,  # Added education level as per the proposal
           Weight_MEC = WTMEC2YR)

  # 2.2 Outcome Variable: Heart Failure
  # MCQ160B: 1 = Yes, 2 = No
  m_clean <- mcq %>% 
    select(SEQN, HeartFailure = MCQ160B)

  # 2.3 Nutritional Predictors[cite: 1]
  nutr_clean <- nutr %>% 
    select(SEQN, 
           Sodium_mg = DR1TSODI, 
           Protein_g = DR1TPROT, 
           Energy_kcal = DR1TKCAL) # Useful to adjust the ratio for total energy intake[cite: 1]

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

 
  
  # --- 4. MERGE ---
  
  tabular_data_NHANES <- d_clean %>%
    left_join(m_clean, by="SEQN") %>%
    left_join(nutr_clean, by="SEQN") %>%
    left_join(b_clean, by="SEQN") %>%
    left_join(diq_clean, by="SEQN") %>%
    left_join(bpx_clean, by="SEQN") %>%
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
    filter(Age >= 18)

message(paste("Total Clinical Records Found:", nrow(all_clinical)))
message(paste("Target Population (Adults):", nrow(target_population)))

#For these cycles, we will fetch mortality:

#Mortality data (gathered in 2019, so it includes deaths up to 2019)

base_url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/"

# List of files corresponding to your cycles G, H, I, J
mort_files <- c(
  "NHANES_2011_2012_MORT_2019_PUBLIC.dat",
  "NHANES_2013_2014_MORT_2019_PUBLIC.dat",
  "NHANES_2015_2016_MORT_2019_PUBLIC.dat",
  "NHANES_2017_2018_MORT_2019_PUBLIC.dat"
)

# Function to read each specific mortality file

# Function to read each specific mortality file
read_mort_file <- function(filename) {
  full_url <- paste0(base_url, filename)
  message(">>> Downloading: ", filename)
  
  read_fwf(full_url, 
           fwf_cols(
             SEQN             = c(1, 6),
             Eligibility      = c(15, 15),
             Mortality_Status = c(16, 16),
             Cause_of_Death   = c(17, 19),
             Follow_up_Months = c(20, 23)
           ),
           na = c(".", "....", ""),
           col_types = cols(
             SEQN = col_double(),
             Eligibility = col_double(),
             Mortality_Status = col_double(),
             Cause_of_Death = col_character(),
             Follow_up_Months = col_double()
           ))
}

# Download and stack all 3 cycles
mortality_df <- lapply(mort_files, read_mort_file) %>% 
  bind_rows()

message("Total mortality records loaded: ", nrow(mortality_df))

jointdata_mortality <- left_join(target_population, mortality_df, by="SEQN")

# Clean dataset (remove NA's in Eligibility, Sodium, Protein, and Heart Failure for future analyses)

df_clean <- jointdata_mortality %>%
  filter(
    !is.na(Eligibility) & Eligibility == 1,  # Only eligible participants
    !is.na(Sodium_mg),                       # Remove records with missing Sodium
    !is.na(Protein_g),                         # Remove records with missing Protein
    !is.na(HeartFailure)                      # Remove records with missing Heart Failure status
  )
  
create_report(df_clean, output_file = "EDA_Report_Mortality.html", output_dir = "reports")

# Save the combined dataset for future analysis
saveRDS(df_clean, file = "df_clean.rds")

#Review the WTMEC2YR variable for representativeness and potential weighting in future analyses