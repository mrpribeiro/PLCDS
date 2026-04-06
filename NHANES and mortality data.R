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

# ==============================================================================

# ==============================================================================
# 1. FUNCTION: FETCH CLINICAL & SUMMARY DATA (UPDATED WITH YOUR VARIABLES)
# ==============================================================================

# Data is organized in 2-year cycles. We will loop through the cycles to fetch and merge data. Check for more cycles here:

browseNHANES(browse = FALSE) # Opens the NHANES data browser in your web browser
cycles <- c("E", "F", "G") # 2007-2008, 2009-2010, 2011-2012
get_clinical_data <- function(suffix) {
  
  message(paste(">>> Fetching Metadata for Cycle:", suffix))
  
    # Download tables
    demo <- nhanes(paste0("DEMO_", suffix)) # Demographics 
    body <- nhanes(paste0("BMX_", suffix))  # Body measurements (e.g., height, weight, BMI)
    med  <- nhanes(paste0("MCQ_", suffix))  # Medical conditions (e.g., asthma, COPD, diabetes)
    smok <- nhanes(paste0("SMQ_", suffix))  # Smoking history (e.g., smoked 100 cigarettes, current smoking status)
    spx  <- nhanes(paste0("SPX_", suffix))  # Spirometry Summary
    cbc  <- nhanes(paste0("CBC_", suffix))  # Eosinophils
    feno <- nhanes(paste0("ENX_", suffix))  # FeNO
    sym  <- nhanes(paste0("RDQ_", suffix))  # Symptoms (e.g., cough, wheezing)
    diq  <- nhanes(paste0("DIQ_", suffix))  # Diabetes
    ghb  <- nhanes(paste0("GHB_", suffix))  # HbA1c
    dpq  <- nhanes(paste0("DPQ_", suffix))  # Depression (PHQ-9)
    tchol<- nhanes(paste0("TCHOL_", suffix))# Total cholesterol
  
  #Add more tables as needed (e.g., dietary, physical activity, etc.)   
  #    
  # --- SELECT VARIABLES ---

  # 1. Demographics
  d_clean <- demo %>% select(SEQN, Age = RIDAGEYR, Gender = RIAGENDR, Ethnicity = RIDRETH1, Poverty_Index_Ratio = INDFMPIR)
  
  # 2. Body
  b_clean <- body %>% select(SEQN, Height_cm = BMXHT, Weight_kg = BMXWT, BMI = BMXBMI) 
  
  # 3. Medical
  m_clean <- med %>% 
    select(SEQN, Asthma=MCQ010, Emphysema=MCQ160G, ChronBronch=MCQ160K, 
           HeartAttack=MCQ160E, HeartFail=MCQ160B, Family_Asthma = MCQ300C) %>%
    mutate(COPD = ifelse(Emphysema==1 | ChronBronch==1, 1, 2))
  
  # 4. Smoking
  s_clean <- smok %>% select(SEQN, Smoked100=SMQ020, SmokeNow=SMQ040)

  # 5. Eosinophils
  e_clean <- cbc %>% select(SEQN, Eos_Count = LBDEONO, Eos_Perc = LBXEOPCT)
 
  # 7. FeNO
  f_clean <- feno %>% select(SEQN, FeNO_ppb = ENXMEAN)

  # 8. Symptoms
  sym_clean <- sym %>% 
    select(SEQN, Cough_3months = RDQ031, Phlegm_3months = RDQ050, Wheezing_12months = RDQ070, Wheezing_exercise = RDQ100, Medication_Wheezing = RDQ134, Respiratory_abs_days = RDQ137, Cough_night = RDQ140)
    
  # 9. Diabetes
  diq_clean <- diq %>% select(SEQN, Diabetes = DIQ010)
  
  # 10. HbA1c
  ghb_clean <- ghb %>% select(SEQN, HbA1c = LBXGH)

  # 11. Total Cholesterol
  tchol_clean <- tchol %>% select(SEQN, Total_Cholesterol = LBXTC)

  # 12. Depression (PHQ-9)
  dpq_clean <- dpq %>% 
    select(SEQN, DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090)
    
 
  # --- MERGE ---
  # minimal sp_clean (replace/select real spirometry vars as needed)
  sp_clean <- spx %>% select(SEQN)

  # then the existing joins will work (left_join(sp_clean, by = "SEQN"))
  tabular_data_NHANES <- d_clean %>%
    left_join(b_clean, by="SEQN") %>%
    left_join(m_clean, by="SEQN") %>%
    left_join(s_clean, by="SEQN") %>%
    left_join(e_clean, by="SEQN") %>%
    left_join(f_clean, by="SEQN") %>%
    left_join(sym_clean, by="SEQN") %>%
    left_join(diq_clean, by="SEQN") %>%
    left_join(ghb_clean, by="SEQN") %>%
    left_join(tchol_clean, by="SEQN") %>%
    left_join(dpq_clean, by="SEQN") %>%
    left_join(sp_clean, by="SEQN") %>%   # ensure this matches placement you prefer
    mutate(Cycle = suffix)
  
  return(tabular_data_NHANES)
}

# A. Download and Combine E, F, G cycles
all_clinical <- map_df(cycles, get_clinical_data)

# B. FILTER TARGET POPULATION
# Crucial: We only want Adults

target_population <- all_clinical %>%
    filter(
      Age >= 18                           # Adults only
      )

message(paste("Total Clinical Records Found:", nrow(all_clinical)))

message(paste("Target Population (Filter to apply afterwards):", nrow(target_population)))

#For these cycles, we will fetch mortality:

#Mortality data

base_url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/"

# List of files corresponding to your cycles E, F, and G
mort_files <- c(
  "NHANES_2007_2008_MORT_2019_PUBLIC.dat",
  "NHANES_2009_2010_MORT_2019_PUBLIC.dat",
  "NHANES_2011_2012_MORT_2019_PUBLIC.dat"
)

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



