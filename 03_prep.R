# PHOSP-COVID analysis: DATA PREP
# Cleaning and preparation of variables
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# Functions require library(tidyverse), requires() nor :: not currently written in.  

# Hospital discharge event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_hosp = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "Hospital Discharge")

# Define tier 2 --------------------------------------------------------------------------
tier2_study_id = phosp %>% 
  filter(!is.na(crf3b_visit)) %>% 
  distinct(study_id) %>% 
  pull(study_id)

# Variable definitions------------ ------------------------------------------------------
phosp_hosp = phosp_hosp %>% 
  mutate(
    
    # Tier 
    tier = if_else(study_id %in% tier2_study_id, 2, 1),
    
    # Respiratory support
    crf1a_resp_support_8levels = case_when(
      crf1a_o2_ecmo == "Yes" ~ "ECMO", 
      crf1a_treat_rrt == "Yes" ~ "RRT",
      crf1a_o2_imv == "Yes" ~ "IMV", 
      crf1a_o2_hfn == "Yes" ~ "HFN", 
      crf1a_o2_blniv == "Yes" ~ "BIPAP", 
      crf1a_o2_cpapv == "Yes" ~ "CPAP", 
      crf1a_o2_supp == "Yes" ~ "O2",
      is.na(crf1a_o2_ecmo) & 
        is.na(crf1a_treat_rrt) & 
        is.na(crf1a_o2_imv) & 
        is.na(crf1a_o2_hfn) & 
        is.na(crf1a_o2_blniv) & 
        is.na(crf1a_o2_cpapv) & 
        is.na(crf1a_o2_supp) ~ NA_character_,
      TRUE ~ "No respiratory support"
    ) %>% 
      factor() %>% 
      fct_relevel("No respiratory support", "O2", "CPAP", "BIPAP", "HFN", "IMV", "RRT", "ECMO") %>% 
      ff_label("Organ support"),
    
    crf1a_resp_support_4levels = case_when(
      crf1a_o2_ecmo == "Yes" ~ "IMV/RRT/ECMO", 
      crf1a_treat_rrt == "Yes" ~ "IMV/RRT/ECMO",
      crf1a_o2_imv == "Yes" ~ "IMV/RRT/ECMO", 
      crf1a_o2_hfn == "Yes" ~ "CPAP/BIPAP/HFN", 
      crf1a_o2_blniv == "Yes" ~ "CPAP/BIPAP/HFN", 
      crf1a_o2_cpapv == "Yes" ~ "CPAP/BIPAP/HFN", 
      crf1a_o2_supp == "Yes" ~ "O2",
      is.na(crf1a_o2_ecmo) & 
        is.na(crf1a_treat_rrt) & 
        is.na(crf1a_o2_imv) & 
        is.na(crf1a_o2_hfn) & 
        is.na(crf1a_o2_blniv) & 
        is.na(crf1a_o2_cpapv) & 
        is.na(crf1a_o2_supp) ~ NA_character_,
      TRUE ~ "No respiratory support"
    ) %>% 
      factor() %>% 
      fct_relevel("No respiratory support", "O2", "CPAP/BIPAP/HFN", "IMV/RRT/ECMO") %>% 
      ff_label("Organ support")
    
  )
