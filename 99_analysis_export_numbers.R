library(readr) # loaded with tidyverse anyway
datadir = "/home/common/phosp/raw/"
timestamp = "2021-03-26_0400"
phosp   = read_rds(paste0(datadir, "phosp_", timestamp, ".rds"))
lastrun = Sys.time()
source("02_functions.R")
source("03_prep.R")
source("05_kco.R")

### Tier 2 cohort before 30 Nov 2020 with < 240 days follow-up
# Filter at 240 days follow up. 
study_id_240 = phosp %>% 
  group_by(study_id) %>% 
  fill(crf1a_date_dis, crf3a_visit_date, .direction = "downup") %>% 
  select(study_id, crf1a_date_dis, crf3a_visit_date) %>% 
  distinct(study_id, .keep_all = TRUE) %>% 
  mutate(discharge2review = (crf3a_visit_date - crf1a_date_dis) %>% as.numeric()) %>% 
  filter(discharge2review <= 240 | is.na(discharge2review)) %>% 
  pull(study_id)

phosp_hosp %>% 
  filter(tier == 2) %>% 
  filter(study_id %in% study_id_before_end_nov) %>% 
  filter(study_id %in% study_id_240) %>% 
  count(crf1a_resp_support_4levels) %>% 
  mytable()

keep = phosp_hosp %>% 
  filter(tier == 2) %>% 
  filter(study_id %in% study_id_before_end_nov) %>%
  filter(study_id %in% study_id_240) %>% 
  drop_na(crf1a_resp_support_4levels) %>% 
  filter(!is.na(crf1a_sex)) %>% 
  filter(study_id != "28-31") %>% # Temp remove as no 3 m data coming through
  pull(study_id)

phosp_hosp = phosp_hosp %>% 
  filter(study_id %in% keep)

out = phosp_hosp %>% select(study_id, phosp_id)

write_csv(out, "study_id_260321.csv")



# Clustering table
## Run up to hear from 02_analysis_cluster.Rmd

out2 = phosp_3m %>% 
  drop_na(explanatory_hrqol) %>%
  left_join(phosp_hosp %>% select(study_id, phosp_id)) %>% 
  select(study_id, phosp_id)
# 767

write_csv(out2, "study_id__clustering_2021-03-12_1624.csv")

