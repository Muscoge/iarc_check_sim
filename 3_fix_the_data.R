# if(!require('pacman')) install.packages('pacman')
pacman::p_load(here, tidyverse, lubridate, writexl)
load(here("output/converted_diag_data.rda"))

# load functions with auxiliary functions based on iacr
source(here("functions/functions_for_iacr_mp.R"))

full_dt_cut <- data_icd10 %>%
  separate(icdo3, 
           into = c("topo_Cxx", "topo_x", "morph", "behav_icdo3", "gr_icdo3"),
           sep = c(3, 4, 8, 9, 10),
           remove = FALSE) %>%
  unite(topo_Cxxx, topo_Cxx:topo_x, sep = '', remove = F)%>%
  mutate(across(c("morph", "behav_icdo3", "gr_icdo3"), as.double)) %>% 
  # check behaviour and topo codes manually
  # add status "exclude" and remove wrong observations 
  add_exclude_status() %>% filter(exclude != "exclude") %>% select(-exclude) %>%
  group_by(id) %>% mutate(n = n(), multiple_tumours = ifelse(n > 1, 1, 0)) %>% ungroup()

# select only patients with multiple cases, add information about topography, morphology 
# and whether a tumour is solid or leukemia/lymphoid
multiple_tumours <- full_dt_cut %>% 
  dplyr::select(id, id_tumour, sex, icdo3, topo_Cxx, topo_Cxxx, topo_x, 
                morph, behav_icdo3, gr_icdo3, diag_date, multiple_tumours) %>% 
  arrange(id, id_tumour, desc(morph)) %>% 
  filter(multiple_tumours == 1) %>% 
  add_morph_groups() %>% 
  add_topo_groups() %>% 
  add_solid_status()

single_tumours <- full_dt_cut %>% filter(multiple_tumours == 0) 

# solid tumours and leukemia/lymphoid are processed separately 
only_solid_cases <- multiple_tumours %>% filter(solid_status == "solid") %>% 
  dplyr::select(-solid_status)

# ------------------------------------------------------------------------------

# THE RULE [International rules for multiple primary cancers]

# If the morphological diagnoses fall into one category in Table 2, and arise in the same primary site, 
# they are considered to be the same morphology for the purpose of counting multiple primaries. 
# If the morphological diagnoses fall into two or more of the categories in Table 2, even if they concern the same site, 
# the morphology is considered to be different, and two or more cases should be counted. 

# Single tumours containing several different histologies which fall into one histological group in Table 2 
# are registered as a single case, using the numerically highest ICD-O morphology code. 

tmp <- only_solid_cases %>% 
  # groups 5 and 17 are considered the same for solid tumours by IACR tools 
  mutate(morph_group = ifelse(morph_group %in% c(5, 17), -1, morph_group)) %>% 
  group_by(id, topo_group, morph_group) %>% mutate(morph = max(morph)) %>%
  arrange(id, diag_date) %>% 
  # change topography according to rule 3 and Table 1
  mutate(
    topo_Cxx = case_when(
      topo_group %in% c("C02","C06","C14","C20","C24","C34","C41","C68") & any(duplicated(diag_date)) ~ topo_group,
      TRUE ~ topo_Cxx),
    topo_x = case_when(
      topo_group %in% c("C02","C06","C14","C20","C24","C34","C41","C68") & any(duplicated(diag_date)) ~ "9",
      TRUE ~ topo_x)
  ) %>% 
  slice_head(n = 1) 

# process non-specific morphology groups 

# If, however, one morphology is not specific (groups (5), (14) and (17)) and a specific morphology is available, 
# the case should be reported with the specific histology and the non-specific diagnosis should be ignored. 
# For solid cases group 14 is not possible. After the previous code only one non-specific case can remain.
# If the earliest case has non-specific code, the diagnosis date of the first specific case should be fixed.

tmp_fix <- tmp %>% 
  group_by(id) %>% arrange(id, diag_date) %>%
  group_by(id, topo_group) %>% 
  mutate(fix_date = ifelse(first(morph_group) == -1, TRUE, FALSE)) %>% 
  mutate(date = ifelse(fix_date & row_number() == 2, 
                       min(diag_date, first(diag_date)), 
                       diag_date)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(date, origin = "1970-01-01"), diag_date = date)

# remove non-specific case if specific is available and update status after cleaning
tmp <- tmp_fix %>% 
  group_by(id, topo_group) %>% mutate(n = n()) %>% ungroup() %>% 
  mutate(action = ifelse(morph_group == -1 & n > 1, "remove", "keep")) %>% 
  filter(action == "keep") 

only_solid_cases <- tmp %>% 
  unite("icdo3_fixed", topo_Cxx:gr_icdo3, sep = "") %>% 
  dplyr::select(id, icdo3_fixed, id_tumour, diag_date_fixed = diag_date)  

# process leukemia and lymphoid
hemo_leuk <- multiple_tumours %>% 
  filter(solid_status == "leukemia and lymphoid") %>% 
  select(-solid_status) %>% 
  group_by(id, morph_group) %>%
  mutate(morph = max(morph)) %>%
  arrange(id, diag_date) %>% 
  slice_head(n = 1)

tmp2 <- hemo_leuk %>% group_by(id) %>% arrange(id, diag_date) %>%
  mutate(fix_date  = ifelse(first(morph_group) == 14, TRUE, FALSE)) %>% 
  mutate(diag_date = ifelse(fix_date & row_number() == 2, 
                            min(diag_date, first(diag_date)), 
                            diag_date),
         topo_Cxx    = ifelse(fix_date & row_number() == 2, first(topo_Cxx), topo_Cxx),
         topo_x      = ifelse(fix_date & row_number() == 2, first(topo_x), topo_x),
         behav_icdo3 = ifelse(fix_date & row_number() == 2, first(behav_icdo3), behav_icdo3),
         gr_icdo3    = ifelse(fix_date & row_number() == 2, first(gr_icdo3), gr_icdo3)
  ) %>% 
  ungroup() %>% 
  mutate(diag_date = as.Date(diag_date, origin = "1970-01-01")) %>% 
  group_by(id) %>% mutate(n = n()) %>% ungroup() %>% 
  mutate(action = ifelse(morph_group == 14 & n > 1, "remove", "keep")) %>% 
  filter(action == "keep")

hemo_leuk <- tmp2 %>% 
  unite("icdo3_fixed", topo_Cxx:gr_icdo3, sep = "") %>% 
  dplyr::select(id, icdo3_fixed, id_tumour, diag_date_fixed = diag_date)

multiple_tumours <- only_solid_cases %>% 
  full_join(hemo_leuk, by = c("id", "icdo3_fixed", "id_tumour", "diag_date_fixed"))

multiple_tumours_test <- full_dt_cut %>% 
  arrange(id, id_tumour, desc(morph)) %>% 
  filter(multiple_tumours == 1) 

x <- multiple_tumours %>% 
  left_join(multiple_tumours_test, by = c("id", "id_tumour")) %>% 
  mutate(icdo3 = icdo3_fixed, diag_date = diag_date_fixed) 

f_dt <- x %>% full_join(single_tumours)

# assign status if it's multiple or single tumour by id_tumour
f_dt <- f_dt %>% 
  group_by(id) %>% 
  arrange(diag_date) %>% mutate(n = n(), id_tumour = row_number()) %>% 
  ungroup() %>% 
  mutate(status = ifelse(n > 1, "multiple tumours", "single tumour"))

# IARC check============================================================================

source(here("functions/functions_for_check.R"))

# # imitates iacr check
iacr_checked <- f_dt %>%
  add_histology_families() %>%
  check_dates_and_age() %>%
  check_sex_site() %>%
  check_behaviour_site() %>%
  check_behaviour_histology() %>%
  check_grade_histology() %>%
  check_site_histology() %>%
  check_sex_histology() %>%
  # check_basis_histology() %>%
  # check_age_site_histology() %>%
  filter(is.na(grade_hist_status)) %>%
  unite(ends_with("_status"), col = "check_message", sep = " ") %>%
  mutate(check_message = str_replace_all(check_message, "NA", "")) %>%
  mutate(check_message = str_squish(check_message))

save(iacr_checked, file = here("output/data_full.rda"))

# for the ruscan project dates required
data_cropped <- iacr_checked %>% filter(year(diag_date) >= 2000 & year(diag_date) <= 2022)

save(data_cropped, file = here("output/data_cropped.rda"))
#-------------------------------------------------------------------------------

source(here("functions/functions_for_dco_dci.R"))

dt_conf <- data_cropped %>% add_diag_basis()

dt_dcn <- dt_conf %>% add_diag_dcn()

dt <- dt_dcn %>% add_final_dco()

save(dt, file = here("output/fixed_data.rda"))