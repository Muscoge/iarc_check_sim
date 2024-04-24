# if(!require('pacman')) install.packages('pacman')
pacman::p_load(here, tidyverse, lubridate)
load(here("output/initial_data.rda"))

# load functions with auxiliary data with previous errors
source(here("functions/functions_for_fix_diag_convert_to_icdo2.R"))

# fix wrong topography codes from the errors vector
# (errors previously were found in CR data from regions)
dt <- data_eng %>% 
  filter(topo_Cxxx %in% errors) %>% 
  mutate(topo_Cxxx = ifelse(str_length(topo_Cxxx) == 3, 
                            str_c(topo_Cxxx, "9"),
                            str_sub(topo_Cxxx, 1, str_length(topo_Cxxx) - 1)))

# fix other errors manually
not_dt <- data_eng %>% filter(!topo_Cxxx %in% errors) %>% 
  fix_diag_codes()

# join and fix wrong diagnosis codes manually
full_dt <- dt %>% full_join(not_dt) %>%
  fix_behaviour_morph_codes() %>% 
  fix_liver_hemo_1() %>% 
  fix_liver_hemo_2()

correction_data$grade = as.character(correction_data$grade)

# add corrections from external objects
full_dt_corr <- full_dt %>% 
  # an object with previously founded code bugs
  left_join(correction_data, by = c("sex", "morph", "grade", "behaviour_2", "topo_liv2")) %>%
  mutate(topo_liv2   = ifelse(is.na(topo_liv2_1), topo_liv2, topo_liv2_1),
         morph       = ifelse(is.na(morph_1), morph, morph_1),
         sex         = ifelse(is.na(sex_1), sex, sex_1),
         behaviour_2 = ifelse(is.na(behaviour_2_1), behaviour_2, behaviour_2_1),
         grade       = ifelse(is.na(grade_1), grade, grade_1)) %>%
  dplyr::select(-c(sex_1:grade_1)) %>% 
  # remove 11 patients with iacr_check errors from previous CR datasets
  anti_join(patient_to_remove, by = c("id_patient", "topo_liv2"))

# remove wrong morphology codes for 14 rows
full_dt_clean <- full_dt_corr %>% 
  mutate(morph = ifelse(id_patient %in% errors_icdo3_icd10$id_patient, 
                        NA_integer_, 
                        morph))

fix_dates$id_tumour <- as.double(fix_dates$id_tumour)

# dates corrections (77 row for 77 errors)
full_dt <- full_dt_clean %>%
  # fix wrong dates from previous CR datasets
  left_join(fix_dates, by = c("id_patient", "id_tumour", "sex")) %>%
  filter(is.na(comment)) %>%
  mutate(across(c("dob_date", "diag_date", "grade"), as.character)) %>% 
  mutate(dob_date  = ifelse(is.na(fixed_dob_date), dob_date, fixed_dob_date),
         diag_date = ifelse(is.na(fixed_diag_date), diag_date, fixed_diag_date)) %>%
  mutate(across(c("dob_date", "diag_date"), ymd)) %>% 
  dplyr::select(-fixed_dob_date, -fixed_diag_date, -comment) %>% 
  arrange(id, diag_date)

# remove all auxiliary variables and rename columns with diagnosis codes 
full_dt <- full_dt %>% 
  dplyr::select(-c(topo, topo_Cxxx, topo_x, behaviour, topo_liv, topo_liv1)) %>% 
  rename(topo = topo_liv2, behaviour = behaviour_2)

#--------------------------------Conversions------------------------------------
# takes time to load (35.8 Mb)
load(here("functions/conversion_tables.rda"))

gc()

data_ex <- full_dt %>%
  filter(
    (morph == 8005 & behaviour %in% c(0,3)) |
      (morph %in% c(8013:8015, 8035, 8046, 8049, 8078, 8084, 8097, 8098, 8131, 8172:8175,
                    8214, 8215, 8249, 8252, 8254, 8255, 8272, 8316:8319, 8335, 8337, 8341:8344,
                    8346, 8347, 8382:8384, 8413, 8482, 8508, 8513, 8514, 8523:8525, 8551,
                    8574:8576, 8586, 8588, 8591:8593, 8633, 8634, 8746, 8805, 8806, 8815, 8912,
                    8921, 8934:8936, 8959, 8973, 9065, 9105, 9186, 9187, 9193:9195, 9242, 9243,
                    9252, 9342, 9365, 9371, 9372, 9474, 9508, 9513, 9571, 9596, 9651, 9678, 9679, 9689,
                    9699, 9708, 9716:9719, 9727:9729, 9754:9758, 9805, 9833:9837, 9871:9876, 9895:9897,
                    9920, 9945, 9946, 9948, 9963, 9964, 9985:9987) & behaviour == 3) |
      (morph %in% c(8103, 8149, 8204, 8212, 8213, 8264, 8272, 8325, 8391, 8392, 8443, 8454, 8336,
                    8587, 8815, 8825, 8826, 8831, 8967, 8983, 8959, 8965, 8966, 8842, 8862, 8905,
                    8936, 9252, 9373, 9413, 9493, 9571, 9582) & behaviour == 0) |
      (morph %in% c(8148, 8507) & behaviour == 2) |
      (morph %in% c(8156, 8157, 8242, 8581:8585) & behaviour %in% c(1,3)) |
      (morph %in% c(8444, 8463, 8642, 8762, 8825, 8827, 8834, 8835, 8974, 8836, 8959, 8898, 8935,
                    8936, 9135, 9136, 9341, 9351, 9352, 9412, 9444, 9514, 9751, 9752, 9753, 9769,
                    9831) & behaviour == 1) |
      (morph == 8453 & behaviour %in% c(0:3)) |
      (morph == 8728 & behaviour %in% c(0,1,3)))

full_dt_filtered <- anti_join(full_dt, data_ex)

# ICD-10 -> ICD-O-2 conversion (doesn't provide type of warning)----------------------------------------------------

# if morph code is NA -- that's ICD-10

data_icdo2_ex <- data_ex %>% 
  left_join(icd10_to_icdo2_conversion, by = c("topo" = "icd10", "sex")) %>%
  # if wasn't converted, add error message
  mutate(status_icdo2 = ifelse(is.na(status_icdo2), "the code was not found in the conversion table", status_icdo2))

full_dt_1 <- full_dt_filtered %>% filter(is.na(morph)) %>%
  left_join(icd10_to_icdo2_conversion, by = c("topo" = "icd10", "sex")) %>%
  # if wasn't converted, add error message
  mutate(status_icdo2 = ifelse(is.na(status_icdo2), "the code was not found in the conversion table", status_icdo2))

icd10_icdo2_to_icdo2_conversion$grade = as.character(icd10_icdo2_to_icdo2_conversion$grade)

# if morph code is not NA -- that's ICD-10 + ICD-O-2
full_dt_2 <- full_dt_filtered %>% filter(!is.na(morph)) %>%
  left_join(icd10_icdo2_to_icdo2_conversion, by = c("sex", "topo", "morph", "behaviour", "grade")) %>%
  # if wasn't converted, add error message
  mutate(status_icdo2 = ifelse(is.na(status_icdo2), "the code was not found in the conversion table", status_icdo2))

data_icdo2 <- full_dt_1 %>% bind_rows(full_dt_2)

# ICD-O-2 -> ICD-O-3 conversion---------------------------------------------------------------------------------------

data_icdo3 <- data_icdo2 %>% 
  # ICD-O-2 to ICD-O-3 conversion, if wasn't converted, add error message
  left_join(icdo2_to_icdo3_conversion, by = c("sex", "icdo2")) %>% 
  mutate(status_icdo3 = ifelse(is.na(status_icdo3), "the code was not found in the conversion table", status_icdo3))

data_ex_icdo3 <- data_icdo2_ex %>% 
  left_join(icdo2_to_icdo3_conversion, by = c("sex", "icdo2")) %>% 
  mutate(status_icdo3 = ifelse(is.na(status_icdo3), "the code was not found in the conversion table", status_icdo3))

data_icdo3_all <- data_icdo3 %>% bind_rows(data_ex_icdo3)

# to use iarc_check windows desktop tool 
# data_exc_check$icdo3 <- paste(data_exc_check$icdo3_xxx, data_except$morph, sep = "")
# data_exc_check$icdo3 <- paste(data_exc_check$icdo3, data_except$behaviour, sep = "")
# data_exc_check$icdo3 <- paste(data_exc_check$icdo3, data_except$grade, sep = "")
# 
# data_exc_check <- data_exc_icdo3 %>% mutate(icdo3_xxx = str_sub(icdo3, 0, 4))
# write_csv2(data_exc_check, file = here("output/data_exc_check.csv"))


# ICD-O-3 -> ICD-10 conversion---------------------------------------------------------------------------------------

data_icd10 <- data_icdo3_all %>% 
  # ICD-O-3 to ICD-10 conversion, if wasn't converted, add error message
  left_join(icdo3_to_icd10_conversion, by = c("sex", "icdo3")) %>% 
  mutate(status_icd10 = ifelse(is.na(status_icd10), "the code was not found in the conversion table", status_icd10))

# remove heavy objects from environment
rm(icd10_icdo2_to_icdo2_conversion, icd10_to_icdo2_conversion, icdo2_to_icdo3_conversion, icdo3_to_icd10_conversion)

save(data_icd10, file = here("output/converted_diag_data.rda"))
