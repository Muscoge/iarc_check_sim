# if(!require('pacman')) install.packages('pacman')
pacman::p_load(here, tidyverse, lubridate)

# add functions to read and rename the data (translate to English)
source(here("functions/functions_for_read_and_name_the_data.R"))

# add functions to read and rename the data (translate to English)
load(here("functions/data_for_read_and_name_the_data.rda"))

# set path to the folder with actual CR data, rename regions
path = "Disk:/path_to_the_folder"

df <- list.files(path) %>%
  set_names(str_remove(list.files(path, pattern = ".*csv"), "(_\\d{2}.csv)|.csv")) %>%
  map_dfr(read_delim, .id = "region", delim = ";",
          locale = locale(encoding = "cp1251"),
          trim_ws = TRUE,
          col_types = cols(`ФАМИЛИЯ` = col_character(),
                           `СУБЪЕКТ РФ` = col_character())) %>%
  mutate(
    region = case_when(region == "Архангельская область+НАО" ~ "arkhangelskandnao",
                       region == "Вологодская область"       ~ "vologda",
                       region == "Калининградская область"   ~ "kaliningrad",
                       region == "Ленинградская область"     ~ "lo",
                       region == "Мурманская область"        ~ "murmansk",
                       region == "Новгородская область"      ~ "novgorod",
                       region == "Псковская область"         ~ "pskov",
                       region == "Республика Карелия"        ~ "karelia",
                       region == "Республика Коми"           ~ "komi",
                       region == "СПб"                       ~ "saint-petersburg"))

dput(unique(df$region))

# check variables values in your data carefully!
data_eng <- df %>%
  rename_with(.cols = any_of(col_names$rus), ~ ru_to_en(.x, col_names)) %>% 
  mutate(sex = case_when(sex == "МУЖЧИНА" | sex == "М" ~ 1,
                       sex == "ЖЕНЩИНА" | sex == "Ж" ~ 2,
                       is.na(sex) ~ 9)) %>%
  mutate(across(contains("date"), ~ dmy(.x, quiet = TRUE))) %>%
  mutate(initial_topo = topo, 
         initial_morph = morph) %>%
  separate(morph, into = c("morph", "behaviour"), convert = TRUE, fill = "right") %>%
  separate(topo, into = c('topo_Cxxx', 'topo_x'), sep = 4, fill = "right", remove = FALSE) %>%
  mutate(morph = ifelse(morph == 0, NA_integer_, morph),
         r_id = row_number() + 1000000, # id_tumour id
         topo = str_sub(topo, start = 1, end = 3),
         topo = str_remove(topo, "C0|C"),
         topo = str_replace(topo, "D0", "11"),
         dead = ifelse(is.na(death_date), 0, 1),
         behaviour = ifelse(is.na(behaviour), 3, behaviour),
         grade = case_when(is.na(grade) ~ "9",
                           grade == "0" ~ "9",
                           TRUE         ~ as.character(grade)),
         t = ifelse(t == "X", NA_character_, t),
         n = case_when(n %in% c("X", "4") ~ NA_character_, 
                       TRUE               ~ n),
         m = ifelse(m == "X", NA, m),
         stage = case_when(str_detect(stage, "^0") ~ "0",
                           str_detect(stage, "^1") ~ "1",
                           str_detect(stage, "^2") ~ "2",
                           str_detect(stage, "^3") ~ "3",
                           str_detect(stage, "^4") ~ "4",
                           TRUE                    ~ NA_character_),
         age = as.numeric(floor(difftime(diag_date, dob_date, units = "days") / 365.25)),
  ) %>% 
  # filter the only malignant behaviour
  filter(!behaviour %in% c(0, 1, 2)) %>%
  mutate(id = match(id_patient, unique(id_patient))) %>%
  rename(node = n)

save(data_eng, file = here("output/initial_data.rda"))