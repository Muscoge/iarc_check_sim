add_age_group <- function(.data) {
  .data %>% 
    mutate(age_group = case_when(
      between(age, 0, 4)   ~ "00-04",
      between(age, 5, 9)   ~ "05-09",
      between(age, 10, 14) ~ "10-14", 
      between(age, 15, 19) ~ "15-19",
      between(age, 20, 24) ~ "20-24",
      between(age, 25, 29) ~ "25-29",
      between(age, 30, 34) ~ "30-34",
      between(age, 35, 39) ~ "35-39",
      between(age, 40, 44) ~ "40-44",
      between(age, 45, 49) ~ "45-49",
      between(age, 50, 54) ~ "50-54",
      between(age, 55, 59) ~ "55-59",
      between(age, 60, 64) ~ "60-64",
      between(age, 65, 69) ~ "65-69",
      between(age, 70, 74) ~ "70-74",
      between(age, 75, 79) ~ "75-79",
      between(age, 80, 84) ~ "80-84",
      age >= 85            ~ "85+"))
}
      

