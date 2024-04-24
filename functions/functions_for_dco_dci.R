add_diag_basis <- function(.data) {
  .data %>% 
    mutate(
      conf1 = case_when(confirmation == 0                   ~ 9,
                        confirmation == 1                   ~ 7,
                        confirmation == 2                   ~ 5,
                        confirmation %in% c(3:6, 9, 12, 13) ~ 2,
                        confirmation %in% c(7, 8, 11)       ~ 1,
                        confirmation == 10                  ~ 4,
                        TRUE                                ~ 9)) %>%
    mutate(
      conf2 = case_when(confirmation == 0                   ~ "no_data",
                        confirmation == 1                   ~ "histology",
                        confirmation == 2                   ~ "cytology",
                        confirmation %in% c(7, 8, 11)       ~ "clinical",
                        confirmation %in% c(3:6, 9, 12, 13) ~ "clin_invest",
                        confirmation == 10                  ~ "biomarkers",
                        TRUE                                ~ "no_data")) %>%
    mutate(
      conf3 = case_when(confirmation %in% c(1, 2)           ~ "morphology",
                        TRUE                                ~ "not")) %>%
    mutate(
      survival_days = case_when(
        dead == 0 ~ difftime(last_cont_date, diag_date, units = "days"),
        dead == 1 ~ difftime(death_date, diag_date, units = "days")
        )
    ) %>%
    mutate(
      status2 = case_when(
        dead == 1 & survival_days == 0 ~ "zero",
        dead == 0 & survival_days == 0 ~ "norm",
        survival_days > 0              ~ "norm",
        dead == 1 & survival_days < 0  ~ "zero")
    ) %>% 
    mutate(
      dcn = case_when(
        reg_group == 5                     ~ "dci",
        reg_group %in% c(6, 7)             ~ "dco",
        is.na(reg_group) | reg_group == 0  ~ "no_data",
        TRUE                               ~ "alive")
    ) %>%
    mutate(
      basis = case_when(
        dead == 0                        ~ conf1,
        dead == 1 & status2 == "norm"    ~ conf1,
        dead == 1 & status2 == "zero" | status2 == "error" & 
          str_detect(dcn, "^dco$") & 
          is.na(morph) | morph %in% c(8800:8005, 8010, 9800, 9801, 9820, 9860, 9590, 9380) &
          is.na(t) & is.na(node) & is.na(m) ~ 0,
        TRUE ~ conf1)) 
}

add_diag_dcn <- function(.data) {
  .data %>% 
    mutate(
      clinnotif_check = case_when(
        !status2 == "norm" & is.na(t) & is.na(node) & is.na(m) ~ 0,
        TRUE ~ 1)
    ) %>% 
    mutate(
      morph_nos = case_when(
        morph %in% c(8010:8015, 8020:8022, 8050, 9590:9591, 
                     9596, 9727, 9760, 9800:9809, 9820, 9832, 
                     9835, 9860, 9960, 9965:9975, 9989,
                     8000:8005)                  ~ "NOS",
        is.na(morph)                             ~ "missing",
        TRUE                                     ~ "specific")
    ) %>%
    mutate(
      new_dcn = case_when(
        dead == 0 ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "alive"   ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "no_data" ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "dci"     ~ "dci",
        dead == 1 & status2 == "norm" & dcn == "dco"     ~ "dci",
        dead == 1 & status2 == "zero" & dcn == "alive" & 
          clinnotif_check == 1                             ~ "norm",
        dead == 1 & status2 == "zero" & dcn == "no_data" & 
          clinnotif_check == 1                             ~ "norm",
        dead == 1 & status2 == "zero" & dcn == "dci" & 
          clinnotif_check == 1                             ~ "dci",
        dead == 1 & status2 == "zero" & dcn == "dco" & 
          clinnotif_check == 1                             ~ "dci",
        dead == 1 & status2 == "zero" & dcn == "alive" & 
          morph_nos == "specific"                          ~ "norm",
        dead == 1 & status2 == "zero" & dcn == "no_data" & 
          morph_nos == "specific"                          ~ "norm",
        dead == 1 & status2 == "zero" & dcn == "dci" & 
          morph_nos == "specific"                          ~ "dci",
        dead == 1 & status2 == "zero" & dcn == "dco" & 
          morph_nos == "specific"                          ~ "dci",
        dead == 1 & status2 == "zero" & 
          clinnotif_check == 0 & morph_nos == "NOS"        ~ "dco",
        TRUE  ~ "more")
    ) %>%
    mutate(
      new_dcn2 = case_when(
        dead == 0 ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "alive"   ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "no_data" ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "dci"     ~ "dci",
        dead == 1 & status2 == "norm" & dcn == "dco"     ~ "dci",
        dead == 1 & status2 == "zero" & dcn == "alive" & 
          clinnotif_check == 1                           ~ "norm",
        dead == 1 & status2 == "zero" & dcn == "no_data" & 
          clinnotif_check == 1                           ~ "norm",
        dead == 1 & status2 == "zero" & dcn == "dci" & 
          clinnotif_check == 1                           ~ "dci",
        dead == 1 & status2 == "zero" & dcn == "dco" & 
          clinnotif_check == 1                           ~ "dci",
        dead == 1 & status2 == "zero" & 
          clinnotif_check == 0                           ~ "dco",
        TRUE  ~ "more")) %>%
    mutate(
      new_dc = case_when(
        new_dcn == "dci"  ~ "dci",
        new_dcn == "dco"  ~ "dci",
        new_dcn == "norm" ~ "norm")) %>%
    mutate(
      new_dc2 = case_when(
        new_dcn == "dci"  ~ "dci",
        new_dcn == "dco"  ~ "dci",
        new_dcn == "norm" ~ "norm"))
}

add_final_dco <- function(.data) {
  .data %>% 
    mutate(
      clin_notif = case_when(
        !status2 == "norm" & is.na(t) & is.na(node) & is.na(m) ~ 0,
        TRUE ~ 1)) %>% 
    mutate(
      mornos = case_when(
        morph >= 8000 & morph <= 8010  ~ "nos_tumor",
        is.na(morph)                   ~ "missing",
        TRUE                           ~ "ok")) %>%
    mutate(
      mornos2 = case_when(
        morph >= 8000 & morph <= 8010              ~ "nos_tumor",
        morph == 8070                              ~ "nos_squamo",
        morph == 8140                              ~ "nos_adeno",
        morph %in% c(9800, 9801, 9820, 9860, 9560) ~ "nos_haemo",
        morph == 9380                              ~ "nos_glioma",
        morph == 8800                              ~ "nos_sarcoma",
        is.na(morph)                               ~ "missing",
        TRUE                                       ~ "ok")) %>%
    mutate(
      path_notif = case_when(
        mornos == "ok" ~ 1,
        TRUE           ~ 0)) %>%
    mutate(
      registered2 = case_when(
        registered == "1"  ~ "during life, for the first time",
        registered == "2а" ~ "during life, repeatedly",
        registered == "2б" ~ "during life, repeatedly (arrived from another district)",
        registered == "2в" ~ "during life, repeatedly (arrived from another subject)",
        registered == "3"  ~ "posthumously, had not previously been registered",
        registered == "4"  ~ "posthumously, previously was on the record")) %>%
    mutate(
      final_dcn = case_when(
        dead == 0 ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "alive"     ~ "norm",
        dead == 1 & status2 == "norm" & dcn == "dci"       ~ "dci",
        dead == 1 & status2 == "norm" & dcn == "dco"       ~ "dci",
        dead == 1 & status2 == "norm" & dcn == "no_data" & 
          str_detect(registered2, "posthumously")           ~ "dci",
        dead == 1 & status2 == "norm" & dcn == "no_data" & 
          str_detect(registered2,"during life")            ~ "norm",
        dead == 1 & !status2 == "norm" & clin_notif == 0 & 
          path_notif == 0                                  ~ "dco",
        dead == 1 & !status2 == "norm" & dcn == "dco" & 
          !clin_notif == 0 | !path_notif == 0              ~ "dci",
        dead == 1 & !status2 == "norm" & dcn == "dci" & 
          !clin_notif == 0 | !path_notif == 0              ~ "dci",
        dead == 1 & !status2 == "norm" & dcn == "alive" & 
          !clin_notif == 0 | !path_notif == 0              ~ "norm",
        dead == 1 & !status2 == "norm" & dcn == "no_data" & 
          str_detect(registered2, "posthumously") & 
          !clin_notif == 0 | !path_notif == 0              ~ "dci",
        dead == 1 & !status2 == "norm" & dcn == "no_data" & 
          str_detect(registered2, "during life") & 
          !clin_notif == 0 | !path_notif == 0              ~ "norm",
        TRUE ~ "more")
    ) %>%
    # mutate(death_notif=case_when(str_detect(death_Xxxx, "C") ~ 1, TRUE ~ 0)) %>% 
    mutate(
      final_dco = case_when(final_dcn == "dco" ~ 1, 
                            TRUE               ~ 0)) %>%
    mutate(year = year(diag_date))
}

# add_diag_basis <- function(.data) {
#   .data %>% 
#     mutate(
#       conf1 = case_when(confirmation == 0                   ~ 9,
#                         confirmation == 1                   ~ 7,
#                         confirmation == 2                   ~ 5,
#                         confirmation %in% c(3:6, 9, 12, 13) ~ 2,
#                         confirmation %in% c(7, 8, 11)       ~ 1,
#                         confirmation == 10                  ~ 4,
#                         TRUE                                ~ 9)) %>%
#     mutate(
#       conf2 = case_when(confirmation == 0                   ~ "no_data",
#                         confirmation == 1                   ~ "histology",
#                         confirmation == 2                   ~ "cytology",
#                         confirmation %in% c(7, 8, 11)       ~ "clinical",
#                         confirmation %in% c(3:6, 9, 12, 13) ~ "clin_invest",
#                         confirmation == 10                  ~ "biomarkers",
#                         TRUE                                ~ "no_data")) %>%
#     mutate(
#       conf3 = case_when(confirmation %in% c(1, 2)           ~ "morphology",
#                         TRUE                                ~ "not")
#       ) %>%
#     mutate(survival_days = case_when(
#         dead == 0 ~ difftime(last_cont_date, 
#                              diag_date, 
#                              units = "days"),
#         dead == 1 ~ difftime(death_date, 
#                              diag_date, 
#                              units = "days"))
#         ) %>%
#     mutate(status = case_when(
#       dead == 1 & survival_days == 0 ~ "zero",
#       dead == 0 & survival_days == 0 ~ "norm",
#       survival_days > 0              ~ "norm",
#       dead == 1 & survival_days < 0  ~ "error")
#       ) %>% 
#     mutate(basis = case_when(
#       dead == 0                                       ~ conf1,
#       dead == 1 & status == "norm"                    ~ conf1,
#       dead == 1 & status == "zero" | 
#         status == "error" & 
#         str_detect(dcn, "^dco$") & 
#         is.na(morph) | 
#         morph %in% c(8800:8005, 8010, 9800, 
#                      9801, 9820, 9860, 9590, 9380) &
#           is.na(t) & is.na(node) & is.na(m)             ~ 0,
#         TRUE                                         ~ conf1)
#       )
#   }
# 
# add_morph_nos <- function(.data) {
#   .data %>% 
#   mutate(clinnotif2 = case_when(
#     !status == "norm" & 
#       is.na(t) & is.na(node) & is.na(m)  ~ 0,
#     TRUE                              ~ 1)
#     ) %>% 
#   mutate(morph_nos = case_when(
#       morph %in% 
#         c(8010:8015, 8020:8022, 8050, 
#           9590:9591, 9596, 9727, 9760,
#           9800:9809, 9820, 9832, 9835, 
#           9860, 9960, 9965:9975, 9989,
#           8000:8005)                  ~ "NOS",
#       is.na(morph)                    ~ "missing",
#       TRUE                            ~ "specific")
#       )
# }

