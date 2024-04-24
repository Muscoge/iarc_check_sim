add_exclude_status <- function(.data) {
  .data %>%
    mutate(
      exclude = case_when(
        behav_icdo3 == "3"                                                  ~ "keep",
        topo_Cxx %in% c("C70", "C71", "C72") & behav_icdo3 %in% c("0", "1") ~ "keep",
        topo_Cxx == "C67" & behav_icdo3 %in% c("1", "2")                    ~ "keep",
        TRUE ~ "exclude")
    )
}

# match a group of morphology from rule 4.2 Table 2
add_morph_groups <- function(.data) {
  .data %>%
    mutate(
      morph_group = case_when(
        morph %in% c(8051:8084, 8120:8131)                        ~ 1,
        morph %in% c(8090:8110)                                   ~ 2,
        morph %in% c(8140:8149, 8160:8162, 8190:8221, 8260:8337, 
                     8350:8551, 8570:8576, 8940:8941)             ~ 3,
        morph %in% c(8030:8046, 8150:8157, 8170:8180, 8230:8255,
                     8340:8347, 8560:8562, 8580:8671)             ~ 4,
        morph %in% c(8010:8015, 8020:8022, 8050)                  ~ 5,
        morph %in% c(8680:8713, 8800:8921, 8990:8991, 9040:9044,
                     9120:9125, 9130:9136, 9141:9252, 9370:9373, 
                     9540:9582)                                   ~ 6,
        morph %in% c(9050:9055)                                   ~ 7,
        morph %in% c(9840, 9861:9931, 9945:9946, 9950, 
                     9961:9964, 9980:9987)                        ~ 8,
        morph %in% c(9670:9699, 9728, 9731:9734, 9761:9767, 
                     9769, 9823:9826, 9833, 9836, 9940)           ~ 9,
        morph %in% c(9700:9719, 9729, 9768, 9827:9831, 9834, 
                     9837, 9948)                                  ~ 10,
        morph %in% c(9650:9667)                                   ~ 11,
        morph %in% c(9740:9742)                                   ~ 12,
        morph %in% c(9750:9758)                                   ~ 13,
        morph %in% c(9590:9591, 9596, 9727, 9760, 9800:9801, 
                     9805, 9820, 9832, 9835, 9860, 9960, 9970, 
                     9975, 9989)                                  ~ 14,
        morph %in% c(9140)                                        ~ 15,
        morph %in% c(8720:8790, 8930:8936, 8950:8983,9000:9030, 
                     9060:9110, 9260:9365, 9380:9539)             ~ 16,
        morph %in% c(8000:8005)                                   ~ 17,
        TRUE ~ morph)
    )
  }

add_topo_groups <- function(.data) {.data %>%
    mutate(
      topo_group = case_when(
        str_detect(topo_Cxx, "(C01|C02)")             ~ "C02",
        str_detect(topo_Cxx, "(C00|C03|C04|C05|C06)") ~ "C06",
        str_detect(topo_Cxx, "(C09|C10|C12|C13|C14)") ~ "C14",
        str_detect(topo_Cxx, "(C19|C20)")             ~ "C20",
        str_detect(topo_Cxx, "(C23|C24)")             ~ "C24",
        str_detect(topo_Cxx, "(C33|C34)")             ~ "C34",
        str_detect(topo_Cxx, "(C40|C41)")             ~ "C41",
        str_detect(topo_Cxx, "(C65|C66|C67|C68)")     ~ "C68",
        TRUE                                          ~ topo_Cxx)
      )
  }

add_solid_status <- function(.data) {
  .data %>%
    mutate(
      solid_status = ifelse(
        between(morph, 9590, 9992), "leukemia and lymphoid", "solid")
    )
  }