load(here("functions/data_for_fix_diag_convert_to_icdo2.rda"))

# the following functions manually fix errors in diagnosis codes
fix_diag_codes <- function(.data) {
  .data %>% 
    left_join(topo_1st_clean, by = "topo_Cxxx") %>% 
    mutate(
      topo_Cxxx = case_when(
        !is.na(topo_Cxxx_fixed)       ~ topo_Cxxx_fixed,
        str_detect(topo_Cxxx, "^C73") ~ "C73",
        str_detect(topo_Cxxx, "^C80") ~ "C80",
        TRUE                          ~ topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D500"), "D050", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D050") & morph == 8500, "D051", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D050") & morph == 8010, "D059", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D051") & morph == 8522, "D057", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D0.$"), str_c(topo_Cxxx, 0), topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D05.*") & 
          morph %in% c(8010, 8140, 8070 , 8050), "D059", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D05.*") & 
          morph %in% c(8522, 8501, 8502, 8503, 8504, 8541), "D057", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D04.*") & 
          morph %in% c(8720:8746), "D039", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D03.*") & 
          !morph %in% c(8720:8746), "D049", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D045") & morph == 8743, "C435", topo_Cxxx),
      topo_Cxxx = ifelse(
        str_detect(topo_Cxxx, "D05.*") & morph == 8540, "C500", topo_Cxxx),
      topo_Cxxx = ifelse(
        sex == 2 & topo_Cxxx %in% c("D074", "D075", "D076"), "D099", topo_Cxxx),
      topo_Cxxx = ifelse(
        sex == 1 & topo_Cxxx %in% c("D069", "D070", "D071", "D072", "D073"), 
        "D099", topo_Cxxx),
      
      morph = ifelse(
        str_detect(topo_Cxxx, "D05.*") & 
          morph %in% c(8260, 8000), NA_integer_, morph),
      morph = ifelse(
        str_detect(topo_Cxxx, "D.*") & morph == 8000, NA_integer_, morph),
      morph = ifelse(
        str_detect(topo_Cxxx, "D03.*|D04.*") & morph == 8210, NA_integer_, morph),
      
      behaviour = ifelse(
        str_detect(topo_Cxxx, "C435") & morph == 8743, 3, behaviour),
      behaviour = ifelse(str_detect(topo_Cxxx, "D05.*"), 2, behaviour),
      behaviour = ifelse(morph == 8540, 3, behaviour)
    ) %>% 
    dplyr::select(-topo_Cxxx_fixed)
  }

fix_behaviour_morph_codes <- function(.data) {
  .data %>% 
    mutate(
      behaviour_2 = case_when(topo %in% c(1110:1136)   ~ 0, 
                              topo %in% c(1137:1148)   ~ 1,
                              topo %in% c(1100:1109)   ~ 2,
                              topo %in% c(0:76, 80:97) ~ 3,
                              topo %in% c(77:79)       ~ 6,
                              TRUE                     ~ as.numeric(behaviour))
    ) %>% 
    left_join(morph_1st_clean, by = "morph") %>% 
    mutate(morph = ifelse(is.na(morph_fixed), morph, morph_fixed)) %>% 
    mutate(morph = ifelse(morph %in% c(8083, 8525, 8575), NA_integer_, morph)) %>% 
    dplyr::select(-morph_fixed)
  }

fix_liver_hemo_1 <- function(.data) {
  .data %>%
    mutate(
      morph = as.numeric(morph),
      morph = ifelse(str_detect(topo_Cxxx , "^C.*") & between(morph, 8000, 8005), 
                     NA_integer_, 
                     morph)
    ) %>%
    mutate(
      topo_liv = ifelse(
        str_detect(topo_Cxxx, "^C22"),
        case_when(between(morph, 8170, 8171)             ~ "C220",
                  between(morph, 8160, 8162)             ~ "C221",
                  morph == 8970                          ~ "C222",
                  morph == 9124                          ~ "C223",
                  morph == 8180                          ~ "C227",
                  between(morph, 8010, 8155)             ~ "C787",
                  between(morph, 8190, 8211)             ~ "C787",
                  between(morph, 8230, 8963)             ~ "C787",
                  morph %in% c(8980, 9070, 9100, 9120)   ~ "C787",
                  TRUE                                   ~ topo_Cxxx),
        topo_Cxxx)
    ) %>%
    mutate(
      topo_liv1 = case_when(
        topo_liv %in% str_c("C0", 000:809) ~ 
          case_when(morph == 9590                              ~ "C859",
                    morph %in% c(9800, 9801, 9803, 9820, 9860) ~ "C959",
                    morph == 9591                              ~ "C859",
                    morph == 9592                              ~ "C850",
                    morph == 9593                              ~ "C833",
                    morph == 9595                              ~ "C839",
                    morph == 9650                              ~ "C819",
                    TRUE                                       ~ topo_liv),
        topo_liv %in% c(str_c("C", 000:809), str_c("C", 860:969)) & 
          morph == 9590                                            ~ "C859",
        topo_liv %in% c(str_c("C", 000:902), str_c("C", 960:969)) & 
          morph %in% c(9800, 9801, 9803, 9820, 9860)                ~ "C959",
        topo_liv %in% c(str_c("C", 000:809), str_c("C", 858:969)) & 
          morph == 9591                                             ~ "C859",
        topo_liv %in% c(str_c("C", 000:809), str_c("C", 858:969)) & 
          morph == 9595                                             ~ "C839",
        topo_liv %in% c(str_c("C", 000:809), str_c("C", 820:969)) & 
          morph == 9650                                             ~ "C819",
        topo_liv %in% c(str_c("C", 000:819), str_c("C", 830:969)) & 
          morph == 9690                                             ~ "C829",
        topo_liv %in% str_c("C", 000:969) ~ 
          ifelse(morph %in% db_topoliv1$morph,
                 db_topoliv1$topo_new[match(morph, db_topoliv1$morph)],
                 topo_liv),
        TRUE ~ topo_liv)
    ) %>%
    mutate(
      morph = case_when(
        topo_liv1 %in% str_c("C", 810:969) & 
          between(morph, 8000, 9589)                       ~ NA_integer_,
        topo_liv1 %in% str_c("C", 810:859) & morph == 9590 ~ NA_integer_,
        topo_liv1 %in% str_c("C", 910:959) & 
          morph %in% c(9800, 9801, 9803, 9820, 9860)       ~ NA_integer_,
        topo_liv1 %in% str_c("C", 810:857) & 
          morph %in% c(9591, 9595)                         ~ NA_integer_,
        topo_liv1 %in% str_c("C", 810:819) & 
          morph %in% c(9650, 9660, 9661, 9662)             ~ NA_integer_,
        topo_liv1 %in% str_c("C", 820:827) & 
          morph == 9690                                    ~ NA_integer_,
        topo_liv1 %in% str_c("C", 810:969) & 
          morph == 9970                                    ~ NA_integer_,
        topo_liv1 %in% c("D471", "D479") & 
          morph %in% c(9960, 9961, 9970)                   ~ NA_integer_,
        TRUE                                               ~ as.integer(morph))
    )
  }  
  
fix_liver_hemo_2 <- function(.data) {
  .data %>% 
    mutate(
      topo_liv2 = case_when(
        str_detect(topo_liv1, "^C44") & morph == 9592 ~ "C850",
        str_detect(topo_liv1, "^C44") & morph == 9593 ~ "C833",
        str_detect(topo_liv1, "^C44") & morph == 9684 ~ "C834",
        str_detect(topo_liv1, "^C44") & morph == 9700 ~ "C840",
        str_detect(topo_liv1, "^C44") & morph == 9704 ~ "C845",
        str_detect(topo_liv1, "^C44") & morph == 9714 ~ "C851",
        str_detect(topo_liv1, "C911") & morph == 9826 ~ "C917",
        str_detect(topo_liv1, "C000") & morph == 9650 ~ "C819",
        str_detect(topo_liv1, "^D09") & morph == 8720 ~ "D038",
        str_detect(topo_liv1, "^C22") & morph == 8981 ~ "C787",
        str_detect(topo_liv1, "^C22") & morph == 8990 ~ "C787",
        str_detect(topo_liv1, "^C22") & morph == 8991 ~ "C787",
        str_detect(topo_liv1, "^C22") & morph == 9020 ~ "C787",
        str_detect(topo_liv1, "^C22") & morph == 9130 ~ "C787",
        str_detect(topo_liv1, "^C22") & morph == 9150 ~ "C787",
        str_detect(topo_liv1, "^C22") & morph == 9440 ~ "C787",
        str_detect(topo_liv1, "^C22") & morph == 9500 ~ "C787",
        str_detect(topo_liv1, "C80")  & morph == 9590 ~ "C859",
        str_detect(topo_liv1, "^C0")  & morph == 9670 ~ "C839",
        str_detect(topo_liv1, "^C85") & morph == 9676 ~ "C832",
        str_detect(topo_liv1, "^C82") & morph == 9676 ~ "C832",
        str_detect(topo_liv1, "^C81") & morph == 9676 ~ "C832",
        str_detect(topo_liv1, "^C0")  & morph == 9676 ~ "C835",
        str_detect(topo_liv1, "^C00") & morph == 9686 ~ "C830",
        str_detect(topo_liv1, "^C0")  & morph == 9687 ~ "C837",
        str_detect(topo_liv1, "^C0")  & morph == 9663 ~ "C811",
        str_detect(topo_liv1, "C839") & morph == 9594 ~ "C859",
        str_detect(topo_liv1, "^C0")  & morph == 9685 ~ "C835",
        str_detect(topo_liv1, "C950") & morph == 9800 ~ "C959",
        str_detect(topo_liv1, "C947") & morph == 9870 ~ "C959",
        str_detect(topo_liv1, "C959") & morph == 9900 ~ "C943",
        str_detect(topo_liv1, "^C0")  & morph == 9690 ~ "C829",
        str_detect(topo_liv1, "^C0")  & morph == 9696 ~ "C829",
        str_detect(topo_liv1, "^C0")  & morph == 9704 ~ "C843",
        str_detect(topo_liv1, "C80")  & morph == 9705 ~ "C844",
        str_detect(topo_liv1, "^C0")  & morph == 9711 ~ "C851",
        str_detect(topo_liv1, "^C8")  & morph == 9722 ~ "C960",
        str_detect(topo_liv1, "^C0")  & morph == 9731 ~ "C902",
        str_detect(topo_liv1, "^C0")  & morph == 9732 ~ "C900",
        str_detect(topo_liv1, "^C81") & morph == 9762 ~ "C881",
        str_detect(topo_liv1, "^C88") & morph == 9764 ~ "C883",
        str_detect(topo_liv1, "^C8")  & morph == 9800 ~ "C950",
        str_detect(topo_liv1, "^C3")  & morph == 9802 ~ "C952",
        str_detect(topo_liv1, "C919") & morph == 9826 ~ "C910",
        str_detect(topo_liv1, "^C9")  & morph == 9841 ~ "C940",
        str_detect(topo_liv1, "^C95") & morph == 9894 ~ "C962",
        str_detect(topo_liv1, "^C93") & morph == 9941 ~ "C914",
        str_detect(topo_liv1, "^C9")  & morph == 9962 ~ "D473",
        str_detect(topo_liv1, "C929") & morph == 9984 ~ "C920",
        str_detect(topo_liv1, "C098") & morph == 9670 ~ "C830",
        str_detect(topo_liv1, "C019") & morph == 9684 ~ "C834",
        topo_liv1 %in% str_c("C", 480:495) & morph == 9592 ~ "C850",
        topo_liv1 %in% str_c("C", 480:495) & morph == 9731 ~ "C902",
        topo_liv1 %in% str_c("C", 480:495) & morph == 9684 ~ "C834",
        TRUE ~ topo_liv1)
    ) %>%
    mutate(
      morph = case_when(
        str_detect(topo_liv2, "C911")  & morph == 9804 ~ NA_integer_,
        str_detect(topo_liv2, "C504")  & morph == 9741 ~ NA_integer_,
        str_detect(topo_liv2, "^C43")  & 
          !morph %in% c(8720:8790)                     ~ NA_integer_,
        str_detect(topo_liv2, "^C45")  & 
          !morph %in% 9050:9055                        ~ NA_integer_,
        str_detect(topo_liv2, "^C46")  & morph != 9140 ~ NA_integer_,
        str_detect(topo_liv2, "^D06")  & morph == 8001 ~ 8010L,
        str_detect(topo_liv2, "^D051") & morph == 8001 ~ 8500L,
        str_detect(topo_liv2, "^D051") & morph == 8230 ~ 8500L,
        str_detect(topo_liv2, "^D051") & morph == 8201 ~ 8500L,
        str_detect(topo_liv2, "C839")  & morph == 8230 ~ 9590L,
        str_detect(topo_liv2, "^C950") & morph == 9767 ~ 9801L,
        str_detect(topo_liv2, "C56")   & morph == 9981 ~ NA_integer_,
        str_detect(topo_liv2, "^C16")  & morph == 8620 ~ 8260L,
        str_detect(topo_liv2, "^C71")  & morph == 8620 ~ NA_integer_,
        str_detect(topo_liv2, "^C62")  & morph == 8620 ~ NA_integer_,
        str_detect(topo_liv2, "D473")  & morph == 9962 ~ NA_integer_,
        str_detect(topo_liv2, "C947")  & morph == 9894 ~ NA_integer_,
        str_detect(topo_liv2, "C929")  & morph == 9894 ~ NA_integer_,
        str_detect(topo_liv2, "C959")  & morph == 9870 ~ NA_integer_,
        !str_detect(topo_liv2, "C942") & morph == 9910 ~ NA_integer_,
        !str_detect(topo_liv2, "D45")  & morph == 9950 ~ NA_integer_,
        TRUE ~ as.integer(morph))
    ) %>%
    mutate(
      behaviour_2 = ifelse(topo_liv2 %in% str_c("C", 770:798), 6, behaviour_2),
      behaviour_2 = ifelse(morph == 9592, 3, behaviour_2),
      behaviour_2 = ifelse(str_detect(topo_liv2, "C835"), 3, behaviour_2),
      behaviour_2 = ifelse(str_detect(topo_liv2, "D45"), 1, behaviour_2),
      behaviour_2 = ifelse(
        str_detect(topo_liv2, "C920") & morph == 9984, 1, behaviour_2)
    ) %>%
    mutate(
      topo_liv2 = case_when(
        topo_liv2 %in% c("C968", "C965", "C966", "C967", 
                         "C96", "C969", "C946")                ~ "D479",
        topo_liv2 == "C920" & morph == 9984 & behaviour_2 == 1 ~ "D463",
        TRUE ~ topo_liv2)
    ) %>%
    mutate(behaviour_2 = ifelse(is.na(topo_liv2), 3, behaviour_2)) %>%
    mutate(morph = ifelse(
      topo_liv2 %in% c("D471", "D479"), NA_integer_, morph)
    ) %>%
    mutate(
      topo_liv2 = ifelse(
        is.na(topo_liv2), 
        case_when(
          morph %in% c(8010, 8050, 8052, 8140, 8330, 8340, 8510) ~ "C80",
          morph == 9690                                          ~ "C829",
          morph == 9590                                          ~ "C859",
          morph == 9800                                          ~ "C959",
          morph == 9705                                          ~ "C844",
          TRUE ~ topo_liv2),
        topo_liv2)
    ) %>%
    mutate(topo_liv2 = ifelse(is.na(topo_liv2), "C80", topo_liv2)) %>%
    mutate(
      sex = case_when(str_detect(topo_liv2, "^C53") ~ 2,
                      str_detect(topo_liv2, "^C52") ~ 2,
                      str_detect(topo_liv2, "^C54") ~ 2,
                      str_detect(topo_liv2, "^C55") ~ 2,
                      str_detect(topo_liv2, "^C56") ~ 2,
                      str_detect(topo_liv2, "^C57") ~ 2,
                      str_detect(topo_liv2, "^C61") ~ 1,
                      str_detect(topo_liv2, "^C62") ~ 1,
                      str_detect(topo_liv2, "^C63") ~ 1,
                      str_detect(topo_liv2, "^D06") ~ 2,
                      TRUE ~ sex)
    )
}

# fix_morph_na <- function(.data) {
#   .data %>% 
#     mutate(
#       morph_1 = case_when(topo == 'C220' & is.na(morph) ~  8170,
#                           topo == 'C221' & is.na(morph) ~  8160,
#                           topo == 'C43' & is.na(morph)  ~  8720,
#                           topo == 'C46' & is.na(morph)  ~  9140,
#                           topo == 'C45' & is.na(morph)  ~  9050,
#                           topo %in% c('C81':'C85', 'C88', 'C90', 'C96') 
#                           & is.na(morph)              ~  9090,
#                           topo == 'C91':'C95' & is.na(morph) ~  9800,
#                           topo == 'C00':'C80' & is.na(morph) ~  8000))}