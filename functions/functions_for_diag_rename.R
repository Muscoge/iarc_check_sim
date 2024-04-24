# adds topo -- localization pairs according to icd10

# according to NORDCAN
add_diag_name <- function(.data) { .data %>%
    mutate(
      localization_1 = case_when(
        icd10_letter == "C" & (icd10_xx %in% c("00","01","02","03","04","05",
                                               "06","07","08","09",11,12,13,14) & 
                                 icd10_x %in% c(0:9) |
             icd10_xx == "10" & icd10_x %in% c(0,2:9))                                  ~ "Lip, oral cavity and pharynx",
        icd10_letter == "C" & ((icd10_xx == "10" & icd10_x == 1) | icd10_xx == "32")    ~ "Larynx",
        icd10_letter == "C" & icd10_xx == 11                                            ~ "Nasopharynx",
        icd10_letter == "C" & icd10_xx %in% c(12:13)                                    ~ "Hypopharynx",
        icd10_letter == "C" & icd10_xx == 15                                            ~ "Oesophagus",
        icd10_letter == "C" & icd10_xx == 16                                            ~ "Stomach",
        icd10_letter == "C" & icd10_xx == 17                                            ~ "Small intestine",
        icd10_letter == "C" & icd10_xx == 18                                            ~ "Colon",
        icd10_letter == "C" & icd10_xx %in% c(19,20)                                    ~ "Rectum",
        icd10_letter == "C" & icd10_xx == 21                                            ~ "Anus and anal canal",
        icd10_letter == "C" & icd10_xx == 22                                            ~ "Liver",
        icd10_letter == "C" & icd10_xx %in% c(23,24)                                    ~ "Gallbladder",
        icd10_letter == "C" & icd10_xx == 25                                            ~ "Pancreas",
        icd10_letter == "C" & (icd10_xx == 26 | 
                                 icd10_xx == 39 | 
                                 icd10_xx %in% c(76:80) | 
                                 icd10_xx == 97)                                        ~ "Other and ill:defined cancers",
        icd10_letter == "C" & icd10_xx %in% c(30,31)                                    ~ "Nasal Cavity, middle ear and sinuses",
        icd10_letter == "C" & icd10_xx %in% c(33,34)                                    ~ "Lung",
        icd10_letter == "C" & 
          ((icd10_xx == "38" & icd10_x == 4) | 
             (icd10_xx == "45" & icd10_x %in% c(0,9)))                                  ~ "Pleura",
        icd10_letter == "C" & icd10_xx %in% c(40,41)                                    ~ "Bone",
        icd10_letter == "C" & icd10_xx == 43                                            ~ "Melanoma of skin",
        icd10_letter == "C" & icd10_xx == 44                                            ~ "Skin, non:melanoma",
        icd10_letter == "C" & icd10_xx == 49                                            ~ "Soft tissues",
        icd10_letter == "C" & icd10_xx == 50                                            ~ "Breast",
        icd10_letter == "C" & icd10_xx == 51                                            ~ "Vulva",
        icd10_letter == "C" & icd10_xx == 52                                            ~ "Vagina",
        icd10_letter == "C" & icd10_xx == 53                                            ~ "Cervix uteri",
        icd10_letter == "C" & icd10_xx == 54                                            ~ "Corpus uteri",
        icd10_letter == "C" & icd10_xx %in% c(55,58)                                    ~ "Uterus, other",
        icd10_letter == "C" & (icd10_xx == 56 | 
             (icd10_xx == 57 & icd10_x %in% c(0:4)))                                    ~ "Ovary and tubes",
        icd10_letter == "C" & icd10_xx == 57 & icd10_x %in% c(7:9)                      ~ "Other female genital organs",
        icd10_letter == "C" & icd10_xx %in% c(60,63)                                    ~ "Penis and other male genital organs",
        icd10_letter == "C" & icd10_xx == 61                                            ~ "Prostate",
        icd10_letter == "C" & icd10_xx == 62                                            ~ "Testis",
        icd10_letter == "C" & icd10_xx == 64                                            ~ "Kidney",
        (icd10_letter == "C" & icd10_xx %in% c(65:68)) |
          (icd10_letter == "D" & ((icd10_xx == "09" & icd10_x %in% c(0:1)) | 
                                    (icd10_xx == 30 & icd10_x %in% c(1:9)) | 
                                    (icd10_xx == 41 & icd10_x %in% c(1:9))))            ~ "Bladder and urinary tract",
        icd10_letter == "C" & icd10_xx == 69                                            ~ "Eye",
        (icd10_letter == "C" & icd10_xx %in% c(70:72)) |
          (icd10_letter == "D" & (icd10_xx %in% c(32,33) | 
                                    (icd10_xx %in% c(42,43))))                          ~ "Brain and CNS excluding endocrine tumors",
        icd10_letter == "C" & icd10_xx == 73                                            ~ "Thyroid",
        (icd10_letter == "C" & icd10_xx == 75 & icd10_x %in% c(1:3)) |
          (icd10_letter == "D" & ((icd10_xx == 35 & icd10_x %in% c(2:4)) | 
                                    (icd10_xx == 44 & icd10_x %in% c(3:5))))            ~ "Endocrine tumors of brain and CNS",
        icd10_letter == "C" & icd10_xx == 81                                            ~ "Hodgkin lymphomas",
        icd10_letter == "C" & icd10_xx %in% c(82:86,96)                                 ~ "Non Hodgkin lymphomas",
        (icd10_letter == "C" & (icd10_xx == 88 | (icd10_xx == 94 & icd10_x == 6))) | 
          (icd10_letter == "D" & (icd10_xx == 47 & icd10_x %in% c(0,2,7:9)))            ~ "Other hematopoietic diseases",
        icd10_letter == "C" & icd10_xx == 90                                            ~ "Multiple myelomas",
        icd10_letter == "C" & icd10_xx == 91 & icd10_x == 0                             ~ "Acute lymphatic leukaemias",
        icd10_letter == "C" & icd10_xx == 91 & icd10_x == 1                             ~ "Chronic lymphatic leukaemias",
        icd10_letter == "C" & icd10_xx == 91 & icd10_x %in% c(2:9)                      ~ "Other and unspecified lymphatic leukaemias",
        icd10_letter == "C" & 
          ((icd10_xx == 92 & icd10_x %in% c(0,3:6)) | 
             (icd10_xx %in% c(93,94) & icd10_x == 0) | 
             (icd10_xx == 94 & icd10_x == 2))                                           ~ "Acute myeloid leukaemias",
        icd10_letter == "C" & icd10_xx %in% c(92:94) & icd10_x == 1                     ~ "Chronic myeloid leukaemias",
        icd10_letter == "C" & 
          ((icd10_xx == 92 & icd10_x %in% c(2,7:9)) | 
             (icd10_xx == 93 & icd10_x %in% c(2:9)) | 
             (icd10_xx == 94 & icd10_x %in% c(3:4,7)))                                ~ "Other and unspecified myeloid leukaemia",
        icd10_letter == "C" & icd10_xx == 95 & icd10_x %in% c(0:9)                    ~ "Leukaemia, cell unspecified",
        icd10_letter == "C" & 
          (icd10_xx == 37 | 
             (icd10_xx == 38 & icd10_x %in% c(0:3,8)) | 
             (icd10_xx == 45 & icd10_x %in% c(1,2,7)) | 
             icd10_xx %in% c(46:48) | 
             icd10_xx == 74 | 
             (icd10_xx == 75 & icd10_x %in% c(0,4:9)))                                ~ "Other specified cancers",
        icd10_letter == "D" & icd10_xx == 46                                          ~ "Myelodysplastic syndromes")) %>%
    mutate(
      localization_2 = case_when(
        (icd10_letter == "C" & 
           icd10_xx %in% c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", c(10:99))) |
          (icd10_letter == "D" & 
             ((icd10_xx == "09" & icd10_x %in% c(0:1)) | (icd10_xx == 30 & icd10_x %in% c(1:9)) | 
                icd10_xx %in% c(32:33) | (icd10_xx == 35 & icd10_x %in% c(2:4)) | icd10_xx == 41 | 
                icd10_xx %in% c(42:43) | (icd10_xx == 44 & icd10_x %in% c(3:5)) | icd10_xx %in% c(45:47))) ~ "All sites")) %>%
    mutate(
      localization_3 = case_when(
        (icd10_letter == "C" & icd10_xx %in% c("00", "01", "02", "03", "04", "05", "06", "07", 
                                               "08", "09", c(10:43,45:99))) |
          (icd10_letter == "D" & ((icd10_xx == "09" & icd10_x %in% c(0:1)) | 
                                    (icd10_xx == 30 & icd10_x %in% c(1:9)) |
                                    icd10_xx %in% c(32:33) | (icd10_xx == 35 & icd10_x %in% c(2:4)) | 
                                    icd10_xx == 41 | icd10_xx %in% c(42:43) | 
                                    (icd10_xx == 44 & icd10_x %in% c(3:5)) | icd10_xx %in% c(45:47))) ~ "All sites but non:melanoma skin cancer")) %>%
  mutate(
    localization_4 = case_when(
      (icd10_letter == "C" & icd10_xx %in% c("00", "01", "02", "03", "04", "05", "06", "07", 
                                             "08", "09", c(10:43, 45:49, 51:60, 62:99))) |
        (icd10_letter == "D" & ((icd10_xx == "09" & icd10_x %in% c(0:1)) | 
                                  (icd10_xx == 30 & icd10_x %in% c(1:9)) | 
                                  icd10_xx %in% c(32:33) | (icd10_xx == 35 & icd10_x %in% c(2:4)) | 
                                  icd10_xx == 41 | icd10_xx %in% c(42:43) | 
                                  (icd10_xx == 44 & icd10_x %in% c(3:5)) | icd10_xx %in% c(45:47))) ~ "All sites but non:melanoma skin cancer, breast and prostate")) %>%
    mutate(
      localization_5 = case_when(
        icd10_letter == "C" & icd10_xx == "00" & icd10_x %in% as.character(c(0:2,6,8:9)) ~ "Lip",
        icd10_letter == "C" & ((icd10_xx == "00" & icd10_x %in% as.character(c(3:5))) | 
                                 icd10_xx %in% c("02", "03", "04") | 
                                 (icd10_xx == "05" & icd10_x %in% c(0,8:9)) | 
                                 icd10_xx == "06")                                       ~ "Oral Cavity",
        icd10_letter == "C" & (icd10_xx == "01" | 
                                 (icd10_xx == "05" & icd10_x %in% c(1:2)) | 
                                 icd10_xx == "09" | 
                                 (icd10_xx == "10" & icd10_x %in% c(0,2:9)) | 
                                 icd10_xx == 14 & icd10_x %in% c(0,2:8))                 ~ "Oropharynx",
        icd10_letter == "C" & icd10_xx %in% c("07", "08")                                ~ "Salivary glands",
        icd10_letter == "C" & icd10_xx %in% c("18","19","20")                            ~ "Colorectal",
        icd10_letter == "C" & icd10_xx %in% c("91","92","93","94","95")                  ~ "Leukaemias",
        icd10_letter == "C" & (icd10_xx %in% c("70","71","72") |
                                 (icd10_xx == "75" & icd10_x %in% c(1:3)))               ~ "Brain and CNS including endocrine tumors",
        icd10_letter == "C" & (icd10_xx %in% c("51","52") | 
             (icd10_xx == "57" & icd10_x %in% c(7:9)))                                   ~ "Vulva, vagina and other female genital organs")) %>%
    mutate(
      localization_6 = case_when(
        (icd10_letter == "C" & icd10_xx %in% c(81:96)) | (icd10_letter == "D" & icd10_xx %in% c(45:47))  ~ "Malignant haematopoietic diseases",
        icd10_letter == "D" & (icd10_xx == 45 | (icd10_xx == 47 & icd10_x %in% c(1, 3:5))) ~ "Vulva, vagina and other female genital organs"))}

# to compare with Kaprin's data
# add_diag_name <- function(.data) {
#   .data %>%
#     mutate(localization = case_when(
#       icd10_xx == "C00" & icd10_x %in% c(0:9) ~ "C00",
#       icd10_xx == "C01" & icd10_x %in% c(0:9) ~ "C01,02",
#       icd10_xx == "C02" & icd10_x %in% c(0:9) ~ "C01,02",
#       icd10_xx == "C03" & icd10_x %in% c(0:9) ~ "C03-06,09",
#       icd10_xx == "C04" & icd10_x %in% c(0:9) ~ "C03-06,09",
#       icd10_xx == "C05" & icd10_x %in% c(0:9) ~ "C03-06,09",
#       icd10_xx == "C06" & icd10_x %in% c(0:9) ~ "C03-06,09",
#       icd10_xx == "C09" & icd10_x %in% c(0:9) ~ "C03-06,09",
#       icd10_xx == "C07" & icd10_x %in% c(0:9) ~ "C07,08",
#       icd10_xx == "C08" & icd10_x %in% c(0:9) ~ "C07,08",
#       icd10_xx == "C10" & icd10_x %in% c(0:9) ~ "C10",
#       icd10_xx == "C11" & icd10_x %in% c(0:9) ~ "C11",
#       icd10_xx == "C12" & icd10_x %in% c(0:9) ~ "C12,13",
#       icd10_xx == "C13" & icd10_x %in% c(0:9) ~ "C12,13",
#       icd10_xx == "C15" & icd10_x %in% c(0:9) ~ "C15",
#       icd10_xx == "C16" & icd10_x %in% c(0:9) ~ "C16",
#       icd10_xx == "C17" & icd10_x %in% c(0:9) ~ "C17",
#       icd10_xx == "C18" & icd10_x %in% c(0:9) ~ "C18",
#       icd10_xx == "C19" & icd10_x %in% c(0:9) ~ "C19-21",
#       icd10_xx == "C20" & icd10_x %in% c(0:9) ~ "C19-21",
#       icd10_xx == "C21" & icd10_x %in% c(0:9) ~ "C19-21",
#       icd10_xx == "C22" & icd10_x %in% c(0:9) ~ "C22",
#       icd10_xx == "C23" & icd10_x %in% c(0:9) ~ "C23,24",
#       icd10_xx == "C24" & icd10_x %in% c(0:9) ~ "C23,24",
#       icd10_xx == "C25" & icd10_x %in% c(0:9) ~ "C25",
#       icd10_xx == "C30" & icd10_x %in% c(0:9) ~ "C30,31",
#       icd10_xx == "C31" & icd10_x %in% c(0:9) ~ "C30,31",
#       icd10_xx == "C32" & icd10_x %in% c(0:9) ~ "C32",
#       icd10_xx == "C33" & icd10_x %in% c(0:9) ~ "C33,34",
#       icd10_xx == "C34" & icd10_x %in% c(0:9) ~ "C33,34",
#       icd10_xx == "C40" & icd10_x %in% c(0:9) ~ "C40,41",
#       icd10_xx == "C41" & icd10_x %in% c(0:9) ~ "C40,41",
#       icd10_xx == "C43" & icd10_x %in% c(0:9) ~ "C43",
#       icd10_xx == "C44" & icd10_x %in% c(0:9) ~ "C44",
#       icd10_xx == "C47" & icd10_x %in% c(0:9) ~ "C47,49",
#       icd10_xx == "C49" & icd10_x %in% c(0:9) ~ "C47,49",
#       icd10_xx == "C50" & icd10_x %in% c(0:9) ~ "C50",
#       icd10_xx == "C51" & icd10_x %in% c(0:9) ~ "C51",
#       icd10_xx == "C52" & icd10_x %in% c(0:9) ~ "C52",
#       icd10_xx == "C53" & icd10_x %in% c(0:9) ~ "C53",
#       icd10_xx == "C54" & icd10_x %in% c(0:9) ~ "C54,55",
#       icd10_xx == "C55" & icd10_x %in% c(0:9) ~ "C54,55",
#       icd10_xx == "C56" & icd10_x %in% c(0:9) ~ "C56",
#       icd10_xx == "C58" & icd10_x %in% c(0:9) ~ "C58",
#       icd10_xx == "C60" & icd10_x %in% c(0:9) ~ "C60",
#       icd10_xx == "C61" & icd10_x %in% c(0:9) ~ "C61",
#       icd10_xx == "C62" & icd10_x %in% c(0:9) ~ "C62",
#       icd10_xx == "C64" & icd10_x %in% c(0:9) ~ "C64",
#       icd10_xx == "C67" & icd10_x %in% c(0:9) ~ "C67",
#       icd10_xx == "C69" & icd10_x %in% c(0:9) ~ "C69",
#       icd10_xx == "C70" & icd10_x %in% c(0:9) ~ "C70-72",
#       icd10_xx == "C71" & icd10_x %in% c(0:9) ~ "C70-72",
#       icd10_xx == "C72" & icd10_x %in% c(0:9) ~ "C70-72",
#       icd10_xx == "C71" & icd10_x %in% c(0:9) ~ "C71,72",
#       icd10_xx == "C72" & icd10_x %in% c(0:9) ~ "C71,72",
#       icd10_xx == "C73" & icd10_x %in% c(0:9) ~ "C73",
#       icd10_xx == "C81" & icd10_x %in% c(0:9) ~ "C81",
#       icd10_xx == "C82" & icd10_x %in% c(0:9) ~ "C82-86,96",
#       icd10_xx == "C83" & icd10_x %in% c(0:9) ~ "C82-86,96",
#       icd10_xx == "C84" & icd10_x %in% c(0:9) ~ "C82-86,96",
#       icd10_xx == "C85" & icd10_x %in% c(0:9) ~ "C82-86,96",
#       icd10_xx == "C86" & icd10_x %in% c(0:9) ~ "C82-86,96",
#       icd10_xx == "C96" & icd10_x %in% c(0:9) ~ "C82-86,96",
#       icd10_xx == "C88" & icd10_x %in% c(0:9) ~ "C88,90",
#       icd10_xx == "C90" & icd10_x %in% c(0:9) ~ "C88,90",
#       icd10_xx == "C91" & icd10_x == 0        ~ "C91.0",
#       icd10_xx == "C91" & icd10_x %in% c(1:9) ~ "C91.1-9",
#       icd10_xx == "C92" & icd10_x == 0        ~ "C92.0",
#       icd10_xx == "C92" & icd10_x %in% c(1:9) ~ "C92.1-9",
#       icd10_xx == "C93" & icd10_x == 0        ~ "C93.0,94.0,2,4,5,95.0",
#       icd10_xx == "C94" & icd10_x %in% c(0, 2, 4, 5) ~ "C93.0,94.0,2,4,5,95.0",
#       icd10_xx == "C95" & icd10_x == 0        ~ "C93.0,94.0,2,4,5,95.0",
#       icd10_xx == "C93" & icd10_x %in% c(1:3, 7, 9) ~ "С93.1-3,7,9,94.1,3,7,95.1,2,7,9",
#       icd10_xx == "C94" & icd10_x %in% c(1, 3, 7) ~ "С93.1-3,7,9,94.1,3,7,95.1,2,7,9",
#       icd10_xx == "C95" & icd10_x %in% c(1, 2, 7, 9) ~ "С93.1-3,7,9,94.1,3,7,95.1,2,7,9"))
# }

# add_diag_name <- function(.data) {
#   .data %>%
#     mutate(localization = case_when(
#         icd10 %in% c("C00","C01", "C02", "C03", "C04", "C05",
#                      "C06", "C07", "C08", "C09", "C10", "C11",
#                      "C12", "C13", "C14")                      ~ "Lip, oral and pharynx",
#         icd10 == "C15"                                         ~ "Oesophagus",
#         icd10 == "C16"                                         ~ "Stomach",
#         icd10 %in% c("C18", "C19", "C20", "C21")               ~ "Colorectal",
#         icd10 == "C22"                                         ~ "Liver",
#         icd10 == "C25"                                         ~ "Pancreas",
#         icd10 == "C32"                                         ~ "Larynx",
#         icd10 %in% c("C33", "C34")                             ~ "Trachea, bronchus and lung",
#         icd10 %in% c("C40", "C41")                             ~ "Bone and cartilages",
#         icd10 == "C43"                                         ~ "Melanoma of skin",
#         icd10 == "C44"                                         ~ "Skin (non-melanoma)",
#         icd10 == "C50"                                         ~ "Breast",
#         icd10 %in% c("C45", "C46", "C47", "C48", "C49")        ~ "Soft tissues",
#         icd10 == "C53"                                         ~ "Cervix uteri",
#         icd10 %in% c("C54", "C55")                             ~ "Corpus uteri, Uterus, parts unspecified",
#         icd10 == "C56"                                         ~ "Ovary",
#         icd10 == "C61"                                         ~ "Prostate",
#         icd10 == "C64"                                         ~ "Kidney",
#         icd10 == "C67"                                         ~ "Bladder",
#         icd10 %in% c("C70", "C71", "C72")                      ~ "Brain, central nervous system",
#         icd10 %in% c("C81", "C82", "C83", "C84", "C85")        ~ "Non-hodgkin lymphoma",
#         icd10 %in% c("C91", "C92", "C93", "C94", "C95")        ~ "Leukaemia",
#         icd10 %in% c("C69", "C73", "C74", "C75", "C76", "C77",
#                      "C78", "C79", "C80", "C88", "C96")        ~ "Other and ill-defined",
#         icd10 %in% c("C17", "C23", "C24", "C26", "C30", "C31",
#                      "C37", "C38", "C39", "C51", "C52", "C57",
#                      "C58", "C60", "C62", "C63", "C65", "C66",
#                      "C68", "C90")                             ~ "Other"))
# }