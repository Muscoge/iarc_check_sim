# Add IACR Tools check simulation and its relevant error messages

load(here("functions/data_for_behaviour_histology_control.rda"))

# ICD-O-3 code should be separated into columns 
# "topo_Cxxx", "topo_Cxxx", "morph", "behav_icdo3", "gr_icdo3".
# Columns "morph", "behav_icdo3", "gr_icdo3" should have type double.

add_histology_families <- function(.data) {
  .data %>% 
    mutate(hist_family = case_when(
      morph %in% c(8561, 8974)                                   ~ 2,
      morph %in% c(8142, 8214)                                   ~ 3,
      morph %in% c(8683, 9764)                                   ~ 4,
      morph %in% c(8213, 8220, 8261, 8265)                       ~ 5,
      morph %in% c(8124, 8215)                                   ~ 6,
      morph %in% c(8144, 8145, 8221, 8936, 9117)                 ~ 7,
      morph %in% c(8170:8179, 8970, 8975, 9124)                  ~ 8,
      morph %in% c(8160:8163, 8180, 8264)                        ~ 9,
      morph %in% c(8150:8155, 8202, 8452:8453, 8971)             ~ 10,
      morph %in% c(9520:9523)                                    ~ 11,
      morph %in% c(8012, 8040:8046, 8250, 8252:8255, 8827, 8972) ~ 12,
      morph %in% c(8973, 9050:9053, 9055)                        ~ 13,
      morph %in% c(8580:8589, 9679)                              ~ 14,
      morph %in% c(8454)                                         ~ 15,
      morph %in% c(9365)                                         ~ 16,
      morph %in% c(9261)                                         ~ 17,
      morph %in% c(8812, 9180:9187, 9191:9195, 9200, 9210, 9250, 
                   9262, 9270:9275, 9280:9282, 9290, 9300:9302, 
                   9310:9312, 9320:9322, 9330, 9340:9342)        ~ 18,
      morph %in% c(9689, 9732:9733, 9742, 9761, 9765, 9800:9801, 
                   9805:9806, 9808:9809, 9811:9812, 9814:9818, 
                   9820, 9823, 9826:9827, 9831:9837, 9840, 
                   9860:9861, 9863, 9865:9867, 9869:9876, 
                   9891, 9895:9898, 9910:9911, 9920, 9931, 
                   9940, 9945:9946, 9948, 9950, 9960:9967, 
                   9980, 9982:9987, 9989, 9991:9992)              ~ 19,
      morph %in% c(8081, 8090:8097, 8100:8103, 8110, 8390:8392, 
                   8400:8410, 8413, 8420, 8542, 8790, 9597, 
                   9700, 9709, 9718, 9726)                        ~ 20,
      morph %in% c(8247, 8832:8833, 9507, 9708)                   ~ 21,
      morph %in% c(8204, 8314:8315, 8501:8502, 8505:8508,
                   8512:8513, 8520:8524, 8530, 8540:8541, 8543, 
                   8983, 9010:9012, 9016, 9020, 9030)             ~ 22,
      morph == 8905                                               ~ 23,
      morph %in% c(8930:8931)                                     ~ 24,
      morph %in% c(8313, 8441:8444, 8451, 8460, 8462:8463, 
                   8470:8473, 8593, 8600:8602, 8610, 8620:8623, 
                   8632, 8641, 8660, 8670, 9000, 9013:9015, 
                   9090:9091)                                     ~ 25,
      morph %in% c(9103:9104)                                     ~ 26,
      morph %in% c(8380:8384, 8482, 8934, 8950:8951)              ~ 27,
      morph == 8080                                               ~ 28,
      morph %in% c(9061:9063, 9102)                               ~ 29,
      morph %in% c(8312, 8316:8319, 8325, 8361, 8959:8960, 
                   8964:8967)                                     ~ 30,
      morph %in% c(9510:9514)                                     ~ 31, 
      morph %in% c(8726, 8773:8774)                               ~ 32,
      morph %in% c(8728, 9530:9535, 9537:9539)                    ~ 33,
      morph %in% c(9470:9472, 9474, 9480, 9493)                   ~ 34,
      morph %in% c(9381, 9390, 9444)                              ~ 35,
      morph %in% c(9121:9123, 9131, 9380, 9382:9384, 9391:9394,
                   9400:9401, 9410:9413, 9420:9421, 9423:9425, 
                   9430:9432, 9440:9442, 9450:9451, 9460, 9473, 
                   9505:9506, 9509, 9508)                         ~ 36,
      morph %in% c(8330:8337, 8340:8347, 8350)                    ~ 37,
      morph %in% c(8370:8375, 8700)                               ~ 38,
      morph %in% c(8321, 8322)                                    ~ 39,
      morph %in% c(8270:8272, 8280:8281, 8300, 9350:9352, 9582)   ~ 40,
      morph %in% c(9360:9362, 9395)                               ~ 41,
      morph == 8692                                               ~ 42,
      morph %in% c(8690:8691)                                     ~ 43,
      morph == 8098                                               ~ 44,
      morph %in% c(8153, 8156:8158)                               ~ 45,
      morph == 8290                                               ~ 46,
      morph == 8450                                               ~ 47,
      morph == 8461                                               ~ 48,
      morph %in% c(8590:8592, 8630:8631, 8633:8634, 8640, 8642, 
                   8650, 9054)                                    ~ 49, 
      morph %in% c(8720:8723, 8725, 8727, 8730, 8740:8746, 8750, 
                   8760:8762, 8770:8772, 8780)                    ~ 50,
      morph %in% c(8932, 8933, 9110)                              ~ 51,
      morph == 8935                                               ~ 52,
      morph %in% c(9040:9044, 9251:9252, 9260)                    ~ 53,
      morph %in% c(9220:9221, 9230:9231, 9240:9243)               ~ 54,
      morph %in% c(8077, 8148)                                    ~ 55,
      morph %in% c(8120:8122, 8130:8131)                          ~ 56,
      morph %in% c(8240:8246, 8248:8249)                          ~ 57,
      morph %in% c(8500, 8503:8504, 8514, 8525)                   ~ 58,
      morph %in% c(8680:8682, 8693, 8710:8713)                    ~ 59,
      morph %in% c(9060, 9064:9065, 9070:9073, 9080:9085, 9105)   ~ 60,
      morph %in% c(9100, 9101)                                    ~ 61,
      morph %in% c(9370:9373)                                     ~ 62,
      morph %in% c(9490:9492, 9500:9504)                          ~ 63,
      morph %in% c(9540:9541, 9550, 9560:9562, 9570:9571)         ~ 64,
      morph %in% c(8004:8005, 8831, 8834:8836, 9170:9175)         ~ 65,
      morph %in% c(8010:8011, 8013:8015, 8020:8022, 8030:8035, 
                   8050:8053, 8060, 8070:8076, 8078, 8082:8084, 
                   8123, 8140:8141, 8143, 8146:8147, 8149, 
                   8190:8191, 8200, 8201, 8210, 8211, 8212, 8230, 
                   8231, 8251, 8260, 8262, 8263, 8310, 8311, 8320, 
                   8323, 8324, 8360, 8430, 8440, 8480, 8481, 8490, 
                   8510, 8550, 8551, 8552, 8560, 8562, 8570, 8571, 
                   8572, 8573, 8574, 8575, 8576, 8940, 8941, 8980, 
                   8981, 8982)                                    ~ 66,
      morph %in% c(8671, 8800, 8801, 8803:8805, 8806, 8963, 9120, 
                   9125, 9130, 9133, 9135, 9136, 9141, 9142, 9150, 
                   9160, 9161, 9363, 9364, 9580, 9581)            ~ 67,
      morph == 8802                                               ~ 68,
      morph %in% c(8810, 8811, 8813, 8814, 8815, 8820, 8821, 8822, 
                   8823, 8824, 8825, 8826, 8830, 8840, 8841, 8842, 
                   8850, 8851, 8852, 8853,8854, 8855, 8856, 8857, 
                   8858, 8860, 8861, 8862, 8870, 8880, 8881, 8890, 
                   8891, 8892, 8893, 8894, 8895, 8896, 8897, 8898,
                   8900, 8901, 8902, 8903, 8904, 8910, 8912, 8920, 
                   8921, 8990, 8991, 9132)                        ~ 69,
      morph == 9140                                               ~ 70,
      morph == 9734                                               ~ 71,
      TRUE                                                        ~ 0
    ))
}

check_site_histology <- function(.data) {
  .data %>% 
    add_histology_families() %>% 
    mutate(site_hist_status = case_when(
      hist_family == 0 ~ NA_character_,
      
      # Group 2: Tumours with specific site-profile
      hist_family == 2 & !topo_Cxx %in% c("C07", "C08") ~ "invalid site/histology combination",
      hist_family == 3 & topo_Cxx != "C16" ~ "invalid site/histology combination",
      hist_family == 4 & topo_Cxx != "C17" ~ "invalid site/histology combination",
      
      # for morph = 8265 from hist_family = 5 topographies are unusual 
      morph == 8265 & 
        !(topo_Cxx == "C18" | topo_Cxxx %in% c("C199", "C209")) ~ "invalid site/histology combination",
      hist_family == 5  & 
        !(topo_Cxx %in% c("C18", "C19", "C20", "C26", "C80") | 
            (topo_Cxxx %in% str_c("C76", c(2, 3, 7, 8))))       ~ "invalid site/histology combination",
      hist_family == 6  & !topo_Cxx %in% c("C20", "C21")        ~ "invalid site/histology combination",
      hist_family == 7  & 
        !(topo_Cxx %in% str_c("C", c(15:20, 26, 80)) |
            (topo_Cxxx %in% str_c("C76", c(2, 3, 7, 8))))       ~ "invalid site/histology combination",
      
      # for morph = 8975 from hist_family = 8 topography is more specific
      morph == 8975 & topo_Cxxx != "C220"                  ~ "invalid site/histology combination",
      hist_family == 8 & topo_Cxx != "C22"                 ~ "invalid site/histology combination",
      hist_family == 9 & !topo_Cxx %in% str_c("C", 22:24)  ~ "invalid site/histology combination",
      hist_family == 10 & topo_Cxx != "C25"                ~ "invalid site/histology combination",
      hist_family == 11 & !topo_Cxx %in% str_c("C", 30:31) ~ "invalid site/histology combination",
      hist_family == 12 & 
        !(topo_Cxx %in% c("C34", "C80") |
            topo_Cxxx %in% c("C398", "C399", "C761", "C767", "C768")) ~ "invalid site/histology combination",
      hist_family == 13 &
        !(topo_Cxx %in% c("C34", "C48", "C80") |
            topo_Cxxx %in% c("C384", "C398", "C399", str_c("C76", c(1:3, 7:8))))  ~ "invalid site/histology combination",
      hist_family == 14 & !(topo_Cxx == "C38" | topo_Cxxx == "C379")              ~ "invalid site/histology combination",
      hist_family == 15 & topo_Cxxx != "C380"                                     ~ "invalid site/histology combination",
      hist_family == 16 & !(topo_Cxx %in% str_c("C", c(39:41, 49, 80)) |
                              topo_Cxxx %in% str_c("C76", c(1, 7:8)))             ~ "invalid site/histology combination",
      hist_family == 17 & !topo_Cxxx %in% str_c("C40", c(0, 2, 8:9))              ~ "invalid site/histology combination",
      hist_family == 18 & !topo_Cxx %in% c("C40", "C41")                          ~ "invalid site/histology combination",
      hist_family == 19 & topo_Cxx != "C42"                                       ~ "works here",
      hist_family == 20 & 
        !(topo_Cxx %in% str_c("C", c("00", 44, 51, 60, 76, 80)) |
            topo_Cxxx == "C632")                                                  ~ "invalid site/histology combination",
      hist_family == 21 & 
        !(topo_Cxx %in% str_c("C", c("00", 44, 49, 51, 60, 76, 80)) |
            topo_Cxxx %in% str_c("C63", c(2, 8, 9)))                              ~ "invalid site/histology combination",
      hist_family == 22 & !(topo_Cxx %in% c("C50", "C80") | 
                              topo_Cxxx %in% str_c("C76", c(1, 7:8)))             ~ "invalid site/histology combination",
      hist_family == 23 & !(topo_Cxx %in% c("C51", "C52") | 
                              topo_Cxxx %in% str_c("C57", 8:9))                   ~ "invalid site/histology combination",
      hist_family == 24 & !(topo_Cxx == "C54" | 
                              topo_Cxxx %in% c("C559", "C578", "C579"))           ~ "invalid site/histology combination",
      hist_family == 25 & !(topo_Cxx %in% c("C57", "C80") |
                              topo_Cxxx %in% c("C569", str_c("C76", c(2:3, 7:8)))) ~ "invalid site/histology combination",
      hist_family == 26 & topo_Cxxx != "C589"                                      ~ "invalid site/histology combination",
      hist_family == 27 & 
        !(topo_Cxx %in% c("C48", "C53", "C54", "C57", "C80") |
            topo_Cxxx %in% 
            c(str_c("C49", c(4:6, 8:9)), "C559", "C569", str_c("C76", c(2:3, 7:8)))) ~ "invalid site/histology combination",
      hist_family == 28 & topo_Cxx != "C60"                                          ~ "invalid site/histology combination",
      hist_family == 29 & !topo_Cxx %in% c("C62", "C63")                             ~ "invalid site/histology combination",
      hist_family == 30 & !topo_Cxxx %in% c("C649", "C688", "C689")                  ~ "invalid site/histology combination",
      hist_family == 31 & !topo_Cxxx %in% c("C692", "C698", "C699")                  ~ "invalid site/histology combination",
      hist_family == 32 & topo_Cxx != "C69"                                          ~ "invalid site/histology combination",
      hist_family == 33 & !topo_Cxx %in% c("C70", "C71", "C72")                      ~ "invalid site/histology combination",
      hist_family == 34 & 
        !topo_Cxxx %in% c(str_c("C71", c(6, 8:9)), str_c("C72", 8:9))                ~ "invalid site/histology combination",
      hist_family == 35 & !(topo_Cxx == "C71" | topo_Cxxx %in% c("C728", "C729"))    ~ "invalid site/histology combination",
      hist_family == 36 & !(topo_Cxx %in% str_c("C", 70:72) | topo_Cxxx == "C753")   ~ "invalid site/histology combination",
      hist_family == 37 & topo_Cxx != "C73"                                          ~ "invalid site/histology combination",
      hist_family == 38 & topo_Cxx != "C74"                                          ~ "invalid site/histology combination",
      hist_family == 39 & topo_Cxxx != "C750"                                        ~ "invalid site/histology combination",
      hist_family == 40 & !topo_Cxxx %in% c("C751", "C752")                          ~ "invalid site/histology combination",
      hist_family == 41 & topo_Cxxx != "C753"                                        ~ "invalid site/histology combination",
      hist_family == 42 & topo_Cxxx != "C754"                                        ~ "invalid site/histology combination",
      hist_family == 43 & topo_Cxxx != "C755"                                        ~ "invalid site/histology combination",
      hist_family == 44 & !(topo_Cxx %in% c("C44", "C53") |
                              topo_Cxxx %in% c("C578", "C579"))                      ~ "invalid site/histology combination",
      hist_family == 45 & !(topo_Cxx %in% c("C16", "C17", "C25", "C26") |
                              topo_Cxxx %in% c("C762", "C767", "C768", "C809"))      ~ "invalid site/histology combination",
      hist_family == 46 & 
        !(topo_Cxx == "C08" | 
            topo_Cxxx %in% c("C079", "C649", "C688", "C689", "C739", "C758", 
                             "C759", "C760", "C762", "C767", "C768", "C809"))        ~ "invalid site/histology combination",
      hist_family == 47 & !(topo_Cxx %in% c("C25", "C26", "C57") | 
                              topo_Cxxx == "C569")                                   ~ "invalid site/histology combination",
      hist_family == 48 & !(topo_Cxx == "C48" | topo_Cxxx == "C569")                 ~ "invalid site/histology combination",
      hist_family == 49 & 
        !(topo_Cxx == "C62" | 
            topo_Cxxx %in% c("C569", "C578", "C579", "C638", "C639", "C762", 
                             "C763", "C767", "C768", "C809"))                        ~ "invalid site/histology combination",
      hist_family == 50 & 
        !(topo_Cxx %in% c("C21", "C30", "C44", "C51", "C60", "C69", "C70", "C76") |
            topo_Cxxx %in% c("C209", "C809"))                                        ~ "invalid site/histology combination",
      hist_family == 51 & 
        !(topo_Cxx %in% c("C51", "C53", "C54", "C57", "C67", "C68") |
            topo_Cxxx %in% c("C529", "C559", "C569", "C649", "C659", "C669", 
                             "C762", "C763", "C767", "C768", "C809"))                ~ "invalid site/histology combination",
      hist_family == 52 &
        !(topo_Cxx %in% c("C50", "C53", "C54", "C57") |
            topo_Cxxx %in% 
            c("C559", "C569", "C761", "C762", "C763", "C767", "C768", "C809"))       ~ "invalid site/histology combination",
      hist_family == 53 & !(topo_Cxx %in% c("C40", "C41", "C49", "C76") |
                              topo_Cxxx == "C809")                                   ~ "invalid site/histology combination",
      hist_family == 54 & 
        !(topo_Cxx %in% c("C31", "C39", "C40", "C41", "C49", "C76") |
            topo_Cxxx %in% c("C300", "C323", "C328", "C329", "C339", "C809"))        ~ "invalid site/histology combination",
      hist_family == 55 & !(topo_Cxx %in% c("C21", "C51", "C53") |
                              topo_Cxxx %in% c("C529", "C619"))                      ~ "invalid site/histology combination",
      hist_family == 56 & 
        !(topo_Cxx %in% 
            c("C11", "C14", "C21", "C26", "C30", "C31", "C39", "C53", "C67", "C68") |
            topo_Cxxx %in% 
            c("C209", "C619", "C649", "C659", "C669", str_c("C76", c(0:3, 7:8)), 
              "C809"))                                                               ~ "invalid site/histology combination",
      hist_family == 57 & 
        !(topo_Cxx %in% 
            c("C15", "C16", "C17", "C18", "C21", "C24", "C25", "C26", "C34") |
            topo_Cxxx %in% 
            c("C199", "C209", "C239", "C379", "C381", "C382", "C383", "C398", 
              "C399", "C569", "C578", "C579", "C739", str_c("C76", c(0:3, 7:8)),
              "C809"))                                                              ~ "invalid site/histology combination",
      hist_family == 58 & 
        !(topo_Cxx %in% c("C08", "C21", "C22", "C24", "C25", "C50") |
            topo_Cxxx %in% 
            c("C069", "C079", "C239", "C268", "C269", "C619", "C638", "C639", 
              "C758", "C759", str_c("C76", c(0, 2:3, 7:8)), "C809"))                ~ "invalid site/histology combination",
      hist_family == 59 & 
        !(topo_Cxx %in% c("C38", "C47", "C48", "C49", "C67", "C68", "C71", "C72", 
                          "C74", "C75", "C76") |
            topo_Cxxx %in% c("C398", "C399", "C739", "C809"))                       ~ "invalid site/histology combination",
      hist_family == 60 & 
        !(topo_Cxx %in% c("C38", "C48", "C49", "C57", "C62", "C63", "C71", "C72", 
                          "C75") |
            topo_Cxxx %in% c("C398", "C399", "C569", str_c("C76", c(0:3, 7:8)), 
                             "C809"))                                               ~ "invalid site/histology combination",
      hist_family == 61 & 
        !(topo_Cxx %in% c("C38", "C57", "C62") |
            topo_Cxxx %in% c("C569", "C589", str_c("C76", c(1:3, 7:8)), "C809"))    ~ "invalid site/histology combination",
      hist_family == 62 & 
        !(topo_Cxx %in% c("C11", "C14", "C30", "C31", "C39", "C40", "C41", "C49", 
                          "C71", "C72", "C75", "C76") |
            topo_Cxxx == "C809")                                                    ~ "invalid site/histology combination",
      hist_family == 63 & 
        !(topo_Cxx %in% c("C38", "C47", "C48", "C49", "C69", "C70", "C71", "C72", 
                          "C74", "C76") |
            topo_Cxxx %in% c("C398", "C399", "C758", "C759", "C809"))               ~ "invalid site/histology combination",
      hist_family == 64 & 
        !(topo_Cxx %in% c("C38", "C47", "C48", "C49", "C69", "C70", "C71", "C72", 
                          "C76") |
            topo_Cxxx %in% c("C398", "C399", "C809"))                               ~ "invalid site/histology combination",
      
      # Group 3 Tumours with inverse site-profile
      hist_family == 65 & topo_Cxx == "C42"                                              ~ "invalid site/histology combination",
      hist_family == 66 & 
        topo_Cxx %in% 
        c("C40", "C41", "C42", "C47", "C48", "C49", "C70", "C71", "C72", "C77")          ~ "invalid site/histology combination",
      hist_family == 67 & (topo_Cxx == "C77" | topo_Cxxx %in% str_c("C42", c(0:1, 3:4))) ~ "invalid site/histology combination",
      hist_family == 68 & (topo_Cxx %in% c("C40", "C41", "C77") | 
                             topo_Cxxx %in% str_c("C42", c(0:1, 3:4)))                   ~ "invalid site/histology combination",
      hist_family == 69 & (topo_Cxx %in% c("C70", "C71", "C72", "C77") |
                             topo_Cxxx %in% str_c("C42", c(0:1, 3:4)))                   ~ "invalid site/histology combination",
      hist_family == 70 & 
        (topo_Cxx %in% c("C08", "C22", "C24", "C25", "C40", "C41", "C42", "C47", "C48", 
                         "C50", "C51", "C53", "C54", "C57", "C62", "C67", "C68", "C70", 
                         "C71", "C72", "C74", "C75") |
           topo_Cxxx %in% c("C079", "C239", "C529", "C559", "C569", "C589", "C619", 
                            "C649", "C659", "C669", "C739"))                             ~ "invalid site/histology combination",
      hist_family == 71 & topo_Cxx %in% c("C40", "C41")                                  ~ "invalid site/histology combination",
      
      TRUE ~ NA_character_
    ))
}

check_dates_and_age <- function(.data) {
  .data %>% 
    mutate(
      dates_status = case_when(
        is.na(diag_date) & is.na(dob_date)                 ~ "diagnosis and birth dates are not available",
        is.na(diag_date)                                   ~ "the date of diagnosis is not available",
        is.na(dob_date)                                    ~ "the date of birth is not available",
        diag_date < dob_date                               ~ "the date of diagnosis is before the date of birth",
        diag_date < "1900-01-01" | dob_date < "1900-01-01" ~ "check the dates manually",
       
         TRUE ~ NA_character_
    ))
}

check_sex_site <- function(.data) {
  .data %>% 
    mutate(
      sex_site_status = case_when(
        sex == 1 & topo_Cxx %in% c("C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58") ~ "invalid sex/site combination",
        sex == 2 & topo_Cxx %in% c("C60", "C61", "C62", "C63")                             ~ "invalid sex/site combination",
        
      TRUE ~ NA_character_
    ))
}

check_age_site_histology <- function(.data) {
  .data %>% 
    mutate(
      age_site_hist_status = case_when(
        age < 15 ~ case_when(
          
          # Hodgkin lymphoma
          morph %in% c(9650:9655, 9659, 9661:9665, 9667) & 
            age %in% c(0:2)                                        ~ "unlikely age/site/histology combination",
         
          # Neuroblastoma
          morph %in% c(9490, 9500) & age %in% c(10:14)             ~ "unlikely age/site/histology combination",
       
          # Retinoblastoma
          morph %in% c(9510:9514) & age %in% c(6:14)               ~ "unlikely age/site/histology combination",
        
           # Wilms' tumors -- check
          (morph %in% c(8959, 8960, 8964:8967) | 
             (morph %in% c(8963, 9364) & topo_Cxxx == "C649")) & 
            age %in% c(9:14)                                       ~ "unlikely age/site/histology combination",

           # Renal carcinoma
           (morph %in% c(8311, 8312, 8316:8319, 8361) | 
              (morph %in% c(8010:8041, 8050:8075, 8082, 8120:8122, 8130:8141, 
                            8143, 8155, 8190:8201, 8210, 8211, 8221:8231, 8240, 
                            8241, 8244:8246, 8260:8263, 8290, 8310, 8320, 8323, 
                            8401, 8430, 8440, 8480:8490, 8504, 8510, 8550, 
                            8560:8576) & topo_Cxxx == "C649")) & 
            age %in% c(0:8)                                                     ~ "unlikely age/site/histology combination",

           # Hepatoblastoma
           morph == 8970 & age %in% c(6:14)                                     ~ "unlikely age/site/histology combination",
        
           # Hepatic carcinomas
           (morph %in% c(8160:8180) | 
              (morph %in% c(8010:8041, 8050:8075, 8082, 8120:8122, 8140, 8141, 
                            8143, 8155, 8190:8201, 8210, 8211, 8230, 8231, 8240, 
                            8241, 8244:8246, 8260:8264, 8310, 8320, 8323, 8401, 
                            8430, 8440, 8480:8490, 8504, 8510, 8550, 8560:8576) & 
                 topo_Cxxx %in% c("C220", "C221"))) & 
            age %in% c(0:8)                                                     ~ "unlikely age/site/histology combination",
        
           # Osteosarcoma
           morph %in% c(9180:9187, 9191:9195, 9200) &
            topo_Cxxx %in% str_c("C", c(400:419, 760:768, 809)) & age %in% c(0:5) ~ "unlikely age/site/histology combination",
        
           # Chondrosarcomas
           (morph %in% c(9221, 9230, 9241:9243) | morph %in% c(9210, 9220, 9240) & 
              topo_Cxxx %in% str_c("C", c(400:419, 760:768, 809))) & 
            age %in% c(0:5)                                                     ~ "unlikely age/site/histology combination",
        
           # Ewing sarcoma
           (morph == 9260 & topo_Cxxx %in% str_c("C", c(400:419, 760:768, 809)) |
              morph %in% c(9363:9365) & topo_Cxxx %in% str_c("C", 400:419)) & 
            age %in% c(0:3)                                                     ~ "unlikely age/site/histology combination",
        
           # Non-gonadal germ cell
           (morph %in% c(9060:9065, 9070:9072, 9080:9085, 9100, 9101) &
              topo_Cxxx %in% str_c("C", c(700:729, 751:753)) |
              morph %in% c(9060:9065, 9070:9072, 9080:9085, 9100:9105) &
              topo_Cxxx %in% c(str_c("C00", c(0:9)), str_c("C0", 10:99), 
                               str_c("C", c(100:559, 570:619, 630:699, 
                                            739:750, 754:768, 809)))) & 
            age %in% c(8:14)                                                    ~ "unlikely age/site/histology combination",
        
           # Gonadal carcinomas
           (morph %in% c(8010:8041, 8050:8075, 8082, 8120:8122, 8130:8141, 8143,
                         8190:8201, 8210, 8211, 8221:8241, 8244:8246, 8260:8263, 
                         8290, 8310, 8313, 8320, 8323, 8380:8384, 8430, 8440, 
                         8480:8490, 8504, 8510, 8550, 8560-8573, 9000, 9014, 9015) &
              topo_Cxxx %in% str_c("C", c(569, 620:629)) |
              morph %in% c(8441:8444, 8450, 8451, 8460:8473)) & 
            age %in% c(0:4)                                                     ~ "unlikely age/site/histology combination",
       
            # Thyroid carcinoma
           (morph %in% c(8330:8337, 8340:8347, 8350) | 
              morph %in% c(8010:8041, 8050:8075, 8082, 8120:8122, 8130:8141, 
                           8190, 8200, 8201, 8211, 8230, 8231, 8244-8246, 
                           8260:8263, 8290, 8310, 8320, 8323, 8430, 8440, 8480,
                           8481, 8510, 8560:8573) & topo_Cxxx == "C739") & 
            age %in% c(0:5)                                                     ~ "unlikely age/site/histology combination",
       
            # Nasopharyngeal carcinoma
            morph %in% c(8010:8041, 8050:8075, 8082, 8083, 8120:8122, 8130:8141, 
                         8190, 8200, 8201, 8211, 8230, 8231, 8244:8246, 
                         8260:8263, 8290, 8310, 8320, 8323, 8430, 8440, 8480, 
                         8481, 8500:8576) & topo_Cxxx %in% str_c("C", 110:119) & 
            age %in% c(0:5)                                                     ~ "unlikely age/site/histology combination",
        
            # Skin carcinoma
            morph %in% c(8010:8041, 8050:8075, 8078, 8082, 8090:8110, 8140, 8143, 
                         8147, 8190, 8200, 8240, 8246, 8247, 8260, 8310, 8320, 
                         8323, 8390:8420, 8430, 8480, 8542, 8560, 8570:8573, 8940, 
                         8941) & topo_Cxxx %in% str_c("C", 440:449) & 
            age %in% c(0:4)                                                     ~ "unlikely age/site/histology combination",
        
            # Carcinoma, NOS
            morph %in% c(8010:8015, 8020:8022, 8050, 9590:9591, 9596, 9727, 9760, 
                         9800:9801, 9805, 9820, 9832, 9835, 9860, 9960, 9970, 
                         9975, 9989, 8000:8005) & 
            age %in% c(0:4)                                                     ~ "unlikely age/site/histology combination",
        
            # Mesothelial neoplasms
            str_detect(icd10, "^C45") & age %in% c(0:14)                        ~ "unlikely age/site/histology combination",    
       
            # 2
            morph == 9720 & age < 15                                            ~ "unlikely age/site/histology combination"
          ),
        
        age >= 15 ~ case_when(
          age < 40 & topo_Cxxx == "C619" & str_detect(morph, "^814")            ~ "unlikely age/site/histology combination",
          age < 20 & 
            (topo_Cxx %in% c("C15", "C19", "C20", "C21", "C23", "C24", "C25", 
                             "C50", "C53", "C54", "C55") | 
               topo_Cxxx == "C384")                                             ~ "unlikely age/site/histology combination",
          age < 20 & topo_Cxx == "C17" & morph < 9590                           ~ "unlikely age/site/histology combination",
          age < 20 & 
            topo_Cxx %in% c("C33", "C34", "C18") & 
            !str_detect(morph, "^824")                                          ~ "unlikely age/site/histology combination",
          age > 45 & topo_Cxx == "C58" & morph == 9100                          ~ "unlikely age/site/histology combination",
          age <= 25 & morph %in% c(9732, 9823)                                  ~ "unlikely age/site/histology combination",
          morph %in% c(8910, 8960, 8970, 8981, 8991, 9072, 9470, 9490, 
                       9500, 9510:9519, 9687)                                   ~ "unlikely age/site/histology combination")
        ))
}

check_sex_histology <- function(.data) {
  .data %>% 
    mutate(
      sex_hist_status = case_when(
        sex == 1 & hist_family %in% c(23:27) ~ "unlikely sex/histology combination",
        sex == 2 & hist_family %in% c(28:29) ~ "unlikely sex/histology combination",
        
        TRUE ~ NA_character_
    ))
}

check_behaviour_site <- function(.data) {
  .data %>% 
    mutate(
      behav_site_status = case_when(
        topo_Cxx %in% c("C40", "C41", "C42", "C47", "C49", "C70", "C71", "C72") & 
          behav_icdo3 == 2 ~ "unlikely behaviour/site combination",
        
      TRUE ~ NA_character_
    ))
}

check_behaviour_histology <- function(.data) {
  .data %>% 
    mutate(tmp = as.double(paste0(morph, behav_icdo3))) %>% 
    mutate(
      behav_hist_status = ifelse(tmp %in% icdo3_morphologies$hist_behav, NA_character_, "unlikely behaviour/histology combination")
      )
}

check_grade_histology <- function(.data) {
  .data %>% 
    mutate(
      grade_hist_status = case_when(
        behav_icdo3 < 3 & gr_icdo3 < 9                                          ~ "unlikely grade/histology combination",
        between(gr_icdo3, 5, 8) & morph < 9590                                  ~ "unlikely grade/histology combination",
        between(gr_icdo3, 1, 4) & morph >= 9590                                 ~ "unlikely grade/histology combination",
        morph %in% c(9702, 9705, 9708, 9709, 9716, 9717, 9718, 9724, 9725, 
                     9726, 9729, 9827, 9834, 9837) & gr_icdo3 != 5              ~ "unlikely grade/histology combination",
        morph == 9714 & !gr_icdo3 %in% c(5, 7)                                  ~ "unlikely grade/histology combination",
        morph %in% c(9700, 9701, 9719, 9831) & !gr_icdo3 %in% c(5, 8)           ~ "unlikely grade/histology combination",
        (between(morph, 9670, 9699) | 
           morph %in% c(9712, 9728, 9737, 9738, 9811, 9812, 9813, 9814, 9815, 
                        9816, 9817, 9818, 9823, 9826, 9833, 9836)) & 
          gr_icdo3 != 6                                                         ~ "unlikely grade/histology combination",
        morph == 9948 & gr_icdo3 != 8                                           ~ "unlikely grade/histology combination",
        morph %in% c(8331, 8851, 9187, 9511) & gr_icdo3 != 1                    ~ "unlikely grade/histology combination",
        morph %in% c(8249, 8332, 8858, 9083, 9243, 9372) & gr_icdo3 != 2        ~ "unlikely grade/histology combination",
        morph %in% c(8631, 8634) & gr_icdo3 != 3                                ~ "unlikely grade/histology combination",
        morph %in% c(8020, 8021, 8805, 9062, 9082, 9392, 9401, 9451, 9505, 
                     9512 ) & gr_icdo3 != 4                                     ~ "unlikely grade/histology combination",
     
        TRUE ~ NA_character_
    ))
}

check_basis_histology <- function(.data) {
  .data %>%
    mutate(basis_status = case_when(
      
      # microscopically confirmed diagnosis
      basis %in% c(5, 6, 7) ~ NA_character_,
      
      # A non-microscopically confirmed diagnosis 
      # is accepted only with the following histological codes: 
      (morph %in% c(8000, 8150:8154, 8170, 8270:8281, 8800, 8960, 9100, 9140, 
                    9380, 9500, 9510, 9530:9539, 9590, 9732, 9761, 9800) |
         (morph == 8720 & topo_Cxx == "C69") |
         (morph == 8720 & topo_Cxx == "C44") |
         (morph == 9384 & behav_icdo3 == 1)) & ! basis %in% c(5, 6, 7)          ~ NA_character_,
     
      TRUE ~ "unlikely basis/histology combination"
    ))
}
