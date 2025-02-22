# HEADER ##############################################################
#     Program:    13_MltLvPGS_DATE.R
#     Project:    GADMJCOG
#     Tasks:       
#                 A) Batch Model Characteristics
#                 B) createModels
#                 C) runModels
#                 D) readModels/summaries
#                 E) readModels/estimates
#                 F) Create Unstandardized Table
#                 G) Create Standardized Table
#                 H) Export Tables
# DATASETS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Libraries:  GADMJCOG
#     Source:     NA
#     Produced:   INSERT
#                 INSERT
# HISTORY # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Jarrod Ellingson    18-08-28
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
     source("Work/01_Functions.R", echo = F)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     INSERT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# A) Batch Model Characteristics ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
project <- "GADMJCOG"
Model <- "13_MltLvPGS"
date <- "190618"



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # B) createModels ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
createModels(templatefile=paste0("Work/", Model, "_", date, ".inp"))
createModels(templatefile=paste0("Work/", Model, "_Gap_", date, ".inp"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # C) runModels ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
runModels(target = paste0(getwd(),"/Work/Models/",Model), filefilter = date,
          recursive=TRUE,
          showOutput=TRUE,
          replaceOutfile="modifiedDate"
          )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # D) readModels/summaries ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
ReadModels13 <- readModels(target = paste0(getwd(),"/Work/Models/",Model),
                           filefilter = c("_1_19|_2_19"))
library(plyr)
Summary13 <- as_tibble(do.call("rbind.fill", sapply(ReadModels13, "[", "summaries")))
detach("package:plyr", unload=TRUE)
Summary13_Problem <- Summary13 %>% dplyr::filter(is.na(AIC))
View(Summary13_Problem)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# E) readModels/summaries ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
unstandardized13 <- sapply(sapply(ReadModels13, 
                                  "[", "parameters"), 
                           "[", "unstandardized")
lapply(names(unstandardized13), function(element) {
  unstandardized13[[element]]$filename <<- element
  })

stdyx.standardized13 <- sapply(sapply(ReadModels13, 
                                  "[", "parameters"), 
                           "[", "stdyx.standardized")

lapply(names(stdyx.standardized13), function(element) {
  stdyx.standardized13[[element]]$filename <<- element
  })



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# F) Create UnstandardizedTables ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Estimates_13_Unst <- as_tibble(do.call("rbind", unstandardized13))  %>%
  mutate(Column = ifelse(param == "COGPGS", "2a) WI Cog PGS",
                  ifelse(param == "COGPGSBW", "1a) BW Cog PGS",
                         
                  ifelse(param == "CANEVR", "2b) WI Mj Ever Used",
                  ifelse(param == "CANONS", "2c) WI Mj Onset",
                  ifelse(param == "CANFLF", "2d) WI Mj Lifetime Freq.",
                  ifelse(param == "CANF6M", "2e) WI Mj 6-Month Freq.",
                  ifelse(param == "CANAC6", "2f) WI Mj 6-Month Acc.",

                  ifelse(param == "CANEVRBW", "1b) BW Mj Ever Used",
                  ifelse(param == "CANONSBW", "1c) BW Mj Onset",
                  ifelse(param == "CANFLFBW", "1d) BW Mj Lifetime Freq.",
                  ifelse(param == "CANF6MBW", "1e) BW Mj 6-Month Freq.",
                  ifelse(param == "CANAC6BW", "1f) BW Mj 6-Month Acc.",

                  ifelse(param == "ALCEVR", "2i) WI Alc Ever Used",                                                                                    
                  ifelse(param == "ALCONS", "2j) WI Alc Onset",
                  ifelse(param == "ALCFLF", "2k) WI Alc Lifetime Freq.",
                  ifelse(param == "ALCF6M", "2l) WI Alc 6-Month Freq.",
                  ifelse(param == "ALCAC6", "2m) WI Alc 6-Month Acc.",

                  ifelse(param == "ALCEVRBW", "1i) BW Alc Ever Used",
                  ifelse(param == "ALCONSBW", "1j) BW Alc Onset",
                  ifelse(param == "ALCFLFBW", "1k) BW Alc Lifetime Freq.",
                  ifelse(param == "ALCF6MBW", "1l) BW Alc 6-Month Freq.",
                  ifelse(param == "ALCAC6BW", "1m) BW Alc 6-Month Acc.",
                         param)))))))))))))))))))))), 
         Row = ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_1_", filename), "1a) Stroop Word (Wave 1)",
               
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_1_", filename), "1b) Block Design (Wave 1)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_1_", filename), "1c) Digit Span (Wave 1)",
               ifelse(grepl("WASVCB", toupper(filename)) & grepl("_1_", filename), "1d) Vocabulary (Wave 1)",
               ifelse(grepl("WSISUM", toupper(filename)) & grepl("_1_", filename), "1e) IQ (Wave 1)",      
                      
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_1_", filename), "1f) Trails A (Wave 1)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_1_", filename), "1g) Trails B (Wave 1)",
                      
               ifelse(grepl("CVLASF", toupper(filename)) & grepl("_1_", filename), "1h) CVLT Short Delay Free (Wave 1)",
               ifelse(grepl("CVLASC", toupper(filename)) & grepl("_1_", filename), "1i) CVLT Short Delay Cue (Wave 1)",
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_1_", filename), "1j) CVLT Long Delay Free (Wave 1)",
               ifelse(grepl("CVLALC", toupper(filename)) & grepl("_1_", filename), "1k) CVLT Long Delay Cue (Wave 1)",
               
               ifelse(grepl("COGCOM", toupper(filename)) & grepl("_1_", filename), "1l) Cognitive Composite (Wave 1)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_1_", filename), "1m) CVLT Composite (Wave 1)",
               ifelse(grepl("WASCOM", toupper(filename)) & grepl("_1_", filename), "1n) WAIS Composite (Wave 1)",
                      
               ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_2_", filename), "2a) Stroop Word (Wave 2)",
                      
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_2_", filename), "2b) Block Design (Wave 2)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_2_", filename), "2c) Digit Span (Wave 2)",
               
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_2_", filename), "2f) Trails A (Wave 2)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_2_", filename), "2g) Trails B (Wave 2)",
                      
               ifelse(grepl("CVLASF", toupper(filename)) & grepl("_2_", filename), "2h) CVLT Short Delay Free (Wave 2)",
               ifelse(grepl("CVLASC", toupper(filename)) & grepl("_2_", filename), "2i) CVLT Short Delay Cue (Wave 2)",
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_2_", filename), "2j) CVLT Long Delay Free (Wave 2)",
               ifelse(grepl("CVLALC", toupper(filename)) & grepl("_2_", filename), "2k) CVLT Long Delay Cue (Wave 2)",
                      
               ifelse(grepl("COGCOM", toupper(filename)) & grepl("_2_", filename), "2l) Cognitive Composite (Wave 2)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_2_", filename), "2m) CVLT Composite (Wave 2)",

               ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_12_", filename), "3a) Stroop Word (change)",
                      
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_12_", filename), "3b) Block Design (change)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_12_", filename), "3c) Digit Span (change)",
               
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_12_", filename), "3f) Trails A (change)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_12_", filename), "3g) Trails B (change)",
                      
               ifelse(grepl("CVLASF", toupper(filename)) & grepl("_12_", filename), "3h) CVLT Short Delay Free (change)",
               ifelse(grepl("CVLASC", toupper(filename)) & grepl("_12_", filename), "3i) CVLT Short Delay Cue (change)",
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_12_", filename), "3j) CVLT Long Delay Free (change)",
               ifelse(grepl("CVLALC", toupper(filename)) & grepl("_12_", filename), "3k) CVLT Long Delay Cue (change)",
                      
               ifelse(grepl("COGCOM", toupper(filename)) & grepl("_12_", filename), "3l) Cognitive Composite (change)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_12_", filename), "3m) CVLT Composite (change)",

               NA))))))))))))))))))))))))))))))))))))) %>%
  filter(grepl("ON", paramHeader)) %>% filter(!grepl("WITH", paramHeader)) %>%
  mutate(estRnd = round2(est, 2),
         seRnd = round2(se, 2),
         estTemp = ifelse(estRnd < 0 & nchar(estRnd) == 5, substr(estRnd, 1, nchar(estRnd)),
                          ifelse(estRnd >= 0 & nchar(estRnd) == 4, substr(estRnd, 1, nchar(estRnd)),
                                 ifelse(estRnd < 0 & nchar(estRnd) == 4, paste0(substr(estRnd, 1, nchar(estRnd)),"0"),
                                        ifelse(estRnd >= 0 & nchar(estRnd) == 3, paste0(substr(estRnd, 1, nchar(estRnd)),"0"),
                                               ifelse(estRnd == 0.00, "0.00",
                                                      ifelse(estRnd == -1.00, "-1.00",
                                                             ifelse(estRnd == 1.00, "1.00",
                                                                    ifelse(estRnd < -10.00, estRnd,
                                                                           ifelse(estRnd > 10.00, estRnd,
                                                                                  NA))))))))),
         seTemp = ifelse(seRnd < 0 & nchar(seRnd) == 5, substr(seRnd, 1, nchar(seRnd)),
                          ifelse(seRnd >= 0 & nchar(seRnd) == 4, substr(seRnd, 1, nchar(seRnd)),
                                 ifelse(seRnd < 0 & nchar(seRnd) == 4, paste0(substr(seRnd, 1, nchar(seRnd)),"0"),
                                        ifelse(seRnd >= 0 & nchar(seRnd) == 3, paste0(substr(seRnd, 1, nchar(seRnd)),"0"),
                                               ifelse(seRnd == 0.00, "0.00",
                                                      ifelse(seRnd == -1.00, "-1.00",
                                                             ifelse(seRnd == 1.00, "1.00",
                                                                    ifelse(seRnd < -10.00, seRnd,
                                                                           ifelse(seRnd > 10.00, seRnd,
                                                                                  NA))))))))),
         DisplayFull = ifelse(pval < .001, paste0("'",estTemp," (",seTemp, ")***"),
                               ifelse(pval < .01, paste0("'",estTemp," (",seTemp, ")**"),
                                      ifelse(pval < .025, paste0("'",estTemp," (",seTemp, ")*"),
                                             ifelse(pval < .05, paste0("'",estTemp," (",seTemp, ")."),
                                                    paste0("'",estTemp," (",seTemp, ")"))))),
         EstP = ifelse(pval < .001, paste0("'",estTemp,"***"),
                               ifelse(pval < .01, paste0("'",estTemp,"**"),
                                      ifelse(pval < .025, paste0("'",estTemp,"*"),
                                             ifelse(pval < .05, paste0("'",estTemp,"."),
                                                    paste0("'",estTemp," ")))))) %>%
  dplyr::select(filename, Column, Row, 
         DisplayFull, EstP, est, pval) %>%
  filter(!is.na(Row))

Estimates_13_Cog_WI_Unst <- Estimates_13_Unst %>%
  filter(grepl("Cog", Column)) %>%
  filter(grepl(")",Column)) %>%
  filter(grepl("WI",Column)) 
Estimates_13_Cog_BW_Unst <- Estimates_13_Unst %>%
  filter(grepl("Cog", Column)) %>%
  filter(grepl("",Column)) %>%
  filter(grepl("BW",Column)) 

Estimates_13_Can_WI_Unst <- Estimates_13_Unst %>%
  filter(grepl("Mj", Column)) %>%
  filter(grepl(")",Column)) %>%
  filter(grepl("WI",Column)) 
Estimates_13_Can_BW_Unst <- Estimates_13_Unst %>%
  filter(grepl("Mj", Column)) %>%
  filter(grepl("",Column)) %>%
  filter(grepl("BW",Column)) 

Estimates_13_Alc_WI_Unst <- Estimates_13_Unst %>%
  filter(grepl("Alc", Column)) %>%
  filter(grepl(")",Column)) %>%
  filter(grepl("WI",Column)) 
Estimates_13_Alc_BW_Unst <- Estimates_13_Unst %>%
  filter(grepl("Alc", Column)) %>%
  filter(grepl(")",Column))%>%
  filter(grepl("BW",Column))  

hist(Estimates_13_Cog_WI_Unst$pval)
hist(Estimates_13_Cog_BW_Unst$pval)
hist(Estimates_13_Can_WI_Unst$pval)
hist(Estimates_13_Can_BW_Unst$pval)
hist(Estimates_13_Alc_WI_Unst$pval)
hist(Estimates_13_Alc_BW_Unst$pval)

Estimates_13_Tab_Unst  <- Estimates_13_Unst %>%
  dplyr::select(filename, Column, Row, 
         DisplayFull, pval) %>%
  mutate(EstP = as.numeric(pval)) %>%
  group_by(Row, Column) %>%
  dplyr::mutate(EstP_M = mean(EstP, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(EstP_D = abs(EstP-EstP_M)) %>%
  group_by(Row, Column) %>%
  dplyr::mutate(EstP_D_Min = min(EstP_D, na.rm=T),
                EstP_D_Rank = rank(EstP_D_Min, ties.method = "random")) %>%
  ungroup() %>%
  filter(EstP_D_Rank == 1) %>%
  dplyr::select(Column, Row, DisplayFull) 

TableEstimates_13_Unst <- Estimates_13_Tab_Unst %>%
  filter(grepl(")",Column)) %>%
  spread(key = Column, value = DisplayFull) 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# G) Create Stdandardized Tables ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Estimates_13_Stdyx <- as_tibble(do.call("rbind", stdyx.standardized13))  %>%
  mutate(Column = ifelse(param == "COGPGS", "2a) WI Cog PGS",
                  ifelse(param == "COGPGSBW", "1a) BW Cog PGS",
                         
                  ifelse(param == "CANEVR", "2b) WI Mj Ever Used",
                  ifelse(param == "CANONS", "2c) WI Mj Onset",
                  ifelse(param == "CANFLF", "2d) WI Mj Lifetime Freq.",
                  ifelse(param == "CANF6M", "2e) WI Mj 6-Month Freq.",
                  ifelse(param == "CANAC6", "2f) WI Mj 6-Month Acc.",

                  ifelse(param == "CANEVRBW", "1b) BW Mj Ever Used",
                  ifelse(param == "CANONSBW", "1c) BW Mj Onset",
                  ifelse(param == "CANFLFBW", "1d) BW Mj Lifetime Freq.",
                  ifelse(param == "CANF6MBW", "1e) BW Mj 6-Month Freq.",
                  ifelse(param == "CANAC6BW", "1f) BW Mj 6-Month Acc.",

                  ifelse(param == "ALCEVR", "2i) WI Alc Ever Used",                                                                                    
                  ifelse(param == "ALCONS", "2j) WI Alc Onset",
                  ifelse(param == "ALCFLF", "2k) WI Alc Lifetime Freq.",
                  ifelse(param == "ALCF6M", "2l) WI Alc 6-Month Freq.",
                  ifelse(param == "ALCAC6", "2m) WI Alc 6-Month Acc.",

                  ifelse(param == "ALCEVRBW", "1i) BW Alc Ever Used",
                  ifelse(param == "ALCONSBW", "1j) BW Alc Onset",
                  ifelse(param == "ALCFLFBW", "1k) BW Alc Lifetime Freq.",
                  ifelse(param == "ALCF6MBW", "1l) BW Alc 6-Month Freq.",
                  ifelse(param == "ALCAC6BW", "1m) BW Alc 6-Month Acc.",
                         param)))))))))))))))))))))), 
         Row = ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_1_", filename), "1a) Stroop Word (Wave 1)",
               
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_1_", filename), "1b) Block Design (Wave 1)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_1_", filename), "1c) Digit Span (Wave 1)",
               ifelse(grepl("WASVCB", toupper(filename)) & grepl("_1_", filename), "1d) Vocabulary (Wave 1)",
               ifelse(grepl("WSISUM", toupper(filename)) & grepl("_1_", filename), "1e) IQ (Wave 1)",      
                      
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_1_", filename), "1f) Trails A (Wave 1)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_1_", filename), "1g) Trails B (Wave 1)",
                      
               ifelse(grepl("CVLASF", toupper(filename)) & grepl("_1_", filename), "1h) CVLT Short Delay Free (Wave 1)",
               ifelse(grepl("CVLASC", toupper(filename)) & grepl("_1_", filename), "1i) CVLT Short Delay Cue (Wave 1)",
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_1_", filename), "1j) CVLT Long Delay Free (Wave 1)",
               ifelse(grepl("CVLALC", toupper(filename)) & grepl("_1_", filename), "1k) CVLT Long Delay Cue (Wave 1)",
               
               ifelse(grepl("COGCOM", toupper(filename)) & grepl("_1_", filename), "1l) Cognitive Composite (Wave 1)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_1_", filename), "1m) CVLT Composite (Wave 1)",
               ifelse(grepl("WASCOM", toupper(filename)) & grepl("_1_", filename), "1n) WAIS Composite (Wave 1)",
                      
               ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_2_", filename), "2a) Stroop Word (Wave 2)",
                      
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_2_", filename), "2b) Block Design (Wave 2)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_2_", filename), "2c) Digit Span (Wave 2)",
               
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_2_", filename), "2f) Trails A (Wave 2)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_2_", filename), "2g) Trails B (Wave 2)",
                      
               ifelse(grepl("CVLASF", toupper(filename)) & grepl("_2_", filename), "2h) CVLT Short Delay Free (Wave 2)",
               ifelse(grepl("CVLASC", toupper(filename)) & grepl("_2_", filename), "2i) CVLT Short Delay Cue (Wave 2)",
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_2_", filename), "2j) CVLT Long Delay Free (Wave 2)",
               ifelse(grepl("CVLALC", toupper(filename)) & grepl("_2_", filename), "2k) CVLT Long Delay Cue (Wave 2)",
                      
               ifelse(grepl("COGCOM", toupper(filename)) & grepl("_2_", filename), "2l) Cognitive Composite (Wave 2)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_2_", filename), "2m) CVLT Composite (Wave 2)",

               ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_12_", filename), "3a) Stroop Word (change)",
                      
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_12_", filename), "3b) Block Design (change)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_12_", filename), "3c) Digit Span (change)",
               
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_12_", filename), "3f) Trails A (change)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_12_", filename), "3g) Trails B (change)",
                      
               ifelse(grepl("CVLASF", toupper(filename)) & grepl("_12_", filename), "3h) CVLT Short Delay Free (change)",
               ifelse(grepl("CVLASC", toupper(filename)) & grepl("_12_", filename), "3i) CVLT Short Delay Cue (change)",
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_12_", filename), "3j) CVLT Long Delay Free (change)",
               ifelse(grepl("CVLALC", toupper(filename)) & grepl("_12_", filename), "3k) CVLT Long Delay Cue (change)",
                      
               ifelse(grepl("COGCOM", toupper(filename)) & grepl("_12_", filename), "3l) Cognitive Composite (change)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_12_", filename), "3m) CVLT Composite (change)",

               NA))))))))))))))))))))))))))))))))))))) %>%
  filter(grepl("ON", paramHeader)) %>% filter(!grepl("WITH", paramHeader)) %>%
  mutate(estRnd = round2(est, 2),
         seRnd = round2(se, 2),
         estTemp = ifelse(estRnd < 0 & nchar(estRnd) == 5, substr(estRnd, 1, nchar(estRnd)),
                          ifelse(estRnd >= 0 & nchar(estRnd) == 4, substr(estRnd, 1, nchar(estRnd)),
                                 ifelse(estRnd < 0 & nchar(estRnd) == 4, paste0(substr(estRnd, 1, nchar(estRnd)),"0"),
                                        ifelse(estRnd >= 0 & nchar(estRnd) == 3, paste0(substr(estRnd, 1, nchar(estRnd)),"0"),
                                               ifelse(estRnd == 0.00, "0.00",
                                                      ifelse(estRnd == -1.00, "-1.00",
                                                             ifelse(estRnd == 1.00, "1.00",
                                                                    ifelse(estRnd < -10.00, estRnd,
                                                                           ifelse(estRnd > 10.00, estRnd,
                                                                                  NA))))))))),
         seTemp = ifelse(seRnd < 0 & nchar(seRnd) == 5, substr(seRnd, 1, nchar(seRnd)),
                          ifelse(seRnd >= 0 & nchar(seRnd) == 4, substr(seRnd, 1, nchar(seRnd)),
                                 ifelse(seRnd < 0 & nchar(seRnd) == 4, paste0(substr(seRnd, 1, nchar(seRnd)),"0"),
                                        ifelse(seRnd >= 0 & nchar(seRnd) == 3, paste0(substr(seRnd, 1, nchar(seRnd)),"0"),
                                               ifelse(seRnd == 0.00, "0.00",
                                                      ifelse(seRnd == -1.00, "-1.00",
                                                             ifelse(seRnd == 1.00, "1.00",
                                                                    ifelse(seRnd < -10.00, seRnd,
                                                                           ifelse(seRnd > 10.00, seRnd,
                                                                                  NA))))))))),
         DisplayFull = ifelse(pval < .001, paste0("'",estTemp," (",seTemp, ")***"),
                               ifelse(pval < .01, paste0("'",estTemp," (",seTemp, ")**"),
                                      ifelse(pval < .025, paste0("'",estTemp," (",seTemp, ")*"),
                                             ifelse(pval < .05, paste0("'",estTemp," (",seTemp, ")."),
                                                    paste0("'",estTemp," (",seTemp, ")"))))),
         EstP = ifelse(pval < .001, paste0("'",estTemp,"***"),
                               ifelse(pval < .01, paste0("'",estTemp,"**"),
                                      ifelse(pval < .025, paste0("'",estTemp,"*"),
                                             ifelse(pval < .05, paste0("'",estTemp,"."),
                                                    paste0("'",estTemp," ")))))) %>%
  dplyr::select(filename, Column, Row, 
         DisplayFull, EstP, est, pval) %>%
  filter(!is.na(Row))

Estimates_13_Cog_WI_Stdyx <- Estimates_13_Stdyx %>%
  filter(grepl("Cog", Column)) %>%
  filter(grepl(")",Column)) %>%
  filter(grepl("WI",Column)) 
Estimates_13_Cog_BW_Stdyx <- Estimates_13_Stdyx %>%
  filter(grepl("Cog", Column)) %>%
  filter(grepl("",Column)) %>%
  filter(grepl("BW",Column)) 
Estimates_13_Can_WI_Stdyx <- Estimates_13_Stdyx %>%
  filter(grepl("Mj", Column)) %>%
  filter(grepl(")",Column)) %>%
  filter(grepl("WI",Column)) 
Estimates_13_Can_BW_Stdyx <- Estimates_13_Stdyx %>%
  filter(grepl("Mj", Column)) %>%
  filter(grepl("",Column)) %>%
  filter(grepl("BW",Column)) 
Estimates_13_Alc_WI_Stdyx <- Estimates_13_Stdyx %>%
  filter(grepl("Alc", Column)) %>%
  filter(grepl(")",Column)) %>%
  filter(grepl("WI",Column)) 
Estimates_13_Alc_BW_Stdyx <- Estimates_13_Stdyx %>%
  filter(grepl("Alc", Column)) %>%
  filter(grepl(")",Column))%>%
  filter(grepl("BW",Column))  

hist(Estimates_13_Cog_WI_Stdyx$pval)
hist(Estimates_13_Cog_BW_Stdyx$pval)
hist(Estimates_13_Can_WI_Stdyx$pval)
hist(Estimates_13_Can_BW_Stdyx$pval)
hist(Estimates_13_Alc_WI_Stdyx$pval)
hist(Estimates_13_Alc_BW_Stdyx$pval)

Estimates_13_Tab_Stdyx  <- Estimates_13_Stdyx %>%
  dplyr::select(filename, Column, Row, 
         DisplayFull, pval) %>%
  mutate(EstP = as.numeric(pval)) %>%
  group_by(Row, Column) %>%
  dplyr::mutate(EstP_M = mean(EstP, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(EstP_D = abs(EstP-EstP_M)) %>%
  group_by(Row, Column) %>%
  dplyr::mutate(EstP_D_Min = min(EstP_D, na.rm=T),
                EstP_D_Rank = rank(EstP_D_Min, ties.method = "random")) %>%
  ungroup() %>%
  filter(EstP_D_Rank == 1) %>%
  dplyr::select(Column, Row, DisplayFull) 

TableEstimates_13_Stdyx <- Estimates_13_Tab_Stdyx %>%
  filter(grepl(")",Column)) %>%
  spread(key = Column, value = DisplayFull) 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# H) Export Tables ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
project <- "GADMJCOG"
Model <- "13_MltLvPGS"
date <- "190618"

write_csv(x = TableEstimates_13_Unst,
          na = "",
          path = paste0(getwd(), "/Work/Results/", Model, "_Unst_Filt_New2_", date,".csv"))

write_csv(x = TableEstimates_13_Stdyx,
          na = "",
          path = paste0(getwd(), "/Work/Results/", Model, "_Stdyx_Filt_New2_", date,".csv"))

