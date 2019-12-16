# HEADER ###############################################################
#      Program:   03_Clean.R
#      Project:   GADMJCOG
#      Tasks:      
                # A) Demographics
                # B) CogNeuro
                # C) SAM Supp (Drug Use)
                # D) GWAS
                # E) Merge
                # F) Export
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Libraries: GADMJCOG
     # Source:    
                # A) Demographics:  
                #                   GADD_CogNeuro
                #                   GADD_Supp_W1
                #                   GADD_Supp_W2
                # B) CogNeuro:      
                #                   GADD_CogNeuro
                # C) SAM Supp:      
                #                   GADD_Supp_W1
                #                   GADD_Supp_W2
                # D) GWAS:          
                #                   GADD_GWAS
      # Derived:   
                # E) Merge:         
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
        source("Work/01_Functions.R", echo = T)
        # source("Work/02_Load.R", echo = T)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      Correction from correspondence with R. Corley
#         FIX: 45-D12020321 
#         w1wstott (WISC/WASI sum): 47  <- should be 97 
#         w1estiq: 97
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   A) Demographics #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#   1) Wave 1 #####
GADD_Demo_Wave1a <- GADD_CogNeuro %>%
  dplyr::select(NIDAID,
         project, afamily, id, nage1, old_age1, nsex1, testyr1, testtype1, wave1,
         exclude1, clinical1, havedna1, comments1, hispanic1, racecat1) %>% 
  dplyr::rename(project1 = project, 
                afamily1 = afamily, 
                id1 = id, 
                sex1 = nsex1) 
names(GADD_Demo_Wave1a) <- c(names(GADD_Demo_Wave1a)[1],
                            substring(names(GADD_Demo_Wave1a[2:dim(GADD_Demo_Wave1a)[2]]), 
                                      1, 
                                      nchar(names(GADD_Demo_Wave1a[2:dim(GADD_Demo_Wave1a)[2]]))-1))

GADD_Demo_Wave1 <- GADD_Demo_Wave1a %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr), 
         Wave = 1,
         male = ifelse(sex == 1, 1, 
                       ifelse(sex == 0, 0, NA))) %>%
  dplyr::select(-sex, -NIDAID, -wave)

# Drop Duplicates
GADD_Supp_ForDemoMerge_W1 <- GADD_Supp_W1 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID),
         testyr = TESTYR) %>%
  dplyr::select(MyNIDAID, testyr)
GADD_Supp_ForDemoMerge_W1_Duplicates <- GADD_Supp_ForDemoMerge_W1 %>% 
  group_by(MyNIDAID) %>%
  filter(n()>2)
GADD_Demo_Wave1_ForMerge <- list(GADD_Supp_ForDemoMerge_W1, GADD_Demo_Wave1) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave))

#   2) Wave 2 #####
GADD_Demo_Wave2a <- GADD_CogNeuro %>%
  dplyr::select(NIDAID,
         project2, afamily2, id2, nage2, old_age2, nsex2, testyr2, testtype2, wave2,
         exclude2, clinical2, havedna2, comments2, hispanic2, racecat2)
names(GADD_Demo_Wave2a) <- c(names(GADD_Demo_Wave2a)[1],
                            substring(names(GADD_Demo_Wave2a[2:dim(GADD_Demo_Wave2a)[2]]), 
                                      1, 
                                      nchar(names(GADD_Demo_Wave2a[2:dim(GADD_Demo_Wave2a)[2]]))-1))

GADD_Demo_Wave2 <- GADD_Demo_Wave2a %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr), 
         Wave = 2,
         male = ifelse(nsex == 1, 1, 
                       ifelse(nsex == 0, 0, NA))) %>%
  dplyr::select(-nsex, -NIDAID, -wave)

# Drop Duplicates
GADD_Supp_ForDemoMerge_W2 <- GADD_Supp_W2 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID),
         testyr = TESTYR) %>%
  dplyr::select(MyNIDAID, testyr)
GADD_Supp_ForDemoMerge_W2_Duplicates <- GADD_Supp_ForDemoMerge_W2 %>% 
  group_by(MyNIDAID) %>%
  filter(n()>2)
GADD_Demo_Wave2_ForMerge <- list(GADD_Supp_ForDemoMerge_W2, GADD_Demo_Wave2) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave))

#   3) Merge Waves #####         
GADD_Demo_BothWaves <- list(GADD_Demo_Wave1_ForMerge,
                            GADD_Demo_Wave2_ForMerge) %>%
  reduce(full_join) %>%
  dplyr::select(MyNIDAID, Wave, testyr, male, 
         project:racecat)

# Includ Pre-Morbid IQ
GADD_Demo_PreIQ <- GADD_CogNeuro %>%
  dplyr::select(NIDAID, w1rpiatr) %>% 
  dplyr::mutate(MyNIDAID = trim(NIDAID),
                PRE_FX = as.numeric(miss999(w1rpiatr))) %>%
  dplyr::select(MyNIDAID:PRE_FX) %>%
  filter(!is.na(PRE_FX))
GADD_Demo_PreIQ_ForMerge <- list(GADD_Supp_ForDemoMerge_W1, GADD_Demo_PreIQ) %>%
  reduce(inner_join) %>%
  dplyr::select(-testyr)

GADD_Demo_ForMerge <- list(GADD_Demo_BothWaves,
                           GADD_Demo_PreIQ_ForMerge) %>%
  reduce(left_join)

# Check Duplicates/Triplicates: n = 0
# GADD_Demo_Duplicates <- GADD_Demo_ForMerge %>%
#   group_by(MyNIDAID) %>%
#   filter(n()>2)
# dim(GADD_Demo_Duplicates)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   B) CogNeuro #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#   1) Wave 1 #####
GADD_Neuro_Wave1 <- GADD_CogNeuro %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr1), 
         Wave = 1, 
         Neuro = "Neuro1",
         # Stroop
         SRPBLK = as.numeric(miss999(w1strpwdts)), # Stroop Black
         SRPCLR = as.numeric(miss999(w1strpcots)), # Stroop Color
         SRPWCR = as.numeric(miss999(w1strpcwts))-SRPCLR, # Stroop Word/Color
         # WAIS/WASI/etc
         WASVCB = as.numeric(ifelse(!is.na(w1wsvoss), miss999(w1wsvoss), #WASI
                              ifelse(!is.na(w1wavoss), 
                                     rescale(x = miss999(w1wavoss),
                                             mean = mean(miss999(w1wsvoss), na.rm=T), 
                                             sd = sd(miss999(w1wsvoss), na.rm=T), 
                                             df = F), #wAIS
                              ifelse(!is.na(w1wivoss), 
                                     rescale(x = miss999(w1wivoss),
                                             mean = mean(miss999(w1wsvoss), na.rm=T), 
                                             sd = sd(miss999(w1wsvoss), na.rm=T), 
                                             df = F), #WISC
                                     NA)))), #VOCAB
         WASBLK = as.numeric(ifelse(!is.na(w1wsblss), miss999(w1wsblss), #WASI
                              ifelse(!is.na(w1wablss), 
                                     rescale(x = miss999(w1wablss),
                                             mean = mean(miss999(w1wsblss), na.rm=T), 
                                             sd = sd(miss999(w1wsblss), na.rm=T), 
                                             df = F), #wAIS
                              ifelse(!is.na(w1wiblss), 
                                     rescale(x = miss999(w1wiblss),
                                             mean = mean(miss999(w1wsblss), na.rm=T), 
                                             sd = sd(miss999(w1wsblss), na.rm=T), 
                                             df = F), #WISC
                                     NA)))), #Block
         WASDGT = as.numeric(miss999(w1wadiss)), #### DIGIT ###
         WSISUM = as.numeric(miss999(w1wstott)), #### SUM ###
         #### TRAILS ###
         TRLATS = as.numeric(miss999(w1trlats)), #### A ###
         TRLBTS = as.numeric(miss999(w1trlbts)), #### B ###
         #### CVLT ###
         CVLASF = as.numeric(miss999(w1cvlt10)),
         CVLASC = as.numeric(miss999(w1cvlt12)),
         CVLALF = as.numeric(miss999(w1cvlt14)),
         CVLALC = as.numeric(miss999(w1cvlt16))) %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, SRPBLK:CVLALC) %>%
  dplyr::mutate(WSISUM = ifelse(MyNIDAID == "45-D12020321", 97, WSISUM))
# FIX: 45-D12020321 # w1wstott (WISC/WASI sum): 47 # w1estiq: 97

# Drop Duplicates
GADD_Neuro_Wave1_ForMerge <- list(GADD_Supp_ForDemoMerge_W1,
                                  GADD_Neuro_Wave1) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave)) %>%
  dplyr::mutate(SRPWCRz = as.numeric(rescale(SRPWCR, mean=0, sd=1, df = F)), 
                
                WASVCBz = as.numeric(rescale(WASVCB, mean=0, sd=1, df = F)), 
                WSISUMz = as.numeric(rescale(WSISUM, mean=0, sd=1, df = F)),  
                
                WASBLKz = as.numeric(rescale(WASBLK, mean=0, sd=1, df = F)), 
                WASDGTz = as.numeric(rescale(WASDGT, mean=0, sd=1, df = F)), 
                
                TRLATSz = as.numeric(rescale(TRLATS, mean=0, sd=1, df = F)),  
                TRLBTSz = as.numeric(rescale(TRLBTS, mean=0, sd=1, df = F)),
                
                CVLASFz = as.numeric(rescale(CVLASF, mean=0, sd=1, df = F)),
                CVLASCz = as.numeric(rescale(CVLASC, mean=0, sd=1, df = F)),
                CVLALFz = as.numeric(rescale(CVLALF, mean=0, sd=1, df = F)),
                CVLALCz = as.numeric(rescale(CVLALC, mean=0, sd=1, df = F))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(MISS = sum(is.na(SRPWCRz), is.na(WASBLKz), is.na(WASDGTz), 
                           is.na(TRLATSz), is.na(TRLBTSz)),
                WASCOM = mean(x = c(WASVCBz, WSISUMz), na.rm = F), 
                COGCOM = ifelse(MISS>1,NA,
                                mean(x = c(SRPWCRz, WASBLKz, WASDGTz, TRLATSz, TRLBTSz), na.rm = T)),
                CVLCOM = mean(x = c(CVLASFz, CVLASCz, CVLALFz, CVLALCz), na.rm = F)) %>%
  dplyr::ungroup() %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, 
         SRPBLK:CVLALC, 
         WASCOM, COGCOM, CVLCOM)

#   2) Wave 2 #####
GADD_Neuro_Wave2 <- GADD_CogNeuro %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr2), 
         Wave = 2, 
         Neuro = "Neuro2",
          # STROOP
         SRPBLK = as.numeric(miss999(w2strpwdts)),
         SRPCLR = as.numeric(miss999(w2strpcots)),
         SRPWCR = as.numeric(miss999(w2strpcwts))-SRPCLR,
          # WAIS
         WASBLK = as.numeric(miss999(w2wsblss)),
         WASDGT = as.numeric(miss999(w2wadiss)),

         TRLATS = as.numeric(miss999(w2trlats)),
         TRLBTS = as.numeric(miss999(w2trlbts)),
         
         CVLASF = as.numeric(miss999(w2cvlt27)),
         CVLASC = as.numeric(miss999(w2cvlt28)),
         CVLALF = as.numeric(miss999(w2cvlt29)),
         CVLALC = as.numeric(miss999(w2cvlt30))) %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, SRPBLK:CVLALC)

# Drop Duplicates
GADD_Neuro_Wave2_ForMerge <- list(GADD_Supp_ForDemoMerge_W2, GADD_Neuro_Wave2) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave)) %>%
  dplyr::mutate(SRPWCRz = as.numeric(rescale(SRPWCR, mean=0, sd=1, df = F)), 
                
                WASBLKz = as.numeric(rescale(WASBLK, mean=0, sd=1, df = F)), 
                WASDGTz = as.numeric(rescale(WASDGT, mean=0, sd=1, df = F)), 
                
                TRLATSz = as.numeric(rescale(TRLATS, mean=0, sd=1, df = F)),  
                TRLBTSz = as.numeric(rescale(TRLBTS, mean=0, sd=1, df = F)),
                
                CVLASFz = as.numeric(rescale(CVLASF, mean=0, sd=1, df = F)),
                CVLASCz = as.numeric(rescale(CVLASC, mean=0, sd=1, df = F)),
                CVLALFz = as.numeric(rescale(CVLALF, mean=0, sd=1, df = F)),
                CVLALCz = as.numeric(rescale(CVLALC, mean=0, sd=1, df = F))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(MISS = sum(is.na(SRPWCRz), is.na(WASBLKz), is.na(WASDGTz), 
                           is.na(TRLATSz), is.na(TRLBTSz)),
                COGCOM = ifelse(MISS>1,NA,
                                mean(x = c(SRPWCRz, WASBLKz, WASDGTz, TRLATSz, TRLBTSz), na.rm = T)),
                CVLCOM = mean(x = c(CVLASFz, CVLASCz, CVLALFz, CVLALCz), na.rm = F)) %>%
  dplyr::ungroup() %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, 
         SRPBLK:CVLALC, 
         COGCOM, CVLCOM)

# ??? many 0s for wsblrw @ W2 but not W1 ####
#   3) Merge Waves #####
GADD_Neuro_ForMerge <- list(GADD_Neuro_Wave1_ForMerge, 
                            GADD_Neuro_Wave2_ForMerge) %>%
  reduce(full_join) %>%
  dplyr::select(-testyr)

# Check Duplicates/Triplicates: n = 0
# GADD_Neuro_Duplicates <- GADD_Neuro_ForMerge %>% 
#   group_by(MyNIDAID) %>% 
#   filter(n()>2)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   C) SAM Supplement #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#   1) Wave 1 #####
GADD_SuppW1_ForMerge <- GADD_Supp_W1 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         Wave = 1, 
         SAM = "SAM1",
         # Cannabis
         SAMX3A = as.character(SAMX3A),
         SAMX3C = as.numeric(SAMX3C),
         SAMX3D = as.character(SAMX3D), 
         SAMX3E = as.character(SAMX3E), 
         SAMX3F = as.numeric(SAMX3F),
         # Alcohol
         SAMX2A = as.character(SAMX2A), 
         SAMX2C = as.numeric(SAMX2C), 
         SAMX2D = as.character(SAMX2D), 
         SAMX2E = as.character(SAMX2E), 
         SAMX2F = as.numeric(SAMX2F)) %>%
  dplyr::select(MyNIDAID, Wave, SAM, 
         # Cannabis
         SAMX3A, SAMX3C, 
         SAMX3D, SAMX3E, SAMX3F,
         # Alcohol
         SAMX2A, SAMX2C, 
         SAMX2D, SAMX2E, SAMX2F) 

#   2) Wave 2 #####
GADD_SuppW2_ForMerge <- GADD_Supp_W2 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         Wave = 2, 
         SAM = "SAM2",
         # Cannabis
         SAMX3A = as.character(SAMX3A), 
         SAMX3C = as.numeric(SAMX3C),
         SAMX3D = as.character(SAMX3D), 
         SAMX3E = as.character(SAMX3E), 
         SAMX3F = as.numeric(SAMX3F),
         # Alcohol
         SAMX2A = as.character(SAMX2A), 
         SAMX2C = as.numeric(SAMX2C),
         SAMX2D = as.character(SAMX2D), 
         SAMX2E = as.character(SAMX2E), 
         SAMX2F = as.numeric(SAMX2F)) %>%
  dplyr::select(MyNIDAID, Wave, SAM, 
         # Cannabis
         SAMX3A, SAMX3C, 
         SAMX3D, SAMX3E, SAMX3F,
         # Alcohol
         SAMX2A, SAMX2C, 
         SAMX2D, SAMX2E, SAMX2F
         ) 

#   3) Merge Waves #####
GADD_SAMSUP_MergedWaves <- list(GADD_SuppW1_ForMerge, 
                                GADD_SuppW2_ForMerge) %>%
  reduce(full_join)  %>%
  dplyr::mutate(CANEVR = ifelse(grepl("Yes", SAMX3A), 1,
                         ifelse(grepl("No", SAMX3A), 0, 
                         NA)),
                CANONS = ifelse(SAMX3C >= 99, NA, 
                                ifelse(SAMX3C < 9, 9, SAMX3C)), 
                CANFLF = ifelse(CANEVR == 0 | SAMX3D == "0" | grepl("never",tolower(SAMX3D)), 0,
                         ifelse(grepl("1-2", SAMX3D), 1.5,
                         ifelse(grepl("3-5", SAMX3D), 4,
                         ifelse(grepl("6-9", SAMX3D), 7.5,
                         ifelse(grepl("10-19", SAMX3D), 15,
                         ifelse(grepl("20-39", SAMX3D), 30,
                         ifelse(grepl("40 or more", SAMX3D), 60,
                         NA))))))),
                CANFTY = ifelse(CANEVR == 0 | SAMX3E == "0" | grepl("never",tolower(SAMX3E)), 0,
                         ifelse(grepl("less than once a month", tolower(SAMX3E)), 0.5,
                         ifelse(grepl("once a month", tolower(SAMX3E)), 1.0,
                         ifelse(grepl("2 or more times a month", tolower(SAMX3E)), 3.0,
                         ifelse(grepl("once a week", tolower(SAMX3E)), round(52/12,2),
                         ifelse(grepl("2 or more times a week", tolower(SAMX3E)), round(52/12*2,2),
                         ifelse(grepl("once a day", tolower(SAMX3E)), round(52/12*7,2),
                                NA))))))), 
                CANF6M = round(ifelse(!is.na(SAMX3F), SAMX3F, 
                               ifelse((grepl("no", tolower(SAMX3A)) | 
                                       grepl("never",tolower(SAMX3D)) |
                                       SAMX3D == "0"), 0,
                                      SAMX3F))/6,2),
                ALCEVR = ifelse(grepl("Yes", SAMX2A), 1,
                         ifelse(grepl("No", SAMX2A), 0, 
                         NA)),
                ALCONS = ifelse(SAMX2C >= 99, NA, 
                                ifelse(SAMX2C < 9, 9, SAMX2C)), 
                ALCFLF = ifelse(ALCEVR == 0 | SAMX2D == "0" | grepl("never",tolower(SAMX2D)), 0,
                         ifelse(grepl("1-2", SAMX2D), 1.5,
                         ifelse(grepl("3-5", SAMX2D), 4,
                         ifelse(grepl("6-9", SAMX2D), 7.5,
                         ifelse(grepl("10-19", SAMX2D), 15,
                         ifelse(grepl("20-39", SAMX2D), 30,
                         ifelse(grepl("40 or more", SAMX2D), 60,
                         NA))))))),
                ALCFTY = ifelse(SAMX2E == "0" | grepl("never",tolower(SAMX2E)), NA,
                         ifelse(SAMX2E == "Less than once a month", 0.5,
                         ifelse(SAMX2E == " Once a month", 1.0,
                         ifelse(SAMX2E == "2 or more times a month", 3.0,
                         ifelse(SAMX2E == "Once a week", round(52/12,2),
                         ifelse(SAMX2E == "2 or more times a week", round(52/12*2,2),
                         ifelse(grepl("once a day", tolower(SAMX2E)), round(52/12*7,2),
                         NA))))))), 
                ALCF6M = round(ifelse(!is.na(SAMX2F), SAMX2F, 
                               ifelse((grepl("no", tolower(SAMX2A)) | 
                                       grepl("never",tolower(SAMX2D)) |
                                       SAMX2D == "0"), 0,
                                      SAMX2F))/6,2)) %>%
  dplyr::select(MyNIDAID, Wave, SAM,
                CANEVR, CANONS, CANFLF, CANFTY, CANF6M, SAMX3E,
                ALCEVR, ALCONS, ALCFLF, ALCFTY, ALCF6M, SAMX2E)

GADD_SAMSUP_MergedWaves_20021 <- GADD_SAMSUP_MergedWaves %>% 
  dplyr::filter(grepl("44-S00020021", MyNIDAID)) 

GADD_SAMSUP_ForMerge <- GADD_SAMSUP_MergedWaves %>%
  dplyr::select(MyNIDAID, Wave, SAM, 
                CANEVR, CANONS, 
                CANFLF, CANFTY, CANF6M, 
                ALCEVR, ALCONS, 
                ALCFLF, ALCFTY, ALCF6M)


# Check Duplicates/Triplicates
# GADD_SAM_Duplicates <- GADD_SAMSUP_ForMerge %>% 
#   group_by(MyNIDAID) %>% 
#   filter(n()>2)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   D) GWAS Data #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
GADD_GWAS_ForMerge <- GADD_GWAS %>%
  mutate(MyNIDAID = trim(nidaid),
         GWAS = 1) %>%
  group_by(MyNIDAID) %>%
  filter(n()==1) %>%
  dplyr::select(MyNIDAID, GWAS)
dim(GADD_GWAS)    




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   E) Merge Scales #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_Merged <- list(GADD_Demo_ForMerge, 
                    GADD_Neuro_ForMerge,
                    GADD_SAMSUP_ForMerge, 
                    GADD_GWAS_ForMerge) %>%
  reduce(full_join)

GADD_Clean <- GADD_Merged %>%
  filter(substr(MyNIDAID,1,2) != "43") %>%
  dplyr::mutate(FILTER44 = ifelse(substr(MyNIDAID,1,2) == "44", 1, 0), 
                FILTER45 = ifelse(substr(MyNIDAID,1,2) == "45", 1, 0), 
                FILTER = ifelse(FILTER44 == 1 | FILTER45 == 1, 1, 0)) %>%
  dplyr::mutate(DEMYER = testyr, 
                DEMMAL = male, 
                DEMPRJ = project, 
                DEMFAM = afamily, 
                DEMAGE = round(nage, 2),
                DEMHSP = hispanic, 
                DEMRAC = racecat,
                NeuroW = ifelse(Neuro == "Neuro1", 1, 
                         ifelse(Neuro == "Neuro2", 2, NA)),
                SAMW = ifelse(SAM == "SAM1", 1, 
                       ifelse(SAM == "SAM2", 2, NA)),
                ID_Cty = ifelse(FILTER == 1, 
                                substr(MyNIDAID,4,4), 
                                substr(MyNIDAID,nchar(MyNIDAID),nchar(MyNIDAID))),
                ID_Fam = ifelse(FILTER == 1, 
                                as.numeric(substr(MyNIDAID,5,8)),
                                as.numeric(substr(MyNIDAID,2,4))),
                ID_Rel = ifelse(FILTER == 1, 
                                substr(MyNIDAID,9,10),
                                substr(MyNIDAID,5,6)),
                ID_MPL = ifelse(FILTER == 1, 
                                as.numeric(substr(MyNIDAID,5,nchar(MyNIDAID))),
                                as.numeric(substr(MyNIDAID,2,nchar(MyNIDAID)-1))),
                CANYRS = ifelse(CANEVR == 0, 0, DEMAGE - CANONS), 
                ALCYRS = ifelse(ALCEVR == 0, 0, DEMAGE - ALCONS)) %>%
  dplyr::select(ID_MPL, ID_Fam, ID_Rel, ID_Cty, 
         Wave, NeuroW, SAMW, 
         DEMYER:DEMRAC,
         PRE_FX,
         SRPBLK:CVLCOM,
         CANEVR, CANONS, CANFLF, CANFTY, CANF6M, 
         ALCEVR, ALCONS, ALCFLF, ALCFTY, ALCF6M, 
         CANYRS, ALCYRS,
         MyNIDAID, FILTER, GWAS)

GADD_Sibs <- GADD_Clean %>%
  filter(FILTER == 1) %>%
  filter(ID_Rel == "00" |
           ID_Rel == "03" |
           ID_Rel == "04")

GADD_DemSibs <- GADD_Sibs %>% 
  dplyr::group_by(ID_MPL) %>%
  dplyr::mutate(Rank = rank(ID_MPL, ties.method = "first"))%>%
  dplyr::ungroup() %>%
  dplyr::filter(Rank==1) %>%
  dplyr::select(MyNIDAID, ID_MPL:ID_Cty, Wave, 
                DEMYER:DEMRAC) %>%
  filter(ID_Rel == "00" |
           ID_Rel == "03" |
           ID_Rel == "04")

library(plyr)
GADD_Change <- GADD_Sibs %>%
  arrange(ID_MPL, Wave) %>%
  dplyr::mutate(SRPWCR1x = ifelse(!is.na(SRPWCR), ddply(.,.(ID_MPL), transform, x = SRPWCR - SRPWCR[1])$x, NA),
                WASBLK1x = ifelse(!is.na(WASBLK), ddply(.,.(ID_MPL), transform, x = WASBLK - WASBLK[1])$x, NA),
                WASDGT1x = ifelse(!is.na(WASDGT), ddply(.,.(ID_MPL), transform, x = WASDGT - WASDGT[1])$x, NA),
                TRLATS1x = ifelse(!is.na(TRLATS), ddply(.,.(ID_MPL), transform, x = TRLATS - TRLATS[1])$x, NA),
                TRLBTS1x = ifelse(!is.na(TRLBTS), ddply(.,.(ID_MPL), transform, x = TRLBTS - TRLBTS[1])$x, NA),
                CVLASF1x = ifelse(!is.na(CVLASF), ddply(.,.(ID_MPL), transform, x = CVLASF - CVLASF[1])$x, NA),
                CVLASC1x = ifelse(!is.na(CVLASC), ddply(.,.(ID_MPL), transform, x = CVLASC - CVLASC[1])$x, NA),
                CVLALF1x = ifelse(!is.na(CVLALF), ddply(.,.(ID_MPL), transform, x = CVLALF - CVLALF[1])$x, NA),
                CVLALC1x = ifelse(!is.na(CVLALC), ddply(.,.(ID_MPL), transform, x = CVLALC - CVLALC[1])$x, NA),
                COGCOM1x = ifelse(!is.na(COGCOM), ddply(.,.(ID_MPL), transform, x = COGCOM - COGCOM[1])$x, NA),
                CVLCOM1x = ifelse(!is.na(CVLCOM), ddply(.,.(ID_MPL), transform, x = CVLCOM - CVLCOM[1])$x, NA))
detach("package:plyr", unload=TRUE)
  
GADD_CrossSectional_1 <- GADD_Change %>%
  dplyr::filter(Wave == 1) %>%
  dplyr::select(ID_MPL, DEMAGE, 
                CANONS, CANEVR, CANFLF, 
                ALCONS, ALCEVR, ALCFLF) 
names(GADD_CrossSectional_1) <- c(names(GADD_CrossSectional_1)[1],
                                  paste0(names(GADD_CrossSectional_1)[2:length(names(GADD_CrossSectional_1))],"01"))
GADD_CrossSectional_2 <- GADD_Change %>%
  dplyr::filter(Wave == 2) %>%
  dplyr::select(ID_MPL, DEMAGE, 
                CANONS, CANEVR, CANFLF,
                ALCONS, ALCEVR, ALCFLF) 
names(GADD_CrossSectional_2) <- c(names(GADD_CrossSectional_2)[1],
                                  paste0(names(GADD_CrossSectional_2)[2:length(names(GADD_CrossSectional_2))],"02"))
  
GADD_CrossSectional <- list(GADD_CrossSectional_1, 
                            GADD_CrossSectional_2) %>%
  purrr::reduce(full_join) %>%
  dplyr::mutate(CANONS01 = ifelse(is.na(DEMAGE01) | is.na(CANEVR01), CANONS01, 
                                  ifelse(CANEVR01 == 1 & DEMAGE01 < CANONS01, DEMAGE01, CANONS01)),
                CANONS02 = ifelse(is.na(DEMAGE01) | is.na(CANEVR01) | is.na(CANONS01), CANONS02, 
                                  ifelse(DEMAGE01 < CANONS02 & CANEVR01 == 1, CANONS01, 
                                         ifelse(is.na(CANONS02), CANONS01, 
                                                CANONS02))),
                CANEVR02 = ifelse(!is.na(CANEVR01) & CANEVR01>CANEVR02, CANEVR01, 
                                  ifelse(is.na(CANEVR02) & CANEVR01 == 1, CANEVR01, 
                                         CANEVR02)),
                CANFLF02 = ifelse(!is.na(CANFLF01) & CANFLF01>CANFLF02, CANFLF01, 
                                   ifelse(is.na(CANFLF02), CANFLF01, 
                                          CANFLF02)),
                
                ALCONS01 = ifelse(is.na(DEMAGE01) | is.na(ALCEVR01), ALCONS01, 
                                  ifelse(ALCEVR01 == 1 & DEMAGE01 < ALCONS01, DEMAGE01, ALCONS01)),
                ALCONS02 = ifelse(is.na(DEMAGE01) | is.na(ALCEVR01) | is.na(ALCONS01), ALCONS02, 
                                  ifelse(DEMAGE01 < ALCONS02 & ALCEVR01 == 1, ALCONS01, 
                                         ifelse(is.na(ALCONS02), ALCONS01, 
                                                ALCONS02))),
                ALCEVR02 = ifelse(!is.na(ALCEVR01) & ALCEVR01>ALCEVR02, ALCEVR01, 
                                  ifelse(is.na(ALCEVR02) & ALCEVR01 == 1, ALCEVR01, 
                                         ALCEVR02)),
                ALCFLF02 = ifelse(!is.na(ALCFLF01) & ALCFLF01>ALCFLF02, ALCFLF01,
                                  ifelse(is.na(ALCFLF02), ALCFLF01, ALCFLF02))) %>%
  dplyr::select(ID_MPL,CANFLF01,
                CANONS01, CANONS02, CANEVR02, CANFLF02,
                ALCONS01, ALCONS02, ALCEVR02, ALCFLF02)

GADD_Long_Change <- GADD_Change %>%
  dplyr::select(ID_MPL:DEMRAC, 
                SRPWCR1x:CVLCOM1x) %>%
  gather(variable,value,SRPWCR1x:CVLCOM1x) %>%
  dplyr::mutate(Wave = as.numeric(paste0(substr(variable,7,7), Wave)),
                variable = substr(variable,1,6)) %>%
  dplyr::filter(!is.na(Wave) & !is.na(value)) %>%
  filter(Wave != 11) %>%
  spread(variable, value) 
                
GADD_SibsCross <- list(GADD_Sibs, 
                       GADD_CrossSectional) %>%
  purrr::reduce(full_join) 

GADD_SibsCross_W1W2 <- GADD_SibsCross %>%
  dplyr::mutate(CANONS = ifelse(Wave == 1, CANONS01, CANONS),
                CANONS = ifelse(Wave == 2, CANONS02, CANONS),
                CANEVR = ifelse(Wave == 2, CANEVR02, CANEVR),
                CANFLF = ifelse(Wave == 2, CANFLF02, CANFLF),
                ALCONS = ifelse(Wave == 1, ALCONS01, ALCONS),
                ALCONS = ifelse(Wave == 2, ALCONS02, ALCONS),
                ALCEVR = ifelse(Wave == 2, ALCEVR02, ALCEVR),
                ALCFLF = ifelse(Wave == 2, ALCFLF02, ALCFLF)) %>%
  dplyr::select(names(GADD_Sibs))

GADD_SibsCross_12 <- GADD_SibsCross_W1W2 %>%
  dplyr::mutate(CANYRStemp = ifelse(Wave == 2, CANYRS, NA),
                ALCYRStemp = ifelse(Wave == 2, ALCYRS, NA)) %>%
  dplyr::group_by(ID_MPL) %>%
  dplyr::mutate(CANONS = min(CANONS, na.rm=F),
                CANEVR = max(CANEVR, na.rm=T),
                CANFLF = max(CANFLF, na.rm=T),
                CANFTY = mean(CANFTY, na.rm=F),
                CANF6M = mean(CANF6M, na.rm=F),
                CANYRS = mean(CANYRStemp, na.rm=T),
                
                ALCONS = min(ALCONS, na.rm=F),
                ALCEVR = max(ALCEVR, na.rm=T),
                ALCFLF = max(ALCFLF, na.rm=T),
                ALCFTY = mean(ALCFTY, na.rm=F),
                ALCF6M = mean(ALCF6M, na.rm=F),
                ALCYRS = mean(ALCYRStemp, na.rm=T),
                FilterID = rank(ID_MPL, ties.method = "random")) %>%
  dplyr::mutate(CANONS = ifelse(is.infinite(CANONS), NA, CANONS), 
                ALCONS = ifelse(is.infinite(ALCONS), NA, ALCONS),
                
                CANEVR = ifelse(is.infinite(CANEVR), NA, CANEVR), 
                ALCEVR = ifelse(is.infinite(ALCEVR), NA, ALCEVR),
                
                CANFLF = ifelse(is.infinite(CANFLF), NA, CANFLF), 
                ALCFLF = ifelse(is.infinite(ALCFLF), NA, ALCFLF)) %>%
  dplyr::filter(FilterID == 1) %>% 
  dplyr::mutate(Wave = 12) %>%
  dplyr::select(ID_MPL:Wave, PRE_FX,
                CANONS, CANEVR, CANFLF, CANFTY, CANF6M, CANYRS,
                ALCONS, ALCEVR, ALCFLF, ALCFTY, ALCF6M, ALCYRS,
                MyNIDAID) 

GADD_Sibs_Long <- list(GADD_Long_Change, 
                       GADD_SibsCross_12) %>%
  purrr::reduce(full_join)

GADD_Long_Full_1 <- list(GADD_SibsCross_W1W2, 
                         GADD_Sibs_Long) %>%
  reduce(full_join) %>%
  arrange(ID_MPL, Wave)  

dim(GADD_Sibs)
table(GADD_Sibs$Wave)
dim(GADD_Long_Full_1)
table(GADD_Long_Full_1$Wave)
  
GADD_SibsCross_22 <- GADD_SibsCross_12 %>%
  dplyr::mutate(Wave = 22) 
GADD_Long_Change_22 <- GADD_SibsCross %>%
  dplyr::filter(Wave ==2) %>%
  dplyr::mutate(Wave = 22) %>%
  dplyr::select(names(GADD_Long_Change))

GADD_Sibs_Long_22 <- list(GADD_Long_Change_22, 
                          GADD_SibsCross_22) %>%
  purrr::reduce(full_join)

GADD_Long_Full <- list(GADD_Long_Full_1,
                       GADD_Sibs_Long_22) %>%
  reduce(full_join) %>%
  arrange(ID_MPL, Wave)

GADD_Sibs_Long_12_Check <- GADD_Sibs_Long_12 %>%
  select(ID_MPL, WASDGT, CANEVR, DEMAGE, DEMMAL)
GADD_Sibs_Long_22_Check <- GADD_Sibs_Long_22 %>%
  select(ID_MPL, WASDGT, CANEVR, DEMAGE, DEMMAL)

dim(GADD_Sibs)
table(GADD_Sibs$Wave)
dim(GADD_Long_Full)
table(GADD_Long_Full$Wave)



