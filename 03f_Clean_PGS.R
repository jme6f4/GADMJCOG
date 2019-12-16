# HEADER ###############################################################
#     Program:   03f_Clean_PGS.R
#     Project:   GADMJCOG
#     Tasks:      
#                 A) Import PGS (*.profile)
#                 B) Merge PGS + Other Data
#                 C) Validate
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Libraries: GADMJCOG
#     Source:    
#                 GADD_Sibs
#                 GADD_PGS_Target
#     Derived:   
#                 Validation Plots
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      source("Work/01_Functions.R", echo = T)
      source("Work/03_Clean.R", echo = T)
      source("Work/03d_Clean_Target.R", echo = T)
#     terminal "Work/03e_Clean_Target_PGS.sh"
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      NEXT:  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   A) Import Plink Data ####
#     i)    PGS (*.profile) #
#     ii)   IBG (*.genome) #
#     iiii) PCs (*.eigenvec) #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_PGS_Target <- read_table("Work/Datasets/GADD_H_PGS.profile") %>%
  rename(COGPGS = SCORE,
         MyNIDAID = IID) %>% 
  dplyr::select(MyNIDAID, COGPGS)

GADD_All <- read_table("Work/Datasets/GADD_I_IBD.genome") %>%
  dplyr::mutate(MyNIDAID1 = IID1,
                MyNIDAID2 = IID2,
                ID_Fam1 = as.numeric(substr(IID1,5,8)),
                ID_Fam2 = as.numeric(substr(IID2,5,8))) %>%
  dplyr::mutate(IID_PI = paste0(IID1,"_",IID2)) %>%
  dplyr::select(MyNIDAID1, MyNIDAID2, 
                ID_Fam1, ID_Fam2, 
                IID_PI, PI_HAT)

GADD_IBD <- read_table("Work/Datasets/GADD_I_IBD.genome") %>%
  dplyr::mutate(ID_Fam1 = as.numeric(substr(IID1,5,8)),
                ID_Fam2 = as.numeric(substr(IID2,5,8)),
                IBDSib = PI_HAT > .375) %>%
  dplyr::filter(ID_Fam1==ID_Fam2) %>%
  dplyr::mutate(ID_Fam = ID_Fam1) %>%
  dplyr::select(ID_Fam, 
                IID1, IID2, 
                ID_Fam1, ID_Fam2, 
                IBDSib, PI_HAT, RT:RATIO)

GADD_IBD_1 <- GADD_IBD %>%
  dplyr::rename(MyNIDAID = IID1) %>%
  dplyr::select(MyNIDAID, ID_Fam, PI_HAT)
GADD_IBD_2 <- GADD_IBD %>%
  dplyr::rename(MyNIDAID = IID2) %>%
  dplyr::select(MyNIDAID, ID_Fam, PI_HAT)

GADD_IBDmiss <- GADD_IBD %>%
  dplyr::filter(ID_Fam1==ID_Fam2 & PI_HAT < .375) %>%
  dplyr::rename(MyNIDAID1 = IID1,
                MyNIDAID2 = IID2) %>% 
  dplyr::select(-ID_Fam1,-ID_Fam2) 

GADD_IBDhit <- GADD_IBD %>%
  dplyr::filter(ID_Fam1==ID_Fam2 & PI_HAT > .375) %>%
  dplyr::rename(MyNIDAID1 = IID1,
                MyNIDAID2 = IID2) %>% 
  dplyr::select(-ID_Fam1,-ID_Fam2) 

GADD_PCA <- read_delim("Work/Datasets/GADD_G_PCA.eigenvec", delim = " ") %>%
  dplyr::rename(MyNIDAID = IID) %>%
  dplyr::select(MyNIDAID, PC1:PC20)

GADD_PCA_ForMerge <- list(GADD_PCA, GADD_IBD) %>%
  purrr::reduce(full_join)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   B) Merge/Validate PGS with other Data ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_PreMplus <- list(GADD_Long_Full, 
                      GADD_PGS_Target) %>%
  reduce(full_join) %>%
  dplyr::select(MyNIDAID, 
                ID_MPL:Wave, 
                DEMMAL,DEMAGE,DEMHSP,DEMRAC, 
                SRPBLK:CVLCOM, 
                CANEVR:ALCYRS,
                COGPGS) %>%
  filter(ID_Rel == "00" |
           ID_Rel == "03" |
           ID_Rel == "04")  %>%
  dplyr::mutate(COGPGS = COGPGS*1000)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   C) Sib Corrs: Check PGS bw Sibs ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_Proband <- list(GADD_PreMplus,
                     GADD_PCA) %>%
  purrr::reduce(full_join) %>%
  filter(ID_Rel == "00") %>%
  dplyr::select(ID_Fam, Wave, MyNIDAID, ID_MPL, 
         DEMMAL,DEMAGE,DEMHSP,DEMRAC, 
         SRPBLK:COGPGS,
         PC1:PC20)
names(GADD_Proband)  <- c(names(GADD_Proband)[1:2], 
                          paste0(names(GADD_Proband)[3:length(names(GADD_Proband))],"Pr"))

GADD_Sibling <-  list(GADD_PreMplus,
                      GADD_PCA) %>%
  purrr::reduce(full_join) %>%
  filter(ID_Rel == "03" | ID_Rel == "04") %>%
  dplyr::select(ID_Fam, Wave, MyNIDAID, ID_MPL, 
         DEMMAL,DEMAGE,DEMHSP,DEMRAC, 
         SRPBLK:COGPGS,
         PC1:PC20) 
names(GADD_Sibling)  <- c(names(GADD_Sibling)[1:2], 
                           paste0(names(GADD_Sibling)[3:length(names(GADD_Sibling))],"Sb"))

GADD_Pairs_A <- list(GADD_Proband,
                     GADD_Sibling) %>%
  reduce(full_join)

GADD_Pairs <- list(GADD_IBD, 
                   GADD_Pairs_A) %>%
  purrr::reduce(full_join) %>%
  dplyr::rename(MyNIDAID1 = IID1, 
                MyNIDAID2 = IID2) %>%
  dplyr::select(ID_Fam, Wave, MyNIDAIDPr, MyNIDAIDSb, PI_HAT,
                ID_MPLPr, ID_MPLSb, 
                DEMMALPr:DEMRACPr,
                DEMMALSb:DEMRACSb, 
                SRPBLKPr:COGPGSPr,
                SRPBLKSb:COGPGSSb,
                PC1Pr:PC20Pr,
                PC1Sb:PC20Sb)

GADD_SibPairs <- GADD_Pairs %>% dplyr::select(ID_Fam, Wave, 
                                              MyNIDAIDPr, MyNIDAIDSb, PI_HAT,
                                              DEMMALPr:DEMRACPr,
                                              DEMMALSb:DEMRACSb) 

write_csv(x = GADD_Pairs, 
          path = "Work/Datasets/GADMJCOG_SibPairs_190618.csv", 
          na = ".", col_names = T)

GADD_Descriptive <- list(GADD_PreMplus,
                         GADD_PCA) %>%
  purrr::reduce(full_join) %>%
  dplyr::mutate(SRPWCRD = ifelse(!is.na(SRPWCR), 1, 0),
                WASVCBD = ifelse(!is.na(WASVCB), 1, 0),
                WASBLKD = ifelse(!is.na(WASBLK), 1, 0),
                WASDGTD = ifelse(!is.na(WASDGT), 1, 0),
                WSISUMD = ifelse(!is.na(WSISUM), 1, 0),
                TRLATSD = ifelse(!is.na(TRLATS), 1, 0),
                TRLBTSD = ifelse(!is.na(TRLBTS), 1, 0),
                CVLASFD = ifelse(!is.na(CVLASF), 1, 0),
                CVLASCD = ifelse(!is.na(CVLASC), 1, 0),
                CVLALFD = ifelse(!is.na(CVLALF), 1, 0),
                CVLALCD = ifelse(!is.na(CVLALC), 1, 0),
                WASCOMD = ifelse(!is.na(WASCOM), 1, 0),
                COGCOMD = ifelse(!is.na(COGCOM), 1, 0),
                CVLCOMD = ifelse(!is.na(CVLCOM), 1, 0)
                ) %>%
  dplyr::group_by(ID_MPL) %>%
  dplyr::mutate(SRPWCRDw = sum(SRPWCRD, na.rm=T),
                WASVCBDw = sum(WASVCBD, na.rm=T),
                WASBLKDw = sum(WASBLKD, na.rm=T),
                WASDGTDw = sum(WASDGTD, na.rm=T),
                WSISUMDw = sum(WSISUMD, na.rm=T),
                TRLATSDw = sum(TRLATSD, na.rm=T),
                TRLBTSDw = sum(TRLBTSD, na.rm=T),
                CVLASFDw = sum(CVLASFD, na.rm=T),
                CVLASCDw = sum(CVLASCD, na.rm=T),
                CVLALFDw = sum(CVLALFD, na.rm=T),
                CVLALCDw = sum(CVLALCD, na.rm=T),
                WASCOMDw = sum(WASCOMD, na.rm=T),
                COGCOMDw = sum(COGCOMD, na.rm=T),
                CVLCOMDw = sum(CVLCOMD, na.rm=T)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(SRPWCRDt = ifelse(SRPWCRDw > 0, 1, 0),
                WASVCBDt = ifelse(WASVCBDw > 0, 1, 0),
                WASBLKDt = ifelse(WASBLKDw > 0, 1, 0),
                WASDGTDt = ifelse(WASDGTDw > 0, 1, 0),
                WSISUMDt = ifelse(WSISUMDw > 0, 1, 0),
                TRLATSDt = ifelse(TRLATSDw > 0, 1, 0),
                TRLBTSDt = ifelse(TRLBTSDw > 0, 1, 0),
                CVLASFDt = ifelse(CVLASFDw > 0, 1, 0),
                CVLASCDt = ifelse(CVLASCDw > 0, 1, 0),
                CVLALFDt = ifelse(CVLALFDw > 0, 1, 0),
                CVLALCDt = ifelse(CVLALCDw > 0, 1, 0),
                WASCOMDt = ifelse(WASCOMDw > 0, 1, 0),
                COGCOMDt = ifelse(COGCOMDw > 0, 1, 0),
                CVLCOMDt = ifelse(CVLCOMDw > 0, 1, 0)
                ) %>% 
  dplyr::group_by(ID_MPL) %>%
  dplyr::mutate(CogDataPoints = sum(x=c(SRPWCRDt,WASVCBDt,WASBLKDt,WASDGTDt,WSISUMDt, 
                                        TRLATSDt,TRLBTSDt,
                                        CVLASFDt,CVLASCDt,CVLALFDt,CVLALCDt), na.rm=T)) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(CogDataPoints > 0)

GADD_Mplus <- GADD_Descriptive %>%
  dplyr::select(ID_Fam, Wave, ID_MPL,
                DEMMAL:DEMRAC,
                SRPWCR:CVLCOM,
                CANEVR:ALCYRS,
                SRPWCRDt:CVLCOMDt,
                COGPGS, 
                PC1:PC6)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   D) Export to Mplus ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

write.table(x = GADD_Pairs, 
            file = "Work/Models/04_Descriptives/SB190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_Mplus, 
            file = "Work/Models/10_PhenoPGS/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_Mplus, 
            file = "Work/Models/10_WIFamPGS/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_Mplus, 
            file = "Work/Models/11_Phenotyp/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_Mplus, 
            file = "Work/Models/12_MultiLev/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_Mplus, 
            file = "Work/Models/13_MltLvPGS/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_Mplus, 
            file = "Work/Models/14_MltLvAlc/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_Mplus, 
            file = "Work/Models/15_PstHcInt/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   E) Validation Plots ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_Pairs_W1_Quantiles <- GADD_Mplus %>%
  filter(Wave == 1 & !is.na(COGPGS)) %>%
  dplyr::mutate(PGSrank = rank(COGPGS, na.last = F)) %>%
  dplyr::mutate(PGSquant = ifelse(PGSrank < 186.25*1, 1,
                           ifelse(PGSrank < 186.25*2, 2, 
                           ifelse(PGSrank < 186.25*3, 3, 
                           ifelse(PGSrank < 186.25*4, 4, NA)))))

GADD_Pairs_W2_Quantiles <- GADD_Mplus %>%
  filter(Wave == 2 & !is.na(COGPGS)) %>%
  dplyr::mutate(PGSrank = rank(COGPGS, na.last = F)) %>%
  dplyr::mutate(PGSquant = ifelse(PGSrank < 186.25*1, 1,
                           ifelse(PGSrank < 186.25*2, 2, 
                           ifelse(PGSrank < 186.25*3, 3, 
                           ifelse(PGSrank < 186.25*4, 4, NA)))))

GADD_Pairs_W1_Q1 <- GADD_Pairs_W1_Quantiles %>%
  filter(PGSquant == 1)
GADD_Pairs_W1_Q2 <- GADD_Pairs_W1_Quantiles %>%
  filter(PGSquant == 2)
GADD_Pairs_W1_Q3 <- GADD_Pairs_W1_Quantiles %>%
  filter(PGSquant == 3)
GADD_Pairs_W1_Q4 <- GADD_Pairs_W1_Quantiles %>%
  filter(PGSquant == 4)

GADD_Pairs_W2_Q1 <- GADD_Pairs_W2_Quantiles %>%
  filter(PGSquant == 1)
GADD_Pairs_W2_Q2 <- GADD_Pairs_W2_Quantiles %>%
  filter(PGSquant == 2)
GADD_Pairs_W2_Q3 <- GADD_Pairs_W2_Quantiles %>%
  filter(PGSquant == 3)
GADD_Pairs_W2_Q4 <- GADD_Pairs_W2_Quantiles %>%
  filter(PGSquant == 4)

mean(GADD_Pairs_W1_Q1$WSISUM, na.rm = T)
mean(GADD_Pairs_W1_Q2$WSISUM, na.rm = T)
mean(GADD_Pairs_W1_Q3$WSISUM, na.rm = T)
mean(GADD_Pairs_W1_Q4$WSISUM, na.rm = T)

mean(GADD_Pairs_W2_Q1$CANYRS, na.rm = T)
mean(GADD_Pairs_W2_Q2$CANYRS, na.rm = T)
mean(GADD_Pairs_W2_Q3$CANYRS, na.rm = T)
mean(GADD_Pairs_W2_Q4$CANYRS, na.rm = T)

mean(GADD_Pairs_W1_Q1$WASVCB, na.rm = T)
mean(GADD_Pairs_W1_Q2$WASVCB, na.rm = T)
mean(GADD_Pairs_W1_Q3$WASVCB, na.rm = T)
mean(GADD_Pairs_W1_Q4$WASVCB, na.rm = T)

mean(GADD_Pairs_W1_Q1$WASBLK, na.rm = T)
mean(GADD_Pairs_W1_Q2$WASBLK, na.rm = T)
mean(GADD_Pairs_W1_Q3$WASBLK, na.rm = T)
mean(GADD_Pairs_W1_Q4$WASBLK, na.rm = T)

mean(GADD_Pairs_W1_Q1$WASCOM, na.rm = T)
mean(GADD_Pairs_W1_Q2$WASCOM, na.rm = T)
mean(GADD_Pairs_W1_Q3$WASCOM, na.rm = T)
mean(GADD_Pairs_W1_Q4$WASCOM, na.rm = T)




GADD_Pairs_W12_Quantiles <- GADD_Mplus %>%
  filter(Wave == 12 & !is.na(COGPGS)) %>%
  dplyr::mutate(PGSrank = rank(COGPGS, na.last = F)) %>%
  dplyr::mutate(PGSquant = ifelse(PGSrank < (max(PGSrank)/4*1), 1,
                           ifelse(PGSrank < (max(PGSrank)/4*2), 2, 
                           ifelse(PGSrank < (max(PGSrank)/4*3), 3, 
                           ifelse(PGSrank < (max(PGSrank)/4*4), 4, NA)))))

GADD_Pairs_W12_Q1 <- GADD_Pairs_W12_Quantiles %>%
  filter(PGSquant == 1)
GADD_Pairs_W12_Q2 <- GADD_Pairs_W12_Quantiles %>%
  filter(PGSquant == 2)
GADD_Pairs_W12_Q3 <- GADD_Pairs_W12_Quantiles %>%
  filter(PGSquant == 3)
GADD_Pairs_W12_Q4 <- GADD_Pairs_W12_Quantiles %>%
  filter(PGSquant == 4)

mean(GADD_Pairs_W12_Q1$WSISUM, na.rm = T)
mean(GADD_Pairs_W12_Q2$WSISUM, na.rm = T)
mean(GADD_Pairs_W12_Q3$WSISUM, na.rm = T)
mean(GADD_Pairs_W12_Q4$WSISUM, na.rm = T)

mean(GADD_Pairs_W12_Q1$WASVCB, na.rm = T)
mean(GADD_Pairs_W12_Q2$WASVCB, na.rm = T)
mean(GADD_Pairs_W12_Q3$WASVCB, na.rm = T)
mean(GADD_Pairs_W12_Q4$WASVCB, na.rm = T)

mean(GADD_Pairs_W12_Q1$WASBLK, na.rm = T)
mean(GADD_Pairs_W12_Q2$WASBLK, na.rm = T)
mean(GADD_Pairs_W12_Q3$WASBLK, na.rm = T)
mean(GADD_Pairs_W12_Q4$WASBLK, na.rm = T)

mean(GADD_Pairs_W12_Q1$WASCOM, na.rm = T)
mean(GADD_Pairs_W12_Q2$WASCOM, na.rm = T)
mean(GADD_Pairs_W12_Q3$WASCOM, na.rm = T)
mean(GADD_Pairs_W12_Q4$WASCOM, na.rm = T)

mean(GADD_Pairs_W12_Q1$SRPWCR, na.rm = T)
mean(GADD_Pairs_W12_Q2$SRPWCR, na.rm = T)
mean(GADD_Pairs_W12_Q3$SRPWCR, na.rm = T)
mean(GADD_Pairs_W12_Q4$SRPWCR, na.rm = T)


GADD_Mplus_PCs <- GADD_Mplus %>%
  dplyr::filter(Wave == 1) %>%
  dplyr::select(SRPWCR:WSISUM,TRLATS:TRLBTS, CVLASF:CVLALC)



write.table(x = GADD_Mplus, 
            file = "Work/Models/10_PhenoPGS/GD190618.csv", sep=",",
            na = ".", col.names = F, row.names = F)

GADD_Long_StudiedPs <- GADD_Long_Full %>%
  dplyr::filter(ID_MPL %in% GADD_Mplus$ID_MPL)
GADD_DemSibs_Ps <- GADD_DemSibs %>%
  dplyr::filter(ID_MPL %in% GADD_Mplus$ID_MPL)
GADD_Pairs_Ps <- GADD_Pairs %>% 
  dplyr::filter((ID_MPLPr %in% GADD_Mplus$ID_MPL) | 
                  (ID_MPLSb %in% GADD_Mplus$ID_MPL))

write_csv(GADD_Long_Full, path = "Work/Datasets/GADMJCOG_LongFormat_190619.csv", na = ".", col_names = T)
write_csv(GADD_DemSibs, path = "Work/Datasets/GADMJCOG_Demographics_190619.csv", na = ".", col_names = T)
write_csv(GADD_Pairs_Ps, path = "Work/Datasets/GADMJCOG_SibPairs_190619.csv", na = ".", col_names = T)

