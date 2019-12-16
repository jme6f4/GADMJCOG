# HEADER ###############################################################
#      Program:   03a_Clean_Discovery.R
#      Project:   GADMJCOG
#      Tasks:      
                # A) Filter Out Sibs
                # B) Export Discovery
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Libraries: GADMJCOG
     # Source:    
                # GADD_Clean
                # GADD_Merged
      # Derived:   
                # GADD_PGS_KEEP
                # GADD_Discovery_Keep.txt
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
       source("Work/03_Clean.R", echo = T)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      NEXT:  ssh jael4447@statgen.colorado.edu
#             bash 03b_PGS_Discovery.sh
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   A) Filter Out Sibs #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_PGS_Discovery <- GADD_Clean %>%
  filter(FILTER == 0) %>%
  filter(GWAS == 1) %>%
  group_by(MyNIDAID) %>%
  dplyr::mutate(WaveMin = min(Wave, na.rm=T)) %>%
  ungroup() %>%
  filter(Wave == WaveMin) %>%
  dplyr::mutate(MyNIDAID2 = MyNIDAID) %>%
  group_by(MyNIDAID2) %>%
  dplyr::mutate(WASCOM = mean(c(WASVCB, WASBLK), na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(MyNIDAID, MyNIDAID2, 
                WASVCB, WASBLK, WASCOM, 
                CANEVR, CANONS, CANFLF, CANFTY, CANF6M)

GADD_PGS_Disc_KEEP <- GADD_PGS_Discovery %>%
  select(MyNIDAID, MyNIDAID2)
dim(GADD_PGS_Disc_KEEP)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   B) Export Discovery Data #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

write.table(GADD_PGS_Disc_KEEP, file="Work/Datasets/GADD_Discovery_Keep.txt",
            sep = "\t", quote=FALSE,row.names=FALSE,col.names=FALSE)