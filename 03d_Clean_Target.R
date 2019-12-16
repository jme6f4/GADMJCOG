# HEADER ###############################################################
#      Program:   03d_Clean_Target.R
#      Project:   GADMJCOG
#      Tasks:      
                # A) Filter Out Sibs
                # B) Export Target Data
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Libraries: GADMJCOG
     # Source:    
                # 
                # GADD_Merged
      # Derived:   
                # GADD_PGS_KEEP
                # GADD_Target_Keep.txt
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
       source("Work/03_Clean.R", echo = T)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      NEXT:  ssh jael4447@statgen.colorado.edu
#             bash 03e_PGS_Target.sh
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   A) Filter Out Sibs #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_PGS_Target <- GADD_Sibs %>%
  filter(GWAS == 1) %>%
  group_by(MyNIDAID) %>%
  dplyr::mutate(WaveMin = min(Wave, na.rm=T)) %>%
  ungroup() %>%
  filter(Wave == WaveMin) %>%
  dplyr::mutate(MyNIDAID2 = MyNIDAID)

GADD_PGS_Targ_KEEP <- GADD_PGS_Target %>%
  select(MyNIDAID, MyNIDAID2)

dim(GADD_PGS_Targ_KEEP)

# QUESTION:  ID_Fam or MyNIDAID2

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   B) Export Target Data #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

write.table(GADD_PGS_Targ_KEEP, file="Work/Datasets/GADD_Target_Keep.txt",
            sep = "\t", quote=FALSE,row.names=FALSE,col.names=FALSE)
