# HEADER ###############################################################
#      Program:   03c_Clean_Validate_PGS.R
#      Project:   GADMJCOG
#      Tasks:      
                # A) Import PGS (*.profile)
                # B) Merge PGS/COG
                # C) Validate
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Libraries: GADMJCOG
     # Source:    
                # GADD_PGS_Disc_*
                # GADD_PGS_Discovery
      # Derived:   
                # Validation Plots
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
        source("Work/03_Clean.R", echo = T)
        source("Work/03a_Clean_PGS_Discovery.R", echo = T)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      NEXT:  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   A) Import PGS (*.profile) #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_PGS_Disc_1e45 <- read_table("Work/Datasets/score_1e45.profile") %>%
  rename(SCORE1e45 = SCORE) %>% select(IID, SCORE1e45)
GADD_PGS_Disc_1e40 <- read_table("Work/Datasets/score_1e40.profile") %>%
  rename(SCORE1e40 = SCORE) %>% select(IID, SCORE1e40)
GADD_PGS_Disc_1e35 <- read_table("Work/Datasets/score_1e35.profile") %>%
  rename(SCORE1e35 = SCORE) %>% select(IID, SCORE1e35)
GADD_PGS_Disc_1e30 <- read_table("Work/Datasets/score_1e30.profile") %>%
  rename(SCORE1e30 = SCORE) %>% select(IID, SCORE1e30)
GADD_PGS_Disc_1e25 <- read_table("Work/Datasets/score_1e25.profile") %>%
  rename(SCORE1e25 = SCORE) %>% select(IID, SCORE1e25)
GADD_PGS_Disc_1e20 <- read_table("Work/Datasets/score_1e20.profile") %>%
  rename(SCORE1e20 = SCORE) %>% select(IID, SCORE1e20)
GADD_PGS_Disc_1e15 <- read_table("Work/Datasets/score_1e15.profile") %>%
  rename(SCORE1e15 = SCORE) %>% select(IID, SCORE1e15)
GADD_PGS_Disc_1e10 <- read_table("Work/Datasets/score_1e10.profile") %>%
  rename(SCORE1e10 = SCORE) %>% select(IID, SCORE1e10)
GADD_PGS_Disc_1e09 <- read_table("Work/Datasets/score_1e09.profile") %>%
  rename(SCORE1e09 = SCORE) %>% select(IID, SCORE1e09)
GADD_PGS_Disc_1e08 <- read_table("Work/Datasets/score_1e08.profile") %>%
  rename(SCORE1e08 = SCORE) %>% select(IID, SCORE1e08)
GADD_PGS_Disc_1e8 <- read_table("Work/Datasets/score_1e8.profile") %>%
  rename(SCORE1e8 = SCORE) %>% select(IID, SCORE1e8)
GADD_PGS_Disc_1e07 <- read_table("Work/Datasets/score_1e07.profile") %>%
  rename(SCORE1e07 = SCORE) %>% select(IID, SCORE1e07)
GADD_PGS_Disc_1e06 <- read_table("Work/Datasets/score_1e06.profile") %>%
  rename(SCORE1e06 = SCORE) %>% select(IID, SCORE1e06)
GADD_PGS_Disc_1e05 <- read_table("Work/Datasets/score_1e05.profile") %>%
  rename(SCORE1e05 = SCORE) %>% select(IID, SCORE1e05)
GADD_PGS_Disc_1e04 <- read_table("Work/Datasets/score_1e04.profile") %>%
  rename(SCORE1e04 = SCORE) %>% select(IID, SCORE1e04)
GADD_PGS_Disc_1e03 <- read_table("Work/Datasets/score_1e03.profile") %>%
  rename(SCORE1e03 = SCORE) %>% select(IID, SCORE1e03)
GADD_PGS_Disc_1e02 <- read_table("Work/Datasets/score_1e02.profile") %>%
  rename(SCORE1e02 = SCORE) %>% select(IID, SCORE1e02)
GADD_PGS_Disc_1e01 <- read_table("Work/Datasets/score_1e01.profile") %>%
  rename(SCORE1e01 = SCORE) %>% select(IID, SCORE1e01)
GADD_PGS_Disc_2e01 <- read_table("Work/Datasets/score_2e01.profile") %>%
  rename(SCORE2e01 = SCORE) %>% select(IID, SCORE2e01)
GADD_PGS_Disc_3e01 <- read_table("Work/Datasets/score_3e01.profile") %>%
  rename(SCORE3e01 = SCORE) %>% select(IID, SCORE3e01)
GADD_PGS_Disc_4e01 <- read_table("Work/Datasets/score_4e01.profile") %>%
  rename(SCORE4e01 = SCORE) %>% select(IID, SCORE4e01)
GADD_PGS_Disc_5e01 <- read_table("Work/Datasets/score_5e01.profile") %>%
  rename(SCORE5e01 = SCORE) %>% select(IID, SCORE5e01)
GADD_PGS_Disc_6e01 <- read_table("Work/Datasets/score_6e01.profile") %>%
  rename(SCORE6e01 = SCORE) %>% select(IID, SCORE6e01)
GADD_PGS_Disc_7e01 <- read_table("Work/Datasets/score_7e01.profile") %>%
  rename(SCORE7e01 = SCORE) %>% select(IID, SCORE7e01)
GADD_PGS_Disc_8e01 <- read_table("Work/Datasets/score_8e01.profile") %>%
  rename(SCORE8e01 = SCORE) %>% select(IID, SCORE8e01)
GADD_PGS_Disc_9e01 <- read_table("Work/Datasets/score_9e01.profile") %>%
  rename(SCORE9e01 = SCORE) %>% select(IID, SCORE9e01)
GADD_PGS_Disc_10e01 <- read_table("Work/Datasets/score_10e1.profile") %>%
  rename(SCORE10e01 = SCORE) %>% select(IID, SCORE10e01)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   B) Merge PGS/COG #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_PGS_Disc_Merged <- list(GADD_PGS_Disc_1e45, GADD_PGS_Disc_1e40,
                             GADD_PGS_Disc_1e35, GADD_PGS_Disc_1e30, 
                             GADD_PGS_Disc_1e25, GADD_PGS_Disc_1e20, 
                             GADD_PGS_Disc_1e15, GADD_PGS_Disc_1e10, 
                             
                             GADD_PGS_Disc_1e09, GADD_PGS_Disc_1e08, GADD_PGS_Disc_1e07, GADD_PGS_Disc_1e06,
                             GADD_PGS_Disc_1e05, GADD_PGS_Disc_1e04, GADD_PGS_Disc_1e03, GADD_PGS_Disc_1e02, GADD_PGS_Disc_1e01,
                             
                             GADD_PGS_Disc_2e01, GADD_PGS_Disc_3e01, GADD_PGS_Disc_4e01, GADD_PGS_Disc_5e01, 
                             GADD_PGS_Disc_6e01, GADD_PGS_Disc_7e01, GADD_PGS_Disc_8e01, GADD_PGS_Disc_9e01, GADD_PGS_Disc_10e01,
                             GADD_PGS_Disc_1e8) %>%
  reduce(full_join) %>%
  dplyr::rename(MyNIDAID = IID) 

GADD_PGS_Disc_Validate <- list(GADD_PGS_Discovery, 
                               GADD_PGS_Disc_Merged) %>%
  reduce(full_join)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   C) Validate #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

scatterHist(GADD_PGS_Disc_Validate$SCORE100, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE90, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE80, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE70, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE60, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE50, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE40, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE30, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE20, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e01, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e02, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e03, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e04, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e05, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e06, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e07, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e08, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e09, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e010, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e015, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e020, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e025, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e030, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e035, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e040, GADD_PGS_Disc_Validate$WASCOM)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e045, GADD_PGS_Disc_Validate$WASCOM)

scatterHist(GADD_PGS_Disc_Validate$SCORE1e08, GADD_PGS_Disc_Validate$WASBLK)
scatterHist(GADD_PGS_Disc_Validate$SCORE1e08, GADD_PGS_Disc_Validate$CANONS)
scatterHist(GADD_PGS_Disc_Validate$WASCOM, GADD_PGS_Disc_Validate$CANONS)
