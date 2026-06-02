

library(readxl)
library(tidyr)
library(rstatix) 
library(dplyr) 
library(readr)
library(tidyverse)



######################## Data import Adherence
IFATICO_Datenexport_Adherence <- read_excel("~/Promotion/Auswertung/Datenexport/Tidy Datenexport/IFATICO_Datenexport_Adherence.xlsx")
View(IFATICO_Datenexport_Adherence)

IFA_Adherence_full <- IFATICO_Datenexport_Adherence
IFA_Adherence_full <- IFA_Adherence_full %>%
  mutate(id = record_id)

Datenexport_Schrittzählerdaten_Erschöpfung_tidy <- read_excel("~/Promotion/Auswertung/Datenexport/Tidy Datenexport/Datenexport_Schrittzählerdaten_Erschöpfung_tidy.xlsx")
View(Datenexport_Schrittzählerdaten_Erschöpfung_tidy)

IFA_RPE <- Datenexport_Schrittzählerdaten_Erschöpfung_tidy

######################## Data import Fidelity

Datenexport_Gruppe1_Fidelity_tidy <- read_excel("~/Promotion/Auswertung/Datenexport/Tidy Datenexport/Datenexport_Gruppe1_Fidelity_tidy.xlsx")
Datenexport_Gruppe2_Fidelity_tidy <- read_excel("~/Promotion/Auswertung/Datenexport/Tidy Datenexport/Datenexport_Gruppe2_Fidelity_tidy.xlsx")

IFA_Fidelity_1 <- Datenexport_Gruppe1_Fidelity_tidy
IFA_Fidelity_2 <- Datenexport_Gruppe2_Fidelity_tidy

#Numerically defining all timepoints

IFA_Fidelity_1$redcap_repeat_instance <- replace(IFA_Fidelity_1$redcap_repeat_instance, is.na(IFA_Fidelity_1$redcap_repeat_instance), 0)
IFA_Fidelity_2$redcap_repeat_instance <- replace(IFA_Fidelity_2$redcap_repeat_instance, is.na(IFA_Fidelity_2$redcap_repeat_instance), 0)
IFA_RPE$redcap_repeat_instance <- replace(IFA_RPE$redcap_repeat_instance, is.na(IFA_RPE$redcap_repeat_instance), 0)


######################## Data import Igroup Presence Questionnaire

IFATICO_Datenexport_Igroup <- read_excel("~/Promotion/Auswertung/Datenexport/Tidy Datenexport/IFATICO_Datenexport_Igroup.xlsx")
View(IFATICO_Datenexport_Igroup)

IFA_Igroup <- IFATICO_Datenexport_Igroup



################################# Adherence 
#How many Dropouts?

IFA_Adherence_full %>%
  group_by(Gruppe) %>%
  summarise(count_0 = sum(drop_out == 1))

#Average training duration of those that didn't drop out
IFA_Adherence <- IFA_Adherence_full %>%
  filter(drop_out == 0)

IFA_Session_Number <-  mean(IFA_Adherence$anzahl_sitz_fid)

IFA_Session_Number_1 <- mean (subset(IFA_Adherence, Gruppe == 1)$anzahl_sitz_fid) 
IFA_Session_Number_2 <- mean (subset(IFA_Adherence, Gruppe == 2)$anzahl_sitz_fid) 

IFA_Session_sd_1 <- sd (subset(IFA_Adherence, Gruppe == 1)$anzahl_sitz_fid) 
IFA_Session_sd_2 <- sd (subset(IFA_Adherence, Gruppe == 2)$anzahl_sitz_fid) 

#How many participants fulfilled fidelity criteria concerning number of sessions?
IFA_Fid_Sess_Num_1 <- sum(subset(IFA_Adherence, Gruppe == 1)$anzahl_sitz_fid >= 10 & subset(IFA_Adherence, Gruppe == 1)$anzahl_sitz_fid <= 14, na.rm = TRUE) #43/ 43
IFA_Fid_Sess_Num_2 <- sum(subset(IFA_Adherence, Gruppe == 2)$anzahl_sitz_fid >= 10 & subset(IFA_Adherence, Gruppe == 2)$anzahl_sitz_fid <= 14, na.rm = TRUE) #38/ 38

IFA_Adherence <- IFA_Adherence %>%
  mutate(sess_num_fid = ifelse(!is.na(anzahl_sitz_fid) & anzahl_sitz_fid >= 10 & anzahl_sitz_fid <= 14, 1, 0))


#Average length of study period of those that didn't drop out
IFA_Adherence <- IFA_Adherence %>%
  filter(drop_out == 0)


IFA_Studyperiod <-  mean(IFA_Adherence$zeitraum_sitz_fid, na.rm = TRUE) 

IFA_Studyperiod_1 <- mean (subset(IFA_Adherence, Gruppe == 1)$zeitraum_sitz_fid, na.rm = TRUE) 
IFA_Studyperiod_2 <- mean (subset(IFA_Adherence, Gruppe == 2)$zeitraum_sitz_fid) 

IFA_Studyperiod_1_sd <- sd (subset(IFA_Adherence, Gruppe == 1)$zeitraum_sitz_fid, na.rm = TRUE)
IFA_Studyperiod_2_sd <- sd (subset(IFA_Adherence, Gruppe == 2)$zeitraum_sitz_fid) 

#How many participants fulfilled fidelity criteria concerning number of length of study period?
IFA_Fid_Studyp_1 <- sum(subset(IFA_Adherence, Gruppe == 1)$zeitraum_sitz_fid >= 4 & subset(IFA_Adherence, Gruppe == 1)$zeitraum_sitz_fid <= 8, na.rm = TRUE)
IFA_Fid_Studyp_2 <- sum(subset(IFA_Adherence, Gruppe == 2)$zeitraum_sitz_fid >= 4 & subset(IFA_Adherence, Gruppe == 2)$zeitraum_sitz_fid <= 8, na.rm = TRUE)

IFA_Adherence <- IFA_Adherence %>%
  mutate(stper_fid = ifelse(!is.na(zeitraum_sitz_fid) & zeitraum_sitz_fid >= 4 & zeitraum_sitz_fid <= 8, 1, 0))


#How many repetitions per exercise?

IFA_Arm1_Reps <- mean(IFA_Fidelity_2$fidelity_comp_rep_aex1, na.rm = TRUE)
IFA_Arm2_Reps <- mean(IFA_Fidelity_2$fidelity_comp_rep_aex2, na.rm = TRUE)

IFA_Legs1_Reps <- mean(IFA_Fidelity_2$fidelity_comp_rep_lex1, na.rm = TRUE)

IFA_Fidelity_2$fidelity_comp_rep_lex2 <- as.numeric(IFA_Fidelity_2$fidelity_comp_rep_lex2)

IFA_Legs2_Reps <- mean(IFA_Fidelity_2$fidelity_comp_rep_lex2, na.rm = TRUE) 


IFA_VR_Reps <- mean(IFA_Fidelity_1$fidelity_int_repetitions, na.rm = TRUE) 


################################# Analysis of exertion and allowed exertion

IFA_RPE_id <- IFA_RPE %>%
  group_by(id, Gruppe) %>%
  summarise(
    mean_exertion_id = mean(erschoepf_monitor_2, na.rm = TRUE),
    mean_allow_exertion_id = mean(erschoepf_monitor_3, na.rm = TRUE)
  )

Mean_exertion_1 <- mean(subset(IFA_RPE_id, Gruppe == 1)$mean_exertion_id)
Mean_allow_exertion_1 <- mean(subset(IFA_RPE_id, Gruppe == 1)$mean_allow_exertion_id)

Mean_exertion_2 <- mean(subset(IFA_RPE_id, Gruppe == 2)$mean_exertion_id)
Mean_allow_exertion_2 <- mean(subset(IFA_RPE_id, Gruppe == 2)$mean_allow_exertion_id)


SD_exertion_1 <- sd(subset(IFA_RPE_id, Gruppe == 1)$mean_exertion_id)
SD_allow_exertion_1 <- sd(subset(IFA_RPE_id, Gruppe == 1)$mean_allow_exertion_id)

SD_exertion_2 <- sd(subset(IFA_RPE_id, Gruppe == 2)$mean_exertion_id)
SD_allow_exertion_2 <- sd(subset(IFA_RPE_id, Gruppe == 2)$mean_allow_exertion_id)


################################# Fidelity according to treatment log Group 1

############### Fidelity Intervention

#Nummerically defining "na" 
IFA_Fidelity_1 <- IFA_Fidelity_1 %>%
  mutate(across(fidelity_int_1:fidelity_int_17, ~ replace(., is.na(.), 2)))

#Counting item fidelity_int_7 twice
IFA_Fidelity_1 <- IFA_Fidelity_1 %>%
  mutate(fidelity_int_7_1 = fidelity_int_7)

IFA_Fidelity_1 <- IFA_Fidelity_1 %>%
  select(1:17, fidelity_int_7, fidelity_int_7_1, everything())

#Counting items of treatment log that weren't according to fidelity criteria 
IFA_Fidelity_1  <- IFA_Fidelity_1 %>%
  mutate(fidelity_int_count1 = rowSums(across(fidelity_int_1:fidelity_int_17, ~ . == 1)))

#Calculating Fidelity as a percentage of fulfilled fidelity criteria
IFA_Fidelity_1  <- IFA_Fidelity_1 %>%
  mutate(fidelity_int_count0 = rowSums(across(fidelity_int_1:fidelity_int_17, ~ . == 0)))

IFA_Fidelity_1  <- IFA_Fidelity_1 %>%
  mutate(fidelity_int_perc = fidelity_int_count0/(fidelity_int_count0 + fidelity_int_count1))

IFA_Fidelity_1  <- IFA_Fidelity_1 %>%
  filter(!is.na(fidelity_int_perc))

#Average Fidelity-score per ID

IFA_Fidelity_1_id_mean <- IFA_Fidelity_1 %>%
  group_by(id) %>%
  summarise(Mean_fidelity_id = mean(fidelity_int_perc))

#How many participants fulfilled Fidelity criteria regarding lenght of therapy sessions?

IFA_Fidelity_1 <- IFA_Fidelity_1 %>%
  mutate(fid_length_id = ifelse(!is.na(duration_sess_int) & duration_sess_int >= 30 & duration_sess_int <= 60,1, 0))


IFA_Fidelity_length_1_id <- IFA_Fidelity_1 %>%
  group_by(id) %>%
  summarise(fid_length = mean(fid_length_id))

#How long was the average length of training sessions?

IFA_length_1_id <- IFA_Fidelity_1 %>%
  group_by(id) %>%
  summarise(mean_length_1 = mean(duration_sess_int, na.rm = TRUE))

IFA_sess_length_1 <- mean(IFA_length_1_id$mean_length_1, na.rm = TRUE)
IFA_sess_length_1_sd <- sd(IFA_length_1_id$mean_length_1, na.rm = TRUE)


#How much overall exercise time?

IFA_Fidelity_1_joined <- IFA_length_1_id %>%
  left_join(
    IFA_Adherence %>% select(id, anzahl_sitz_fid),
    by = "id", "Gruppe"
  )

IFA_Fidelity_1_joined <- IFA_Fidelity_1_joined %>%
  mutate(timeoverall_1_id = anzahl_sitz_fid * mean_length_1, na.rm = TRUE) 

IFA_time_overall_1 <- mean(IFA_Fidelity_1_joined$timeoverall_1_id, na.rm = TRUE)
IFA_time_overall_sd_1 <- sd(IFA_Fidelity_1_joined$timeoverall_1_id, na.rm = TRUE)

#Average Group-Fidelity-score

IFA_Fidelity_1_id <- merge(IFA_Fidelity_1_id_mean, IFA_Fidelity_length_1_id, by = c("id"))

Fidelity_Int <- mean(IFA_Fidelity_1_id$Mean_fidelity_id)

#How many sessions had a Fidelity score over 80%?

IFA_Fidelity_1 %>%
  summarise(
    count_1 = sum(fidelity_int_perc >= 0.8, na.rm = TRUE),
    count_2 = sum(!is.na(fidelity_int_perc)))



############### Fidelity Immersion

IFA_Igroup <- IFA_Igroup %>%
  filter(!is.na(igpq_1))


# Spatial Presence: 1, (-6), 5, 3, 13

IFA_Igroup <- IFA_Igroup %>%
  mutate(Igroup_spatial = (igpq_1 + (6 - igpq_6) + igpq_5 + igpq_3 + igpq_13)/5)

IFA_Spatial <- IFA_Igroup %>% 
  summarise(Count_sp = sum(Igroup_spatial > 3, na.rm = TRUE))

IFA_Spatial <- IFA_Spatial/37 

Summary_Igroup_spatial <-  IFA_Igroup %>%
  summarise(mean_igroup_spatial = mean(IFA_Igroup$Igroup_spatial),
            sd_igroup_spatial = sd(IFA_Igroup$Igroup_spatial))
  

# Involvment: 7, 11, (-14), 8

IFA_Igroup <- IFA_Igroup %>%
  mutate(Igroup_involvment = (igpq_7 + (6 - igpq_14) + igpq_11 + igpq_8)/4)

IFA_Involvment <- IFA_Igroup %>% 
  summarise(Count_inv = sum(Igroup_involvment > 3, na.rm = TRUE))

IFA_Involvment <- IFA_Involvment/37 

Summary_Igroup_involvment <-  IFA_Igroup %>%
  summarise(mean_igroup_involvment = mean(IFA_Igroup$Igroup_involvment),
            sd_igroup_involvment = sd(IFA_Igroup$Igroup_involvment))


# Experienced Realism: 10, 2, (-4), 9

IFA_Igroup <- IFA_Igroup %>%
  mutate(Igroup_realism = (igpq_10 + (6 - igpq_4) + igpq_2 + igpq_9)/4)

IFA_Realism <- IFA_Igroup %>% 
  summarise(Count_rea = sum(Igroup_realism > 3, na.rm = TRUE))

IFA_Realism <- IFA_Realism/37 

Summary_Igroup_realism <-  IFA_Igroup %>%
  summarise(mean_igroup_realism = mean(IFA_Igroup$Igroup_realism),
            sd_igroup_realism = sd(IFA_Igroup$Igroup_realism))


# Feeling of being there: 12

IFA_Igroup <- IFA_Igroup %>%
  mutate(Igroup_being = igpq_12)

IFA_Being <- IFA_Igroup %>% 
  summarise(Count_being = sum(Igroup_being > 3, na.rm = TRUE))

IFA_Being <- IFA_Being/37

Summary_Igroup_being <-  IFA_Igroup %>%
  summarise(mean_igroup_being = mean(IFA_Igroup$Igroup_being),
            sd_igroup_being = sd(IFA_Igroup$Igroup_being))


# Total score

IFA_Igroup <- IFA_Igroup %>%
  mutate(Igroup_total = (Igroup_spatial + Igroup_involvment + Igroup_realism + Igroup_being)/ 4)

IFA_Igroup_total <- IFA_Igroup %>% 
  summarise(Count_toatl = sum(Igroup_total > 3, na.rm = TRUE))

IFA_Igroup_total <- IFA_Igroup_total/37 

Summary_Igroup_total <-  IFA_Igroup %>%
  summarise(mean_igroup_total = mean(IFA_Igroup$Igroup_total),
            sd_igroup_total = sd(IFA_Igroup$Igroup_total))

################################# Fidelity according to treatment log Group 2


#Nummerically defining "na" 
IFA_Fidelity_2 <- IFA_Fidelity_2 %>%
  mutate(across(fidelity_comp_1:fidelity_comp_22, ~ replace(., is.na(.), 2)))

#Counting Item fidelity_comp_6 soll twice
IFA_Fidelity_2 <- IFA_Fidelity_2 %>%
  mutate(fidelity_comp_6_1 = fidelity_comp_6)

IFA_Fidelity_2 <- IFA_Fidelity_2 %>%
  select(1:17, fidelity_comp_6, fidelity_comp_6_1, everything())

#Counting items of treatment log that weren't according to fidelity criteria 
IFA_Fidelity_2  <- IFA_Fidelity_2 %>%
  mutate(fidelity_comp_count1 = rowSums(across(fidelity_comp_1:fidelity_comp_22, ~ . == 1)))

#Calculating Fidelity as a percentage of fulfilled fidelity criteria
IFA_Fidelity_2  <- IFA_Fidelity_2 %>%
  mutate(fidelity_comp_count0 = rowSums(across(fidelity_comp_1:fidelity_comp_22, ~ . == 0)))

IFA_Fidelity_2  <- IFA_Fidelity_2 %>%
  mutate(fidelity_comp_perc = fidelity_comp_count0/(fidelity_comp_count0 + fidelity_comp_count1))

IFA_Fidelity_2  <- IFA_Fidelity_2 %>%
  filter(!is.na(fidelity_comp_perc))

#Average Fidelity-score per ID

IFA_Fidelity_2_id_mean <- IFA_Fidelity_2 %>%
  group_by(id) %>%
  summarise(Mean_fidelity_id = mean(fidelity_comp_perc))

#How many participants fulfilled Fidelity criteria regarding lenght of therapy sessions?

IFA_Fidelity_2 <- IFA_Fidelity_2 %>%
  mutate(fid_length_id = ifelse(!is.na(duration_sess_comp) & duration_sess_comp >= 30 & duration_sess_comp <= 60,1, 0))

IFA_Fidelity_length_2_id <- IFA_Fidelity_2 %>%
  group_by(id) %>%
  summarise(fid_length = mean(fid_length_id))

#How long was the average length of training sessions?

IFA_length_2_id <- IFA_Fidelity_2 %>%
  group_by(id) %>%
  summarise(mean_length_2 = mean(duration_sess_comp, na.rm = TRUE))

IFA_sess_length_2 <- mean(IFA_length_2_id$mean_length_2, na.rm = TRUE)
IFA_sess_length_2_sd <- sd(IFA_length_2_id$mean_length_2, na.rm = TRUE)

#How much overall exercise time?


IFA_Fidelity_2_joined <- IFA_length_2_id %>%
  left_join(
    IFA_Adherence %>% select(id, anzahl_sitz_fid),
    by = "id", "Gruppe"
  )

IFA_Fidelity_2_joined <- IFA_Fidelity_2_joined %>%
  mutate(timeoverall_2_id = anzahl_sitz_fid * mean_length_2, na.rm = TRUE) 

IFA_time_overall_2 <- mean(IFA_Fidelity_2_joined$timeoverall_2_id, na.rm = TRUE) 
IFA_time_overall_sd_2 <- sd(IFA_Fidelity_2_joined$timeoverall_2_id, na.rm = TRUE) 



#Average Group-Fidelity-score

IFA_Fidelity_2_id <- merge(IFA_Fidelity_2_id_mean, IFA_Fidelity_length_2_id, by = c("id"))

Fidelity_Comp <- mean(IFA_Fidelity_2_id$Mean_fidelity_id)

#How many sessions had a Fidelity score over 80%?

IFA_Fidelity_2 %>%
  summarise(
    count_1 = sum(fidelity_comp_perc >= 0.8, na.rm = TRUE),
    count_2 = sum(!is.na(fidelity_comp_perc))) #426/479


######################## Calculating perticipant-fidelity-scores

IFA_Adherence <- IFA_Adherence %>%
  mutate(adherence = ifelse(stper_fid ==1 & sess_num_fid == 1, 1, 0))


IFA_Fidelity_id <- bind_rows(IFA_Fidelity_1_id, IFA_Fidelity_2_id) %>%
  mutate(fidel_id = ifelse(Mean_fidelity_id > 0.8, 1, 0))

IFA_PerP <- merge(IFA_Adherence, IFA_Fidelity_id, by = c("id"), na.rm = TRUE) %>%
  mutate(perp_id = ifelse(adherence == 1 & fidel_id ==1 & fid_length >= 0.8, 1, 0))

#How many participants fulfilled Fidelity criteria according to treatment logs?
IFA_PerP %>%
  group_by(Gruppe) %>%
  summarise(count_1 = sum(Mean_fidelity_id >= 0.8, na.rm = TRUE))

#Which ids weren't treated per protocol?
filtered_ids <- IFA_PerP %>%
  filter(perp_id == 0) %>%
  select(id)

print(filtered_ids)


#How many participants per group weren't treated per protocol?

IFA_PerP %>% 
  group_by(Gruppe) %>%
  summarise(count_2 = sum(perp_id == 0))

IFA_PerP %>% 
  group_by(Gruppe) %>% 
  summarise(betroffene_ids = list(id[perp_id == 0]))


IFA_PerP %>% 
  filter(perp_id == 0) %>% 
  group_by(Gruppe) %>% 
  summarise(
    betroffene_ids = list(unique(id))
  ) %>% 
  tidyr::unnest(betroffene_ids) %>% 
  print(n = 50)

table(IFA_PerP$perp_id, IFA_PerP$Gruppe)


#How many participants weren't treated per protocol for what reason?

table(IFA_PerP$Gruppe, IFA_PerP$adherence) 
table(IFA_PerP$Gruppe, IFA_PerP$fidel_id)  
table(IFA_PerP$Gruppe, IFA_PerP$fid_length)



######################## Calculating a continous fidelity score

IFA_Fidelity_id <- IFA_Fidelity_id %>%
  mutate(fidelity_kont = (Mean_fidelity_id * fid_length))






