install.packages("tidyverse")
install.packages("dplyr")
install.packages("readxl")
install.packages("tidyr")
install.packages("rstatix")
install.packages("readr")

library(readxl)
library(tidyr)
library(rstatix) 
library(dplyr) 
library(readr)
library(tidyverse)



######################## Data import

Datenexport_Gruppe1_AEs_tidy <- read_excel("~/Promotion/Auswertung/Datenexport/Tidy Datenexport/Datenexport_Gruppe1_AEs_tidy.xlsx")
View(Datenexport_Gruppe1_AEs_tidy)

Datenexport_Gruppe2_AEs_tidy <- read_excel("~/Promotion/Auswertung/Datenexport/Tidy Datenexport/Datenexport_Gruppe2_AEs_tidy.xlsx")
View(Datenexport_Gruppe2_AEs_tidy)

IFA_Gruppe_1_AE <- Datenexport_Gruppe1_AEs_tidy
IFA_Gruppe_2_AE <- Datenexport_Gruppe2_AEs_tidy
IFA_AE <- rbind(IFA_Gruppe_1_AE, IFA_Gruppe_2_AE)

#Nummerically defining all time points

IFA_AE$redcap_repeat_instance <- replace(IFA_AE$redcap_repeat_instance, is.na(IFA_AE$redcap_repeat_instance), 0)


                
###################### Adverse Events: Severity


#Defining variables for adverse event severity

IFA_AE <- IFA_AE %>%
  mutate(AE_mild = ifelse(ae1_severity == 1 | ae2_severity == 1 | ae3_severity == 1 | ae4_severity == 1, 1, 0))
IFA_AE <- IFA_AE %>%
  mutate(AE_moderate = ifelse(ae1_severity == 2 | ae2_severity == 2 | ae3_severity == 2 | ae4_severity == 2, 1, 0))
IFA_AE <- IFA_AE %>%
  mutate(AE_severe = ifelse(ae1_severity == 3 | ae2_severity == 3 | ae3_severity == 3 | ae4_severity == 3, 1, 0))
IFA_AE <- IFA_AE %>%
  mutate(AE_total = ifelse(AE_mild == 1 | AE_moderate == 1 | AE_severe == 1, 1, 0))

#Defining data sets for number of participants with mild/ moderate/ severe adverse events
IFA_AE_pers_mild <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_mild == 1, na.rm = TRUE)) 

IFA_AE_pers_mild <- na.omit(IFA_AE_pers_mild)

IFA_AE_pers_moderate <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_moderate == 1, na.rm = TRUE)) 

IFA_AE_pers_moderate <- na.omit(IFA_AE_pers_moderate)

IFA_AE_pers_severe <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_severe == 1, na.rm = TRUE)) 

IFA_AE_pers_severe <- na.omit(IFA_AE_pers_severe)


IFA_AE_pers_total <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_total == 1, na.rm = TRUE)) 

IFA_AE_pers_total <- na.omit(IFA_AE_pers_total)

#How many training sessions with mild/ moderate/ severe adverse events?

#How many training sessions per group?
table(IFA_AE$Gruppe)

#How many training sessions with mild adverse events?
table(subset(IFA_AE, Gruppe == 1)$AE_mild)
table(subset(IFA_AE, Gruppe == 2)$AE_mild)

#How many training sessions with moderate adverse events?
table(subset(IFA_AE, Gruppe == 1)$AE_moderate)
table(subset(IFA_AE, Gruppe == 2)$AE_moderate)

#How many training sessions with severe adverse events?
table(subset(IFA_AE, Gruppe == 1)$AE_severe)
table(subset(IFA_AE, Gruppe == 2)$AE_severe)

#How many training sessions with any adverse event?
table(subset(IFA_AE, Gruppe == 1)$AE_total)
table(subset(IFA_AE, Gruppe == 2)$AE_total)

#How many participants with mild/ moderate/ severe adverse events?
#How many participants per group?
IFA_AE %>%
  group_by(Gruppe) %>%
  summarise(unique_IDs = n_distinct(id))

#How many participants with mild adverse events?
table(subset(IFA_AE_pers_mild, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_mild, Gruppe == 2)$Count_1_1)

#How many participants with moderate adverse events?
table(subset(IFA_AE_pers_moderate, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_moderate, Gruppe == 2)$Count_1_1)

#How many participants with severe adverse events?
table(subset(IFA_AE_pers_severe, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_severe, Gruppe == 2)$Count_1_1)

#How many participants with any adverse event?
table(subset(IFA_AE_pers_total, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_total, Gruppe == 2)$Count_1_1)



###################### Adverse Events: Relationship to treatment
#Defining variables for adverse event relationship

IFA_AE <- IFA_AE %>%
  mutate(AE_related = ifelse(ae1_related == 1 | ae2_related == 1 | ae3_related == 1 | ae4_related == 1, 1, 0))
IFA_AE <- IFA_AE %>%
  mutate(AE_maybe_related = ifelse(ae1_related == 2 | ae2_related == 2 | ae3_related == 2 | ae4_related == 2, 1, 0))
IFA_AE <- IFA_AE %>%
  mutate(AE_unrelated = ifelse(ae1_related == 3 | ae2_related == 3 | ae3_related == 3 | ae4_related == 3, 1, 0))

IFA_AE <- IFA_AE %>%
  mutate(AE_total2 = ifelse(AE_related == 1 | AE_unrelated == 1| AE_maybe_related == 1, 1, 0))

#Defining datasets for number of participants with related/ maybe related/ unrelated adverse events
IFA_AE_pers_related <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_related == 1, na.rm = TRUE)) 

IFA_AE_pers_related <- na.omit(IFA_AE_pers_related)

IFA_AE_pers_maybe_related <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_maybe_related == 1, na.rm = TRUE)) 

IFA_AE_pers_maybe_related <- na.omit(IFA_AE_pers_maybe_related)

IFA_AE_pers_unrelated <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_unrelated == 1, na.rm = TRUE)) 

IFA_AE_pers_unrelated <- na.omit(IFA_AE_pers_unrelated)

IFA_AE_pers_total2 <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(AE_total2 == 1, na.rm = TRUE)) 

IFA_AE_pers_total2 <- na.omit(IFA_AE_pers_total2)

#How many training sessions per group?
table(IFA_AE$Gruppe)

#How many training sessions with adverse events related to treatment?
table(subset(IFA_AE, Gruppe == 1)$AE_related)
table(subset(IFA_AE, Gruppe == 2)$AE_related)

#How many training sessions with adverse events maybe related to treatment?
table(subset(IFA_AE, Gruppe == 1)$AE_maybe_related)
table(subset(IFA_AE, Gruppe == 2)$AE_maybe_related) 

#How many training sessions with adverse events unrelated to treatment?
table(subset(IFA_AE, Gruppe == 1)$AE_unrelated)
table(subset(IFA_AE, Gruppe == 2)$AE_unrelated)

#How many training sessions with any adverse event?
table(subset(IFA_AE, Gruppe == 1)$AE_total2)
table(subset(IFA_AE, Gruppe == 2)$AE_total2)

#How many participants per group?
IFA_AE %>%
  group_by(Gruppe) %>%
  summarise(unique_IDs = n_distinct(id))

#How many participants with adverse events related to treatment?
table(subset(IFA_AE_pers_related, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_related, Gruppe == 2)$Count_1_1)

#How many participants with adverse events maybe related to treatment?
table(subset(IFA_AE_pers_maybe_related, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_maybe_related, Gruppe == 2)$Count_1_1)

#How many participants with adverse events unrelated to treatment?
table(subset(IFA_AE_pers_unrelated, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_unrelated, Gruppe == 2)$Count_1_1)

#How many participants with any adverse event?
table(subset(IFA_AE_pers_total2, Gruppe == 1)$Count_1_1)
table(subset(IFA_AE_pers_total2, Gruppe == 2)$Count_1_1)


###################### Adverse Events: VR trias

#How many participants with motion sickness?
IFA_AE_motion_sickness <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(ae_motion_sickness == 1, na.rm = TRUE)) 
table(subset(IFA_AE_motion_sickness, Gruppe == 1)$Count_1_1)

#How many training sessions with motion sickness?
table(subset(IFA_AE, Gruppe == 1)$ae_motion_sickness)

#Average severity of motion sickness?
IFA_AE %>%
  filter(ae_motion_sickness == 1, is.na(ae_motion_sickness_sev)) %>%
  distinct(id, Gruppe)

summary_motion_sickness <- IFA_AE %>%
  summarise(mean_motion_sickness = mean(ae_motion_sickness_sev, na.rm = TRUE),
    sd_motion_sickness = sd(ae_motion_sickness_sev, na.rm = TRUE))

print(summary_motion_sickness)


#How many participants with vertigo? 
IFA_AE_vertigo <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(ae_vertigo == 1, na.rm = TRUE)) 
table(subset(IFA_AE_vertigo, Gruppe == 1)$Count_1_1)

#How many training sessions with vertigo?
table(subset(IFA_AE, Gruppe == 1)$ae_vertigo)

#Average severity of vertigo?
summary_vertigo <- IFA_AE %>%
  summarise(mean_vertigo = mean(ae_vertigo_sev, na.rm = TRUE),
            sd_vertigo = sd(ae_vertigo_sev, na.rm = TRUE))

print(summary_vertigo)



#How many participants with fear?
IFA_AE_fear <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_1 = sum(ae_fear == 1, na.rm = TRUE)) 
table(subset(IFA_AE_fear, Gruppe == 1)$Count_1_1)

#How many training sessions with fear?
table(subset(IFA_AE, Gruppe == 1)$ae_fear)

#Average severity of fear?
summary_fear <- IFA_AE %>%
  summarise(mean_fear = mean(ae_fear_sev, na.rm = TRUE),
            sd_fear = sd(ae_fear_sev, na.rm = TRUE))

print(summary_fear) 


#How many participants with any adverse event of VR-trias?
IFA_AE <- IFA_AE %>%
  mutate(VR_Trias_pos = (ifelse(ae_motion_sickness == 1 | ae_fear == 1 | ae_vertigo == 1, 1, 0)))

IFA_AE_Trias <- IFA_AE %>%
  group_by(id, Gruppe) %>%
  summarise(Count_1_3 = sum(VR_Trias_pos == 1, na.rm = TRUE)) 


write.csv2(IFA_AE_Trias, "IFA_AE_Trias.csv", row.names = FALSE, quote = FALSE)
file.show("IFA_AE_Trias.csv")
