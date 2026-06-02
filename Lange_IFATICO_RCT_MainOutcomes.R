install.packages("zoo")
install.packages("compareGroups")
install.packages("DHARMa")
install.packages("performance")
install.packages("lmerTest")
install.packages("emmeans")
install.packages("epitools")
install.packages("lme4")
install.packages("ordinal")
install.packages("ggplot2")

library(readxl)
library(tidyverse)
library(dplyr)
library(rstatix)
library(lme4)
library(zoo)
library(compareGroups)
library(DHARMa)
library(performance)
library(lmerTest)
library(emmeans)
library(epitools)
library(lme4)
library(ordinal)
library(ggplot2)

######################## Data import

IFATICO <- read_excel("~/Promotion/Auswertung/Datenexport/Datenexport_gesamt/IFATICO_T0_T1_T2_T3_long_ovtrdu.xlsx")

IFATICO <- IFATICO %>%
  mutate(
    redcap_event_name = case_when(
      redcap_event_name == "baseline_query_arm_4" ~ "t0",
      redcap_event_name == "t1_post_interventi_arm_1" ~ "t1",
      redcap_event_name == "t1_post_interventi_arm_2" ~ "t1",
      redcap_event_name == "t2_follow_up_1_arm_1" ~ "t2",
      redcap_event_name == "t2_follow_up_1_arm_2" ~ "t2",
      redcap_event_name == "t3_follow_up_2_arm_1" ~ "t3",
      redcap_event_name == "t3_follow_up_2_arm_2" ~ "t3",
      TRUE ~ redcap_event_name
    )
  )


########################## Defining outcome variables

#---------- FSS

#IFATICO$fss_total

#---------- HADS

IFATICO <- IFATICO %>% mutate(hads_a = hads_1 + hads_3 + hads_5 + hads_9 + hads_11 + hads_13 + hads_7,
                                
                                hads_d = hads_8 + hads_10 + hads_2 + hads_4 + hads_6 + hads_12 + hads_14) %>% 
  mutate(hads_total = hads_a + hads_d)

#---------- PEM-Questionnaire

IFATICO$pem1 <- ifelse(IFATICO$dsq_pem_freq_1 >= 2 & IFATICO$dsq_pem_sev_1 >= 2,1,0)

IFATICO$pem2 <- ifelse(IFATICO$dsq_pem_freq_2 >= 2 & IFATICO$dsq_pem_sev_2 >= 2,1,0)

IFATICO$pem3 <- ifelse(IFATICO$dsq_pem_freq_3 >= 2 & IFATICO$dsq_pem_sev_3 >= 2,1,0)

IFATICO$pem4 <- ifelse(IFATICO$dsq_pem_freq_4 >= 2 & IFATICO$dsq_pem_sev_4 >= 2,1,0)

IFATICO$pem5 <- ifelse(IFATICO$dsq_pem_freq_5 >= 2 & IFATICO$dsq_pem_sev_5 >= 2,1,0)



IFATICO <- IFATICO %>% mutate(pem_combined = pem1 + pem2 + pem3 + pem4 + pem5) %>% 
  
  mutate(pem_level_1 = ifelse(pem_combined > 0,1,0),
         
         pem_level_2 = dsq_pem_dur_1)


#---------- JSS
IFATICO <- IFATICO %>% mutate(jss_total = jss_1 + jss_2 + jss_3 +  jss_4)


#---------- SF12

IFATICO <- IFATICO %>% mutate(rolph_2 = sf12_4,
                                
                                rolph_3 = sf12_5,
                                
                                rolem_2 = sf12_6,
                                
                                rolem_3 = sf12_7,
                                
                                pfi_2 = sf12_2,
                                
                                pfi_4 = sf12_3,
                                
                                ghp_1 = sf12_1,
                                
                                pain_2 = sf12_8,
                                
                                soc_2 = sf12_12,
                                
                                vital_2 = sf12_10,
                                
                                mhi_3 = sf12_9,
                                
                                mhi_4 = sf12_11) %>% 
  
  mutate(rpain_2 = 6 - pain_2,
         
         rghp_1 = 6 - ghp_1,
         
         rvital_2 = 7 - vital_2,
         
         rmhi_3 = 7 - mhi_3) %>%
  
  mutate(pf_02_1 = ifelse(pfi_2 == 1,1,0),
         
         pf_02_2 = ifelse(pfi_2 == 2,1,0),
         
         pf_04_1 = ifelse(pfi_4 == 1,1,0),
         
         pf_04_2 = ifelse(pfi_4 == 2,1,0),
         
         rp_02_1 = ifelse(rolph_2 == 1,1,0),
         
         rp_03_1 = ifelse(rolph_3 == 1,1,0),
         
         bp_02_1 = ifelse(rpain_2 == 1,1,0),
         
         bp_02_2 = ifelse(rpain_2 == 2,1,0),
         
         bp_02_3 = ifelse(rpain_2 == 3,1,0),
         
         bp_02_4 = ifelse(rpain_2 == 4,1,0),
         
         gh_01_1 = ifelse(rghp_1 == 1,1,0),
         
         gh_01_2 = ifelse(rghp_1 == 2,1,0),
         
         gh_01_3 = ifelse(rghp_1 == 3,1,0),
         
         gh_01_4 = ifelse(rghp_1 == 4,1,0),
         
         vt_02_1 = ifelse(rvital_2 == 1,1,0),
         
         vt_02_2 = ifelse(rvital_2 == 2,1,0),
         
         vt_02_3 = ifelse(rvital_2 == 3,1,0),
         
         vt_02_4 = ifelse(rvital_2 == 4,1,0),
         
         vt_02_5 = ifelse(rvital_2 == 5,1,0),
         
         sf_02_1 = ifelse(soc_2 == 1,1,0),
         
         sf_02_2 = ifelse(soc_2 == 2,1,0),
         
         sf_02_3 = ifelse(soc_2 == 3,1,0),
         
         sf_02_4 = ifelse(soc_2 == 4,1,0),
         
         re_02_1 = ifelse(rolem_2 == 1,1,0),
         
         re_03_1 = ifelse(rolem_3 == 1,1,0),
         
         mh_03_1 = ifelse(rmhi_3 == 1,1,0),
         
         mh_03_2 = ifelse(rmhi_3 == 2,1,0),
         
         mh_03_3 = ifelse(rmhi_3 == 3,1,0),
         
         mh_03_4 = ifelse(rmhi_3 == 4,1,0),
         
         mh_03_5 = ifelse(rmhi_3 == 5,1,0),
         
         mh_04_1 = ifelse(mhi_4 == 1,1,0),
         
         mh_04_2 = ifelse(mhi_4 == 2,1,0),
         
         mh_04_3 = ifelse(mhi_4 == 3,1,0),
         
         mh_04_4 = ifelse(mhi_4 == 4,1,0),
         
         mh_04_5 = ifelse(mhi_4 == 5,1,0)) %>%
  
  mutate(kroh12 = (-7.23216*pf_02_1) + (-3.45555*pf_02_2) + (-6.24397*pf_04_1) +     (-2.73557*pf_04_2) +
           
           (-4.61617*rp_02_1) +  (-5.51747*rp_03_1) + (-11.25544*bp_02_1) + (-8.38063*bp_02_2) +
           
           (-6.50522*bp_02_3) + (-3.80130*bp_02_4) + (-8.37399*gh_01_1) + (-5.56461*gh_01_2) + 
           
           (-3.02396*gh_01_3) + (-1.31872*gh_01_4) + (-2.44706*vt_02_1) + (-2.02168*vt_02_2) + 
           
           (-1.6185*vt_02_3) + (-1.14387*vt_02_4) + (-0.42251*vt_02_5) + (-0.33682*sf_02_1) +
           
           (-0.94342*sf_02_2) + (-0.18043*sf_02_3) + (0.11038*sf_02_4) + (3.04365*re_02_1) + 
           
           (2.32091*re_03_1) + (3.46638*mh_03_1) +  (2.90426*mh_03_2) + (2.37241*mh_03_3) + 
           
           (1.36689*mh_03_4) + (0.66514*mh_03_5) + (4.61446*mh_04_1) + (3.41593*mh_04_2) +
           
           (2.34247*mh_04_3) + (1.28044*mh_04_4) + (0.41188*mh_04_5),
         
         proh12 = (3.93115*pf_02_1) + (1.8684*pf_02_2) + (2.68282*pf_04_1) + (1.43103*pf_04_2) + 
           
           (1.4406*rp_02_1) +  (1.66968*rp_03_1) + (1.48619*bp_02_1) + (1.76691*bp_02_2) +
           
           (1.49384*bp_02_3) + (0.90384*bp_02_4) + (-1.71175*gh_01_1) + (-0.16891*gh_01_2) +
           
           (0.03482*gh_01_3) + (-0.06064*gh_01_4) + (-6.02409*vt_02_1) + (-4.88962*vt_02_2) +
           
           (-3.29805*vt_02_3) + (-1.65178*vt_02_4) + (-0.92057*vt_02_5) + (-6.29724*sf_02_1) +
           
           (-8.26066*sf_02_2) + (-5.63286*sf_02_3) + (-3.13896*sf_02_4) + (-6.82672*re_02_1) +
           
           (-5.69921*re_03_1) + (-10.19085*mh_03_1) + (-7.92717*mh_03_2) + (-6.31121*mh_03_3) +
           
           (-4.09842*mh_03_4) + (-1.94949*mh_03_5) + (-16.15395*mh_04_1) + (-10.77911*mh_04_2) +
           
           (-8.09914*mh_04_3) + (-4.59055*mh_04_4) + (-1.95934*mh_04_5)) %>%
  
  mutate(ksk12 = (kroh12 + 56.57706),
         
         psk12 = (proh12 + 60.75781)) 

#---------- SSD12
IFATICO <- IFATICO %>% mutate(ssd_kog = ssd12_1 + ssd12_4 + ssd12_7 + ssd12_10,
                                
                                ssd_aff = ssd12_2 + ssd12_5 + ssd12_8 + ssd12_12,
                                
                                ssd_beh = ssd12_3 + ssd12_6 + ssd12_9 + ssd12_11) %>% mutate(
                                  
                                  ssd_total = ssd_kog + ssd_aff + ssd_beh) 
                                  
                                

#---------- SSS8

IFATICO <- IFATICO %>% mutate(sss8_sum = sss8_1 + sss8_2 + sss8_3 + sss8_4 + sss8_5 + sss8_6 + sss8_7 + sss8_8)

#---------- EVS

IFATICO <- IFATICO %>% mutate(evs_total = evs_freq*evs_dur)

#---------- PCFS

IFATICO$pcfs_1

#---------- PGIC & TGIC

IFATICO$pgic_1 
IFATICO$tgic_1

#---------- 6min walk test

IFATICO$sechsm_geh_ergebnis

#---------- hand grip strength

IFATICO <- IFATICO %>% rowwise() %>% 
  mutate(hand_grip_fmean = mean(c(hand_grip_1,hand_grip_2,hand_grip_3,hand_grip_4,hand_grip_5,hand_grip_6, hand_grip_7,hand_grip_8,hand_grip_9,hand_grip_10))) %>%
  mutate(hand_grip_fmax = max(c(hand_grip_1,hand_grip_2,hand_grip_3,hand_grip_4,hand_grip_5,hand_grip_6, hand_grip_7,hand_grip_8,hand_grip_9,hand_grip_10))) %>% 
  mutate(hand_grip_ratio = hand_grip_fmax/ hand_grip_fmean)

#---------- Stroop

IFATICO <- IFATICO %>%
  mutate(stroop_effect = stroop_incongruent - stroop_congruent)

#---------- N-Back

IFATICO$n_back_ergebnis_treffer
IFATICO$n_back_false_alarms

#---------- Digit Span

IFATICO <- IFATICO %>%
  mutate(digit_total = digit_span_punkte_vorw + digit_span_punkte_rueckw)





######################## Raw means of FSS scores
raw_means <- IFATICO %>%
  group_by(Group, redcap_event_name) %>%
  summarise(
    mean_raw = mean(fss_total, na.rm = TRUE),
    sd_raw   = sd(fss_total, na.rm = TRUE),
    n        = n()
  )

raw_means

######################## Main Analysis
#Defining variables for main analysis

IFATICO$Group <- factor(IFATICO$Group)
IFATICO$time  <- factor(IFATICO$redcap_event_name, levels=c("t0", "t1"))
IFATICO$id    <- factor(IFATICO$id)

#-------- Adding varibale "age" to all timepoints
IFATICO <- IFATICO %>% 
  arrange(id, time) %>% 
  group_by(id) %>% 
  mutate(alter = first(na.omit(alter))) %>% 
  ungroup()


#-------- Main analysis
model_fss_itt <- lmer(fss_total ~ Group * time + alter + (1 | id), data = IFATICO)
summary(model_fss_itt)
sim_res <- simulateResiduals(model_fss_itt)
plot(sim_res)
performance::check_model(model_fss_itt)


dev.new() 
check_model(model_fss_itt)
ggsave("modelldiagnostik2.png", width = 12, height = 12, dpi = 900)
ggsave("modelldiagnostik.svg", device = "svg", width = 12, height = 12)
dev.off()

isSingular(model_fss_itt) #FALSE -> no problematic singularity in variances or correlations between random effects
VarCorr(model_fss_itt) #both bigger than zero -> no "fixed" model
ranef(model_fss_itt)$id  # all between -13 and 6,3, most close to zero


confint(model_fss_itt, method = "profile")

emmeans(model_fss_itt, pairwise ~ time | Group)

table(IFATICO$time, IFATICO$Group)

######################## Subgroup Analyses

#Subgroup analysis with gender as an interaction term
#-------- Adding varibale "geschlecht" to all timepoints
IFATICO <- IFATICO %>%
  arrange(id, redcap_event_name) %>%
  group_by(id) %>%
  mutate(geschlecht = na.locf(geschlecht, na.rm = FALSE)) %>%
  ungroup()

model_fss_itt_sex <- lmer(fss_total ~ Group * time * geschlecht + alter + (1 | id), data = IFATICO)
summary(model_fss_itt_sex)
sim_res_sex <- simulateResiduals(model_fss_itt_sex)
plot(sim_res_sex)

check_model(model_fss_itt_sex, check = c("normality", "qq", "linearity"))
isSingular(model_fss_itt_sex) #FALSE -> no problematic singularity in variances or correlations between random effects
VarCorr(model_fss_itt_sex) #both bigger than zero -> no "fixed" model
ranef(model_fss_itt_sex)$id  # all between -12,7 and 6,7, most close to zero


confint(model_fss_itt_sex, method = "profile")

emmeans(model_fss_itt_sex, pairwise ~ time | Group | geschlecht)

#Defining data sets for both groups
IFATICO_women <- IFATICO %>% 
  group_by(id) %>% 
  filter(geschlecht == 1) %>% 
  ungroup()

IFATICO_men <- IFATICO %>% 
  group_by(id) %>% 
  filter(geschlecht == 0) %>% 
  ungroup()

#Subgroup Analysis for Women
model_fss_itt_women <- lmer(fss_total ~ Group * time + alter + (1 | id), data = IFATICO_women)
summary(model_fss_itt_women)
sim_res_women <- simulateResiduals(model_fss_itt_women)
plot(sim_res_women)

check_model(model_fss_itt_women, check = c("normality", "qq", "linearity"))
isSingular(model_fss_itt_women) #FALSE -> no problematic singularity in variances or correlations between random effects
VarCorr(model_fss_itt_women) #both bigger than zero -> no "fixed" model
ranef(model_fss_itt_women)$id  # all between -11,7 and 6,3, most close to zero


confint(model_fss_itt_women, method = "profile")

emmeans(model_fss_itt_women, pairwise ~ time | Group )

#Subgroup Analysis for men
model_fss_itt_men <- lmer(fss_total ~ Group * time + alter + (1 | id), data = IFATICO_men)
summary(model_fss_itt_men)
sim_res_men <- simulateResiduals(model_fss_itt_men)
plot(sim_res_men)

check_model(model_fss_itt_men, check = c("normality", "qq", "linearity"))
isSingular(model_fss_itt_men) #FALSE -> no problematic singularity in variances or correlations between random effects
VarCorr(model_fss_itt_men) #both bigger than zero -> no "fixed" model
ranef(model_fss_itt_men)$id  # all between -10,8 and 5,7, most close to zero


confint(model_fss_itt_men, method = "profile")

emmeans(model_fss_itt_men, pairwise ~ time | Group )

#Subgroup analysis with overall training time as an interaction term
#-------- Adding varibale "overall training duration" to all timepoints
IFATICO <- IFATICO %>%
  group_by(id) %>%
  mutate(ov_dur_train = first(ov_dur_train[!is.na(ov_dur_train)])) %>%
  ungroup()
IFATICO$ov_dur_train <- as.numeric(IFATICO$ov_dur_train)


model_fss_itt_ovdur <- lmer(fss_total ~ Group * time * ov_dur_train + alter + (1 | id), data = IFATICO)
summary(model_fss_itt_ovdur)
sim_res_ovdur <- simulateResiduals(model_fss_itt_ovdur)
plot(sim_res_ovdur)
check_model(model_fss_itt_ovdur, check = c("normality", "qq", "linearity"))
isSingular(model_fss_itt_ovdur) #FALSE -> no problematic singularity in variances or correlations between random effects
VarCorr(model_fss_itt_ovdur) #both bigger than zero -> no "fixed" model
ranef(model_fss_itt_ovdur)$id  # all between -14,7 and 7,3, most close to zero


confint(model_fss_itt_ovdur, method = "profile")

emmeans(model_fss_itt_ovdur, pairwise ~ time | Group | ov_dur_train)


#Subgroup analysis with PEM-diagnosis as an interaction term

model_fss_itt_pem<- lmer(fss_total ~ Group * time * pem_level_1 + alter + (1 | id), data = IFATICO)
summary(model_fss_itt_pem)
sim_res_pem <- simulateResiduals(model_fss_itt_pem)
plot(sim_res_pem)
check_model(model_fss_itt_pem, check = c("normality", "qq", "linearity"))
isSingular(model_fss_itt_pem) #FALSE -> no problematic singularity in variances or correlations between random effects
VarCorr(model_fss_itt_pem) #both bigger than zero -> no "fixed" model
ranef(model_fss_itt_pem)$id  # all between -14,3 and 9,2, most close to zero


confint(model_fss_itt_pem, method = "profile")

emmeans(model_fss_itt_pem, pairwise ~ time | Group | pem_level_1)

#Defining data sets for both groups
IFATICO_pem <- IFATICO %>% 
  group_by(id) %>% 
  filter(pem_level_1 == 1) %>% 
  ungroup()

IFATICO_nopem <- IFATICO %>% 
  group_by(id) %>% 
  filter(pem_level_1 == 0) %>% 
  ungroup()

#Subgroup Analysis for Participants with PEM
model_fss_itt_pem <- lmer(fss_total ~ Group * time + alter + (1 | id), data = IFATICO_pem)
summary(model_fss_itt_pem)
sim_res_pem <- simulateResiduals(model_fss_itt_pem)
plot(sim_res_pem)

check_model(model_fss_itt_pem, check = c("normality", "qq", "linearity"))
isSingular(model_fss_itt_pem) #FALSE -> no problematic singularity in variances or correlations between random effects
VarCorr(model_fss_itt_pem) #both bigger than zero -> no "fixed" model
ranef(model_fss_itt_pem)$id  # all between -19,3 and 7,0


confint(model_fss_itt_pem, method = "profile")

emmeans(model_fss_itt_pem, pairwise ~ time | Group )

#Subgroup Analysis for Participants without PEM
model_fss_itt_nopem <- lmer(fss_total ~ Group * time + alter + (1 | id), data = IFATICO_nopem)
#CAVE: Model did not converge


######################## Analysis including all time points
#Defining variables

IFATICO$Group <- factor(IFATICO$Group)
IFATICO$timeall  <- factor(IFATICO$redcap_event_name, levels=c("t0", "t1", "t2", "t3"))
IFATICO$id    <- factor(IFATICO$id)

IFATICO <- IFATICO %>% 
  arrange(id, timeall) %>%             
  group_by(id) %>% 
  mutate(alter = first(na.omit(alter))) %>% 
  ungroup()

#Analysis including all time points

model_fss_itt_all <- lmer(fss_total ~ Group * timeall + alter + (1 | id), data = IFATICO)
summary(model_fss_itt_all)

sim_res <- simulateResiduals(model_fss_itt_all)
plot(sim_res)

performance::check_model(model_fss_itt_all, check = c("normality", "qq", "linearity"))
check_model(model_fss_itt_all)

confint(model_fss_itt_all, method = "profile")

emmeans(model_fss_itt_all, pairwise ~ timeall | Group)
emmeans(model_fss_itt_all, pairwise ~ Group | timeall)
confint(emmeans(model_fss_itt_all, pairwise ~ Group | timeall))


######################## Analysis of Secondary Outcomes
#------- How many n per variable?
vars_to_count <- c("hand_grip_ratio", "digit_total", "fss_total", "n_back_false_alarms", "n_back_ergebnis_treffer", "stroop_effect", "sechsm_geh_ergebnis", 
                   "pem_level_1", "ssd_total", "sss8_sum", "evs_total", "jss_total", "hads_a", "hads_d",
                   "hads_total", "fss_total", "ksk12", "psk12" )

Count_Variables <- IFATICO %>%
  filter(redcap_event_name %in% c("t0", "t1")) %>%
  group_by(Group, redcap_event_name) %>%
  summarise(
    across(
      all_of(vars_to_count),
      ~ sum(!is.na(.x)),
      .names = "n_valid_{.col}"
    ),
    .groups = "drop"
  )


#Analysis of secondary outcomes
modelhand <- lmer(hand_grip_ratio ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelhand)
confint(modelhand, method = "profile")
emmeans(modelhand, pairwise ~ time | Group)

modeldigit <- lmer(digit_total ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modeldigit)
confint(modeldigit, method = "profile")
emmeans(modeldigit, pairwise ~ time | Group)

modelfalse <- lmer(n_back_false_alarms ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelfalse)
confint(modelfalse, method = "profile")
emmeans(modelfalse, pairwise ~ time | Group)

modeltreffer <- lmer(n_back_ergebnis_treffer ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modeltreffer)
confint(modeltreffer, method = "profile")
emmeans(modeltreffer, pairwise ~ time | Group)

modelstroop <- lmer(stroop_effect ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelstroop)
confint(modelstroop, method = "profile")
emmeans(modelstroop, pairwise ~ time | Group)

modelsechs <- lmer(sechsm_geh_ergebnis ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelsechs)
confint(modelsechs, method = "profile")
emmeans(modelsechs, pairwise ~ time | Group)



IFATICO$pem_level_1 <- factor(IFATICO$pem_level_1)
modelpem <- glm(pem_level_1 ~ Group * time + alter, #categorial variable -> GLMM
                   data = IFATICO,
                  family = binomial(link = "logit"))
summary(modelpem)
confint(modelpem, method = "profile")
emmeans(modelpem, pairwise ~ time | Group, type = "response")
emm_resp <- emmeans(modelpem, ~ Group * time, type = "response")
contrast(emm_resp, 
         method = list("DiD_Prob" = c(-1, 1, 1, -1)))   


modelssd <- lmer(ssd_total ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelssd)
confint(modelssd, method = "profile")
emmeans(modelssd, pairwise ~ time | Group)

modelsss8 <- lmer(sss8_sum ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelsss8)
confint(modelsss8, method = "profile")
emmeans(modelsss8, pairwise ~ time | Group)

modelevs <- lmer(evs_total ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelevs)
confint(modelevs, method = "profile")
emmeans(modelevs, pairwise ~ time | Group)

modeljss <- lmer(jss_total ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modeljss)
confint(modeljss, method = "profile")
emmeans(modeljss, pairwise ~ time | Group)

modelhadsa <- lmer(hads_a ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelhadsa)
confint(modelhadsa, method = "profile")
emmeans(modelhadsa, pairwise ~ time | Group)

modelhadsd <- lmer(hads_d ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelhadsd)
confint(modelhadsd, method = "profile")
emmeans(modelhadsd, pairwise ~ time | Group)

modelhads <- lmer(hads_total ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelhads)
confint(modelhads, method = "profile")
emmeans(modelhads, pairwise ~ time | Group)

modelksk12 <- lmer(ksk12 ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelksk12)
confint(modelksk12, method = "profile")
emmeans(modelksk12, pairwise ~ time | Group)

modelpsk12 <- lmer(psk12 ~ Group * time + alter + (1 | id), data = IFATICO)
summary(modelpsk12)
confint(modelpsk12, method = "profile")
emmeans(modelpsk12, pairwise ~ time | Group)

######################## Sensitivity Analyses

#----------- As treated
#Defining data set
IFATICO_astreated <- IFATICO %>% 
  group_by(id) %>% 
  filter(
    !any(Dropout == 1, na.rm = TRUE)        
  ) %>% 
  ungroup()

IFATICO_astreated$Group <- factor(IFATICO_astreated$Group)
IFATICO_astreated$time  <- factor(IFATICO_astreated$redcap_event_name, levels=c("t0", "t1"))
IFATICO_astreated$id    <- factor(IFATICO_astreated$id)

table(IFATICO_astreated$id)
table(IFATICO_astreated$time)
table(IFATICO_astreated$id, IFATICO_astreated$time)

#Model: as treated
model_fss_astreated <- lmer(fss_total ~ Group * time + alter + (1 | id), data = IFATICO_astreated)
summary(model_fss_astreated)

sim_res <- simulateResiduals(model_fss_astreated)
plot(sim_res)

performance::check_model(model_fss_astreated, check = c("normality", "qq", "linearity"))

check_model(model_fss_astreated)

confint(model_fss_astreated, method = "profile")

emmeans(model_fss_astreated, pairwise ~ time | Group)

#----------- Per-Protocol
#Defining data set

IFATICO_perp <- IFATICO %>% 
  group_by(id) %>% 
  filter(
    PerP_cat[redcap_event_name == "t1"] == 1  
  ) %>% 
  ungroup()

table(IFATICO_perp$PerP_cat)


IFATICO_perp$Group <- factor(IFATICO_perp$Group)
IFATICO_perp$time  <- factor(IFATICO_perp$redcap_event_name, levels=c("t0", "t1"))
IFATICO_perp$id    <- factor(IFATICO_perp$id)

table(IFATICO_perp$time)

#Model: per protocol
model_fss_perp <- lmer(fss_total ~ Group * IFATICO_perp$time + alter + (1 | id), data = IFATICO_perp)
summary(model_fss_perp)

sim_res <- simulateResiduals(model_fss_perp)
plot(sim_res)

performance::check_model(model_fss_perp, check = c("normality", "qq", "linearity"))

check_model(model_fss_perp)

confint(model_fss_perp, method = "profile")

emmeans(model_fss_perp, pairwise ~ time | Group)

#----------- Unblinding of Participants
# Defining data set

IFATICO_blindpart <- IFATICO %>% 
  group_by(id) %>% 
  filter(
    ass2_verblindung[redcap_event_name == "t1"] == 0  
  ) %>% 
  ungroup()

IFATICO_blindpart$Group <- factor(IFATICO_blindpart$Group)
IFATICO_blindpart$time  <- factor(IFATICO_blindpart$redcap_event_name, levels=c("t0", "t1"))
IFATICO_blindpart$id    <- factor(IFATICO_blindpart$id)

table(IFATICO_blindpart$time)

# Model: Unblinding of participants
model_fss_blindpart <- lmer(fss_total ~ Group * IFATICO_blindpart$time + alter + (1 | id), data = IFATICO_blindpart)
summary(model_fss_blindpart)

sim_res <- simulateResiduals(model_fss_blindpart)
plot(sim_res)

performance::check_model(model_fss_blindpart, check = c("normality", "qq", "linearity"))

check_model(model_fss_blindpart)

confint(model_fss_blindpart, method = "profile")

emmeans(model_fss_blindpart, pairwise ~ time | Group)


#----------- Unblinding of Outcome-Assesors
# Defining data set
IFATICO_blindpers <- IFATICO %>% 
  group_by(id) %>% 
  filter(
   blind_per[redcap_event_name == "t1"] == 0  
  ) %>% 
  ungroup()

IFATICO_blindpers$Group <- factor(IFATICO_blindpers$Group)
IFATICO_blindpers$time  <- factor(IFATICO_blindpers$redcap_event_name, levels=c("t0", "t1"))
IFATICO_blindpers$id    <- factor(IFATICO_blindpers$id)

table(IFATICO$Group, IFATICO$blind_per)

#Model: Unblinding of outcome assesors
model_fss_blindpers <- lmer(fss_total ~ Group * IFATICO_blindpers$time + alter + (1 | id), data = IFATICO_blindpers)
summary(model_fss_blindpers)

sim_res <- simulateResiduals(model_fss_blindpers)
plot(sim_res)

performance::check_model(model_fss_blindpers, check = c("normality", "qq", "linearity"))

check_model(model_fss_blindpers)

confint(model_fss_blindpers, method = "profile")

emmeans(model_fss_blindpers, pairwise ~ time | Group)


#----------- Complete case
# Defining data set
IFATICO_complete <- IFATICO %>%
  filter(redcap_event_name %in% c("t0", "t1")) %>%
  group_by(id) %>%
  filter(all(!is.na(fss_total))) %>%
  ungroup()

IFATICO_complete$fss_total


IFATICO_complete$Group <- factor(IFATICO_complete$Group)
IFATICO_complete$time  <- factor(IFATICO_complete$redcap_event_name, levels=c("t0", "t1"))
IFATICO_complete$id    <- factor(IFATICO_complete$id)

# Model: Complete case
model_complete <- lmer(fss_total ~ Group * IFATICO_complete$time + alter + (1 | id), data = IFATICO_complete)
summary(model_complete)

sim_res <- simulateResiduals(model_complete)
plot(sim_res)

performance::check_model(model_complete, check = c("normality", "qq", "linearity"))

check_model(model_complete)

confint(model_complete, method = "profile")

emmeans(model_complete, pairwise ~ time | Group)



#Number of participants for sensitivity analyses
IFATICO_astreated %>%
  group_by(Group) %>%
  summarise(n_ids = n_distinct(id))

IFATICO_perp %>%
  group_by(Group) %>%
  summarise(n_ids = n_distinct(id))

IFATICO_blindpart %>%
  group_by(Group) %>%
  summarise(n_ids = n_distinct(id))

IFATICO_blindpers %>%
  group_by(Group) %>%
  summarise(n_ids = n_distinct(id))

# (How many assessments?)
table(IFATICO$time, IFATICO$assessment_complete)
table(IFATICO$Group, IFATICO$ass2_verblindung)

# (How many complete cases?)
IFATICO_complete %>%
  group_by(Group) %>%
  summarise(n_ids = n_distinct(id))

######################## Responder Analysis

IFATICO_change <- IFATICO %>%
  select(id, Group, redcap_event_name, fss_total) %>%
  pivot_wider(names_from = redcap_event_name, values_from = fss_total) %>%
  mutate(fss_change = t1 - t0)

table(IFATICO_change$fss_change)
table(IFATICO_change$Group, IFATICO_change$fss_change)

IFATICO_change <- IFATICO_change %>%
  group_by(id) %>%
  mutate(respondstatus = (ifelse(fss_change <= -6.3, "Responder", "Non_Responder")))

table_responder <- table(IFATICO_change$Group, IFATICO_change$respondstatus)
table_responder
chisq.test(table_responder, correct = FALSE)


IFATICO_change <- IFATICO_change %>%
  group_by(id) %>%
  mutate(worsening = (ifelse(fss_change > 0, "Worsening", "No Worsening")))

table_worsening <- table(IFATICO_change$Group, IFATICO_change$worsening)
table_worsening
chisq.test(table_worsening, correct = FALSE)

######################## Analysis of Patients Global Impression of Change

IFATICO_T1 <- subset(IFATICO, redcap_event_name == "t1")
table(IFATICO_T1$pgic_1, IFATICO_T1$Group)

IFATICO_T1 <- IFATICO_T1 %>%
  group_by(id) %>%
  mutate(pgicstatus = (ifelse(pgic_1 %in% c(0, 1), "Much improved", "Not much improved")))

table_pgic <- table(IFATICO_T1$Group, IFATICO_T1$pgicstatus)
table_pgic
chisq.test(table_pgic, correct = FALSE)

######################## Analysis of baseline data
#Defining all variables for baseline data

#-----------Sick leave
IFATICO$sickdays <- as.numeric(IFATICO$krankschreiben_1)


#----------- BMI

IFATICO$groesse <- as.numeric(IFATICO$groesse)
IFATICO$gewicht <- as.numeric(IFATICO$gewicht)

IFATICO <- IFATICO %>% mutate(bmi = (gewicht/((groesse/100)^2)))

#----------- Smoking

IFATICO <- IFATICO %>%
  mutate(rauchencat = ifelse(rauchen == 1, 1, 0))

#----------- marital status

IFATICO <- IFATICO %>%
  mutate(familienstandcat = ifelse(familienstand == 2, 1, 0))

table(IFATICO$familienstand)
table(IFATICO$familienstandcat, useNA = "ifany")

#----------- employment

IFATICO <- IFATICO %>%
  mutate(berufcat = ifelse(beruf %in% c(6, 7), 0, 1))

table(IFATICO$berufcat)
table(IFATICO$beruf)

#----------- Post-Covid symptoms

IFATICO <- IFATICO %>% mutate(symptoms_res = ifelse(post_cov_1___1 == 1 | post_cov_1___2 == 1,1,0),
                                
                                symptoms_neu = ifelse(post_cov_1___8 == 1 | post_cov_1___9 == 1 | post_cov_1___10 == 1 | post_cov_1___12 == 1 | post_cov_1___13 == 1
                                                      
                                                      | post_cov_1___22 == 1 | post_cov_1___25 == 1 | post_cov_1___26 == 1 | post_cov_1___30 == 1| post_cov_1___2 ==                         31,1,0),
                                
                                symptoms_inf = ifelse(post_cov_1___7 == 1 | post_cov_1___18 == 1| post_cov_1___19 == 1| post_cov_1___23 == 1| post_cov_1___24 == 
                                                        
                                                        1,1,0),
                                
                                symptoms_car = ifelse(post_cov_1___3 == 1 | post_cov_1___4 == 1 | post_cov_1___5 == 1,1,0),
                                
                                symptoms_der = ifelse(post_cov_1___27 == 1 | post_cov_1___28 == 1 | post_cov_1___29 == 1,1,0),
                                
                                symptoms_gas = ifelse(post_cov_1___14 == 1 | post_cov_1___15 == 1 | post_cov_1___16 == 1 | post_cov_1___17 == 1,1,0),
                                
                                symptoms_mh = ifelse(post_cov_1___20 == 1 | post_cov_1___21 == 1 | post_cov_1___11 == 1,1,0)) 


#---------- Post-Covid symptom duration

table(IFATICO$post_cov_3)

IFATICO <- IFATICO %>%
  mutate(durat6 = ifelse(post_cov_3 == 3, 1, 0))

table(IFATICO$durat6)

#----------- Creating table 1

IFATICO_T0 <- subset(IFATICO, redcap_event_name == "t0")

IFATICO_T0$rauchencat <- factor(IFATICO_T0$rauchencat, levels = c("0", "1"))
IFATICO_T0$familienstandcat <- factor(IFATICO_T0$familienstandcat, levels = c("0", "1"))
IFATICO_T0$berufcat <- factor(IFATICO_T0$berufcat, levels = c("0", "1"))
IFATICO_T0$durat6 <- factor(IFATICO_T0$durat6, levels = c("0", "1"))
IFATICO_T0$pem_level_1 <- factor(IFATICO_T0$pem_level_1, levels = c("0", "1"))


q_table <- compareGroups::compareGroups(Group ~ alter + geschlecht + familienstandcat + rauchencat + berufcat + bmi + hand_grip_ratio 
                                        + digit_total + n_back_false_alarms + n_back_ergebnis_treffer + stroop_effect + sechsm_geh_ergebnis 
                                        + pcfs_1 + pem_level_1 + ssd_total + sss8_sum + evs_total + jss_total + hads_a + hads_d 
                                        + hads_total + fss_total + ksk12 + psk12 + durat6 + krankschreiben_1,
                                          
                                          data = IFATICO_T0, ref = 0)


createTable(q_table)
 
tab <- createTable(q_table)

export2csv(tab, file = "q_table.csv")
getwd()

#----------------- Analysis of baseline dropout data

IFATICO_drop <- IFATICO %>% 
  group_by(id) %>% 
  filter(
    any(Dropout == 1, na.rm = TRUE)        
  ) %>% 
  ungroup()

IFATICO_drop_T0 <- subset(IFATICO_drop, redcap_event_name == "t0")

IFATICO_drop_T0$rauchencat <- factor(IFATICO_drop_T0$rauchencat, levels = c("0", "1"))
IFATICO_drop_T0$familienstandcat <- factor(IFATICO_drop_T0$familienstandcat, levels = c("0", "1"))
IFATICO_drop_T0$berufcat <- factor(IFATICO_drop_T0$berufcat, levels = c("0", "1"))
IFATICO_drop_T0$durat6 <- factor(IFATICO_drop_T0$durat6, levels = c("0", "1"))
IFATICO_drop_T0$pem_level_1 <- factor(IFATICO_drop_T0$pem_level_1, levels = c("0", "1"))


table(IFATICO$rauchencat, useNA = "ifany")
table(IFATICO_drop_T0$rauchencat, useNA = "ifany")
IFATICO_drop_T0$familienstandcat
IFATICO_drop_T0 %>% 
  filter(is.na(rauchencat) | is.na(familienstandcat)) %>% 
  select(id, redcap_event_name, Dropout,
         rauchen, rauchencat,
         familienstand, familienstandcat)

q_table2 <- compareGroups::compareGroups(Group ~ alter + geschlecht + familienstandcat + rauchencat + berufcat + bmi + hand_grip_ratio 
                                        + digit_total + n_back_false_alarms + n_back_ergebnis_treffer + stroop_effect + sechsm_geh_ergebnis 
                                        + pcfs_1 + pem_level_1 + ssd_total + sss8_sum + evs_total + jss_total + hads_a + hads_d 
                                        + hads_total + fss_total + ksk12 + psk12 + durat6,
                                        
                                        data = IFATICO_drop_T0, ref = 0)

createTable(q_table2)
tab2 <- createTable(q_table2)

export2csv(tab2, file = "q_table2.csv")
getwd()