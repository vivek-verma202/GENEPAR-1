library(tidyverse)
library(haven)
df <- readxl::read_excel("data.xlsx")
df <- within(df, {
  DIAGNOSIS <- as.factor(DIAGNOSIS)
  SEX <- as.factor(SEX)
  LANG <- as.factor(LANG)
  CLINIC <- as.factor(CLINIC)
  DOB <- as.Date(DOB)
  PAIN_DURATION <- factor(PAIN_DURATION, 
                         labels=c("less than 1 month",
                                  "1-3 months","3-5 months","6-11 months",
                                  "1-5 years","more than 5 years"),ordered = T)
  PAIN_FREQ <- factor(PAIN_FREQ, labels = c("everyday",
                                          "at least half of the days",
                                          "less than half of the days"))
  PAIN_AB <- factor(PAIN_AB,labels = c("not at all","a little","a lot"),
                    ordered = T)
  PAIN_HEAD <- factor(PAIN_HEAD,labels = c("not at all","a little","a lot"),
                      ordered = T)
  PAIN_LIMB <- factor(PAIN_LIMB,labels = c("not at all","a little","a lot"),
                      ordered = T)
  PAIN_WIDESPREAD <- factor(PAIN_WIDESPREAD,
                            labels = c("not at all","a little","a lot"),
                            ordered = T)
  PROMIS_SFv1_PI_4a_1 <- factor(PROMIS_SFv1_PI_4a_1,labels = c("Not at all",
                                                               "A little bit",
                                                               "Somewhat",
                                                               "Quite a bit",
                                                               "Very much"),
                                ordered = T)
  PROMIS_SFv1_PI_4a_2 <- factor(PROMIS_SFv1_PI_4a_2,labels = c("Not at all",
                                                               "A little bit",
                                                               "Somewhat",
                                                               "Quite a bit",
                                                               "Very much"),
                                ordered = T)
  PROMIS_SFv1_PI_4a_3 <- factor(PROMIS_SFv1_PI_4a_3,labels = c("Not at all",
                                                               "A little bit",
                                                               "Somewhat",
                                                               "Quite a bit",
                                                               "Very much"),
                                ordered = T)
  PROMIS_SFv1_PI_4a_4 <- factor(PROMIS_SFv1_PI_4a_4,labels = c("Not at all",
                                                               "A little bit",
                                                               "Somewhat",
                                                               "Quite a bit",
                                                               "Very much"),
                                ordered = T)
  PROMIS_SFv2_PF_4a_1 <- factor(PROMIS_SFv2_PF_4a_1,
                                labels=c("Without any difficulty",
                                "With a little difficulty",
                                "With some difficulty",
                                "With much difficulty",
                                "Unable to do"),ordered = T)
  PROMIS_SFv2_PF_4a_2 <- factor(PROMIS_SFv2_PF_4a_2,
                                labels=c("Without any difficulty",
                                         "With a little difficulty",
                                         "With some difficulty",
                                         "With much difficulty",
                                         "Unable to do"),ordered = T)
  PROMIS_SFv2_PF_4a_3 <- factor(PROMIS_SFv2_PF_4a_3,
                                labels=c("Without any difficulty",
                                         "With a little difficulty",
                                         "With some difficulty",
                                         "With much difficulty",
                                         "Unable to do"),ordered = T)
  PROMIS_SFv2_PF_4a_4 <- factor(PROMIS_SFv2_PF_4a_4,
                                labels=c("Without any difficulty",
                                         "With a little difficulty",
                                         "With some difficulty",
                                         "With much difficulty",
                                         "Unable to do"),ordered = T)
  PROMIS_SFv1_DD_4a_1 <- factor(PROMIS_SFv1_DD_4a_1,
                                labels=c("Never","Rarely","Sometimes",
                                "Often","Always"),ordered = T)
  PROMIS_SFv1_DD_4a_2 <- factor(PROMIS_SFv1_DD_4a_2,
                                labels=c("Never","Rarely","Sometimes",
                                         "Often","Always"),ordered = T)
  PROMIS_SFv1_DD_4a_3 <- factor(PROMIS_SFv1_DD_4a_3,
                                labels=c("Never","Rarely","Sometimes",
                                         "Often","Always"),ordered = T)
  PROMIS_SFv1_DD_4a_4 <- factor(PROMIS_SFv1_DD_4a_4,
                                labels=c("Never","Rarely","Sometimes",
                                         "Often","Always"),ordered = T)
  PROMIS_SFv1_SD_4a_1 <- factor(PROMIS_SFv1_SD_4a_1,
                                labels=c("Very poor","Poor","Fair",
                                         "Good","Very good"),ordered = T)
  PROMIS_SFv1_SD_4a_2 <- factor(PROMIS_SFv1_SD_4a_2,
                                labels=c("Not at all",
                                         "A little bit",
                                         "Somewhat",
                                         "Quite a bit",
                                         "Very much"),ordered = T)
  PROMIS_SFv1_SD_4a_3 <- factor(PROMIS_SFv1_SD_4a_3,
                                labels=c("Not at all",
                                         "A little bit",
                                         "Somewhat",
                                         "Quite a bit",
                                         "Very much"),ordered = T)
  PROMIS_SFv1_SD_4a_4 <- factor(PROMIS_SFv1_SD_4a_4,
                                labels=c("Not at all",
                                         "A little bit",
                                         "Somewhat",
                                         "Quite a bit",
                                         "Very much"),ordered = T)
  PAIN_SAFE <- factor(PAIN_SAFE, labels=c("Disagree",
                                        "Agree"))
  PAIN_TERRIBLE <- factor(PAIN_TERRIBLE, 
                         labels=c("Disagree","Agree"))
  PAIN_LAWSUIT <- factor(PAIN_LAWSUIT, labels=c("No",
                                              "Yes","not sure"))
  ALCO_DRUGS_CONSUMED <- factor(ALCO_DRUGS_CONSUMED, labels=c("never",
                                                              "rarely",
                                                              "sometimes",
                                                              "often",
                                                              "always"),
                                ordered = T)
  ALCO_DRUGS_CRAVING <- factor(ALCO_DRUGS_CRAVING, labels=c("never",
                                                            "rarely",
                                                            "sometimes",
                                                            "often",
                                                            "always"),
                               ordered = T)
  ABORIGINAL <- factor(ABORIGINAL, labels=c("No","First Nations",
                                            "Metis","Inuk"))
  RACE <- factor(RACE, labels=c("White","South Asian","Chinese","Black",
                                "Latin American","Arab","West Asian","Other"))
  EMPLOYMENT <- factor(EMPLOYMENT, 
                       labels=c("Full-time","Part-time",
                                "Unemployed","Sick or Mat leave",
                                "Disable (LBP)","Disable (other)","Student",
                                "Temp laid off","Retired","Keeping house",
                                "Unknown","Other"))
  EDUCATION <- factor(EDUCATION, labels=c("No high school","High school",
                                          "Registered Apprenticeship",
                                          "College (CEGEP)",
                                          "University UG Certificate",
                                          "Bachelors degree",
                                          "University G Certificate",
                                          "Masters","Medicine degree",
                                          "Doctorate"), ordered = T)
  SMOKING <- factor(SMOKING, labels=c("Never smoked","Current smoker",
                                      "Used to smoke"))
  DN4_BURNING <- factor(DN4_BURNING, labels=c("No","Yes"))
  DN4_PAINFUL_COLD <- factor(DN4_PAINFUL_COLD, labels=c("No","Yes"))
  DN4_ELECTRIC_SHOCK <- factor(DN4_ELECTRIC_SHOCK, labels=c("No","Yes"))
  DN4_TINGLING <- factor(DN4_TINGLING, labels=c("No","Yes"))
  DN4_PINS_NEEDLES <- factor(DN4_PINS_NEEDLES, labels=c("No","Yes"))
  DN4_NUMBNESS <- factor(DN4_NUMBNESS, labels=c("No","Yes"))
  DN4_ITCHING <- factor(DN4_ITCHING, labels=c("No","Yes"))
  SCL_HEADACHE <- factor(SCL_HEADACHE,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_CHEST_PAIN <- factor(SCL_CHEST_PAIN,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_LBP <- factor(SCL_LBP,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_NAUSEA_UPSET_STOMACH <- factor(SCL_NAUSEA_UPSET_STOMACH,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_MUSCLE_SORENESS <- factor(SCL_MUSCLE_SORENESS,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_DIZZY <- factor(SCL_DIZZY,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_TROUBLE_BREATH <- factor(SCL_TROUBLE_BREATH,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_HOT_COLD_SPELLS <- factor(SCL_HOT_COLD_SPELLS,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_NUMBNESS <- factor(SCL_NUMBNESS,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_LUMP_THROAT <- factor(SCL_LUMP_THROAT,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_WEAKNESS <- factor(SCL_WEAKNESS,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
  SCL_HEAVY_LIMBS <- factor(SCL_HEAVY_LIMBS,
                         labels=c("Not at all","A little bit","Moderately",
                                  "Quite a bit","Extremely"),ordered = T)
})

# height data has a lot of extreme values:
boxplot(df$HEIGHT)
# fix height outliers:
df$HEIGHT <- DescTools::Winsorize(df$HEIGHT)
# calculate BMI:
df$BMI <- df$WEIGHT/(df$HEIGHT/100)^2
# get baseline pain and covariate data
df1 <- read_sas("/mnt/nfs/backup/data/quebec_pain_registry/outdir/requete.sas7bdat",
               "/mnt/nfs/backup/data/quebec_pain_registry/outdir/formats.sas7bcat")
df1 <- df1 %>% select(c(ID, PQ00_AGE, PQ00_SEX,PQ00_YOUR_PAIN_Q2)) %>% 
  mutate_all(as.character) %>% mutate_all(zap_empty)
colnames(df1) <- c("ID","AGE","SEX_1","PAIN_BASELINE")
df1$AGE <- as.numeric(df1$AGE)
df1$SEX_1 <- as.factor(ifelse(df1$SEX_1==1,"Women","Men")) # coding checked
df1$PAIN_BASELINE <- as.numeric(df1$PAIN_BASELINE)
df1 <- zap_label(df1)
df <- merge(df,df1,by="ID",all.x=T)
# QC:
table(df$SEX,df$SEX_1)
df$SEX_1 <- NULL
plot(df$DOB,df$AGE)
# baseline medication data:
df1 <- haven::read_sas("/mnt/nfs/backup/data/quebec_pain_registry/outdir/in00_pmed_actuell_boni.sas7bdat",
                "/mnt/nfs/backup/data/quebec_pain_registry/outdir/formats.sas7bcat")
# select the columns with ID and active drug:
df1 <- df1 %>% select(c(ID,NQ00_CPM_ACT)) %>%
  mutate_all(as.character) %>% mutate_all(zap_empty) %>% filter(complete.cases(.))
length(df1$ID) # 2243
length(unique(df1$ID)) # 812
# remove redundant rows:
df1 <- unique(df1)
length(df1$ID) # 2036
# create medication classes:
df1$ACETAMINOPHEN <- NA
df1$GABA_ANALOGS <- NA
df1$OPIOIDS <- NA
df1$NSAIDS <- NA
df1$OTHER <- NA
df1$ACETAMINOPHEN <- ifelse(grepl("ACETAMINOPH", df1$NQ00_CPM_ACT),1,0)
df1$GABA_ANALOGS <- ifelse(grepl("GABA", df1$NQ00_CPM_ACT),1,0)
df1$OPIOIDS <- ifelse(grepl("MORPH|CODONE|CODEINE|CODÉINE|METHADONE|
                                      TRAMADOL|FENTANYL|TAPENTADOL",
                                      df1$NQ00_CPM_ACT),1,0)
df1$NSAIDS <- ifelse(grepl("PROFEN|NAPROX|KETORO|COXIB|FENAC",
                                  df1$NQ00_CPM_ACT),1,0)
# there were no missing drug data, if a drug was not assigned a class, count it as "OTHER":
df1$OTHER <- ifelse(
  df1$ACETAMINOPHEN+df1$GABA_ANALOGS+df1$OPIOIDS+df1$NSAIDS == 0, 1, 0)
df1$NQ00_CPM_ACT <- NULL
df1 <- unique(df1) # from 2036 to 1801
# but IDs are still not unique
df1[df1 == 0] <- NA 
df1 <- df1 %>% group_by(ID) %>% summarise_all(list(~ .[!is.na(.)][1]))
df1[is.na(df1)] <- 0
df1 <- df1 %>% mutate_if(is.numeric,as.factor)
length(df1$ID) == length(unique(df1$ID)) # TRUE :)
length(df$ID) == length(unique(df$ID)) # TRUE :)
df <- merge(df,df1,by="ID",all.x=T)
# visualize missing data
naniar::vis_miss(df)
naniar::gg_miss_var(df)
# naniar::gg_miss_upset(df)
# final cleaning
df <- df[,c(1:8,76,75,77,11,9,10,12:74,78:82)]
names(df)[12] <- "PAIN_FOLLOW_UP"
# make sure all is well:
glimpse(df)
####################################
saveRDS(df,"./genepar/GENEPAR1.RDS")
####################################