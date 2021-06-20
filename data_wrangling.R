df <- readr::read_delim("genepar/GENEPAR-1.txt", "\t", escape_double = F,
                 col_types = cols(SEX = col_factor(levels = c("Female","Male")),
                                  LANG = col_factor(levels = c("French","English")),
                                  DOB = col_date(format = "%d-%m-%Y")),
                 trim_ws = T)
names(df)[c(51,52)] <- c("ALCO_DRUGS_CONSUMED","ALCO_DRUGS_CRAVING")
df <- within(df, {
  CLINIC <- as.factor(CLINIC)
  SCORE_SCL90RSSP <- as.numeric(SCORE_SCL90RSSP)
  SCORE_SCL90RSSWP <- as.numeric(SCORE_SCL90RSSWP)
  SCORE_SCL90R <- as.numeric(SCORE_SCL90R)
  ABORIGINAL <- factor(ABORIGINAL, labels=c('No','First Nations',
                                            'Metis','Inuk'))
  RACE <- factor(RACE, labels=c('White','Chinese','Black',
                                'Latin American','Arab','Other'))
  ALCO_DRUGS_CONSUMED <- factor(ALCO_DRUGS_CONSUMED, labels=c('never',
                                                            'rarely',
                                                            'sometimes',
                                                            'often',
                                                            'always'),
                               ordered = T)
  ALCO_DRUGS_CRAVING <- factor(ALCO_DRUGS_CRAVING, labels=c('never',
                                                            'rarely',
                                                            'sometimes',
                                                            'often',
                                                            'always'),
                               ordered = T)
  EDUCATION <- factor(EDUCATION, labels=c('No high school','High school',
                                          'Registered Apprenticeship',
                                          'College, CEGEP',
                                          'University UG Certificate',
                                          'Bachelors degree',
                                          'University G Certificate',
                                          'Masters','Medicine degree',
                                          'Doctorate'), ordered = T)
  EMPLOYMENT <- factor(EMPLOYMENT, 
                       labels=c('Full-time','Part-time',
                                'Looking for work','Sick or Mat leave',
                                'Disable - LBP','Disable - other','Student',
                                'Temp laid off','Retired','Keeping house',
                                'Other'))
  EXERCISE <- factor(EXERCISE, labels=c('No',
                                        'Yes - currently','Yes - in past',
                                        'not sure'))
  INJECTIONS <- factor(INJECTIONS, labels=c('No',
                                            'Yes - currently','Yes - in past',
                                            'not sure'))
  OPIOIDS <- factor(OPIOIDS, labels=c('No',
                                      'Yes - currently','Yes - in past',
                                      'not sure'))
  PSYCHO <- factor(PSYCHO, labels=c('No',
                                    'Yes - currently','Yes - in past',
                                    'not sure'))
  LB_SURGERY_ARTHRODESIS <- 
    factor(LB_SURGERY_ARTHRODESIS, labels=c('No','Yes',
                                            'not sure'))
  LBP_LAWSUIT <- factor(LBP_LAWSUIT, labels=c('No',
                                              'Yes','not sure'))
  LBP_RADIATING <- factor(LBP_RADIATING, 
                          labels=c('No','Yes','not sure'))
  LB_SURGERY_EVER <- factor(LB_SURGERY_EVER, 
                            labels=c('No','Yes - one','Yes - more than one'))
  LB_SURGERY_WHEN <- factor(LB_SURGERY_WHEN, 
                            labels=c('Less than 6 months ago','6 -12 months',
                                     '1 - 2 years','More than 2 years'))
  LBP_COMPENSATION <- factor(LBP_COMPENSATION, 
                             labels=c('No','Yes','not sure'))
  LBP_DURATION <- factor(LBP_DURATION, 
                         labels=c('less than 1 month','6-11 months',
                                  '1-5 years','more than 5 years'),ordered = T)
  LBP_SAFE <- factor(LBP_SAFE, labels=c('Disagree',
                                        'Agree'))
  LBP_TERRIBLE <- factor(LBP_TERRIBLE, 
                         labels=c('Disagree','Agree'))
  PAIN_AB <- factor(PAIN_AB,labels = c('not at all','a little','a lot'),
                    ordered = T)
  PAIN_HEAD <- factor(PAIN_HEAD,labels = c('not at all','a little','a lot'),
                      ordered = T)
  PAIN_LIMB <- factor(PAIN_LIMB,labels = c('not at all','a little','a lot'),
                      ordered = T)
  PAIN_WIDESPREAD <- factor(PAIN_WIDESPREAD,
                            labels = c('not at all','a little','a lot'),
                            ordered = T)
  Q1a <- factor(Q1a, labels=c('No','Yes'))
  Q1b <- factor(Q1b, labels=c('No','Yes'))
  Q1c <- factor(Q1c, labels=c('No','Yes'))
  Q2a <- factor(Q2a, labels=c('No','Yes'))
  Q2b <- factor(Q2b, labels=c('No','Yes'))
  Q2c <- factor(Q2c, labels=c('No','Yes'))
  Q2d <- factor(Q2d, labels=c('No','Yes'))
  SLEEP_QUALITY <- factor(SLEEP_QUALITY, labels=c('Very poor',
                                                  'Poor',
                                                  'Fair',
                                                  'Good',
                                                  'Very good'),
                          ordered = T)
  SMOKING <- factor(SMOKING, labels=c('Never smoked','Current smoker',
                                      'Used to smoke'))
    Q12a <- factor(Q12a, labels=c('Without any difficulty',
                                  'With a little difficulty',
                                  'With some difficulty',
                                  'With much difficulty',
                                  'Unable to do'),ordered = T)
    Q12b <- factor(Q12b, labels=c('Without any difficulty',
                                  'With a little difficulty',
                                  'With some difficulty',
                                  'With much difficulty',
                                  'Unable to do'),ordered = T)
    Q12c <- factor(Q12c, labels=c('Without any difficulty',
                                  'With a little difficulty',
                                  'With some difficulty',
                                  'With much difficulty',
                                  'Unable to do'),ordered = T)
    Q12d <- factor(Q12d, labels=c('Without any difficulty',
                                  'With a little difficulty',
                                  'With some difficulty',
                                  'With much difficulty',
                                  'Unable to do'),ordered = T)
    Q13a <- factor(Q13a, labels=c('Never',
                                    'Rarely',
                                    'Sometimes',
                                    'Often',
                                    'Always'),ordered = T)
    Q13b <- factor(Q13b, labels=c('Not at all',
                                    'Rarely',
                                    'Sometimes',
                                    'Often',
                                    'Always'),ordered = T)
    Q13c <- factor(Q13c, labels=c('Never',
                                    'Rarely',
                                    'Sometimes',
                                    'Often',
                                    'Always'),ordered = T)
    Q13d <- factor(Q13d, labels=c('Never',
                                    'Rarely',
                                    'Sometimes',
                                    'Often',
                                    'Always'),ordered = T)    
    Q15a <- factor(Q15a, labels=c('Not at all',
                                      'A little bit',
                                      'Somewhat',
                                      'Quite a bit',
                                      'Very much'),ordered = T)
    Q15b <- factor(Q15b, labels=c('Not at all',
                                      'A little bit',
                                      'Somewhat',
                                      'Quite a bit',
                                      'Very much'),ordered = T)
    Q15c <- factor(Q15c, labels=c('Not at all',
                                      'A little bit',
                                      'Somewhat',
                                      'Quite a bit',
                                      'Very much'),ordered = T)
    Q9a <- factor(Q9a, labels=c('Not at all',
                                  'A little bit',
                                  'Somewhat',
                                  'Quite a bit',
                                  'Very much'),ordered = T)
    Q9b <- factor(Q9b, labels=c('Not at all',
                                  'A little bit',
                                  'Somewhat',
                                  'Quite a bit',
                                  'Very much'),ordered = T)
    Q9c <- factor(Q9c, labels=c('Not at all',
                                  'A little bit',
                                  'Somewhat',
                                  'Quite a bit',
                                  'Very much'),ordered = T)
    Q9d <- factor(Q9d, labels=c('Not at all',
                                'A little bit',
                                'Somewhat',
                                'Quite a bit',
                                'Very much'),ordered = T)
    LBP_FREQ <- factor(LBP_FREQ, labels = c('everyday',
                                            'at least half of the days',
                                            'less than half of the days'))
})

saveRDS(df,"C:/Users/Vivek/Desktop/GENEPAR-1/genepar/GENEPAR1.RDS")