#load file
vaxvote.raw <- read_csv(file = "/Users/christopheroconnor/Dropbox/My things/School Stuff/Fall 2021/MPH 5907 Capstone 1/Datasets/VaxVote2020total.csv")

#create new variables----
vaxvote.new <- vaxvote.raw %>% 
  drop_na() %>% 
  mutate(CntyPol = ifelse(TRUMP_PCT > 0.5, 1, 0)) %>% 
  mutate(CntyPol = recode_factor(.x = CntyPol,
                                 '1' = 'Republican',
                                 '0' = 'Democrat')) %>% 
  mutate(COVID_VAX_USAVG = ifelse(CURRFULLPCT > '65', 'Above', 'below')) %>%
  mutate(COVID_VAX_USAVG_BINOM = recode_factor (.x = COVID_VAX_USAVG,
                                                'Above' = '1',
                                                'Below' = '0')) %>% 
  mutate(EST_FLUVAX_PCT = EST_FLUVAX_PCT*100) %>% 
  mutate(TRUMP_PCT = TRUMP_PCT*100) %>% 
  mutate(BIDEN_PCT = BIDEN_PCT*100) %>% 
  mutate(WHITE_PCT = (WHITE/TOTALPOP)*100) %>% 
  mutate(BLACK_PCT = (BLACK/TOTALPOP)*100) %>% 
  mutate(NATIVE_AMER_PCT = (NATIVE_AMER/TOTALPOP)*100) %>% 
  mutate(ASIAN_PCT = (ASIAN/TOTALPOP)*100) %>% 
  mutate(HISPANIC_PCT = (HISPANIC/TOTALPOP)*100) %>% 
  mutate(AGE0_17_PCT = (AGE0_17/TOTALPOP)*100) %>% 
  mutate(AGE18_24_PCT = (AGE18_24/TOTALPOP)*100) %>% 
  mutate(AGE25_44_PCT = (AGE25_44/TOTALPOP)*100) %>% 
  mutate(AGE45_64_PCT = (AGE45_64/TOTALPOP)*100) %>% 
  mutate(AGE65_OLDER_PCT = (AGE65_OLDER/TOTALPOP)*100) %>% 
  mutate(Fluvax_USAVG = ifelse(EST_FLUVAX_PCT > '48.4', 'Above', 'Below')) %>% 
  mutate(Fluvax_USAVG_BINOM = recode_factor (.x = Fluvax_USAVG,
                                                'Above' = '1',
                                                'Below' = '0'))
  
  
summary(vaxvote.new)

