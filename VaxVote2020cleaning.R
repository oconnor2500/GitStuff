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

vaxvote.demo <- vaxvote.new %>% 
  select(COUNTY, WHITE, BLACK, NATIVE_AMER, ASIAN, HISPANIC) 
vaxvote.demo

long.vaxvote.demo <- melt(setDT(vaxvote.demo), id.vars = ('COUNTY'), variable.name = 'Race')
long.vaxvote.demo

top.trump.vote <- vaxvote.new %>% 
  slice_max(TRUMP_PCT, n = 5) %>% 
  select(COUNTY, TRUMP_VOTES, BIDEN_VOTES, TRUMP_PCT, CURRFULLPCT, EST_FLUVAX_PCT, COVID_VAX_USAVG, CntyPol)

top.trump.vote

top.biden.vote <- vaxvote.new %>% 
  slice_max(BIDEN_PCT, n = 5) %>% 
  select(COUNTY, WHITE_PCT, BLACK_PCT, TRUMP_VOTES, BIDEN_VOTES, BIDEN_PCT, CURRFULLPCT, EST_FLUVAX_PCT, COVID_VAX_USAVG, CntyPol)

top.biden.vote

summary(vaxvote.new$WHITE_PCT)
#make long table for descriptives-----

#descriptives-----

table1(~WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + TRUMP_PCT + BIDEN_PCT + TOTAL_VOTE_PCT + COVID_VAX_USAVG | CntyPol, data = vaxvote.new) 
table1(~WHITE + BLACK + NATIVE_AMER + ASIAN + HISPANIC + AGE18_24 + AGE25_44 + AGE45_64 + AGE65_OLDER + TRUMP_PCT + BIDEN_PCT + TOTAL_VOTE_PCT | COVID_VAX_USAVG, data = vaxvote.new) 
table1(~CURRFULLPCT + EST_FLUVAX_PCT + TRUMP_PCT + COVID_VAX_USAVG | COUNTY, data = top.trump.vote) 
table1(~CURRFULLPCT + EST_FLUVAX_PCT + BIDEN_PCT + COVID_VAX_USAVG | COUNTY, data = top.biden.vote) 
table1(~TRUMP_PCT + BIDEN_PCT + CURRFULLPCT + EST_FLUVAX_PCT + COVID_VAX_USAVG + Fluvax_USAVG|CntyPol , data = vaxvote.new) 
table1(~WHITE + BLACK + NATIVE_AMER + ASIAN + HISPANIC + TRUMP_PCT + BIDEN_PCT + AGE18_24 + AGE25_44 + AGE45_64 + AGE65_OLDER + TRUMP_PCT + BIDEN_PCT + TOTAL_VOTE_PCT, data = top.biden.vote) 

#Graphs----
histo.age0_17 <- vaxvote.new %>% 
  drop_na(AGE0_17_PCT) %>% 
  ggplot(aes(x = AGE0_17_PCT)) +
  geom_histogram() +
  theme_minimal()
histo.age0_17

histo.age18_24 <- vaxvote.new %>% 
  drop_na(AGE18_24_PCT) %>%  
  ggplot(aes(x = AGE18_24_PCT)) +
  geom_histogram() +
  theme_minimal()

histo.age25_44 <- vaxvote.new %>% 
  drop_na(AGE25_44_PCT) %>% 
  ggplot(aes(x = AGE25_44_PCT)) +
  geom_histogram() +
  theme_minimal()

histo.age45_64 <- vaxvote.new %>% 
  drop_na(AGE45_64_PCT) %>% 
  ggplot(aes(x = AGE45_64_PCT)) +
  geom_histogram() +
  theme_minimal()

histo.age65_older <- vaxvote.new %>% 
  drop_na(AGE65_OLDER_PCT) %>% 
  ggplot(aes(x = AGE65_OLDER_PCT)) +
  geom_histogram() +
  theme_minimal()

gridExtra::grid.arrange(histo.age0_17,
                        histo.age18_24,
                        histo.age25_44,
                        histo.age45_64,
                        histo.age65_older,
                        nrow = 3)
#Stats----
#skewness
skewness(vaxvote.new$BIDEN_PCT)
#chi square
fisher.test(vaxvote.new$COVID_VAX_USAVG, vaxvote.new$CntyPol)
fisher.test(vaxvote.new$COVID_VAX_USAVG, vaxvote.new$Fluvax_USAVG)
fisher.test(vaxvote.new$Fluvax_USAVG, vaxvote.new$CntyPol)
#ttestm
wilcox.test(CURRFULLPCT ~ CntyPol, data = vaxvote.new)
wilcox.test(TRUMP_PCT ~ CntyPol, data = vaxvote.new)
wilcox.test(BIDEN_PCT ~ CntyPol, data = vaxvote.new)
wilcox.test(EST_FLUVAX_PCT ~ CntyPol, data = vaxvote.new)
#correlation
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$TRUMP_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$BIDEN_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$WHITE_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$BLACK_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$NATIVE_AMER_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$ASIAN_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$HISPANIC_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$AGE18_24_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$AGE25_44_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$AGE45_64_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$AGE65_OLDER_PCT, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$TOTALPOP, method = 'spearman')
cor.test(x = vaxvote.new$CURRFULLPCT, y = vaxvote.new$EST_FLUVAX_PCT, method = 'spearman')
#logistic models
covid.vote.model <- glm(COVID_VAX_USAVG_BINOM ~ CntyPol, data = vaxvote.new,
                             family = 'binomial')
covid.trumppct.model <- glm(COVID_VAX_USAVG_BINOM ~ TRUMP_PCT, data = vaxvote.new, family = 'binomial')
covid.vote.race.model <- glm(COVID_VAX_USAVG_BINOM ~ CntyPol + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                        family = 'binomial')
covid.trump.race.model <- glm(COVID_VAX_USAVG_BINOM ~ TRUMP_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                              family = 'binomial')
covid.vote.age.model <- glm(COVID_VAX_USAVG_BINOM ~ CntyPol + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new,
                             family = 'binomial')
covid.trump.age.model <- glm(COVID_VAX_USAVG_BINOM ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new,
                            family = 'binomial')
covid.vote.monster.model <- glm(COVID_VAX_USAVG_BINOM ~ CntyPol + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                                family = 'binomial')
covid.trump.monster.model <- glm(COVID_VAX_USAVG_BINOM ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                                family = 'binomial')

covid.race.model <- glm(COVID_VAX_USAVG_BINOM ~ WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                            family = 'binomial')


covid.biden.model <- glm(COVID_VAX_USAVG_BINOM ~ BIDEN_PCT, data = vaxvote.new, family = 'binomial')
covid.biden.race.model <- glm(COVID_VAX_USAVG_BINOM ~ BIDEN_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                              family = 'binomial')
covid.biden.age.model <- glm(COVID_VAX_USAVG_BINOM ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new,
                             family = 'binomial')
covid.biden.monster.model <- glm(COVID_VAX_USAVG_BINOM ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                                family = 'binomial')

summary(covid.vote.model)
summary(covid.trumppct.model)
summary(covid.trump.monster.model)
odds.n.ends(covid.vote.model)
odds.n.ends(covid.vote.race.model)
odds.n.ends(covid.vote.age.model)
odds.n.ends(covid.vote.monster.model)
odds.n.ends(covid.race.model)
odds.n.ends(covid.trumppct.model)
odds.n.ends(covid.trump.race.model)
summary(covid.trump.race.model)
covid.trumppct.model
odds.n.ends(covid.trump.age.model)
summary(covid.trump.age.model)
odds.n.ends(covid.trump.monster.model)
odds.n.ends(covid.biden.model)
summary(covid.biden.model)
odds.n.ends(covid.biden.race.model)
summary(covid.biden.race.model)
odds.n.ends(covid.biden.age.model)
summary(covid.biden.age.model)
odds.n.ends(covid.biden.monster.model)
summary(covid.biden.monster.model)

white.trump.lm <- lm(TRUMP_PCT ~ WHITE_PCT, data = vaxvote.new)
black.trump.lm <- lm(TRUMP_PCT ~ BLACK_PCT, data = vaxvote.new)
native.trump.lm <- lm(TRUMP_PCT ~ NATIVE_AMER_PCT, data = vaxvote.new)
asian.trump.lm <- lm(TRUMP_PCT ~ ASIAN_PCT, data = vaxvote.new)
hispanic.trump.lm <- lm(TRUMP_PCT ~ HISPANIC_PCT, data = vaxvote.new)
summary(white.trump.lm)
summary(black.trump.lm)
summary(native.trump.lm)
summary(asian.trump.lm)
summary(hispanic.trump.lm)

white.biden.lm <- lm(BIDEN_PCT ~ WHITE_PCT, data = vaxvote.new)
black.biden.lm <- lm(BIDEN_PCT ~ BLACK_PCT, data = vaxvote.new)
native.biden.lm <- lm(BIDEN_PCT ~ NATIVE_AMER_PCT, data = vaxvote.new)
asian.biden.lm <- lm(BIDEN_PCT ~ ASIAN_PCT, data = vaxvote.new)
hispanic.biden.lm <- lm(BIDEN_PCT ~ HISPANIC_PCT, data = vaxvote.new)
summary(white.biden.lm)
summary(black.biden.lm)
summary(native.biden.lm)
summary(asian.biden.lm)
summary(hispanic.biden.lm)

covid.totalpop.lm <- lm(CURRFULLPCT ~ TOTALPOP, data = vaxvote.new)
summary(covid.totalpop.lm)

covid.trump.lm <- lm(CURRFULLPCT ~ TRUMP_PCT, data = vaxvote.new)
covid.trump.race.lm <- lm(CURRFULLPCT ~ TRUMP_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)
covid.trump.age.lm <- lm(CURRFULLPCT ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new)
covid.trump.monster.lm <- lm(CURRFULLPCT ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)
covid.biden.lm <- lm(CURRFULLPCT ~ BIDEN_PCT, data = vaxvote.new)
covid.biden.race.lm <- lm(CURRFULLPCT ~ BIDEN_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)
covid.biden.age.lm <- lm(CURRFULLPCT ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new)
covid.biden.monster.lm <- lm(CURRFULLPCT ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)
flu.biden.lm <- lm(CURRFULLPCT ~ BIDEN_PCT, data = vaxvote.new)

summary(covid.trump.lm)
summary(covid.trump.race.lm)
summary(covid.trump.age.lm)
summary(covid.trump.monster.lm)
summary(covid.biden.lm)
summary(covid.biden.race.lm)
summary(covid.biden.age.lm)
summary(covid.biden.monster.lm)

covid.white.lm <- lm(CURRFULLPCT ~ WHITE_PCT, data = vaxvote.new)
covid.black.lm <- lm(CURRFULLPCT ~ BLACK_PCT, data = vaxvote.new)
covid.native.lm <- lm(CURRFULLPCT ~ NATIVE_AMER_PCT, data = vaxvote.new)
covid.asian.lm <- lm(CURRFULLPCT ~ ASIAN_PCT, data = vaxvote.new)
covid.hispanic.lm <- lm(CURRFULLPCT ~ HISPANIC_PCT, data = vaxvote.new)
covid.18.lm <- lm(CURRFULLPCT ~ AGE18_24_PCT, data = vaxvote.new)
covid.25.lm <- lm(CURRFULLPCT ~ AGE25_44_PCT, data = vaxvote.new)
covid.45.lm <- lm(CURRFULLPCT ~ AGE45_64_PCT, data = vaxvote.new)
covid.65.lm <- lm(CURRFULLPCT ~ AGE65_OLDER_PCT, data = vaxvote.new)
covid.hispanic.lm <- lm(CURRFULLPCT ~ HISPANIC_PCT, data = vaxvote.new)
summary(covid.white.lm)
summary(covid.black.lm)
summary(covid.native.lm)
summary(covid.asian.lm)
summary(covid.hispanic.lm)
summary(covid.18.lm)
summary(covid.25.lm)
summary(covid.45.lm)
summary(covid.65.lm)

flu.vote.model <- glm(Fluvax_USAVG_BINOM ~ CntyPol, data = vaxvote.new, family = 'binomial')
flu.vote.race.model <- glm(Fluvax_USAVG_BINOM ~ CntyPol + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                             family = 'binomial')
flu.vote.age.model <- glm(Fluvax_USAVG_BINOM ~ CntyPol + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new,
                            family = 'binomial')
flu.vote.monster.model <- glm(Fluvax_USAVG_BINOM ~ CntyPol + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                             family = 'binomial')

flu.trump.or <- glm(Fluvax_USAVG_BINOM ~ TRUMP_PCT, data = vaxvote.new, family = 'binomial')
flu.trump.race.or <- glm(Fluvax_USAVG_BINOM ~ TRUMP_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                           family = 'binomial')
flu.trump.age.or <- glm(Fluvax_USAVG_BINOM ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new,
                          family = 'binomial')
flu.trump.monster.or <- glm(Fluvax_USAVG_BINOM ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                              family = 'binomial')

flu.biden.or <- glm(Fluvax_USAVG_BINOM ~ BIDEN_PCT, data = vaxvote.new, family = 'binomial')
flu.biden.race.or <- glm(Fluvax_USAVG_BINOM ~ BIDEN_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                           family = 'binomial')
flu.biden.age.or <- glm(Fluvax_USAVG_BINOM ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new,
                          family = 'binomial')
flu.biden.monster.or <- glm(Fluvax_USAVG_BINOM ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new,
                              family = 'binomial')

flu.trump.model <- lm(EST_FLUVAX_PCT ~ TRUMP_PCT, data = vaxvote.new)
flu.trump.race.model <- lm(EST_FLUVAX_PCT ~ TRUMP_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)
flu.trump.age.model <- lm(EST_FLUVAX_PCT ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new)
flu.trump.monster.model <- lm(EST_FLUVAX_PCT ~ TRUMP_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)
flu.biden.model <- lm(EST_FLUVAX_PCT ~ BIDEN_PCT, data = vaxvote.new)
flu.biden.race.model <- lm(EST_FLUVAX_PCT ~ BIDEN_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)
flu.biden.age.model <- lm(EST_FLUVAX_PCT ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT, data = vaxvote.new)
flu.biden.monster.model <- lm(EST_FLUVAX_PCT ~ BIDEN_PCT + AGE18_24_PCT + AGE25_44_PCT + AGE45_64_PCT + AGE65_OLDER_PCT + WHITE_PCT + BLACK_PCT + NATIVE_AMER_PCT + ASIAN_PCT + HISPANIC_PCT, data = vaxvote.new)


summary(flu.trump.model)
summary(flu.biden.model)
summary(flu.trump.race.model)
summary(flu.biden.race.model)
summary(flu.trump.age.model)
summary(flu.biden.age.model)
summary(flu.trump.monster.model)
summary(flu.biden.monster.model)

summary(flu.biden)
summary(covid.vote.model)
odds.n.ends(flu.vote.model)
odds.n.ends(flu.vote.race.model)
odds.n.ends(flu.vote.age.model)
odds.n.ends(flu.vote.monster.model)
odds.n.ends(flu.trump.or)
summary(flu.trump.or)
odds.n.ends(flu.trump.race.or)
summary(flu.trump.race.or)
odds.n.ends(flu.trump.age.or)
summary(flu.trump.age.or)
odds.n.ends(flu.trump.monster.or)
summary(flu.trump.monster.or)
odds.n.ends(flu.biden.or)
summary(flu.biden.or)
odds.n.ends(flu.biden.race.or)
summary(flu.biden.race.or)
odds.n.ends(flu.biden.age.or)
summary(flu.biden.age.or)
odds.n.ends(flu.biden.monster.or)
summary(flu.biden.monster.or)
summary(vaxvote.new$NATIVE_AMER_PCT)

covid.flu.model <- glm(COVID_VAX_USAVG_BINOM ~ Fluvax_USAVG, data = vaxvote.new, family = 'binomial')
odds.n.ends(covid.flu.model)


#linear regression-----
#scatterplot
scatter.vax.trump <- vaxvote.new %>% 
  ggplot(aes(x = TRUMP_PCT, y = CURRFULLPCT)) +
  geom_point()+
  geom_smooth(method = lm, se = FALSE)
scatter.vax.trump

#correlation analysis

cor.test(x = vaxvote.new$TRUMP_PCT, y = vaxvote.new$CURRFULLPCT, method = 'spearman')
#model
vax.trump.lm <- lm(CURRFULLPCT~TRUMP_PCT, data = vaxvote.new)
summary(vax.trump.lm)
plot(vax.trump.lm$residuals)

trump.loess <- vaxvote.new %>% 
  ggplot(aes(y = CURRFULLPCT, x = TRUMP_PCT)) +
  geom_point(aes(size = "County"), color = "blue", alpha = .6) +
  geom_smooth(aes(color = 'Linear Fit line'), method = 'lm', se = FALSE) +
  geom_smooth(aes(color = 'Loess Curve'), se = FALSE) +
  theme_minimal() +
  labs(y = '% COVID-19 Vaccinated',
       x = '% voted Trump') +
  scale_color_manual(values = c('gray60', 'red'), name = '') +
  scale_size_manual(values = 2, name = '')

trump.loess
vax.biden.lm <- lm(CURRFULLPCT~BIDEN_PCT, data = vaxvote.new)
summary(vax.biden.lm)
plot(vax.biden.lm$residuals)

library(ggfortify)
autoplot(vax.trump.lm)
write.dta(vaxvote.new, file = '/Users/christopheroconnor/Desktop/vaxvote_new.dta')
