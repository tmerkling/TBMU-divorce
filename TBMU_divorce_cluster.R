library(purrr)
library(dplyr)
#library(glmmTMB)
#library(MuMIn)
#library(corrplot)
library(arm)
library(ggplot2)
#library(DHARMa)
library(brms)
library(shinystan)

# Data processing

TBMU_divorce = read.table("TBMU_Coats_divorce_data.csv", header = TRUE, sep = ",")
dim(TBMU_divorce) # 1624   20

TBMU_divorce = TBMU_divorce %>% filter(Outcome != "DEAD")
dim(TBMU_divorce) # 952  20
TBMU_divorce = TBMU_divorce %>% mutate(ProbDiv = if_else(Outcome == "DIV",1,0))

TBMU_divorce_clean = TBMU_divorce %>% dplyr::select(c(-Hatching,-Outcome)) %>% na.omit() %>% mutate(Plot = substr(Site,1,1)) %>% filter(Plot != "N" & Plot != "J")
TBMU_divorce_clean$BandNo1 = factor(TBMU_divorce_clean$BandNo1)
TBMU_divorce_clean$BandNo2 = factor(TBMU_divorce_clean$BandNo2)
TBMU_divorce_clean$Site = factor(TBMU_divorce_clean$Site)

TBMU_divorce_outExp = TBMU_divorce %>% dplyr::select(c(-Hatching,-Outcome,-ExpMale,-ExpFemale,-ExpBird1,-ExpBird2,-PairBond)) %>% na.omit() %>% mutate(Plot = substr(Site,1,1))  %>% filter(Plot != "N" & Plot != "J")

TBMU_divorce_cleanQ = TBMU_divorce_clean %>% filter(Plot == "Q")
TBMU_divorce_outExpQ = TBMU_divorce_outExp %>% filter(Plot == "Q")

TBMU_divorce_cleanQ = TBMU_divorce_cleanQ %>% mutate_if(is.integer, as.numeric) %>% 
  mutate(stdAgeMale = if_else(AgeMale >=22,22, if_else(AgeMale <= 5, 5, AgeMale)), stdAgeFemale = if_else(AgeFemale >= 22, 22, if_else(AgeFemale <= 5, 5, AgeFemale)), stdExpMale = if_else(ExpMale >= 11, 11, ExpMale), ExpFemale = if_else(ExpFemale >= 11, 11, ExpFemale), stdPairBond = if_else(PairBond >= 6, 6, PairBond), stdLaying = if_else(Laying >= 50,50, Laying), stdQlty = if_else(QLTY <= 0.25, 0.25, QLTY), PairID = factor(paste(BandNo1,BandNo2, sep = "_"))) %>%
  mutate(stdAgeMale = arm::rescale(stdAgeMale), stdAgeFemale = rescale(stdAgeFemale), stdExpMale = rescale(stdExpMale), stdExpFemale = rescale(ExpFemale), stdPairBond = rescale(stdPairBond), stdQlty = rescale(stdQlty), stdFledge = rescale(Fledge), stdLaying = rescale(stdLaying),stdYear = rescale(Year)) %>% mutate(stdAgeMaleSq = I(stdAgeMale^2), stdAgeFemaleSq = I(stdAgeFemale^2), stdExpMaleSq = I(stdExpMale^2), stdExpFemaleSq = I(stdExpFemale^2), stdPairBondSq = I(stdPairBond^2), stdQltySq = I(stdQlty^2), stdLayingSq = I(stdLaying^2), stdYearSq = I(stdYear^2))

TBMU_divorce_outExpQ = TBMU_divorce_outExpQ %>% mutate_if(is.integer, as.numeric)%>% 
  mutate(stdAgeMale = if_else(AgeMale >=22,22, if_else(AgeMale <= 5, 5, AgeMale)), stdAgeFemale = if_else(AgeFemale >= 22, 22, if_else(AgeFemale <= 5, 5, AgeFemale)), stdLaying = if_else(Laying >= 50,50, Laying), stdQlty = if_else(QLTY <= 0.25, 0.25, QLTY), PairID = factor(paste(BandNo1,BandNo2, sep = "_"))) %>%
  mutate(stdAgeMale = arm::rescale(stdAgeMale), stdAgeFemale = rescale(stdAgeFemale), stdQlty = rescale(QLTY), stdFledge = rescale(Fledge), stdLaying = rescale(Laying),stdYear = rescale(Year))%>%
  mutate(stdAgeMaleSq = I(stdAgeMale^2), stdAgeFemaleSq = I(stdAgeFemale^2), stdQltySq = I(stdQlty^2), stdLayingSq = I(stdLaying^2), stdYeaSq = I(stdYear^2))


## Bayesian models with brms

#I won't consider interactions with more than 2 terms and not models with age and breeding experience together
#Had some convergence issues when using lme4 or glmmTMB so moved to brms and a Bayesian framework ...

#Following code found on the brms Github by #keesterbrugge I try to use list-column format as suggested in the R for Data Science book

mod_divQ <- list(
  # main effect models
  m1 = ProbDiv ~ 0 + intercept + stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m2 = ProbDiv ~ 0 + intercept + stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m3 = ProbDiv ~ 0 + intercept + stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m4 = ProbDiv ~ 0 + intercept + stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m5 = ProbDiv ~ 0 + intercept + stdPairBond + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m6 = ProbDiv ~ 0 + intercept + stdFledge + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m7 = ProbDiv ~ 0 + intercept + stdQlty + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m8 = ProbDiv ~ 0 + intercept + stdLaying + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m9 = ProbDiv ~ 0 + intercept + stdYear + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m10 = ProbDiv ~ 0 + intercept + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # additive models
  # models with fledging probability
  m11 = ProbDiv ~ 0 + intercept + stdFledge + stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m12 = ProbDiv ~ 0 + intercept + stdFledge + stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m13 = ProbDiv ~ 0 + intercept + stdFledge + stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m14 = ProbDiv ~ 0 + intercept + stdFledge + stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m15 = ProbDiv ~ 0 + intercept + stdFledge + stdQlty + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m16 = ProbDiv ~ 0 + intercept + stdFledge + stdPairBond + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m17 = ProbDiv ~ 0 + intercept + stdFledge + stdLaying + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with fenale age
  m18 = ProbDiv ~ 0 + intercept + stdAgeMale + stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m19 = ProbDiv ~ 0 + intercept + stdQlty + stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m20 = ProbDiv ~ 0 + intercept + stdPairBond + stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m21 = ProbDiv ~ 0 + intercept + stdLaying + stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with male age
  m22 = ProbDiv ~ 0 + intercept + stdQlty + stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m23 = ProbDiv ~ 0 + intercept + stdPairBond + stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m24 = ProbDiv ~ 0 + intercept + stdLaying + stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with fenale breeding experience
  m25 = ProbDiv ~ 0 + intercept + stdExpMale + stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m26 = ProbDiv ~ 0 + intercept + stdQlty + stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m27 = ProbDiv ~ 0 + intercept + stdPairBond + stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m28 = ProbDiv ~ 0 + intercept + stdLaying + stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with male breeding experience
  m29 = ProbDiv ~ 0 + intercept + stdQlty + stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m30 = ProbDiv ~ 0 + intercept + stdPairBond + stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m31 = ProbDiv ~ 0 + intercept + stdLaying + stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with paid-bond
  m32 = ProbDiv ~ 0 + intercept + stdPairBond + stdQlty + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m33 = ProbDiv ~ 0 + intercept + stdPairBond + stdLaying + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with site quality
  m34 = ProbDiv ~ 0 + intercept + stdQlty + stdLaying + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with quadratic effects
  m35 = ProbDiv ~ 0 + intercept + stdAgeMale + stdAgeMaleSq + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m36 = ProbDiv ~ 0 + intercept + stdAgeFemale + stdAgeFemaleSq + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m37 = ProbDiv ~ 0 + intercept + stdExpMale + stdExpMaleSq + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m38 = ProbDiv ~ 0 + intercept + stdExpFemale + stdExpFemaleSq + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m39 = ProbDiv ~ 0 + intercept + stdQlty + stdQltySq + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m40 = ProbDiv ~ 0 + intercept + stdPairBond + stdPairBondSq + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m41 = ProbDiv ~ 0 + intercept + stdLaying + stdLayingSq + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with two-way interactions
  # models with fledging probability
  m42 = ProbDiv ~ 0 + intercept + stdFledge * stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m43 = ProbDiv ~ 0 + intercept + stdFledge * stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m44 = ProbDiv ~ 0 + intercept + stdFledge * stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m45 = ProbDiv ~ 0 + intercept + stdFledge * stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m46 = ProbDiv ~ 0 + intercept + stdFledge * stdQlty + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m47 = ProbDiv ~ 0 + intercept + stdFledge * stdPairBond + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m48 = ProbDiv ~ 0 + intercept + stdFledge * stdLaying + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with fenale age
  m49 = ProbDiv ~ 0 + intercept + stdAgeMale * stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m50 = ProbDiv ~ 0 + intercept + stdQlty * stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m51 = ProbDiv ~ 0 + intercept + stdPairBond * stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m52 = ProbDiv ~ 0 + intercept + stdLaying * stdAgeFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with male age
  m53 = ProbDiv ~ 0 + intercept + stdQlty * stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m54 = ProbDiv ~ 0 + intercept + stdPairBond * stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m55 = ProbDiv ~ 0 + intercept + stdLaying * stdAgeMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with fenale breeding experience
  m56 = ProbDiv ~ 0 + intercept + stdExpMale * stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m57 = ProbDiv ~ 0 + intercept + stdQlty * stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m58 = ProbDiv ~ 0 + intercept + stdPairBond * stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m59 = ProbDiv ~ 0 + intercept + stdLaying * stdExpFemale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with male age
  m60 = ProbDiv ~ 0 + intercept + stdQlty * stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m61 = ProbDiv ~ 0 + intercept + stdPairBond * stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m62 = ProbDiv ~ 0 + intercept + stdLaying * stdExpMale + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  # models with paid-bond
  m63 = ProbDiv ~ 0 + intercept + stdPairBond * stdQlty + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m64 = ProbDiv ~ 0 + intercept + stdPairBond * stdLaying + (1|Year) + (1|Site) + (1|BandM) + (1|BandF),
  m65 = ProbDiv ~ 0 + intercept + stdQlty * stdLaying + (1|Year) + (1|Site) + (1|BandM) + (1|BandF)
)


divQ_data <- tibble( 
  name = names(mod_divQ),
  formulas = mod_divQ
) %>% 
  mutate(model = mod_divQ %>% purrr::map( 
    .f = brm, 
    data = TBMU_divorce_cleanQ, 
    family = bernoulli(),
    prior = c(set_prior("normal(0,5)", class = "b"), 
              set_prior("student_t(3,0,5)", class = "sd")),
    control = list(adapt_delta = 0.99),
    chains = 4, iter = 10000, warmup = 1000, thin = 5, cores = getOption("mc.cores", 1L)))


save(divQ_data, file = "brms_divQ.Rdata")
