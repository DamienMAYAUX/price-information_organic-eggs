
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")

#### HOUSEHOLD CHARACTERISTICS

## CLEANING THE HOUSEHOLD DATABASE

consumption_without_nosale = readRDS("Inputs/shopping_trips_without_nosale.rds")
relevant_households = consumption_without_nosale$hhid %>% unique()

household = read_dta("Data/hhold2012_PF_annuel.dta")

household_cleaner1 = household%>%
  filter(hhid %in% relevant_households)%>%
  select(
    hhid,
    cycle,
    clas,
    jenf,
    habi_inra,
    rve,
    ageind1,
    ageind2,
    etud1,
    etud2
  )%>%
  mutate(
    max_etud = max(etud1, etud2),
    age1 = ageind1/12,
    age2 = ageind2/12,
    min_age = min(age1, age2),
    rve_num = case_when(
      rve == "DE 900 EURO A 1 099 EURO (6 000 F A 6 999 F)" ~ 1000,
      rve == "DE 750 EURO A 899 EURO (5 000 F A 5 999 F)" ~ 825,
      rve == "DE 1 900 EURO A 2 299 EURO (12 500 F A 14 999 F)" ~ 2100,
      rve == "DE 3 800 EURO A 4 499 EURO (25 000 F A 29 999 F)" ~ 4150,
      rve == "DE 2 700 EURO A 2 999 EURO (17 500 F A 19 999 F)" ~ 2850,
      rve == "DE 1 500 EURO A 1 899 EURO (10 000 F A 12 499 F)" ~ 1700,
      rve == "DE 3 000 EURO A 3 799 EURO (20 000 F A 24 999 F)" ~ 3400,
      rve == "DE 1 400 EURO A 1 499 EURO (9 000 F A 9 999 F)" ~ 1450,
      rve == "DE 600 EURO A 749 EURO (4 000 F A 4 999 F)" ~ 675,
      rve == "DE 5 400 EURO A 6 999 EURO (35 000 F A 44 999 F)" ~ 6200,
      rve == "DE 2 300 EURO A 2 699 EURO (15 000 F A 17 499 F)" ~ 2500,
      rve == "DE 1 200 EURO A 1 399 EURO (8 000 F A 8 999 F)" ~ 1300,
      rve == "DE 4 500 EURO A 5 399 EURO (30 000 F A 34 999 F)" ~ 4950,
      rve == "7 000 EURO ET PLUS (45 000 F ET PLUS)" ~ 8000,
      rve == "DE 1 100 EURO A 1 199 EURO (7 000 F A 7 999 F)" ~ 1150,
      rve == "DE 450 EURO A 599 EURO (3 000 F A 3 999 F)" ~ 525,
      rve == "DE 300 EURO A 449 EURO (2 000 F A 2 999 F)" ~ 375,
      rve == "MOINS DE 300 EURO (MOINS DE 2 000 F)" ~ 200
    ),
    cycle = case_when(
      cycle == "CÉLIBATAIRES ÂGE MOYEN" ~ "SINGLE (35-65 YEARS OLD)",
      cycle == "COUPLES ÂGE MOYEN" ~ "COUPLE (35-65 YEARS OLD)",
      cycle == "FAMILLE COLLÈGE ET LYCÉE" ~ "FAMILY (ELDEST CHILD 12-17 Y.O.)",
      cycle == "FAMILLE ÉCOLE PRIMAIRE" ~ "FAMILY (ELDEST CHILD 6-11 Y.O.)",
      cycle == "FAMILLE ENFANTS MAJEURS" ~ "FAMILY (ELDEST CHILD 18-24 Y.O.)",
      cycle == "FAMILLE MATERNELLE" ~ "FAMILY (ELDEST CHILD 0-5 Y.O.)",
      cycle == "JEUNE CÉLIBATAIRES" ~ "SINGLE (< 35 YEARS OLD)",
      cycle == "JEUNES COUPLES" ~ "COUPLE (< 35 YEARS OLD)",
      cycle == "VIEUX CÉLIBATAIRES" ~ "SINGLE (> 65 YEARS OLD)",
      cycle == "VIEUX COUPLES" ~ "COUPLE (> 65 YEARS OLD)",
    ),
    clas = case_when(
      clas == "AISEE" ~ "HIGH INCOME",
      clas == "MOYENNE SUPERIEURE" ~ "HIGH MIDLLE INCOME",
      clas == "MOYENNE INFERIEURE" ~ "LOW MIDDLE INCOME",
      clas == "MODESTE" ~ "LOW INCOME"
    ),
    jenf = case_when(
      jenf == "LE PLUS JEUNE A DE 11 A 15 ANS" ~ "YOUNGEST CHILD 11-15 Y.O",
      jenf == "LE PLUS JEUNE A DE 25 MOIS A 5 ANS" ~ "YOUNGEST CHILD 2-5 Y.O",
      jenf == "LE PLUS JEUNE A DE 6 A 10 ANS" ~ "YOUNGEST CHILD 6-10 Y.O",
      jenf == "LE PLUS JEUNE A MOINS DE 25 MOIS" ~ "YOUNGEST CHILD < 2 Y.O",
      jenf == "PAS D ENFANT" ~ "NO CHILDREN"
    )
  )%>%
  select(
    -rve, -age1, -age2
  )

summary(as.factor(household_cleaner1$jenf))

## DEFINING RELEVANT CONSUMER CATEGORIES

household_cleaner2 = household_cleaner1%>%
  mutate(
    young_higly_educated_single = ,
    educated_parents_middle_high_income = 
  )



saveRDS(household_cleaner2, "Inputs/household.rds")





# household_cleaner = readRDS("Inputs/full_household_sample.rds")


model_nosale = apollo_loadModel("nosale")
#model_lnorm_control = apollo_loadModel("mixed_lnorm_control")


# apollo_fitTests
# une colonne par categorie de consommateur
# pour chaque achat, 0 ou 1 selon si le menage appartient a la categorie

# fitsTest_settings = list()
# fitsTest_settings[["subsamples"]] = list()
# fitsTest_settings$subsamples[["business"]] = database$business==1
# fitsTest_settings$subsamples[["leisure"]] = database$business==0
# apollo_fitsTest(model,apollo_probabilities,apollo_inputs,fitsTest_settings)




# apollo_lrTest(model1, model2)
# apollo_basTest(model1, model2)


# Use apollo_prediction to simulate the effect of a change in the price of the organic products
# This could serve as a complement to the theoretical model

model_lnorm_control$estimate["b_labelbio"]

conditionals = apollo_conditionals(model_lnorm_control, apollo_probabilities, apollo_inputs)

df_household = readRDS("Inputs/full_household_sample.rds")

posterior = conditionals%>%
  mutate(
   wtp = - model_lnorm_control$estimate["b_labelbio"] / post.mean
  )%>%
  left_join(
    df_household,
    by = c("ID"=  "hhid")
  )
  

ggplot()+
  geom_density(
    data= posterior, 
    aes(x=wtp)
    )+
  geom_density(
    data = posterior %>% filter(clas == "AISEE"), 
    aes(x=wtp, color = "HIGH-INCOME")
    )+
  geom_density(
    data = posterior %>% filter(clas == "MODESTE"), 
    aes(x=wtp, color = "LOW-INCOME")
  )
  

price_sensitive_consumers = conditionals%>%
  arrange(post.mean)%>%
  .[1:5,]

price_insensitive_consumers = conditionals%>%
  arrange(-post.mean)%>%
  .[1:5,]

database%>%
  left_join(price_sensitive_consumers, by = c("hhid" = "ID"))%>%
  left_join(household)






  
