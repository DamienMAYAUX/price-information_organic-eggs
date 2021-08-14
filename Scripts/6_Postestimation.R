
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")



#### MODEL TESTING

model_nosale = apollo_loadModel("Inputs/Apollo/nosale")
model_nosale_with_control = apollo_loadModel("Inputs/Apollo/nosale_with_control")
model_nosale_lnorm = apollo_loadModel("Inputs/Apollo/nosale_lnorm")
model_nosale_lnorm_with_control = apollo_loadModel("Inputs/Apollo/nosale_lnorm_with_control")

## With VS without controls

apollo_lrTest(model_nosale, model_nosale_with_control)
apollo_basTest(model_nosale, model_nosale_with_control)


## With VS without random coefficient

apollo_lrTest(model_nosale, model_nosale_lnorm)
apollo_basTest(model_nosale, model_nosale_lnorm)




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
    max_etud = ifelse(is.na(etud2) | etud1>etud2, etud1, etud2),
    age1 = as.integer(ageind1/12),
    age2 = as.integer(ageind2/12),
    single = is.na(age2),
    min_age = ifelse(is.na(age2) | age1<age2, age1, age2),
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
      clas == "MOYENNE SUPERIEURE" ~ "HIGH MIDDLE INCOME",
      clas == "MOYENNE INFERIEURE" ~ "LOW MIDDLE INCOME",
      clas == "MODESTE" ~ "LOW INCOME"
    ),
    jenf = case_when(
      jenf == "LE PLUS JEUNE A DE 11 A 15 ANS" ~ "YOUNGEST CHILD 11-15 Y.O.",
      jenf == "LE PLUS JEUNE A DE 25 MOIS A 5 ANS" ~ "YOUNGEST CHILD 2-5 Y.O.",
      jenf == "LE PLUS JEUNE A DE 6 A 10 ANS" ~ "YOUNGEST CHILD 6-10 Y.O.",
      jenf == "LE PLUS JEUNE A MOINS DE 25 MOIS" ~ "YOUNGEST CHILD < 2 Y.O.",
      jenf == "PAS D ENFANT" ~ "NO CHILDREN"
    ),
    habi_inra = case_when(
      habi_inra == "COMMUNES RURALES" ~ "RURAL AREAS",
      habi_inra == "PARIS + AGGLOMERATION" ~ "PARIS URBAN AREA",
      TRUE ~ habi_inra
      ),
    habi_inra = gsub("HABITANTS", "INHABITANTS", habi_inra),
    habi_inra = gsub("UNITES URBAINES DE", "URBAN AREAS FROM", habi_inra),
    habi_inra = gsub(" A ", " TO ", habi_inra),
    habi_inra = gsub(" ET PLUS", "", habi_inra)
  )%>%
  select(
    -rve, -age1, -age2, -ageind1, -ageind2, -etud1, -etud2
  )


## DEFINING RELEVANT CONSUMER CATEGORIES

household_cleaner2 = household_cleaner1%>%
  mutate(
    young_highly_educated_single = 
      (min_age < 35) & (max_etud >= 7) & single,
    educated_parents_middle_high_income = 
      cycle %in% c(
        "FAMILY (ELDEST CHILD 0-5 Y.O.)", 
        "FAMILY (ELDEST CHILD 6-11 Y.O.)",
        "FAMILY (ELDEST CHILD 12-17 Y.O.)"
        ) & (max_etud >= 5) & 
      clas %in% c(
        "HIGH MIDDLE INCOME",
        "HIGH INCOME"
      )
  )

saveRDS(household_cleaner2, "Inputs/household.rds")


#### BAYESIAN POSTERIORS ANALYSIS


# household_cleaner2 = readRDS("Inputs/household.rds")

model_nosale_lnorm_with_control$estimate["b_labelbio"]

conditionals = apollo_conditionals(model_nosale_lnorm_with_control, apollo_probabilities, apollo_inputs)


posterior = conditionals%>%
  mutate(
   wtp = - model_nosale_lnorm_with_control$estimate["b_labelbio"] / post.mean
  )%>%
  left_join(
    household_cleaner2,
    by = c("ID"=  "hhid")
  )

saveRDS(posterior, "Inputs/posterior_nosale_lnorm_with_control.rds")
  

ggplot()+
  geom_density(
    data= posterior, 
    aes(x=wtp, color = "TOTAL")
    )+
  # geom_density(
  #   data = posterior %>% filter(young_highly_educated_single), 
  #   aes(x=wtp, color = "YHES")
  #   )+
  # geom_density(
  #   data = posterior %>% filter(educated_parents_middle_high_income), 
  #   aes(x=wtp, color = "EPMHI")
  # )+
  geom_density(
    data = posterior %>% filter(clas == "HIGH INCOME"), 
    aes(x=wtp, color = "HIGH INCOME")
  )+
  geom_density(
    data = posterior %>% filter(clas == "LOW MIDDLE INCOME"), 
    aes(x=wtp, color = "LOW MIDDLE INCOME")
  )+
  geom_density(
    data = posterior %>% filter(clas == "HIGH MIDDLE INCOME"), 
    aes(x=wtp, color = "HIGH MIDDLE INCOME")
  )+
  geom_density(
    data = posterior %>% filter(clas == "LOW INCOME"), 
    aes(x=wtp, color = "LOW INCOME")
  )+
  scale_x_continuous(limits = c(0,0.3))
  
posterior%>% filter(wtp > 0.12)
# 0.12 c'est à peu près le troisième quartile


price_sensitive_consumers = conditionals%>%
  arrange(post.mean)%>%
  .[1:5,]

price_insensitive_consumers = conditionals%>%
  arrange(-post.mean)%>%
  .[1:5,]

database%>%
  left_join(price_sensitive_consumers, by = c("hhid" = "ID"))%>%
  left_join(household)


####################################################


#### EFFECT OF A CHANGE IN PRICE

database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo_testing.rds"))

df_product = as.data.frame(readRDS("Inputs/product_with_nosale.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)

## RECONSTRUCT A MODIFIED VERSION OF THE DATABASE (IN THE NOSALE CASE)

percent_price_decrease = 0
organic_product_list = df_product%>%
  rownames_to_column("product_nb")%>%
  filter(label == "labelbio")%>%
  .$product_nb

database = database_before_merge%>%
  mutate(
    marque_simple = 
      c(
        rep(c(df_product$marque_simple, NA_character_), rep_nb),
        as.character(df_product$marque_simple)[1:res_nb]
      ),
    label = 
      c(
        rep(c(df_product$label, NA_character_), rep_nb),
        df_product$label[1:res_nb]
      ),
    calibre = 
      c(
        rep(c(df_product$calibre, NA_character_), rep_nb),
        df_product$calibre[1:res_nb]
      ),
  )%>%
  mutate_at(vars(marque_simple, label, calibre), as.factor)%>%
  mutate_at(
    paste0(organic_product_list, "_price"),
    ~.*(1-percent_price_decrease/100)
  )

apollo_inputs = apollo_validateInputs()


## FORECAST MARKET SHARES

prediction_settings = list()

forecast = apollo_prediction(
  model,
  apollo_probabilities,
  apollo_inputs,
  prediction_settings
)

predicted_market_shares = forecast%>%
  pivot_longer(
    cols = grep("alt", names(forecast), value = TRUE), 
    names_to = "alternative", values_to = "shopping_trip_market_share"
  )%>%
  mutate(
    organic = alternative %in% paste0("alt", organic_product_list),
    total_market = sum(shopping_trip_market_share)
  )%>%
  group_by(organic)%>%
  summarise(
    market_share = 100 * sum(shopping_trip_market_share) / max(total_market)
  )



  
