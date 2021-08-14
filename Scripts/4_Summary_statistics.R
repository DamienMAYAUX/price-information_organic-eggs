
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")


######################################
######################################
######### DATA PRESENTATION ##########
######################################
######################################


##############################
###### 1. PRODUCT DATA #######
##############################

# Question : what are the market shares for the different egg characteristics ?
# Question : What are the main distribution channels ?

# Answer : one large tbl_summary

shopping_trips_without_nosale = readRDS("Inputs/shopping_trips_without_nosale.rds")

shopping_trips_renamed = shopping_trips_without_nosale%>%
  select(
    -X, -hhid, -unit_price, -dateachat, -marque, -periode
  )%>%
  mutate(
    distribution_channel = gsub("_[0-9]+", "", retailer),
    distribution_channel = case_when(
      distribution_channel == "41" ~ "Traditional Food Store (no trolley)",
      distribution_channel == "7" ~ "\"Popular\" Store (no trolley)",
      distribution_channel == "42" ~ "Supermarkets (with trolley)",
      distribution_channel == "43" ~ "Hypermarkets (with trolley)"
    )
  )%>%
  select(-retailer)%>%
  mutate(
    calibre = case_when(
      calibre == "calibreL" ~ "L",
      calibre == "calibreM" ~ "M"
    ),
    label = case_when(
      label == "labelbio" ~ "Organic",
      label == "labelpleinair" ~ "Free-Range",
      label == "nolabel" ~ "No Label"
    ),
    marque_simple = case_when(
      marque_simple == "high" ~ "Top-Range Retail Brand",
      marque_simple == "medium" ~ "Medium-Range Retail Brand",
      marque_simple == "low" ~ "Low-Range Retail Brand",
      marque_simple == "indep" ~ "Popular National brand",
      TRUE ~ "Other National Brand"
    ),
    valqvol = as.factor(as.character(valqvol))
  )%>%
  rename(
    Calibre = calibre,
    Label = label,
    `Box Size` = valqvol,
    `Simplified Brand Category` = marque_simple,
    `Distribution Channel` = distribution_channel
  )

tbl_summary(shopping_trips_renamed)



########################
###### 2. PRICES #######
########################

# Question : What is the average price for each label ?

shopping_trips_without_nosale_refactored =
  within(
    shopping_trips_without_nosale, 
    {label <- relevel(label, ref = 3)
    calibre <- relevel(calibre, ref = 2)
    marque_simple <- relevel(marque_simple, ref = 4)}
    )

fit_avg_price = lm(
  `Unit Price` ~ Label + `Box Size` + Calibre + `Simplified Brand Category`,
  shopping_trips_without_nosale_refactored%>%
    mutate(valqvol = as.numeric(as.character(valqvol)))%>%
    mutate(
      calibre = case_when(
        calibre == "calibreL" ~ "L",
        calibre == "calibreM" ~ "M"
      ),
      label = case_when(
        label == "labelbio" ~ "Organic",
        label == "labelpleinair" ~ "Free-Range",
        label == "nolabel" ~ "No Label"
      ),
      marque_simple = case_when(
        marque_simple == "high" ~ "Top-Range Retail Brand",
        marque_simple == "medium" ~ "Medium-Range Retail Brand",
        marque_simple == "low" ~ "Low-Range Retail Brand",
        marque_simple == "indep" ~ "Popular National brand",
        TRUE ~ "Other National Brand"
      )
    )%>%
    rename(
      Calibre = calibre,
      Label = label,
      `Box Size` = valqvol,
      `Simplified Brand Category` = marque_simple,
       `Unit Price` = unit_price
    )
)

stargazer(
  fit_avg_price,
  type = "html"
  # type = "text"
)
# The html table needs to be cleaned


# Question : Do prices vary over time ?

ggplot(
  shopping_trips_without_nosale%>%
    group_by(retailer, marque, label, calibre, valqvol)%>%
    mutate(sales = sum(as.numeric(as.character(valqvol))))%>%
    ungroup()%>%
    filter(sales > 1300)%>%
    mutate(product_label_retailer = paste(marque, calibre, label, retailer, valqvol, sep = '_'))%>%
    rename(
      `eggs per box` = valqvol,
      `sales date` = dateachat,
      `price per egg` = unit_price
      )
  )+
  geom_point(aes(x = `sales date`, y = `price per egg` , color = `eggs per box`))+
  facet_grid(rows = "product_label_retailer", scales = "free")

# Question : Do we observe a correlation between prices and demographics ?

# 

############################
###### 3. HOUSEHOLDS #######
############################

# Question : is the sample representative of French households ?


##############################
###### 4. COMPETITION ########
##############################

# Is it relev








################################
##### HOUSEHOLD STATISTICS #####
################################

#### HOUSEHOLD CHARACTERISTICS

## CLEANING THE HOUSEHOLD DATABASE

# consumption_without_nosale = readRDS("Inputs/shopping_trips_without_nosale.rds")
# relevant_households = consumption_without_nosale$hhid %>% unique()

household = read_dta("Data/hhold2012_PF_annuel.dta")

household_cleaner1 = household%>%
  #filter(hhid %in% relevant_households)%>%#
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
    max_age = ifelse(is.na(age2) | age1<age2, age2, age1),
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
    rve = gsub("DE ", "", rve),
    rve = gsub(" A ", "- ", rve),
    rve = gsub("EURO ", "€", rve),
    rve = gsub("EURO", "", rve),
    rve = gsub("\\(.*$","", rve),
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
    -age1, -age2, -ageind1, -ageind2, -etud1, -etud2
  )


## DEFINING RELEVANT CONSUMER CATEGORIES

household_cleaner2 = household_cleaner1%>%
  mutate(
    young_highly_educated_single = 
      (max_age < 35) & (max_etud >= 6) & single,
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

#saveRDS(household_cleaner2, "Inputs/household.rds")


household_renamed = household_cleaner2%>%
  select(
    -hhid,
    -rve_num,
    -single
  )%>%
  mutate(
    max_etud = case_when(
      max_etud == 1 ~ "UNDER PRIMARY EDUCATION",
      max_etud == 2 ~ "PRIMARY EDUCATION",
      max_etud == 3 ~ "SECONDARY EDUCATION",
      max_etud == 4 ~ "VOCATIONNAL TRAINING",
      max_etud == 5 ~ "TWO YEARS OF HIGHER EDUCATION",
      max_etud == 6 ~ "THREE YEARS OF HIGHER EDUCATION",
      max_etud == 7 ~ "FIVE YEARS OF HIGHER EDUCATION"
    )
  )%>%
  rename(
    `Life Cycle` = cycle,
    `Income` = clas,
    `Children` = jenf,
    `Area` = habi_inra,
    `Education` = max_etud,
    `Pre-tax Income` = rve,
    `Age (Oldest Household Member)` = max_age,
    `Young Highly-Educated Single` = young_highly_educated_single,
    `Educated Parents with Middle or High Income` = educated_parents_middle_high_income
  )


###############

#household = readRDS("Inputs/household.rds")


# INSEE DATA

###########################################
### A FAIRE DEPUIS LE SERVEUR DE PSE !! ###
######## A PARTIR DU FICHIER BRUT #########
###########################################

srcv = readRDS("E:/RA/WB_Inequality/Inputs/df_filtered_srcv.rds")
load("E:/RA/WB_Inequality/Inputs/srcv_complement.Rda")

tbl_summary(
  srcv%>% 
    filter(aenq == "2012")%>% 
    select(men, typmen15)%>% 
    unique() %>% select(typmen15)
  )
# 28% personne seule
# 7,3% famille monoparentale
# 30,5% couple sans enfant
# 31,4% couple avec enfant
# 2,1% autre

tbl_summary(
  srcv%>% 
    filter(aenq == "2012")%>% 
    group_by(men)%>%
    summarise(
      min_age = min(age),
      age_below_35 = min_age<36,
      age_above_65 = min_age>66
      )%>%
    ungroup()%>%
    select(age_below_35, age_above_65)
)
# 24% avec min age en dessous de 35 ans
# 21% avec min age au dessus de 65 ans

tbl_summary(
  srcv%>% 
    filter(aenq == "2012")%>% 
    group_by(men)%>%
    summarise(
      min_age = min(age),
      age_below_35 = min_age<36,
      age_above_65 = min_age>66
    )%>%
    ungroup()%>%
    select(age_below_35, age_above_65)
)
# 24% avec min age en dessous de 35 ans
# 21% avec min age au dessus de 65 ans







### 

shopping_trips_with_nosale = readRDS("Inputs/shopping_trips_with_nosale.rds")


### COL 1 : SHOPPING TRIPS | COL 2 : SHOPPING TRIPS WITH EGGS

### ROW 1 : NB RETAILER DURING THE YEAR
shopping_trips_with_nosale%>%
  select(hhid, retailer)%>%
  unique()%>%
  count(hhid)%>%
  .$n%>%
  as.factor()%>%
  summary()

shopping_trips_without_nosale%>%
  select(hhid, retailer)%>%
  unique()%>%
  count(hhid)%>%
  .$n%>%
  as.factor()%>%
  summary()

### ROW 2 : SHARE OF VISITS TO THE MAIN RETAILERS
shopping_trips_with_nosale%>%
  count(hhid, retailer)%>%
  group_by(hhid)%>%
  mutate(share_max_retailer = max(n)/sum(n))%>%
  .$share_max_retailer%>%
  #hist()
  summary()

shopping_trips_without_nosale%>%
  count(hhid, retailer)%>%
  group_by(hhid)%>%
  mutate(share_max_retailer = max(n)/sum(n))%>%
  .$share_max_retailer%>%
  #hist()
  summary()

### ROW 3 : SHARE EMPTY SHOPPING TRIPS TO THE MAIN RETAILER

share_empty_shopping_trips_at_main_retailer_for_eggs = 
  shopping_trips_with_nosale%>%
  mutate(
    nosale = (label == "nosale")
  )%>%
  count(nosale, hhid, retailer)%>%
  arrange(hhid, nosale, n)%>%
  group_by(hhid)%>%
  mutate(
    is_main_egg_retailer = 1:n()
  )%>%
  ungroup()%>%
  group_by(hhid, retailer)%>%
  mutate(is_main_egg_retailer_generalized = (min(is_main_egg_retailer) == 1))%>%
  ungroup()%>%
  filter(is_main_egg_retailer_generalized)%>%
  group_by(hhid)%>%
  summarise(share_nosale_main_egg_retailer = sum(n*nosale)/sum(n) )

hist(share_empty_shopping_trips_at_main_retailer_for_eggs$share_nosale_main_egg_retailer)





## Nb periode d'achat par hhid (6 ou moins, 9 ou moins, 12)

shopping_trips_without_nosale%>%
  select(hhid, periode)%>%
  unique()%>%
  count(hhid)%>%
  .$n%>%
  as.factor()%>%
  summary()
  


