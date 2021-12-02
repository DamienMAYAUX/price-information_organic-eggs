
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")
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
      marque_simple == "other" ~ "Other National Brand",
      TRUE ~ "Error"
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


# CHoice sets sizes ! 

choice_set = readRDS("Inputs/choice_sets_with_nosale.rds")

choice_set_extended = choice_set%>%
  mutate(
    distribution_channel = gsub("_[0-9]+", "", retailer),
    distribution_channel = case_when(
      distribution_channel == "41" ~ "Traditional Food Store (no trolley)",
      distribution_channel == "7" ~ "\"Popular\" Store (no trolley)",
      distribution_channel == "42" ~ "Supermarkets (with trolley)",
      distribution_channel == "43" ~ "Hypermarkets (with trolley)"
    )
    # marque_simple = case_when(
    #   marque_simple == "high" ~ "Top-Range Retail Brand",
    #   marque_simple == "medium" ~ "Medium-Range Retail Brand",
    #   marque_simple == "low" ~ "Low-Range Retail Brand",
    #   marque_simple == "indep" ~ "Popular National brand",
    #   marque_simple == "other" ~ "Other National Brand",
    #   TRUE ~ "No purchase"
    # )
  )%>%
  count(retailer, distribution_channel, periode)%>%
  group_by(retailer, distribution_channel)%>%
  summarise(
    min_n = min(n),
    max_n = max(n),
    avg_n = mean(n)
    )%>%
  group_by(distribution_channel)%>%
  summarise(
    min_n = min(min_n),
    max_n = max(max_n),
    avg_n = mean(avg_n)
  )



########################
###### 2. PRICES #######
########################

# Question : What is the average price for each label ?

shopping_trips_without_nosale_refactored =
  shopping_trips_without_nosale%>%
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
  mutate_at( vars(calibre, marque_simple, label), as.factor)
    
shopping_trips_without_nosale_refactored =    
  within(
    shopping_trips_without_nosale_refactored, 
    {calibre <- relevel(calibre, ref = 2)}
    )

shopping_trips_without_nosale_refactored =    
  within(
    shopping_trips_without_nosale_refactored, 
    {marque_simple <- relevel(marque_simple, ref = 4)}
  )


shopping_trips_without_nosale_refactored =    
  within(
    shopping_trips_without_nosale_refactored, 
    {label <- relevel(label, ref = 2)}
  )



fit_avg_price = lm(
  `Unit Price` ~ Label + `Box Size` + Calibre + `Simplified Brand Category`,
  shopping_trips_without_nosale_refactored%>%
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
  type = "html",
  out = "Outputs/reg_observed_prices.html"
  #type = "text"
)
# The html table needs to be cleaned


# Question : Do prices vary over time ?

Sys.setlocale("LC_ALL", "English")
df_price = readRDS("Inputs/product_price.rds")


ggplot(
  shopping_trips_without_nosale%>%
    group_by(retailer, marque, label, calibre, valqvol)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    arrange(-sales)%>%
    mutate_at(vars(sales), as.factor)%>%
    filter(as.numeric(sales) == 191)
  # 191 U CALIBRE M 12 PLEIN AIR
)+
  geom_point(aes(x = dateachat, y = unit_price))+
  xlab("Purchase date") + ylab("Price per egg")


ggsave(
  "Outputs/price_plot.png", device = "png", 
  width = 15, height = 15, units = "cm"
)

# With the reference price

ggplot(
  shopping_trips_without_nosale%>%
    group_by(retailer, marque, label, calibre, valqvol)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    arrange(-sales)%>%
    mutate_at(vars(sales), as.factor)%>%
    filter(as.numeric(sales) == 191)%>%
    left_join(df_price)
    # 191 U CALIBRE M 12 PLEIN AIR
  )+
  geom_point(aes(x = dateachat, y = unit_price))+
  geom_line(
    aes(x = dateachat, y = ref_price, color = "Reference price"), 
    linetype = "dashed", size = 1.5
    )+
  xlab("Purchase date") + ylab("Price per egg")+
  scale_colour_discrete(name = "Dashed line")


ggsave(
  "Outputs/price_plot_ref_price.png", device = "png", 
  width = 20, height = 15, units = "cm"
)

# Question : Do we observe a correlation between prices and demographics ?

household = readRDS("Inputs/household.rds")

ggplot(
  shopping_trips_without_nosale%>%
    group_by(retailer, marque, label, calibre, valqvol)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    arrange(-sales)%>%
    mutate_at(vars(sales), as.factor)%>%
    filter(as.numeric(sales) == 191)%>%
    left_join(household)
  # 191 U CALIBRE M 12 PLEIN AIR
)+
  geom_point(aes(x = dateachat, y = unit_price, color = log(rve_num)))+
  xlab("Purchase date") + ylab("Price per egg")+
  scale_color_continuous(type = "viridis")



ggplot(
  shopping_trips_without_nosale%>%
    group_by(retailer, marque, label, calibre, valqvol)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    arrange(-sales)%>%
    mutate_at(vars(sales), as.factor)%>%
    filter(as.numeric(sales) == 191)%>%
    left_join(household)
  # 191 U CALIBRE M 12 PLEIN AIR
)+
  geom_point(aes(x = dateachat, y = unit_price, color = clas), size = 2)+
  xlab("Purchase date") + ylab("Price per egg") +
  scale_colour_manual(
    name = "",
    labels = c(
      "High Income (4%)",
      "High Middle Income (31%)",
      "Low Income (41%)",
      "Low Middle Income (24%)"
    ),
    values = c("red", "orange", "green", "blue")
    )

ggsave(
  "Outputs/price_plot_income.png", device = "png", 
  width = 20, height = 15, units = "cm"
  )


shopping_trips_without_nosale%>%
  group_by(retailer, marque, label, calibre, valqvol)%>%
  mutate(sales = n())%>%
  ungroup()%>%
  arrange(-sales)%>%
  mutate_at(vars(sales), as.factor)%>%
  filter(as.numeric(sales) == 191)%>%
  left_join(household)%>%
  .$clas%>%
  as.factor()%>%
  summary()

# 30 + 196 + 153 + 260 = 639
# 30 / 639
# 196 / 639
# 260 / 639


############################
###### 3. HOUSEHOLDS #######
############################

# Question : is the sample representative of French households ?

# Critères de comparaison :
# - Age max entre personne de reference et conjoint if any (pyramide)
# - Niveau d'education max dans le menage
# - TUU10
# - Revenu brut en tranches

srcv = readRDS("Data/SRCV_for_demographics.rds")

srcv_renamed = srcv%>%
  filter(aenq == 2014)%>%
  mutate(
    educ = case_when(
      hatlevel == "" ~ -1,
      hatlevel == "." ~ 0,
      TRUE ~ as.numeric(hatlevel)
    ),
    area = ifelse(tuu10 == "", -1, as.numeric(tuu10)),
    rve_num = hy010/12,
    age_max = ifelse(is.na(agcj) | agpr >= agcj, agpr, agcj),
    enfant = ifelse(enfant == "", -1, 2 - as.numeric(enfant)) 
  )%>%
  filter(
    educ >= 0 | enfant == 1,
    area >= 0,
    enfant >= 0,
  )%>%
  group_by(men)%>%
  summarise(
    age_youngest_child = min(80 * (1-enfant) + ag),
    rve_num = max(rve_num),
    age_max = max(age_max),
    max_etud = as.integer(max(educ)/100),
    area = max(area)
  )%>%
  mutate(
    max_etud = case_when(
      max_etud %in% c(0,1) ~ "PRIMARY EDUCATION AND BELOW",
      max_etud == 2 ~ "SECONDARY EDUCATION (FIRST CYCLE)",
      max_etud %in% c(3,4) ~ "SECONDARY EDUCATION (SECOND CYCLE)",
      max_etud == 5 ~ "SHORT HIGHER EDUCATION TRAINING",
      max_etud == 6 ~ "BACHELOR DEGREE OR EQUIVALENT",
      max_etud >= 7 ~ "MASTER DEGREE AND BEYOND",
      ),
    rve = case_when(
      rve_num < 300 ~ "BELOW 300 €", 
      rve_num < 450 ~ "300 - 449 €", 
      rve_num < 600 ~ "450 - 599 €", 
      rve_num < 750 ~ "600 - 749 €", 
      rve_num < 900 ~ "750 - 899 €", 
      rve_num < 1100 ~ "900 - 1,099 €", 
      rve_num < 1200 ~ "1,100 - 1,199 €", 
      rve_num < 1400 ~ "1,200 - 1,399 €", 
      rve_num < 1500 ~ "1,400 - 1,499 €", 
      rve_num < 1900 ~ "1,500 - 1,899 €", 
      rve_num < 2300 ~ "1,900 - 2,299 €", 
      rve_num < 2700 ~ "2,300 - 2,699 €", 
      rve_num < 3000 ~ "2,700 - 2,999 €", 
      rve_num < 3800 ~ "3,000 - 3,799 €", 
      rve_num < 4500 ~ "3,800 - 4,499 €", 
      rve_num < 5400 ~ "4,500 - 5,399 €", 
      rve_num < 7000 ~ "5,400 - 6,999 €",
      TRUE ~ "BEYOND 7,000 €"
    ),
    jenf = case_when(
      age_youngest_child < 2 ~ "YOUNGEST CHILD < 2 Y.O.",
      age_youngest_child < 6 ~ "YOUNGEST CHILD 2-5 Y.O.",
      age_youngest_child < 11 ~ "YOUNGEST CHILD 6-10 Y.O.",
      age_youngest_child < 16 ~ "YOUNGEST CHILD 11-15 Y.O.",
      TRUE ~ "NO CHILD UNDER 16"
    ),
    area = case_when(
      area == 0 ~ "RURAL AREAS",
      area == 1 ~ "URBAN AREAS FROM 2,000 TO 4,999 INHABITANTS",
      area == 2 ~ "URBAN AREAS FROM 5,000 TO 9,999 INHABITANTS",
      area == 3 ~ "URBAN AREAS FROM 10,000 TO 19,999 INHABITANTS",
      area == 4 ~ "URBAN AREAS FROM 20,000 TO 49,999 INHABITANTS",
      area == 5 ~ "URBAN AREAS FROM 50,000 TO 99,999 INHABITANTS",
      area == 6 ~ "URBAN AREAS FROM 100,000 TO 199,999 INHABITANTS",
      area == 7 ~ "URBAN AREAS FROM 200,000 TO 1,999,999 INHABITANTS",
      area == 8 ~ "PARIS URBAN AREA"
      )
  )%>%
  select(
    -age_youngest_child,
    -rve_num,
    -men
  )%>%
  rename(
    `Children` = jenf,
    `Area` = area,
    `Education` = max_etud,
    `Monthly Pre-tax Income` = rve,
    `Age (Oldest Household Member)` = age_max
  )

tbl_summary(srcv_renamed)


consumption_without_nosale = readRDS("Inputs/shopping_trips_without_nosale.rds")
relevant_hhid = consumption_without_nosale$hhid %>% unique()

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
    max_age = ifelse(is.na(age2) | age1>age2, age1, age2),
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
    rve = case_when(
      rve == "MOINS DE 300 EURO (MOINS DE 2 000 F)" ~ "BELOW 300 €",
      rve == "7 000 EURO ET PLUS (45 000 F ET PLUS)" ~ "BEYOND 7,000 €",
      TRUE ~ rve
    ),
    rve = gsub("DE ", "", rve),
    rve = gsub(" A ", "- ", rve),
    rve = gsub("EURO ", "€", rve),
    rve = gsub("EURO", "", rve),
    rve = gsub("\\(.*$","", rve),
    rve = str_replace_all(rve, "([0-9]) ([0-9]{3})", "\\1\\,\\2"),
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
      jenf == "PAS D ENFANT" ~ "NO CHILD UNDER 16"
    ),
    habi_inra = case_when(
      habi_inra == "COMMUNES RURALES" ~ "RURAL AREAS",
      habi_inra == "PARIS + AGGLOMERATION" ~ "PARIS URBAN AREA",
      TRUE ~ habi_inra
    ),
    habi_inra = gsub("HABITANTS", "INHABITANTS", habi_inra),
    habi_inra = gsub("UNITES URBAINES DE", "URBAN AREAS FROM", habi_inra),
    habi_inra = gsub(" A ", " TO ", habi_inra),
    habi_inra = gsub(" ET PLUS", "", habi_inra),
    habi_inra = ifelse(
      habi_inra == "URBAN AREAS FROM 200 000 INHABITANTS", 
      "URBAN AREAS FROM 200 000 TO 1 999 999 INHABITANTS",
      habi_inra
      ),
    habi_inra = str_replace_all(habi_inra, "([0-9]) ([0-9]{3})", "\\1\\,\\2"),
    habi_inra = str_replace_all(habi_inra, "([0-9]) ([0-9]{3})", "\\1\\,\\2")
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
      ) & (max_etud >= 4) & 
      clas %in% c(
        "HIGH MIDDLE INCOME",
        "HIGH INCOME"
      )
  )

saveRDS(household_cleaner2, "Inputs/household.rds")
saveRDS(
  household_cleaner2%>% filter(hhid %in% relevant_hhid), 
  "Inputs/household_opt.rds")


household_renamed = household_cleaner2%>%
  select(
    -hhid,
    -rve_num,
    -single
  )%>%
  mutate(
    max_etud = case_when(
      max_etud %in% c(0,1) ~ "PRIMARY EDUCATION AND BELOW",
      max_etud == 2 ~ "SECONDARY EDUCATION (FIRST CYCLE)",
      max_etud %in% c(3,4) ~ "SECONDARY EDUCATION (SECOND CYCLE)",
      max_etud == 5 ~ "SHORT HIGHER EDUCATION TRAINING",
      max_etud == 6 ~ "BACHELOR DEGREE OR EQUIVALENT",
      max_etud >= 7 ~ "MASTER DEGREE AND BEYOND",
    )
  )%>%
  rename(
    `Life Cycle` = cycle,
    `Income` = clas,
    `Children` = jenf,
    `Area` = habi_inra,
    `Education` = max_etud,
    `Monthly Pre-tax Income` = rve,
    `Age (Oldest Household Member)` = max_age,
    `Young Highly-Educated Single` = young_highly_educated_single,
    `Educated Parents with Middle or High Income` = educated_parents_middle_high_income
  )

household_renamed_opt = household_cleaner2%>%
  filter(hhid %in% relevant_hhid)%>%
  select(
    -hhid,
    -rve_num,
    -single
  )%>%
  mutate(
    max_etud = case_when(
      max_etud %in% c(0,1) ~ "PRIMARY EDUCATION AND BELOW",
      max_etud == 2 ~ "SECONDARY EDUCATION (FIRST CYCLE)",
      max_etud %in% c(3,4) ~ "SECONDARY EDUCATION (SECOND CYCLE)",
      max_etud == 5 ~ "SHORT HIGHER EDUCATION TRAINING",
      max_etud == 6 ~ "BACHELOR DEGREE OR EQUIVALENT",
      max_etud >= 7 ~ "MASTER DEGREE AND BEYOND",
    )
  )%>%
rename(
  `Life Cycle` = cycle,
  `Income` = clas,
  `Children` = jenf,
  `Area` = habi_inra,
  `Education` = max_etud,
  `Monthly Pre-tax Income` = rve,
  `Age (Oldest Household Member)` = max_age,
  `Young Highly-Educated Single` = young_highly_educated_single,
  `Educated Parents with Middle or High Income` = educated_parents_middle_high_income
)%>% drop_na()

tbl_summary(household_renamed_opt)


df_pyramid_plot = bind_rows(
  srcv_renamed%>% mutate(Source = "Insee (2014)"),
  household_renamed%>% mutate(Source = "Kantar WorldPanel (2012)"),
  household_renamed_opt%>% mutate(Source = "Kantar Subsample for Estimation")
)%>%
  select(
    `Age (Oldest Household Member)`,
    `Monthly Pre-tax Income`,
    Area,
    Education,
    Children,
    Source
  )%>%
  mutate(
    `Age (Oldest Household Member)` =
      case_when(
        `Age (Oldest Household Member)` < 25 ~ "UNDER 25",
        `Age (Oldest Household Member)` < 35 ~ "25 - 34",
        `Age (Oldest Household Member)` < 45 ~ "35 - 44",
        `Age (Oldest Household Member)` < 55 ~ "45 - 54",
        `Age (Oldest Household Member)` < 65 ~ "55 - 64",
        `Age (Oldest Household Member)` < 75 ~ "65 - 74",
        `Age (Oldest Household Member)` < 85 ~ "75 - 84",
        TRUE ~ "OVER 85",
      )
  )

#### PYRAMID DENSITY ####
ggplot( df_pyramid_plot )+
  geom_bar(
    data = df_pyramid_plot,
    aes(x = Area, y = ..prop.., group = Source, fill = Source),
    stat="count", position = "dodge"
    )+
  coord_flip()+
  #facet_grid(cols = vars(Source))+
  scale_x_discrete(
    limits = 
      c(
        "RURAL AREAS",
        "URBAN AREAS FROM 2,000 TO 4,999 INHABITANTS",
        "URBAN AREAS FROM 5,000 TO 9,999 INHABITANTS",
        "URBAN AREAS FROM 10,000 TO 19,999 INHABITANTS",
        "URBAN AREAS FROM 20,000 TO 49,999 INHABITANTS",
        "URBAN AREAS FROM 50,000 TO 99,999 INHABITANTS",
        "URBAN AREAS FROM 100,000 TO 199,999 INHABITANTS",
        "URBAN AREAS FROM 200,000 TO 1,999,999 INHABITANTS",
        "PARIS URBAN AREA"
        )
  )+
  scale_y_continuous(labels=scales::percent) +
  ylab("")+ xlab("Location of the Household")+
  scale_fill_discrete(
    breaks = c(
      "Insee (2014)", 
      "Kantar WorldPanel (2012)", 
      "Kantar Subsample for Estimation"
    ),
    type = c("#F8766D","#00BA38", "#619CFF")
  )

ggsave(
  filename = "Outputs/household_density_pyramid.png",
  device = "png",
  width = 28,
  height = 11,
  unit = "cm"
)

#### PYRAMID INCOME ####
ggplot( df_pyramid_plot )+
  geom_bar(
    data = df_pyramid_plot,
    aes(x = `Monthly Pre-tax Income`, y = ..prop.., group = Source, fill = Source),
    stat="count", position = "dodge"
  )+
  coord_flip()+
  #facet_grid(cols = vars(Source))+
  scale_x_discrete(
    limits = 
      c(
        "BELOW 300 €", 
        "300 - 449 €", 
        "450 - 599 €", 
        "600 - 749 €", 
        "750 - 899 €", 
        "900 - 1,099 €",
        "1,100 - 1,199 €",
        "1,200 - 1,399 €",
        "1,400 - 1,499 €",
        "1,500 - 1,899 €",
        "1,900 - 2,299 €",
        "2,300 - 2,699 €",
        "2,700 - 2,999 €",
        "3,000 - 3,799 €",
        "3,800 - 4,499 €",
        "4,500 - 5,399 €",
        "5,400 - 6,999 €",
        "BEYOND 7,000 €"
      )
  )+
  scale_y_continuous(labels=scales::percent) +
  ylab("")+ xlab("Monthly Pre-tax Income")+
  scale_fill_discrete(
    breaks = c(
      "Insee (2014)", 
      "Kantar WorldPanel (2012)", 
      "Kantar Subsample for Estimation"
    ),
    type = c("#F8766D","#00BA38", "#619CFF")
  )

ggsave(
  filename = "Outputs/household_income_pyramid.png",
  device = "png",
  width = 18,
  height = 11,
  unit = "cm"
)


#### PYRAMID EDUCATION ####
ggplot( df_pyramid_plot )+
  geom_bar(
    data = df_pyramid_plot,
    aes(x = `Education`, y = ..prop.., group = Source, fill = Source),
    stat="count", position = "dodge"
  )+
  coord_flip()+
  #facet_grid(cols = vars(Source))+
  scale_x_discrete(
    limits = 
      c(
        "PRIMARY EDUCATION AND BELOW",
        "SECONDARY EDUCATION (FIRST CYCLE)",
        "SECONDARY EDUCATION (SECOND CYCLE)",
        "SHORT HIGHER EDUCATION TRAINING",
        "BACHELOR DEGREE OR EQUIVALENT",
        "MASTER DEGREE AND BEYOND"
      )
  )+
  scale_y_continuous(labels=scales::percent) +
  ylab("")+ xlab("Highest Education among Household Members")+
  scale_fill_discrete(
    breaks = c(
      "Insee (2014)", 
      "Kantar WorldPanel (2012)", 
      "Kantar Subsample for Estimation"
      ),
    type = c("#F8766D","#00BA38", "#619CFF")
  )

ggsave(
  filename = "Outputs/household_education_pyramid.png",
  device = "png",
  width = 25,
  height = 11,
  unit = "cm"
)


#### PYRAMID CHILDREN #####
ggplot( df_pyramid_plot )+
  geom_bar(
    data = df_pyramid_plot,
    aes(x = Children, y = ..prop.., group = Source, fill = Source),
    stat="count", position = "dodge"
  )+
  coord_flip()+
  #facet_grid(cols = vars(Source))+
  scale_x_discrete(
    limits = 
      c(
        "YOUNGEST CHILD < 2 Y.O.",
        "YOUNGEST CHILD 2-5 Y.O.",
        "YOUNGEST CHILD 6-10 Y.O.",
        "YOUNGEST CHILD 11-15 Y.O.",
        "NO CHILD UNDER 16"
      )
  )+
  scale_y_continuous(labels=scales::percent) +
  ylab("")+ xlab("")+
  scale_fill_discrete(
    breaks = c(
      "Insee (2014)", 
      "Kantar WorldPanel (2012)", 
      "Kantar Subsample for Estimation"
    ),
    type = c("#F8766D","#00BA38", "#619CFF")
  )

ggsave(
  filename = "Outputs/household_children_pyramid.png",
  device = "png",
  width = 20,
  height = 11,
  unit = "cm"
)


#### PYRAMID AGE ######
ggplot( df_pyramid_plot )+
  geom_bar(
    data = df_pyramid_plot,
    aes(x = `Age (Oldest Household Member)`, y = ..prop.., group = Source, fill = Source),
    stat="count", position = "dodge"
  )+
  coord_flip()+
  #facet_grid(cols = vars(Source))+
  scale_x_discrete(
    limits = 
      c(
        "UNDER 25",
        "25 - 34",
        "35 - 44",
        "45 - 54",
        "55 - 64",
        "65 - 74",
        "75 - 84",
        "OVER 85"
      )
  )+
  scale_y_continuous(labels=scales::percent) +
  ylab("")+ xlab("Age of the Oldest Household Member")+
  scale_fill_discrete(
    breaks = c(
      "Insee (2014)", 
      "Kantar WorldPanel (2012)", 
      "Kantar Subsample for Estimation"
    ),
    type = c("#F8766D","#00BA38", "#619CFF")
  )

ggsave(
  filename = "Outputs/household_age_pyramid.png",
  device = "png",
  width = 18,
  height = 11,
  unit = "cm"
)


##############################
###### 4. COMPETITION ########
##############################


shopping_trips_with_nosale = readRDS("Inputs/shopping_trips_with_nosale.rds")
shopping_trips_without_nosale = readRDS("Inputs/shopping_trips_without_nosale.rds")

shopping_trips_with_nosale_for_estimation = readRDS("Inputs/shopping_trips_with_nosale_for_estimation.rds")
shopping_trips_without_nosale_for_estimation = readRDS("Inputs/shopping_trips_without_nosale_for_estimation.rds")


### COL 1 : SHOPPING TRIPS | COL 2 : SHOPPING TRIPS WITH EGGS

### ROW 1 : NB RETAILER DURING THE YEAR
shopping_trips_with_nosale%>%
  select(hhid, retailer)%>%
  unique()%>%
  count(hhid)%>%
  .$n%>%
  #as.factor()%>%
  summary()

shopping_trips_without_nosale%>%
  select(hhid, retailer)%>%
  unique()%>%
  count(hhid)%>%
  .$n%>%
  #as.factor()%>%
  summary()

### ROW 2 : SHARE OF VISITS TO THE MAIN RETAILERS
shopping_trips_with_nosale%>%
  count(hhid, retailer)%>%
  group_by(hhid)%>%
  summarise(share_max_retailer = max(n)/sum(n))%>%
  .$share_max_retailer%>%
  #hist()
  summary()

shopping_trips_without_nosale%>%
  count(hhid, retailer)%>%
  group_by(hhid)%>%
  summarise(share_max_retailer = max(n)/sum(n))%>%
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

summary(share_empty_shopping_trips_at_main_retailer_for_eggs)


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

summary(share_empty_shopping_trips_at_main_retailer_for_eggs)


## Nb periode d'achat par hhid (6 ou moins, 9 ou moins, 12)

shopping_trips_with_nosale_for_estimation = 
  readRDS("Inputs/shopping_trips_with_nosale_for_estimation.rds")

shopping_trips_without_nosale_for_estimation = 
  readRDS("Inputs/shopping_trips_without_nosale_for_estimation.rds")

hhid_final_sample = shopping_trips_with_nosale_for_estimation$hhid%>% unique()

shopping_trips_without_nosale_for_estimation%>%
  select(hhid, periode)%>%
  unique()%>%
  count(hhid)%>%
  mutate(
    category = case_when(
      n < 6 ~ "five_or_below",
      n < 10 ~ "six to nine",
      TRUE ~ "ten to thirteen"
    ),
      )%>%
  .$category%>%
  as.factor()%>%
  summary()

  # 1742 / 5641
  # 1598 / 5641
  # 2301 / 5641

  # 1598 / (1598 + 2301)

  # 13 / 5641
  # 58 / 5641
  # 5570 / 5641
  



