
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")

start = Sys.time()

#### DATA LOADING

consumption = read_dta("Data/eggs2012_PF_annuel.dta")
shopping_trips = read_dta("Data/shoppingtrips2012v2.dta")


#### FILTERING OBSERVATIONS

  ## STEP 1 : RESHAPE COLUMNS, DELETE EMPTY SHOPPING PERIODS

consumption_cleaner1 = consumption%>%
  
  # Merging the columns related to the characteristic "label"
  mutate("nolabel" = (labelpleinair + labelbio + nglabel + labelmissing == 0))%>%
  pivot_longer(cols = c("labelpleinair", "labelbio", "nglabel","labelmissing", "nolabel"), names_to = "label")%>%
  filter(!is.na(value) | label == "labelbio")%>%
  mutate(label = ifelse(is.na(value), NA, label))%>%
  filter(is.na(label) | value == 1)%>%
  select(-value)%>%
  mutate(label = as.factor(label))%>%

  # Merging the columns related to the characteristic "calibre"
  mutate("nocalibre" = (caille + calibreS + calibreM + calibreL + calibreXL + calibremissing == 0))%>%
  pivot_longer(cols = c("caille", "calibreS", "calibreM", "calibreL", "calibreXL","calibremissing", "nocalibre"), names_to = "calibre")%>%
  filter(!is.na(value) | calibre == "calibreS")%>%
  mutate(calibre = ifelse(is.na(value), NA, calibre))%>%
  filter(is.na(calibre) | value == 1)%>%
  select(-value)%>%
  mutate(calibre = as.factor(calibre))%>%

  select(
    hhid,
    periode,
    semaine,
    dateachat,
    weightPFan,
    circuit,
    surface,
    sdachat,
    fabricant,
    centraleachat,
    marque,
    mdd,
    label,
    calibre,
    weightach,
    boite,
    qaachat,
    qatotale,
    vn_2,
    valqvol,
    #gencode
  ) %>%

  mutate_at(
    vars(
      fabricant,
      marque,
      circuit,
      centraleachat,
      periode,
      valqvol
    ),
    as.factor
  )%>%
  mutate(
    dateachat = as.Date(dateachat, format = "%d/%m/%Y")
  )%>%

  # The rows for which the week is missing correspond to periods during which no egg was bought
  filter(!is.na(semaine))


  ## STEP 2 : FILTER RELEVANT CHARACTERISTICS, DROP_NA

consumption_cleaner2 = consumption_cleaner1%>%
  filter(
    is.na(sdachat) | valqvol %in% c(1, 4, 6, 10, 12, 20, 30),
    is.na(sdachat) | calibre != "calibreS",
    is.na(sdachat) | calibre != "calibremissing",
    is.na(sdachat) | calibre != "nocalibre",
    is.na(sdachat) | label != "labelmissing",
    is.na(sdachat) | label != "nglabel",
    is.na(sdachat) | sdachat < 100,
    is.na(sdachat) | centraleachat %in% c(1,2,3,4,8,10,18,21,30),
    is.na(sdachat) | circuit %in% c(7,41,42,43)
  )%>%
  drop_na()


  ## STEP 3 : DEFINE RELEVANT CHARACTERISTICS, FILTER ABSURD PRICES

consumption_cleaner3 = consumption_cleaner2%>%
  mutate(
    unit_price = sdachat / ( qaachat * as.numeric(as.character(valqvol))),
    retailer = paste(circuit, centraleachat, sep = "_")
  )%>%
  rownames_to_column("X")%>%
  select(
    X, # Defines choice situation
    hhid, # Defines the household
    marque, calibre, valqvol, label, # Defines the product,
    unit_price, # Defines the price
    retailer, # Defines the retailer
    periode,
    dateachat
  )%>%
  filter(unit_price < 1)

  ## STEP 4 : DETERMINE RELEVANT RETAILERS, DROP OTHER SALES 

retailer_df = consumption_cleaner3%>% 
  group_by(retailer)%>% 
  summarise(
    total_sales = n(),
  )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

retailer_list = retailer_df%>%
  filter(total_sales>1000)%>%
  .$retailer

consumption_cleaner4 = consumption_cleaner3%>%
  filter(retailer %in% retailer_list)

# From now on, the number of shopping trips with a sales shall not change


#### CHOICE SETS AND CHOICE SITUATIONS

  ## STEP 5 : DEFINING BRAND CATEGORIES

## Je définis pour chaque label la liste des marques les plus importantes
# j'essaye d'avoir les différents labels pour une même marque
marque_bio_df = consumption_cleaner4%>%
  filter(label == "labelbio")%>%
  group_by(marque)%>% 
  summarise(
    total_sales = n()
  )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

marque_bio_list = marque_bio_df%>%
  filter(total_sales>95)%>%
  .$marque%>%
  as.character()%>%
  c(., "LUSTUCRU", "NETTO", "BABY COQUE", "REFLETS DE FRANCE", " OEUF DE NOS VILLAGES")%>%
  .[.!= "PETITJOUR"]%>%
  .[.!= "DUCLAIR"]

marque_pleinair_df = consumption_cleaner4%>%
  filter(label == "labelpleinair")%>%
  group_by(marque)%>% 
  summarise(
    total_sales = n()
  )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

marque_pleinair_list = marque_pleinair_df%>%
  filter(total_sales>180)%>%
  .$marque%>%
  as.character()%>%
  c(., "LUSTUCRU", "NETTO", "MONOPRIX", "MONOPRIX GOURMET")%>%
  .[.!= "PETITJOUR"]%>%
  .[.!= "DUCLAIR"]

marque_nolabel_df = consumption_cleaner4%>%
  filter(label == "nolabel")%>%
  group_by(marque)%>% 
  summarise(
    total_sales = n()
  )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

marque_nolabel_list = marque_nolabel_df%>%
  filter(total_sales>341)%>%
  .$marque%>%
  as.character()%>%
  c(., "LUSTUCRU LES SELECTIONS", "MONOPRIX", "BABY COQUE")%>%
  .[.!= "PETITJOUR"]%>%
  .[.!= "DUCLAIR"]

# A partir des marques sélectionnées ci-dessus, je construis ma liste des marques
marque_list = c(
  marque_bio_list,
  marque_pleinair_list,
  marque_nolabel_list
)%>%
  unique()

marques_mdd_low = c(
  "AUCHAN.PP POUCE",
  "BIEN VU",
  "CARREFOUR.DISCOUNT",
  "ECO+",
  "LIDL",
  "MARQUES ALDI",
  "NETTO",
  "TOP BUDGET"
)

marques_mdd_medium = c(
  "AUCHAN",
  "CARREFOUR",
  "CASINO",
  "MONOPRIX",
  "U"
)

marques_mdd_high = c(
  "MONOPRIX GOURMET",
  "BIO VILLAGE",
  "MOISSON",
  "OEUFS DE NOS REGIONS LES",
  "REFLETS DE FRANCE",
  "NOS REGIONS ONT DU TALENT"
)

marques_indep = c(
  "BABY COQUE",
  "COCORETTE",
  "OEUF DE NOS VILLAGES",
  "GAULOIS LE",
  "LOUE",
  "LUSTUCRU",
  "MATINES",
  "BIO"
)

df_marques = data.frame(marque = marque_list, marque_simple = NA)%>%
  mutate(
    marque_simple = case_when(
      marque %in% marques_indep ~ "indep",
      marque %in% marques_mdd_low ~ "low",
      marque %in% marques_mdd_medium ~ "medium",
      marque %in% marques_mdd_high ~ "high",
      TRUE ~ "other"
    )
  )

# Tous les produits qui ne sont pas dans les marques sélectionnées finissent dans "Marque non trouvée"
# Je regroupe entre elles certaines marque quand la différence correspond juste à un label
# Je regroupe entre elles les MDD selon le niveau de qualité associé
consumption_cleaner5  = consumption_cleaner4%>%
  mutate_at(vars(marque), as.character)%>%
  mutate(
    marque = ifelse(marque %in% marque_list, marque, "Marque Non Trouvee"),
    marque = ifelse(marque == "AUCHAN.BIO", "AUCHAN", marque),
    marque = ifelse(marque == "U.BIO U", "U", marque),
    marque = ifelse(marque == "MONOPRIX BIO", "MONOPRIX", marque),
    marque = ifelse(marque == "CARREFOUR.BIO", "CARREFOUR", marque),
    marque = ifelse(marque == "CASINO.BIO", "CASINO", marque)
  )%>%
  left_join(df_marques)%>%
  mutate_at(vars(retailer, marque_simple, marque), as.factor)


  ## STEP 6 : ADDING SHOPPING TRIPS WITHOUT SALE

hhid_list = consumption_cleaner5$hhid%>% unique()
 
shopping_trips_cleaner = shopping_trips%>%
  mutate(retailer = paste(circuit, centraleachat, sep = "_"))%>%
  filter(
    hhid %in% hhid_list,
    retailer %in% retailer_list
  )%>%
  select(-codeshopachat, -shoppingtrip, -periode)

X_max = max(as.numeric(consumption_cleaner5$X))
nb_nosale = nrow(shopping_trips_cleaner)

consumption_cleaner6 = 
  bind_rows(
    consumption_cleaner5%>%
      mutate_at(vars(valqvol, periode), ~as.numeric(as.character(.))),
    
    shopping_trips_cleaner2 = shopping_trips_cleaner%>%
      mutate(
        X = as.character((X_max+1):(X_max+nb_nosale)),
        periode = 1 + (semaine - 1) %/% 4 
        # The first week of the year 2012 starts on Jan 2nd
        # THe first week in period 1 is the one before
      )%>%
      select(X, hhid, periode, retailer)%>%
      mutate(
        marque = "nosale",
        marque_simple = "nosale",
        calibre = "nosale",
        label = "nosale",
        valqvol = 0,
        unit_price = 0
      )
    
  )%>%
  arrange(hhid, periode)%>%
  mutate_at(
    vars(retailer, periode, marque, marque_simple, label, calibre, valqvol),
    as.factor
  )%>%
  mutate_at(
    vars(X, hhid), as.integer
  )


  ## STEP 7 : CHOICE SET DEFINITION

df_choice_set = consumption_cleaner6%>%
  select(retailer, periode, marque, marque_simple, label, calibre, valqvol)%>%
  unique()

# Check the size of the choice set for each retailer and period
# df_choice_sets%>% count(retailer, periode)%>% View()


# Check that that nosale is in the choice set for each period and retailer
# df_choice_sets%>%
#   filter(marque_simple == "nosale")%>%
#   count(retailer)%>% View()
  

  ## STEP 8 : CHOICE SITUATION DEFINITION

df_choice_situation = consumption_cleaner6%>%
  rename(
    marque_chosen = marque,
    label_chosen = label,
    calibre_chosen = calibre,
    valqvol_chosen = valqvol,
    marque_simple_chosen = marque_simple
  )%>%
  left_join(
    df_choice_set, 
    by = c("retailer", "periode")
    )%>%
  mutate(
    choice = (marque_chosen == marque & 
                label_chosen == label & 
                calibre_chosen == calibre &
                valqvol_chosen == valqvol),
    price = ifelse(choice, unit_price, NA)
  )%>%
  select(-unit_price)


#### SAVING

    ## FIRST VERSION, WITH NOSALE

  saveRDS(consumption_cleaner6, "Inputs/shopping_trips_with_nosale.rds")
  saveRDS(df_choice_set, "Inputs/choice_sets_with_nosale.rds")
  saveRDS(df_choice_situation, "Inputs/choice_situations_without_price_with_nosale.rds")

    ## SECOND VERSION, WITHOUT NOSALE

  saveRDS(consumption_cleaner6%>% filter(marque != "nosale"), "Inputs/shopping_trips_without_nosale.rds")
  saveRDS(df_choice_set%>% filter(marque != "nosale"), "Inputs/choice_sets_without_price_without_nosale.rds")
  saveRDS(
    df_choice_situation%>% filter(marque != "nosale", marque_chosen != "nosale"), 
    "Inputs/choice_situations_without_nosale.rds"
    )
  
end = Sys.time()

print("Duration : ")
print(end-start)

# 2min40

  
  