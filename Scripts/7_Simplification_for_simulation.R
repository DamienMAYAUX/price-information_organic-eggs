

if (Sys.info()["sysname"] == "Windows"){
  setwd("U:/price-information_organic-eggs")
} else {
  setwd("~/U/price-information_organic-eggs")
}

source("Scripts/0_Packages_Libraries.R")

tic("Simplification for simulation")


## LE BUT DE CE FICHIER EST DE POUVOIR GENERER DES DATABASE COMPATIBLE AVEC APOLLO
## MEME APRES AVOIR REDUIT LE NOMBRE DE PRODUITS


## Je pars de la liste des consommations initiales
df_product_with_nosale = readRDS("Inputs/product_with_nosale_20211129.rds")%>%
  mutate_at(vars(calibre, label, marque_simple), as.character)
df_hhid_retailer = readRDS("Inputs/df_hhid_retailer_set_20211129.rds")

df_price = readRDS("Inputs/product_price_20211129.rds")

choice_situation_with_nosale_for_estimation =
  readRDS("Inputs/choice_situation_with_nosale_for_estimation_20211129.rds")

consumption = readRDS("Inputs/shopping_trips_with_nosale.rds")





###### 1

consumption_cleaner6 = readRDS("Inputs/shopping_trips_with_nosale_20211222.rds")

df_retailer_set = readRDS("Inputs/df_hhid_retailer_set_20211222.rds")

df_choice_set = readRDS("Inputs/choice_sets_with_nosale_20211222.rds")
df_choice_set_simplified = df_choice_set%>%
  select(-valqvol)%>% 
  mutate(
    marque_simplified = ifelse(
      marque_simple %in% c("high", "medium", "low"), 
      paste(marque_simple, retailer, sep = '_'),
      as.character(marque)
      )
  )%>% 
  unique()

df_product_simplified = df_choice_set_simplified%>%
  select(-periode, -retailer) %>% unique()

df_choice_situation = consumption_cleaner6%>%
  
  rename(
    marque_chosen = marque,
    label_chosen = label,
    calibre_chosen = calibre,
    valqvol_chosen = valqvol,
    marque_simple_chosen = marque_simple,
    retailer_chosen = retailer
  )%>%
  select(-valqvol_chosen)%>%
  left_join(
    df_retailer_set,
    by = c("hhid", "periode")
  )%>%
  left_join(
    df_choice_set_simplified, 
    by = c("retailer", "periode")
  )%>%
  mutate(
    choice = (marque_chosen == marque & 
                label_chosen == label & 
                calibre_chosen == calibre),
    price = ifelse(choice, unit_price, NA)
  )%>%
  select(-unit_price)%>%
  mutate_at(
    vars(marque, label, calibre, marque_simple, retailer),
    as.character
  )%>%
  bind_rows(
    consumption_cleaner6%>%
      select(-valqvol)%>%
      rename(
        marque_chosen = marque,
        label_chosen = label,
        calibre_chosen = calibre,
        marque_simple_chosen = marque_simple,
        retailer_chosen = retailer
      )%>%
      mutate(
        marque = "nosale",
        label = "nosale",
        calibre = "nosale",
        marque_simple = "nosale",
        retailer = "nosale",
        choice = (marque_chosen == "nosale"),
        price = 0,
        unit_price = 0
      )
  )%>%
  mutate_at(
    vars(marque, label, calibre, marque_simple, retailer),
    as.factor
  )

saveRDS(consumption_cleaner6, "Inputs/shopping_trips_with_nosale_20211222_simplified.rds")
saveRDS(df_choice_situation, "Inputs/choice_situations_without_price_with_nosale_20211222_simplified.rds")



#### 2 #### 

df_price4 = readRDS("Inputs/product_price_20211222.rds")



#### 3 #### 


consumption_with_nosale = readRDS("Inputs/shopping_trips_with_nosale_20211222.rds")
consumption_without_nosale = readRDS("Inputs/shopping_trips_without_nosale_20211222.rds")

hhid_with_enough_period_with_visit = consumption_without_nosale%>%
  group_by(hhid, periode)%>%
  summarise( nb_visits = n() )%>%
  group_by(hhid)%>%
  summarise( nb_periode_with_visit = n() )%>%
  ######### MAIN PARAMETER #############
filter(nb_periode_with_visit >= 6)%>%
  ######################################
.$hhid%>%
  .[order(.)]


#### STEP 2 : Complete the missing periods with nosale shopping trips

missing_hhid_periode =
  # Create a table with all the households and all the periods
  full_join(
    data.frame(hhid = hhid_with_enough_period_with_visit, constant = 1),
    data.frame(periode = as.factor(1:13), constant = 1)
  )%>%
  select(-constant)%>%
  # Keep only couple household period for which there is no shopping trip
  anti_join(
    consumption_without_nosale%>% select(periode, hhid)%>% unique
  )

consumption_completed_with_nosale = 
  # Build the dataframe of nosale to draw from
  consumption_with_nosale%>%
  filter(marque_simple == "nosale")%>%
  semi_join(missing_hhid_periode)%>%
  # Pick one nosale per hhid x periode
  group_by(hhid, periode)%>%
  mutate(
    rank = 1:n(),
    lottery = sample(1:n(),1)
  )%>%
  ungroup()%>%
  filter(rank == lottery)%>%
  select(-rank, -lottery)%>%
  # Bind with the original table
  bind_rows(
    consumption_without_nosale%>%
      filter(hhid %in% hhid_with_enough_period_with_visit)
  )

# Keep only one observation per period, sales included
X_one_per_period = consumption_completed_with_nosale%>% 
  group_by(hhid, periode)%>%
  mutate(
    rank = 1:n(),
    lottery = sample(1:n(),1)
  )%>%
  ungroup()%>%
  filter(rank == lottery)%>%
  select(-rank, -lottery)%>%
  select(X)

saveRDS(X_one_per_period, "Inputs/X_one_per_period_20211222.rds")



consumption_completed_without_nosale = consumption_completed_with_nosale%>% filter(marque_simple != "nosale")

saveRDS(consumption_completed_with_nosale, "Inputs/shopping_trips_with_nosale_for_estimation_20211222.rds")


#### STEP 3 : INCORPORATE PRICES AND CONTROL FUNCTIONS

product_price = readRDS("Inputs/product_price_20211222.rds") 
control_residuals_distribution = readRDS("Inputs/control_residuals_distribution_20211222.rds")
control_residuals_med_centile_hhid = readRDS("Inputs/control_residuals_med_centile_hhid_20211222.rds")
control_first_stage_coefficients = readRDS("Inputs/control_first_stage_coefficients_20211222.rds")

choice_situation_without_price_with_nosale_for_estimation = 
  readRDS("Inputs/choice_situations_without_price_with_nosale_20211222_simplified.rds")%>%
  filter(X %in% (consumption_completed_with_nosale$X %>% unique()))

choice_situation_with_nosale_for_estimation =
  choice_situation_without_price_with_nosale_for_estimation%>%
  # Obtain household x retailer x product control function value
  left_join(control_residuals_med_centile_hhid)%>%
  left_join(
    control_residuals_distribution,
    by = c("label", "calibre", "marque", "retailer", "med_centile_v" = "centile_nb")
  )%>%
  # Obtain retailer x product average price and mu_jk
  left_join(product_price)%>%
  left_join(control_first_stage_coefficients)%>%
  mutate(
    # DIFFERENT CHOICES CAN BE MADE AT THIS STAGE
    control = ifelse(marque_simple == "nosale", 0, 500*centile_value),
    #control = ifelse(marque_simple == "nosale", 0, med_centile_nb),
    # DIFFERENT CHOICES CAN BE MADE AT THIS STAGE
    #price = ifelse(marque_simple == "nosale", 0, avg_price),
    price = ifelse(marque_simple == "nosale", 0, ref_price)
  )%>%
  select(-centile_value, -mu_value, -med_centile_v, -avg_price, -ref_price, -ref_price_vol_adjusted)

choice_situation_without_nosale_for_estimation = 
  choice_situation_with_nosale_for_estimation%>% 
  filter(marque_simple_chosen != "nosale", marque_simple != "nosale")

saveRDS(choice_situation_with_nosale_for_estimation, "Inputs/choice_situation_with_nosale_for_estimation_20211222_simplified.rds")

df_product_with_nosale = consumption_with_nosale%>%
  mutate(retailer = as.factor(ifelse(marque == "nosale", "nosale", as.character(retailer))))%>%
  select(marque, marque_simple, calibre, label,  retailer)%>%
  unique()%>%
  mutate(constant = 1)%>%
  group_by(constant)%>%
  mutate(product_number = 1:n())%>%
  ungroup()%>%
  select(-constant)

saveRDS(df_product_with_nosale, "Inputs/product_with_nosale_20211222_simplified.rds")


choice_situation_with_nosale_for_apollo = choice_situation_with_nosale_for_estimation%>%
  left_join(
    df_product_with_nosale
  )%>%
  group_by(X)%>%
  mutate(
    choice = max(choice * product_number)
  )%>%
  ungroup()%>%
  select(
    X, hhid, choice, 
    retailer, marque_simple, label, calibre, valqvol, price, control, product_number
  )%>%
  mutate_at(vars(X, hhid, choice, valqvol), ~as.integer(as.character(.)))%>%
  mutate_at(vars(price, control), as.numeric)%>%
  pivot_wider(
    id_cols = c("X", "hhid", "choice"),
    names_from = "product_number", 
    values_from = c(
      valqvol, 
      price,
      control
    ),
    names_glue = "{product_number}_{.value}",
    names_sort = TRUE
  )%>%
  mutate(across(ends_with('price'), list(avl = ~as.numeric(!is.na(.))), "{.col}_{.fn}"))

saveRDS(
  choice_situation_with_nosale_for_apollo,
  "Inputs/choice_situation_with_nosale_for_apollo_20211222_simplified.rds"
)

choice_situation_with_nosale_for_apollo_testing = 
  choice_situation_with_nosale_for_apollo%>%
  filter(
    hhid %in%
      (choice_situation_with_nosale_for_apollo$hhid%>% 
         unique()%>% .[sample(1:length(.), 200)]
      )
  )

saveRDS(
  choice_situation_with_nosale_for_apollo_testing,
  "Inputs/choice_situation_with_nosale_for_apollo_testing_20211222_simplified.rds"
)





#### 7 #### 


## GENERATE A DATABASE 

# Generates the database with the choice situations corresponding to the 
# set of retailer prices and activist prices considered

generate_database = function(retailer_price_list, activist_price_list = NULL, hhid_activist_list = NULL, df_hhid_conditional = NULL){
  
  # retailer_price_list = retailer_price_list_test
  # activist_price_list = radical_activist_price_list
  # hhid_activist_list = hhid_list
  
  choice_situation_with_nosale_for_estimation_with_new_prices = choice_situation_with_nosale_for_estimation%>%
    left_join(df_product_with_nosale)%>%
    # On remplace l'ancien prix par le nouveau prix
    left_join(
      data.frame(product_number = 1:nrow(df_product_with_nosale), new_price = retailer_price_list)
    )%>%
    mutate(price = new_price)%>%
    select(-new_price)
  
  # S'il y a boycott, on modifie les prix
  if (!is.null(activist_price_list)){
    choice_situation_with_nosale_for_estimation_with_new_prices =
      choice_situation_with_nosale_for_estimation_with_new_prices%>%
      left_join(
        data.frame(product_number = 1:nrow(df_product_with_nosale), activist_price = activist_price_list)
      )%>%
      mutate(
        price = ifelse( (price>activist_price) & (hhid%in%hhid_activist_list), NA, price)
      )
    
    # Il faudrait faire un test ici
  }
  
  choice_situation_with_nosale_for_apollo =
    choice_situation_with_nosale_for_estimation_with_new_prices%>%
    group_by(X)%>%
    mutate(
      choice = max(choice * product_number)
    )%>%
    ungroup()%>%
    select(
      X, hhid, choice,
      retailer, marque_simple, label, calibre, valqvol, price, control, product_number
    )%>%
    mutate_at(vars(X, hhid, choice, valqvol), ~as.integer(as.character(.)))%>%
    mutate_at(vars(price, control), as.numeric)%>%
    pivot_wider(
      id_cols = c("X", "hhid", "choice"),
      names_from = "product_number",
      values_from = c(
        valqvol,
        price,
        control
      ),
      names_glue = "{product_number}_{.value}",
      names_sort = TRUE
    )%>%
    mutate(across(ends_with('price'), list(avl = ~as.numeric(!is.na(.))), "{.col}_{.fn}"))%>%
    as.data.frame()
  
  rep_nb = as.integer(nrow(choice_situation_with_nosale_for_apollo) / (nrow(df_product_with_nosale) + 1))
  res_nb = as.integer(nrow(choice_situation_with_nosale_for_apollo) %% (nrow(df_product_with_nosale) + 1))
  
  database = as.data.frame(choice_situation_with_nosale_for_apollo)%>%
    # On r?cupere les caracteristiques du produit
    mutate(
      marque_simple =
        c(
          rep(c(df_product_with_nosale$marque_simple, NA_character_), rep_nb),
          as.character(df_product_with_nosale$marque_simple)[1:res_nb]
        ),
      label =
        c(
          rep(c(df_product_with_nosale$label, NA_character_), rep_nb),
          df_product_with_nosale$label[1:res_nb]
        ),
      calibre =
        c(
          rep(c(df_product_with_nosale$calibre, NA_character_), rep_nb),
          df_product_with_nosale$calibre[1:res_nb]
        )
    )%>%
    mutate_at(vars(marque_simple, label, calibre), as.factor)
  
  if (!is.null(df_hhid_conditional)){
    database = database%>%
      left_join(df_hhid_conditional)
  }
  
  return(database)
  
}



toc()








