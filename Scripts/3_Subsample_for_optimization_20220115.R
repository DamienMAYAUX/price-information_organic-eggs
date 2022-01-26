
if (Sys.info()["sysname"] == "Windows"){
  setwd("U:/price-information_organic-eggs")
} else {
  setwd("~/U/price-information_organic-eggs")
}

source("Scripts/0_Packages_Libraries.R")

start = Sys.time()

#### STEP 1 : Keep only households with a large number of non-empty shopping trips 

consumption_with_nosale = readRDS("Inputs/shopping_trips_with_nosale_20220115.rds")
consumption_without_nosale = readRDS("Inputs/shopping_trips_without_nosale_20220115.rds")

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

saveRDS(X_one_per_period, "Inputs/X_one_per_period_20220115.rds")



consumption_completed_without_nosale = consumption_completed_with_nosale%>% filter(marque_simple != "nosale")

saveRDS(consumption_completed_with_nosale, "Inputs/shopping_trips_with_nosale_for_estimation_20220115.rds")
saveRDS(consumption_completed_without_nosale, "Inputs/shopping_trips_without_nosale_for_estimation_20220115.rds")


#### STEP 3 : INCORPORATE PRICES AND CONTROL FUNCTIONS

product_price = readRDS("Inputs/product_price_20211222.rds") 
control_residuals_distribution = readRDS("Inputs/control_residuals_distribution_20211222.rds")
control_residuals_med_centile_hhid = readRDS("Inputs/control_residuals_med_centile_hhid_20211222.rds")
control_first_stage_coefficients = readRDS("Inputs/control_first_stage_coefficients_20211222.rds")

choice_situation_without_price_with_nosale_for_estimation = 
  readRDS("Inputs/choice_situations_without_price_with_nosale_20220115.rds")%>%
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
    price = ifelse(marque_simple == "nosale", 0, ref_price),
    #price = ifelse(marque_simple == "nosale", 0, ref_price * mu_value + centile_value)
    
  )%>%
  select(-centile_value, -mu_value, -med_centile_v, -avg_price, -ref_price, -ref_price_vol_adjusted)

choice_situation_without_nosale_for_estimation = 
  choice_situation_with_nosale_for_estimation%>% 
  filter(marque_simple_chosen != "nosale", marque_simple != "nosale")

saveRDS(choice_situation_with_nosale_for_estimation, "Inputs/choice_situation_with_nosale_for_estimation_20220115.rds")
saveRDS(choice_situation_without_nosale_for_estimation, "Inputs/choice_situation_without_nosale_for_estimation_20220115.rds")


#### STEP 4 : MAKE UP FOR APOLLO

# consumption_with_nosale = readRDS("Inputs/shopping_trips_with_nosale_20220115.rds")
# consumption_without_nosale = readRDS("Inputs/shopping_trips_without_nosale_20220115.rds")
# consumption_completed_with_nosale = readRDS("Inputs/shopping_trips_with_nosale_for_estimation_20220115.rds")
# consumption_completed_without_nosale = readRDS("Inputs/shopping_trips_without_nosale_for_estimation_20220115.rds")
# 
# choice_situation_with_nosale_for_estimation =
#   readRDS("Inputs/choice_situation_with_nosale_for_estimation_20220115.rds")
# choice_situation_without_nosale_for_estimation =
#   readRDS("Inputs/choice_situation_without_nosale_for_estimation_20220115.rds")

df_product_with_nosale = consumption_with_nosale%>%
  mutate(retailer = as.factor(ifelse(marque == "nosale", "nosale", as.character(retailer))))%>%
  select(marque, marque_simple, calibre, label, valqvol, retailer)%>%
  unique()%>%
  mutate(constant = 1)%>%
  group_by(constant)%>%
  mutate(product_number = 1:n())%>%
  ungroup()%>%
  select(-constant)
  
df_product_without_nosale = consumption_without_nosale%>%
  select(marque, marque_simple, calibre, label, valqvol, retailer)%>%
  unique()%>%
  mutate(constant = 1)%>%
  group_by(constant)%>%
  mutate(product_number = 1:n())%>%
  ungroup()%>%
  select(-constant)

saveRDS(df_product_with_nosale, "Inputs/product_with_nosale_20220115.rds")
saveRDS(df_product_without_nosale, "Inputs/product_without_nosale_20220115.rds")


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

choice_situation_without_nosale_for_apollo = choice_situation_without_nosale_for_estimation%>%
  filter(marque_simple != "nosale", marque_simple_chosen != "nosale")%>%
  left_join(df_product_without_nosale)%>%
  group_by(X)%>%
  mutate(
    choice = max(choice * product_number)
  )%>%
  ungroup()%>%
  mutate_at(vars(X, hhid, choice, valqvol), as.integer)%>%
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
  "Inputs/choice_situation_with_nosale_for_apollo_20220115.rds"
)

saveRDS(
  choice_situation_without_nosale_for_apollo,
  "Inputs/choice_situation_without_nosale_for_apollo_20220115.rds"
  )



#### STEP 5 : CREATE A SMALLER BASE FOR TESTING PURPOSES

# choice_situation_with_nosale_for_apollo =
#   readRDS("Inputs/choice_situation_with_nosale_for_apollo_20220115.rds")
# choice_situation_without_nosale_for_apollo =
#   readRDS("Inputs/choice_situation_without_nosale_for_apollo_20220115.rds")

choice_situation_with_nosale_for_apollo_testing = 
  choice_situation_with_nosale_for_apollo%>%
  filter(
    hhid %in%
      (choice_situation_with_nosale_for_apollo$hhid%>% 
         unique()%>% .[sample(1:length(.), 200)]
       )
  )

choice_situation_without_nosale_for_apollo_testing = 
  choice_situation_without_nosale_for_apollo%>%
  filter(
    hhid %in%
      (choice_situation_without_nosale_for_apollo$hhid%>% 
         unique()%>% .[sample(1:length(.), 200)]
      )
  )

saveRDS(
  choice_situation_with_nosale_for_apollo_testing,
  "Inputs/choice_situation_with_nosale_for_apollo_testing_20220115.rds"
)

saveRDS(
  choice_situation_without_nosale_for_apollo_testing,
  "Inputs/choice_situation_without_nosale_for_apollo_testing_20220115.rds"
)



end = Sys.time()

print("Duration: ")
print(end-start)

# 3 min

