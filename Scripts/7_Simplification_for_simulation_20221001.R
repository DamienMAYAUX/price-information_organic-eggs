



if (Sys.info()["sysname"] == "Windows"){
  setwd("U:/price-information_organic-eggs")
} else {
  setwd("~/U/price-information_organic-eggs")
}

source("Scripts/0_Packages_Libraries.R")


## CHOICE SITUATIONS

consumption_cleaner6 = readRDS("Inputs/shopping_trips_with_nosale_20211222.rds")
df_retailer_set = readRDS("Inputs/df_hhid_retailer_set_20211222.rds")
df_choice_set = readRDS("Inputs/choice_sets_with_nosale_20211222.rds")

df_choice_set_simplified = df_choice_set%>%
  select(-valqvol)%>% 
  mutate(
    marque = ifelse(
      marque_simple %in% c("high", "medium", "low"), 
      paste(marque_simple, retailer, sep = '_'),
      as.character(marque)
    )
  )%>% 
  unique()

df_product_simplified = df_choice_set_simplified%>%
  select(-retailer, -periode)%>% unique()

consumption_cleaner6_simplified = consumption_cleaner6%>%
  select(-marque, -valqvol)%>%
  left_join(
    df_product_simplified,
    by = c("marque_simple", "label", "calibre")
    )%>%
  mutate(
    marque = ifelse(marque_simple == "nosale", "nosale", marque)
  )

df_choice_situation_simplified = consumption_cleaner6_simplified%>%
  rename(
    marque_chosen = marque,
    label_chosen = label,
    calibre_chosen = calibre,
    marque_simple_chosen = marque_simple,
    retailer_chosen = retailer
  )%>%
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
    consumption_cleaner6_simplified%>%
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

## ON INTRODUIT LES BONS NO-SALE

hhid_with_enough_period_with_visit = consumption_cleaner6_simplified%>%
  filter(marque_simple != "nosale")%>%
  group_by(hhid, periode)%>%
  summarise( nb_visits = n() )%>%
  group_by(hhid)%>%
  summarise( nb_periode_with_visit = n() )%>%
  ######### MAIN PARAMETER #############
filter(nb_periode_with_visit >= 6)%>%
  ######################################
.$hhid%>%
  .[order(.)]

missing_hhid_periode =
  # Create a table with all the households and all the periods
  full_join(
    data.frame(hhid = hhid_with_enough_period_with_visit, constant = 1),
    data.frame(periode = as.factor(1:13), constant = 1)
  )%>%
  select(-constant)%>%
  # Keep only couple household period for which there is no shopping trip
  anti_join(
    consumption_cleaner6_simplified%>%
      filter(marque_simple != "nosale")%>%
      select(periode, hhid)%>% unique
  )

consumption_completed_with_nosale = 
  # Build the dataframe of nosale to draw from
  consumption_cleaner6_simplified%>%
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
    consumption_cleaner6_simplified%>%
      filter(marque_simple != "nosale")%>%
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



## PRICES AND CONTROL FUNCTIONS

# df_price4 = readRDS("Inputs/product_price_20211222.rds")
# 
# df_price_simplified = df_price4%>%
#   select(-marque, -valqvol)%>%
#   left_join(
#     df_product_simplified,
#     by = c("marque_simple", "label", "calibre")
#   )%>%
#   group_by(marque_simple, label, calibre, marque, retailer, periode)%>%
#   summarise(avg_price = mean(avg_price), .groups = 'drop')

df_price = consumption_cleaner6_simplified%>%
  group_by(marque_simple, label, calibre, periode)%>%
  mutate(
    avg_price_across_retailer = mean(unit_price),
    nb_sales_across_retailer = n()
    )%>%
  ungroup()%>%
  group_by(marque_simple, label, calibre, marque, retailer, periode)%>%
  summarise(
    avg_price = ifelse(marque_simple == "nosale", 0, mean(unit_price)),
    avg_price_across_retailer = max(avg_price_across_retailer),
    nb_sales = n(),
    nb_sales_across_retailer = max(nb_sales_across_retailer),
    avg_price_other_retailer = ifelse(marque_simple == "nosale", 0,
      avg_price_across_retailer * nb_sales_across_retailer / (nb_sales_across_retailer-nb_sales) -
      avg_price * nb_sales / (nb_sales_across_retailer-nb_sales) ),
    .groups = 'drop')

df_price_for_regression = consumption_completed_with_nosale%>%
  left_join(df_price)%>%
  pivot_wider(names_from = c("label", "calibre", "marque", "retailer"), values_from = "avg_price")%>%
  select(unit_price, contains("calibre"))%>%
  mutate_all(~replace_na(., 0))

fit_lm_ref_price = lm(unit_price ~ avg_price_other_retailer - 1, data = df_price_for_regression)

df_mu = data.frame(mu_value = fit_lm_ref_price$coefficient)%>%
  rownames_to_column("mu_name")%>%
  mutate(
    label = str_replace(str_extract(mu_name, "^[^_]*(?=_)"),"`",""),
    calibre = str_extract(mu_name, "calibre[A-Z]"),
    retailer = str_extract(mu_name, "[0-9]{1,2}_[0-9]{1,2}"),
    marque = str_replace(str_extract(mu_name, paste0("_[^_]+(?=_", retailer, ")")),"_","")
  )%>%
  select(-mu_name)



## DES CHOICE SET A CHOICE SET POUR APOLLO

choice_situation_without_price_with_nosale_for_estimation = 
  df_choice_situation_simplified%>%
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