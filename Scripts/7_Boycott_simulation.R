

setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")

start = Sys.time()

model_name = "nosale_with_control"

model = apollo_loadModel(paste0("Inputs/Apollo/", model_name))

database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo_testing.rds"))

df_product = as.data.frame(readRDS("Inputs/product_with_nosale.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)

## RECONSTRUCT A MODIFIED VERSION OF THE DATABASE (IN THE NOSALE CASE)

percent_price_decrease = -1
organic_product_list = df_product%>%
  rownames_to_column("product_nb")%>%
  filter(label == "labelbio")%>%
  .$product_nb

rep_nb = as.integer(nrow(database_before_merge) / (nrow(df_product) + 1))
res_nb = as.integer(nrow(database_before_merge) %% (nrow(df_product) + 1))

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


## FORECAST MARKET SHARES

apollo_beta = model$apollo_beta
apollo_fixed = model$apollo_fixed
apollo_control = model$apollo_control

apollo_inputs = apollo_validateInputs()

apollo_probabilities = model$apollo_probabilities

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

# Il faut que je joigne "forecast" avec le retailer de chaque shopping trip
# Ensuite je peux calculer des pdm en volume par periode et retailer
# Enfin, je pourrais joindre avec une base des prix pour calculer des pdm en valeur et des profits



## GENERATE A DATABASE 

# Generates the database with the choice situations corresponding to the 
# set of retailer prices and activist prices considered

database_generation = function(df_retailer_price, df_activist_price, test = FALSE){
  
  if (test){
    database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo_testing.rds"))
  } else {
    database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo.rds"))
  }
  
  df_product = as.data.frame(readRDS("Inputs/product_with_nosale.rds"))%>%
    select(marque_simple, calibre, label)%>%
    mutate_all(as.character)%>%
    rownames_to_column("product_nb")%>%
    left_join(df_retailer_price)%>%
    left_join(df_activist_price)
  
  boycotted_product_list = df_product%>%
    filter(retailer_price>boycott_price)%>%
    .$product_nb
  
  rep_nb = as.integer(nrow(database_before_merge) / (nrow(df_product) + 1))
  res_nb = as.integer(nrow(database_before_merge) %% (nrow(df_product) + 1))
  
  database = database_before_merge%>%
    # On récupere les caracteristiques du produit
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
    # On récupère le retailer du shopping trip
    left_join(df_shopping_trip_retailer)
    # On fixe les bons prix en fonction du retailer
    mutate_at(
      paste0(product_list, "_price"),
      ~ifelse(ID %in% activist_list, 0, .)
    )%>%    
    # On ne donne pas aux activistes la possibilite d'acheter des produits boycottes
    mutate_at(
      paste0(boycotted_product_list, "_price_avl"),
      ~ifelse(ID %in% activist_list, 0, .)
    )

  
  return(database)
  
}

df_activist_price_example = as.data.frame(readRDS("Inputs/product_with_nosale.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)%>%
  mutate(activist_price = rnorm(177))


df_retailer_price_example = as.data.frame(readRDS("Inputs/product_with_nosale.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)%>%
  mutate(retailer_price = rnorm(177))


database_generation(df_activist_price_example, df_retailer_price_example)


## CREATE A FUNCTION THAT SIMULATES THE DEMAND FOR A GIVEN PRICE VECTOR

demand_function = function(model, df_price, df_activist_price){
  
  database
  
  
  apollo_beta = model$apollo_beta
  apollo_fixed = model$apollo_fixed
  apollo_control = model$apollo_control
  
  apollo_inputs = apollo_validateInputs()
  
  apollo_probabilities = model$apollo_probabilities
  
  prediction_settings = list()
  
  forecast = apollo_prediction(
    model,
    apollo_probabilities,
    apollo_inputs,
    prediction_settings
  )
  
  return(forecast)
}


## SIMULATING CROSS ELASTICITIES


## FINDING AN OPTIMAL GLOBAL BOYCOTT PRICE

## FINDING AN OPTIMAL RETAIL-SPECIFIC BOYCOTT PRICE

# Define the price vector
boycott_has_converged = FALSE

while (!boycott_has_converged){
  
  # On détermine le boycott maximum que chaque retailer peut supporter
  # HYPOTHESE FORTE : les boycotts sont complémentaires
  for (retailer in retailer_list){
    
    # On genere database
    
  }
  
  
}

# Je change les prix d'un retailer dans la base
# Je calcule les pdm





