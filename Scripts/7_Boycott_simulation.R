

setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")

start = Sys.time()

model_name = "nosale_with_control"
model = apollo_loadModel(paste0("Inputs/Apollo/", model_name))

df_product_with_nosale = readRDS("Inputs/product_with_nosale_20211129.rds")%>%
  mutate_at(vars(calibre, label, marque_simple), as.character)
df_hhid_retailer = readRDS("Inputs/df_hhid_retailer_set_20211129.rds")

df_price = readRDS("Inputs/product_price_20211129.rds")

choice_situation_with_nosale_for_estimation =
  readRDS("Inputs/choice_situation_with_nosale_for_estimation_20211129.rds")

consumption = readRDS("Inputs/shopping_trips_without_nosale.rds")


# Average prices observed in the data, the price list by default
retailer_price_list_test = df_price%>%
  group_by(label, calibre, marque, retailer, valqvol)%>%
  summarize(
    avg_price_across_period = mean(avg_price)
  )%>%
  right_join(
    df_product_with_nosale
  )%>%
  mutate(
    avg_price_across_period = ifelse(marque == "nosale", 0, avg_price_across_period)
  )%>%
  arrange(product_number)%>%
  .$avg_price_across_period

# Some possible boycott prices
radical_activist_price_list = df_product_with_nosale%>%
  mutate(price = ifelse(label == "labelbio", 0.35, 0.01))%>%
  .$price

moderate_activist_price_list = df_product_with_nosale%>%
  mutate(
    price = case_when(
      label == "labelbio" ~ 0.45,
      label == "labelpleinair" ~ 0.25,
      TRUE ~ 0.01
    )
  )%>%
  .$price

# Several population samples for the boycott
hhid_list = consumption%>%
  .$hhid %>% unique()

hhid_top_organic_consumer_list = consumption%>%
  group_by(hhid)%>%
  summarize(
    organic_share = sum(label == "labelbio")/n()
  )%>%
  filter(organic_share > 0.8)%>%
  .$hhid



## GENERATE A DATABASE 

# Generates the database with the choice situations corresponding to the 
# set of retailer prices and activist prices considered

generate_database = function(retailer_price_list, activist_price_list = NULL, hhid_activist_list = NULL){

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
    # On récupere les caracteristiques du produit
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

  return(database)

}



## CREATE A FUNCTION THAT SIMULATES THE DEMAND FOR A GIVEN PRICE VECTOR

compute_demand = function(model, retailer_price_list, activist_price_list = NULL, hhid_activist_list = NULL){
  
  # df_retailer_price = df_retailer_price_test
  
  database <<- generate_database(retailer_price_list, activist_price_list, hhid_activist_list)
  
  apollo_beta <<- model$apollo_beta
  apollo_fixed <<- model$apollo_fixed
  apollo_control <<- model$apollo_control
  apollo_inputs <<- apollo_validateInputs()
  apollo_probabilities <<- model$apollo_probabilities
  prediction_settings <<- list(silent = TRUE)
  
  forecast = apollo_prediction(
    model,
    apollo_probabilities,
    apollo_inputs,
    prediction_settings
  )
  
  demand = forecast%>%
    select(-ID, -Observation, -chosen)%>%
    pivot_longer(
      cols = 1:(length(forecast)-3),
      names_to = "product_number",
      values_to = "probability_sales" 
    )%>%
    group_by(product_number)%>%
    summarize(
      aggregate_sales = sum(probability_sales)
    )%>%
    mutate(
      product_number = as.integer(gsub("alt", "", product_number))
    )%>% 
    arrange(product_number)%>% 
    .$aggregate_sales
  
  return(demand)
}

# demand_function(model, df_retailer_price_test)

## DISPLAYS SOME SUMMARY STATISTICS ON THE DEMAND
demand_summary = function(demand){
  
  df_market_share = df_product_with_nosale%>%
    mutate(market_size = demand)%>%
    mutate(total_market_size = sum(market_size))
  
  # Label market share
  df_market_share%>% 
    group_by(label)%>%
    summarise(share = sum(market_size)/max(total_market_size))%>%
    print()
  
  # Retailer market share
  df_market_share%>% 
    mutate(retailer = ifelse(marque == "nosale", "nosale", as.character(retailer)))%>%
    group_by(retailer)%>%
    summarise(share = sum(market_size)/max(total_market_size))%>%
    arrange(retailer)%>%
    print()
  
}

## TESTING DEMAND RESPONSE DURING A BOYCOTT

demand_test = compute_demand(
  model, 
  retailer_price_list_test,
  activist_price_list = moderate_activist_price_list, 
  hhid_activist_list = hhid_top_organic_consumer_list
)

demand_summary(demand_test)



## SIMULATING CROSS ELASTICITIES

generate_cross_product_price_derivative_matrix = 
  function(model, retailer_price_list, price_change = 0.01, activist_price_list = NULL, hhid_activist_list = NULL){
  
  start_time = Sys.time()
  
  product_nb = nrow(df_product_with_nosale)
  cross_product_price_derivative_matrix = matrix(data = 0, nrow = product_nb, ncol = product_nb)
  demand = compute_demand(model, retailer_price_list)
  
  database_original <<- generate_database(retailer_price_list)
  apollo_beta <<- model$apollo_beta
  apollo_fixed <<- model$apollo_fixed
  apollo_control <<- model$apollo_control
  
  apollo_probabilities <<- model$apollo_probabilities
  prediction_settings <<- list(silent = TRUE)
  
  # On itere selon le prix que l'on fait changer
  # Cela revient à remplir ligne à ligne la matrice des elasticites
  for (i in 1:product_nb){
    
    # i = 12 
    print(paste("Computing column", i))
    
    # On ne calcule l'elasticite que par rapport au changement de prix d'un vrai produit
    if (df_product_with_nosale[i,"marque"] != "nosale"){
      
      database <<- database_original%>%
        mutate_at(vars(paste0(i, "_price")), ~.+price_change)
      apollo_inputs <<- apollo_validateInputs()
      
      forecast = apollo_prediction(
        model,
        apollo_probabilities,
        apollo_inputs,
        prediction_settings
      )
      
      demand_modified = forecast%>%
        select(-ID, -Observation, -chosen)%>%
        pivot_longer(
          cols = 1:(length(forecast)-3),
          names_to = "product_number",
          values_to = "probability_sales" 
        )%>%
        group_by(product_number)%>%
        summarize(
          aggregate_sales = sum(probability_sales)
        )%>%
        mutate(
          product_number = as.integer(gsub("alt", "", product_number))
        )%>% 
        arrange(product_number)%>% 
        .$aggregate_sales
      
      cross_product_price_derivative_matrix[i,] = (demand_modified-demand)/price_change
      
    }
  } 
  
  end_time = Sys.time()
  
  print("Cross_product price derivative simulation time :")
  print(end_time - start_time)
  
  return(cross_product_price_derivative_matrix)
  
}

# cross_product_price_derivative_matrix_test = generate_cross_product_price_derivative_matrix(model, retailer_price_list_test)
# 11 min
## PENSER A LA RETOURNER !
# saveRDS(cross_product_price_derivative_matrix_test, "Inputs/cross_product_price_derivative_matrix.rds")
# cross_product_price_derivative_matrix_test = readRDS("Inputs/cross_product_price_derivative_matrix.rds")


## FINDING THE COSTS

determine_costs = function(model, retailer_price_list){
  
  # On oublie les lignes et colonnes concernant le non-achat
  cross_product_price_derivative_matrix = generate_cross_product_price_derivative_matrix(model, df_retailer_price)
  
  # cross_product_price_derivative_matrix = cross_product_price_derivative_matrix_test
  
  relevant_product_number_list = df_product_with_nosale%>%
    filter(marque != "nosale")%>%
    arrange(retailer)%>%
    .$product_number
  
  relevant_retailer_price = retailer_price_list[relevant_product_number_list]
  relevant_demand = compute_demand(model, retailer_price_list)%>%
    .[relevant_product_number_list]
  
  # On construit la matrice d'appartenance
  relevant_cross_product_price_derivative_matrix = 
    cross_product_price_derivative_matrix[relevant_product_number_list, relevant_product_number_list]
  
  df_product_retailer = df_product_with_nosale%>%
    filter(marque != "nosale")%>%
    arrange(retailer)%>%
    select(product_number, retailer)%>%
    mutate(constant = 1)
  
  belonging_matrix = 
    full_join(
      df_product_retailer,
      df_product_retailer,
      by = c("constant")
    )%>%
    mutate(same_retailer = (retailer.x == retailer.y))%>%
    select(-constant, -retailer.x, -retailer.y)%>%
    pivot_wider(names_from = product_number.y, values_from = same_retailer)%>%
    column_to_rownames("product_number.x")%>%
    as.matrix()
  
  omega = belonging_matrix * relevant_cross_product_price_derivative_matrix   
  
  # On resout le system C = P + Omega^-1 D
  absolute_margin = solve(omega, relevant_demand)
  cost_list = relevant_retailer_price + absolute_margin%>% .[order(.)]
  
  return(cost_list)
  
}

# cost_list = determine_costs(model, retailer_price_list)
# saveRDS(cost_list, "Inputs/cost_list.rds")



## FINDING AN OPTIMAL RETAIL-SPECIFIC BOYCOTT PRICE




## FINDING AN OPTIMAL GLOBAL BOYCOTT PRICE

# Je demarre a 50c et je diminue centime apres centime jusqu'a 30c

df_results = data.frame(
  activist_organic_price = (30:50)/100, 
  activist_freerange_price = rep(0.25,21)
  )

activist_organic_price = 0.43
retailer_price_list  = retailer_price_list_test

activist_price_list = df_product_with_nosale%>%
  mutate(
    price = case_when(
      label == "labelbio" ~ activist_organic_price,
      label == "labelpleinair" ~ 0.25,
      TRUE ~ 0.01
    )
  )%>%
  .$price

for (retailer in retailer_list){
  
  relevant_households = 
  relevant_products = 
  
  database
  
  retailer_specific_demand = function(retailer_specific_price){
    
    database <<- database_original%>%
      mutate_at(vars(paste0(i, "_price")), ~.+price_change)
    apollo_inputs <<- apollo_validateInputs()
    
    forecast = apollo_prediction(
      model,
      apollo_probabilities,
      apollo_inputs,
      prediction_settings
    )
    
  }
  
  
}




## CONSTRUCT A HYPOTHETICAL SITUATION

# Each retailer has 
# - three retail brands, bottom- mid- and top-range
# - at most three national brands, LE GAULOIS, LOUE and MATINES
# We consider only one egg box size and calibre
# There are 14 retailers
# Thus, there is a total of 14*6 = 100 - 16 = 84 products

# We use actual costs estimated previously (check that unambigous !)
# and actual national brand availability at the retailer level

df_price = readRDS("Inputs/product_price_20211129.rds")%>%
  filter(valqvol %in% c(4,6,10,12), calibre == "calibreM")%>%
  group_by(label, marque, retailer)%>%
  summarise(avg_price = mean(avg_price))%>%
  rowid_to_column("toy_number")

df_product = readRDS("Inputs/product_with_nosale_20211129.rds")

cost_vector = 
price_vector = df_price$avg_price



