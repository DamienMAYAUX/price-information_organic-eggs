

if (Sys.info()["sysname"] == "Windows"){
  setwd("U:/price-information_organic-eggs")
} else {
  setwd("~/U/price-information_organic-eggs")
}

source("Scripts/0_Packages_Libraries.R")

start = Sys.time()

model_name = "nosale_with_control"
model_lnorm_name = "nosale_lnorm_with_control"

model = apollo_loadModel(paste0("Inputs/Apollo/", model_name))
model_lnorm = apollo_loadModel(paste0("Inputs/Apollo/", model_lnorm_name))


df_product_with_nosale = readRDS("Inputs/product_with_nosale_20211129.rds")%>%
  mutate_at(vars(calibre, label, marque_simple), as.character)
df_hhid_retailer = readRDS("Inputs/df_hhid_retailer_set_perfect_competition.rds")

df_price = readRDS("Inputs/product_price_20211129.rds")

choice_situation_with_nosale_for_estimation =
  readRDS("Inputs/choice_situation_with_nosale_for_estimation_20220115.rds")

consumption = readRDS("Inputs/shopping_trips_with_nosale.rds")


# df_product_reduced = df_product_with_nosale%>%
#   filter(calibre %in% c("calibreM","nosale"))%>%
#   select(label, retailer, marque_simple)%>%
#   unique()%>%
#   arrange(retailer, marque_simple, label)%>%
#   rownames_to_column("reduced_product_number")%>%
#   mutate_at(vars("reduced_product_number"), as.integer)


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

# retailer_price_list_test = df_price%>%
#   left_join(df_product_reduced)%>%
#   group_by(label, retailer, marque_simple, reduced_product_number)%>%
#   summarise(
#     avg_price_across_period = mean(avg_price)
#   )
  


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

##### COMPUTE THE CONDITIONALS ! ######

# database <<- generate_database(retailer_price_list_test)
# 
# apollo_beta <<- model_lnorm$apollo_beta
# apollo_fixed <<- model_lnorm$apollo_fixed
# apollo_control <<- model_lnorm$apollo_control
# apollo_draws <<-model_lnorm$apollo_draws
# apollo_randCoeff <<-model_lnorm$apollo_randCoeff
# apollo_inputs <<- apollo_validateInputs()
# apollo_probabilities <<- model_lnorm$apollo_probabilities
# prediction_settings <<- list(silent = TRUE)
# 
# start_conditional = Sys.time()
# 
# conditionals = apollo_conditionals(
#   model,
#   apollo_probabilities,
#   apollo_inputs
#   )
# 
# end_conditional = Sys.time()
# print("Computing the conditionals")
# print(end_conditional - start_conditional)
# Trente deux miutes sur le serveur distant
#
# saveRDS(conditionals, paste0("Inputs/Apollo/",model_lnorm_name, "_conditionals.rds"))

conditionals = readRDS(paste0("Inputs/Apollo/",model_lnorm_name, "_conditionals_20211214.rds"))%>%
  rename(hhid = ID, conditional = post.mean)

#######################################


# database <<- generate_database(retailer_price_list_test)
# database <<- generate_database(retailer_price_list_test, df_hhid_conditional = conditionals)


## SIMULATE THE DEMAND FOR A GIVEN PRICE VECTOR

compute_demand = function(model, retailer_price_list, 
                          activist_price_list = NULL, hhid_activist_list = NULL, 
                          df_hhid_conditional = NULL){
  
  # retailer_price_list = retailer_price_list_test
  # df_hhid_conditional = conditionals
  # activist_price_list = NULL
  # hhid_activist_list = NULL
  
  database <<- generate_database(
    retailer_price_list, 
    activist_price_list, 
    hhid_activist_list, 
    df_hhid_conditional
    )
    
  apollo_beta <<- model$estimate
  apollo_fixed <<- model$apollo_fixed
  apollo_control <<- model$apollo_control
  apollo_control$mixing <- FALSE
  
  # if (model$apollo_control$mixing & is.null(df_hhid_conditional)){
  #   apollo_draws <<-model$apollo_draws
  #   apollo_randCoeff <<-model$apollo_randCoeff
  #   apollo_control$mixing <- TRUE
  # } else {
  #   
  #   apollo_beta <<- model$apollo_beta
  # }
  
  apollo_inputs <<- apollo_validateInputs()
  
  if (is.null(df_hhid_conditional)){

    apollo_probabilities <<- model$apollo_probabilities

  } else {

  apollo_probabilities <<-
    function(apollo_beta, apollo_inputs, functionality="raw"){

      apollo_attach( apollo_beta, apollo_inputs)
      on.exit( apollo_detach( apollo_beta, apollo_inputs) )

      P = list()

      good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
      good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
      good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
      residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
      J = length(good_label)- length(residual_label) - 1

      V = list()
      for(j in 1:J) V[[paste0("alt",j)]] =
        get( paste0("b_", good_label[j])) +
        get( paste0("b_", good_marque_simple[j])) +
        get( paste0("b_", good_calibre[j])) +
        conditional * get(paste0(j, "_price")) +
        b_control * get(paste0(j, "_control")) +
        b_valqvol * get(paste0(j, "_valqvol"))

      mnl_settings = list(
        alternatives  = setNames(1:J, names(V)),
        avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
        choiceVar     = choice,
        V             = V
      )

      P[["model"]] = apollo_mnl(mnl_settings , functionality)
      P = apollo_panelProd(P, apollo_inputs , functionality)
      P = apollo_prepareProb(P, apollo_inputs , functionality)
      return(P)
    }
  
  }
  
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

# compute_demand(model, retailer_price_list_test)
# compute_demand(model, retailer_price_list_test, df_hhid_conditional = conditionals)

# blu = apollo_probabilities(model$estimate, apollo_inputs)





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

# demand_summary( compute_demand(model, retailer_price_list_test) )

# demand_summary(
#   compute_demand(
#     model, 
#     retailer_price_list_test, 
#     df_hhid_conditional = conditionals
#     )
# )

## TESTING DEMAND RESPONSE DURING A BOYCOTT

demand_test = compute_demand(
  model, 
  retailer_price_list_test,
  activist_price_list = moderate_activist_price_list, 
  hhid_activist_list = hhid_top_organic_consumer_list
)

demand_summary(demand_test)



# ## SIMULATING CROSS ELASTICITIES
# 
# generate_cross_product_price_derivative_matrix = 
#   function(model, retailer_price_list, price_change = 0.01, 
#            activist_price_list = NULL, hhid_activist_list = NULL, 
#            precomputed_demand = NULL){
#   
#   start_time = Sys.time()
#   
#   product_nb = nrow(df_product_with_nosale)
#   cross_product_price_derivative_matrix = matrix(data = 0, nrow = product_nb, ncol = product_nb)
#   
#   demand = compute_demand(model, retailer_price_list)
#   
#   database_original <<- generate_database(retailer_price_list)
#   apollo_beta <<- model$apollo_beta
#   apollo_fixed <<- model$apollo_fixed
#   apollo_control <<- model$apollo_control
#   
#   apollo_probabilities <<- model$apollo_probabilities
#   prediction_settings <<- list(silent = TRUE)
#   
#   # On itere selon le prix que l'on fait changer
#   # Cela revient a remplir ligne a ligne la matrice des elasticites
#   for (i in 1:product_nb) {
#     # i = 12
#     print(paste("Computing column", i))
#     
#     # On ne calcule l'elasticite que par rapport au changement de prix d'un vrai produit
#     if (df_product_with_nosale[i, "marque"] != "nosale") {
#       database <<- database_original %>%
#         mutate_at(vars(paste0(i, "_price")), ~ . + price_change)
#       apollo_inputs <<- apollo_validateInputs()
#       
#       forecast = apollo_prediction(model,
#                                    apollo_probabilities,
#                                    apollo_inputs,
#                                    prediction_settings)
#       
#       demand_modified = forecast %>%
#         select(-ID,-Observation,-chosen) %>%
#         pivot_longer(
#           cols = 1:(length(forecast) - 3),
#           names_to = "product_number",
#           values_to = "probability_sales"
#         ) %>%
#         group_by(product_number) %>%
#         summarize(aggregate_sales = sum(probability_sales)) %>%
#         mutate(product_number = as.integer(gsub("alt", "", product_number))) %>%
#         arrange(product_number) %>%
#         .$aggregate_sales
#       
#       cross_product_price_derivative_matrix[i, ] = (demand_modified - demand) /
#         price_change
#       
#     }
#   } 
#   
#   end_time = Sys.time()
#   
#   print("Cross_product price derivative simulation time :")
#   print(end_time - start_time)
#   
#   return(cross_product_price_derivative_matrix)
#   
# }


# cross_product_price_derivative_matrix_test =
#   generate_cross_product_price_derivative_matrix(
#     model, 
#     retailer_price_list = retailer_price_list_test, 
#     price_change = -0.02
#     )
# 11 min
# saveRDS(cross_product_price_derivative_matrix_test, "Inputs/cross_product_price_derivative_matrix.rds")
# cross_product_price_derivative_matrix_test = readRDS("Inputs/cross_product_price_derivative_matrix.rds")


## COIMPUTE ANALYTICALLY THE DEMAND AND CROSS-PRODUCT DERIVATIVE MATRIX 

generate_cross_product_derivative_and_demand = 
  function(model, retailer_price_list,  df_hhid_conditional = NULL,
           hhid_activist_list = NULL, activist_price_list = NULL){
  
  # retailer_price_list = retailer_price_list_test
  # df_hhid_conditional = conditionals
  # activist_price_list = NULL
  # hhid_activist_list = NULL
  
  ## 1) Generate forecasts
  
  database <<- generate_database(retailer_price_list, activist_price_list, hhid_activist_list, df_hhid_conditional)
  
  apollo_beta <<- model$apollo_beta
  apollo_fixed <<- model$apollo_fixed
  apollo_control <<- model$apollo_control
  apollo_control$mixing <- FALSE
  
  apollo_inputs <<- apollo_validateInputs()
  
  if (is.null(df_hhid_conditional)){
    
    apollo_probabilities <<- model$apollo_probabilities
    
  } else {
    
    apollo_probabilities <<-
      function(apollo_beta, apollo_inputs, functionality="raw"){
        
        apollo_attach( apollo_beta, apollo_inputs)
        on.exit( apollo_detach( apollo_beta, apollo_inputs) )
        
        P = list()
        
        good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
        good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
        good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
        residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
        J = length(good_label)- length(residual_label) - 1
        
        V = list()
        for(j in 1:J) V[[paste0("alt",j)]] =
          get( paste0("b_", good_label[j])) +
          get( paste0("b_", good_marque_simple[j])) +
          get( paste0("b_", good_calibre[j])) +
          conditional * get(paste0(j, "_price")) +
          b_control * get(paste0(j, "_control")) +
          b_valqvol * get(paste0(j, "_valqvol"))
        
        mnl_settings = list(
          alternatives  = setNames(1:J, names(V)),
          avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
          choiceVar     = choice,
          V             = V
        )
        
        P[["model"]] = apollo_mnl(mnl_settings , functionality)
        P = apollo_panelProd(P, apollo_inputs , functionality)
        P = apollo_prepareProb(P, apollo_inputs , functionality)
        return(P)
      }
    
  }
  
  prediction_settings <<- list(silent = TRUE)
  
  # forecast = apollo_prediction(
  #   model,
  #   apollo_probabilities,
  #   apollo_inputs,
  #   prediction_settings
  # )%>% .[[1]]
  
  forecast = apollo_probabilities(
    model$estimate,
    apollo_inputs,
  )%>% .[[1]]%>% as.data.frame()
  
  ## Compute the product-level demand from the forecast
  
  product_weight = as.numeric(df_product_with_nosale$valqvol)
  
  demand_per_product = forecast%>%
    select(starts_with("alt"))%>%
    pivot_longer(
      starts_with("alt"),
      names_to = "product_number",
      values_to = "probability_sales"
    ) %>%
    group_by(product_number) %>%
    summarize(aggregate_sales = sum(probability_sales)) %>%
    mutate(product_number = as.integer(gsub("alt", "", product_number))) %>%
    arrange(product_number) %>%
    .$aggregate_sales  # * product_weight  # Prise en compte du nb d'oeufs par boite
  
  ## Compute the cross product elasticity matrix from the forecast
  
  product_nb = nrow(df_product_with_nosale)
  
  # Je calcule s_i(dirac_i_j - s_j) et je multiple par la taille du march?
  # J'obtiens une matrix 302x302 pour chacune des 53601  observations, il faut sommer direct
  product_share_matrix = forecast%>% 
    select(starts_with("alt"))%>% 
    mutate_all(as.numeric)%>%
    as.matrix()%>%
    Matrix(., sparse = TRUE)
  
  # nnzero(product_share_matrix) / (product_nb * nrow(product_share_matrix))
  
  # La formule de la d?riv?e de la proba de choix s_i par rapport au prix p_j est - s_i(dirac_i_j - s_j) 
  cross_product_price_derivative_matrix = 
    # On prend en compte le volume des produits ?
    # Matrix(diag(product_weight), sparse = TRUE) %*% 
    (
      t(product_share_matrix) %*% (product_share_matrix) - 
      # On rajoute, sur la diagonale, la somme de la colonne de la matrice initiale
      diag( demand_per_product)
      # pourquoi faire simple quand on peut faire complique ?
      # diag( as.vector(t(as.matrix(rep(1, nrow(product_share_matrix)))) %*% product_share_matrix) )
    )
  
  return( 
    list(
      demand = demand_per_product, 
      matrix = cross_product_price_derivative_matrix
      )
  )
  
  }

tic("Demand estimation")
generate_cross_product_derivative_and_demand(model, retailer_price_list_test, conditionals)$demand
toc()

## FINDING THE COSTS

determine_costs = function(model, retailer_price_list, df_hhid_conditional = NULL){
  
  # On oublie les lignes et colonnes concernant le non-achat
  # cross_product_price_derivative_matrix = generate_cross_product_price_derivative_matrix(model, df_retailer_price)
  # cross_product_price_derivative_matrix = cross_product_price_derivative_matrix_test
  # demand = compute_demand(model, retailer_price_list)
  
  demand_and_matrix = generate_cross_product_derivative_and_demand(model, retailer_price_list, df_hhid_conditional)
  demand = demand_and_matrix$demand
  cross_product_price_derivative_matrix = demand_and_matrix$matrix
  
  relevant_product_number_list = df_product_with_nosale%>%
    filter(marque != "nosale")%>%
    arrange(retailer)%>%
    .$product_number
  
  relevant_retailer_price = retailer_price_list[relevant_product_number_list]
  relevant_demand = demand %>% .[relevant_product_number_list]
  
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
    mutate_all(as.numeric)%>%
    as.matrix()%>%
    Matrix(sparse = TRUE)
  
  omega = belonging_matrix * relevant_cross_product_price_derivative_matrix   
  
  product_weight = as.numeric(df_product_with_nosale$valqvol)
  
  # On resout le systeme
  # Omega (P-C) + D = 0 
  # C = P + Omega^-1 D
  absolute_margin = solve(omega, relevant_demand) %>% as.vector()
  cost_list = relevant_retailer_price + absolute_margin
  #  product_weight[relevant_product_number_list] + 

  
  return(cost_list)
}

# cost_list = determine_costs(model, retailer_price_list)
# saveRDS(cost_list, "Inputs/cost_list.rds")
# cost_list = readRDS("Inputs/cost_list.rds")
extended_cost_list = df_product_with_nosale%>%
  filter(marque != "nosale")%>%
  mutate(cost = cost_list)%>%
  right_join(df_product_with_nosale)%>%
  arrange(product_number)%>%
  .$cost
  

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


database_original = generate_database(retailer_price_list)


for (retailer_considered in retailer_list){
  
  # retailer_considered = "43_4"
  
  retailer_specific_shopping_trip_list = consumption%>%
    select(X, periode, hhid)%>%
    right_join(
      df_hhid_retailer%>% filter(retailer == retailer_considered)
    )%>% 
    .$X
  
  retailer_specific_product_list = df_product_with_nosale%>%
    filter(retailer == retailer_considered)%>%
    select(product_number, retailer)%>%
    .$product_number
  
  retailer_specific_database = database_original%>%
    filter(X %in% retailer_specific_shopping_trip_list)
  
  retailer_specific_price_list_init = retailer_price_list[retailer_specific_product_list]
  
  retailer_specific_profit = function(retailer_specific_price_list){
    
    # retailer_specific_price_list = retailer_specific_price_list_init
    
    for (i in 1:length(retailer_specific_price_list)){
      database <<- database_original%>%
        mutate_at(vars(paste0(retailer_specific_product_list[i], "_price")), ~retailer_specific_price_list[i])
    }
      
    apollo_inputs <<- apollo_validateInputs()
      
    forecast = apollo_prediction(
        model,
        apollo_probabilities,
        apollo_inputs,
        prediction_settings
      )
    
    profit = forecast%>%
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
        product_number = as.integer(gsub("alt", "", product_number)),
      )%>% 
      arrange(product_number)%>% 
      filter(
        product_number %in% retailer_specific_product_list
        )%>%
      left_join(
        df_product_with_nosale%>%
          select(product_number, valqvol)%>%
          mutate(valqvol = as.integer(as.character(valqvol)))
        )%>%
      mutate(
        price = retailer_specific_price_list,
        cost = extended_cost_list[retailer_specific_product_list],
        sales_profit = (price-cost)*aggregate_sales*valqvol
        )%>%
      filter(price != 0)%>%
      .$sales_profit%>%
      sum(.)
      
    return(as.numeric(profit))
    }
    
  retailer_specific_profit(retailer_specific_price_list_init)
  
  start_time = Sys.time()
  
  control_parameters = list(
    trace = TRUE
  )
  
  optim_result = optim(
    # lower = rep(0.05, length(retailer_specific_price_list_init)),
    # upper = rep(1, length(retailer_specific_price_list_init)),
    # method = "L-BFGS-B",
    par = retailer_specific_price_list_init, 
    fn = retailer_specific_profit, 
    control = control_parameters
  )
  
  end_time = Sys.time()
  
  print(paste("Optimization duration for retailer", retailer_considered))
  print(end_time-start_time)
  # Pour retailer = 43_4, 24 minutes, 511 appel a la fonction
  # Tout ?a pour juste v?rifier qu'on est ? l'?quilibre...
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



