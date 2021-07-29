
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")
setwd("E:/Mémoire/Code/price-information_organic-eggs/Inputs")


## Parameter for all models
nb_halton_draw = 3
nb_core = 3

apollo_initialise()


#### ADVICE ON MODEL BUILDING

## 1. UNCOMMENT THE MODEL YOU WANT
## 2. USE THE APPROPRIATE DATABASE PREPARATION
## 3. CHECH THE SECTION ON RANDOM COEFFICIENT, PICK THE APPROPRIATE DISTRIBUTION
## 4. CHECK THE SECTION ON LATENT CLASS
## 5. IF RANDOM COEFF, CHECK AVGINTERDRAW AT THE END OF APOLLO_PROB


#### DATABASE PREPARATION

## WITH NO SALE

database_before_merge = as.data.frame(readRDS("choice_situation_with_nosale_for_apollo_testing.rds"))

df_product = as.data.frame(readRDS("product_with_nosale.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)

## WITHOUT NO SALE

# database_before_merge = as.data.frame(readRDS("choice_situation_without_nosale_for_apollo_testing.rds"))
# 
# df_product = as.data.frame(readRDS("product_without_nosale.rds"))%>%
#   select(marque_simple, calibre, label)%>%
#   mutate_all(as.character)



# All the information required to compute the probabilities must appear 
# in the dataframe called "database"
# The following trick inserts space-efficiently the content of df_product
# in this database by repeating several time the information.
# From one repetition to another, informaiton is separated by a NA
# THerefore, any core given a number of line higher than twice the number of products
# can recover the full information from the database

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
  mutate_at(vars(marque_simple, label, calibre), as.factor)


#### MODEL 0a : NOSALE 

# apollo_control = list(
#   mixing = FALSE,
#   modelName = "nosale",
#   modelDescr = "A usual MNL without random coefficient. Not buying is always possible.",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# 
# apollo_beta = c(
#   "b_nosale" = 0,
#   "b_medium" = 0,
#   "b_low" = 0,
#   "b_indep" = 0,
#   "b_high" = 0,
#   "b_other" = 0,
#   "b_labelbio" = 0,
#   "b_labelpleinair" = 0,
#   "b_nolabel" = 0,
#   "b_calibreL" = 0,
#   "b_calibreM" = 0,
#   "b_valqvol" = 0,
#   "b_price" = -5
# )
# 
# apollo_fixed = c("b_nosale", "b_medium", "b_nolabel", "b_calibreM")
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
# 
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
# 
#   P = list()
# 
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
# 
#   # What follows is based on the same hack as previously, applied to good_label.
#   # Hence, residual_label should be as long as good_label minus (the number of products plus 1 for the NA)
#   residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
#   J = length(good_label)- length(residual_label) - 1
# 
#   V = list()
#   for(j in 1:J) V[[paste0("alt",j)]] =
#     get( paste0("b_", good_label[j])) +
#     get( paste0("b_", good_marque_simple[j])) +
#     get( paste0("b_", good_calibre[j])) +
#     b_price * get(paste0(j, "_price")) +
#     #b_control * get(paste0(j, "_control")) +
#     b_valqvol * get(paste0(j, "_valqvol"))
# 
#   mnl_settings = list(
#     alternatives  = setNames(1:J, names(V)),
#     avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
#     choiceVar     = choice,
#     V             = V
#   )
# 
#   P[["model"]] = apollo_mnl(mnl_settings , functionality)
#   P = apollo_panelProd(P, apollo_inputs , functionality)
# 
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }

#### MODEL 0b : WITHOUT NOSALE 

# apollo_control = list(
#   mixing = FALSE,
#   modelName = "without_nosale",
#   modelDescr = "A usual MNL without random coefficient. Not buying is impossible.",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# 
# apollo_beta = c(
#   "b_medium" = 0,
#   "b_low" = 0,
#   "b_indep" = 0,
#   "b_high" = 0,
#   "b_other" = 0,
#   "b_labelbio" = 0,
#   "b_labelpleinair" = 0,
#   "b_nolabel" = 0,
#   "b_calibreL" = 0,
#   "b_calibreM" = 0,
#   "b_valqvol" = 0,
#   "b_price" = -5
# )
# 
# apollo_fixed = c("b_medium", "b_nolabel", "b_calibreM")
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
# 
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
# 
#   P = list()
# 
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
# 
#   # What follows is based on the same hack as previously, applied to good_label.
#   # Hence, residual_label should be as long as good_label minus (the number of products plus 1 for the NA)
#   residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
#   J = length(good_label)- length(residual_label) - 1
# 
#   V = list()
#   for(j in 1:J) V[[paste0("alt",j)]] =
#     get( paste0("b_", good_label[j])) +
#     get( paste0("b_", good_marque_simple[j])) +
#     get( paste0("b_", good_calibre[j])) +
#     b_price * get(paste0(j, "_price")) +
#     #b_control * get(paste0(j, "_control")) +
#     b_valqvol * get(paste0(j, "_valqvol"))
# 
#   mnl_settings = list(
#     alternatives  = setNames(1:J, names(V)),
#     avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
#     choiceVar     = choice,
#     V             = V
#   )
# 
#   P[["model"]] = apollo_mnl(mnl_settings , functionality)
#   P = apollo_panelProd(P, apollo_inputs , functionality)
# 
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }


#### MODEL 0c : WITH NOSALE, WITH CONTROL 

# apollo_control = list(
#   mixing = FALSE,
#   modelName = "nosale_with_control",
#   modelDescr = "A usual MNL without random coefficient. Not buying is always possible. A control function is included",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# 
# apollo_beta = c(
#   "b_nosale" = 0,
#   "b_medium" = 0,
#   "b_low" = 0,
#   "b_indep" = 0,
#   "b_high" = 0,
#   "b_other" = 0,
#   "b_labelbio" = 0,
#   "b_labelpleinair" = 0,
#   "b_nolabel" = 0,
#   "b_calibreL" = 0,
#   "b_calibreM" = 0,
#   "b_valqvol" = 0,
#   "b_price" = -5,
#   "b_control" = 0
# )
# 
# apollo_fixed = c("b_nosale", "b_medium", "b_nolabel", "b_calibreM")
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
# 
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
# 
#   P = list()
# 
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
# 
#   # What follows is based on the same hack as previously, applied to good_label.
#   # Hence, residual_label should be as long as good_label minus (the number of products plus 1 for the NA)
#   residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
#   J = length(good_label)- length(residual_label) - 1
# 
#   V = list()
#   for(j in 1:J) V[[paste0("alt",j)]] =
#     get( paste0("b_", good_label[j])) +
#     get( paste0("b_", good_marque_simple[j])) +
#     get( paste0("b_", good_calibre[j])) +
#     b_price * get(paste0(j, "_price")) +
#     b_control * get(paste0(j, "_control")) +
#     b_valqvol * get(paste0(j, "_valqvol"))
# 
#   mnl_settings = list(
#     alternatives  = setNames(1:J, names(V)),
#     avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
#     choiceVar     = choice,
#     V             = V
#   )
# 
#   P[["model"]] = apollo_mnl(mnl_settings , functionality)
#   P = apollo_panelProd(P, apollo_inputs , functionality)
# 
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }

#### MODEL 0d : WITHOUT NOSALE, CONTROL

# apollo_control = list(
#   mixing = FALSE,
#   modelName = "without_nosale_with_control",
#   modelDescr = "A usual MNL without random coefficient. Not buying is impossible. A control function is included.",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# 
# apollo_beta = c(
#   "b_medium" = 0,
#   "b_low" = 0,
#   "b_indep" = 0,
#   "b_high" = 0,
#   "b_other" = 0,
#   "b_labelbio" = 0,
#   "b_labelpleinair" = 0,
#   "b_nolabel" = 0,
#   "b_calibreL" = 0,
#   "b_calibreM" = 0,
#   "b_valqvol" = 0,
#   "b_price" = -5
# )
# 
# apollo_fixed = c("b_medium", "b_nolabel", "b_calibreM")
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
# 
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
# 
#   P = list()
# 
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
# 
#   # What follows is based on the same hack as previously, applied to good_label.
#   # Hence, residual_label should be as long as good_label minus (the number of products plus 1 for the NA)
#   residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
#   J = length(good_label)- length(residual_label) - 1
# 
#   V = list()
#   for(j in 1:J) V[[paste0("alt",j)]] =
#     get( paste0("b_", good_label[j])) +
#     get( paste0("b_", good_marque_simple[j])) +
#     get( paste0("b_", good_calibre[j])) +
#     b_price * get(paste0(j, "_price")) +
#     #b_control * get(paste0(j, "_control")) +
#     b_valqvol * get(paste0(j, "_valqvol"))
# 
#   mnl_settings = list(
#     alternatives  = setNames(1:J, names(V)),
#     avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
#     choiceVar     = choice,
#     V             = V
#   )
# 
#   P[["model"]] = apollo_mnl(mnl_settings , functionality)
#   P = apollo_panelProd(P, apollo_inputs , functionality)
# 
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }





#### MODEL 1 : LNORM, NOSALE 

# apollo_control = list(
#   mixing = TRUE,
#   modelName = "lnorm_nosale",
#   modelDescr = "",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# 
# apollo_beta = c(
#   "b_nosale" = 0,
#   "b_medium" = 0,
#   "b_low" = 0,
#   "b_indep" = 0,
#   "b_high" = 0,
#   "b_other" = 0,
#   "b_labelbio" = 0,
#   "b_labelpleinair" = 0,
#   "b_nolabel" = 0,
#   "b_calibreL" = 0,
#   "b_calibreM" = 0,
#   "b_valqvol" = 0,
#   "mu_lnorm" = 0,
#   "sigma_lnorm" = 0
# )
# 
# apollo_fixed = c("b_nosale")
# 
# apollo_draws = list(
#   #interUnifDraws = c("draws_unif_exp"),
#   interNormDraws = c("draws_norm_lognorm"),
#   interDrawsType = "halton",
#   interNDraws = nb_halton_draw
# )
# 
# apollo_randCoeff = function (apollo_beta , apollo_inputs) {
#   randcoeff = list()
#   #randcoeff[["b_price"]] = - log(draws_unif_exp)
#   randcoeff[["b_price"]] = - exp(mu_lnorm + sigma_lnorm*draws_norm_lognorm)
#   return (randcoeff)
# }
# 
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
# 
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
# 
#   P = list()
# 
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
# 
#   # ALLER CHERCHER LE 2e NA POUR CALCULER CA
#   # THis is the same hack as previously, applied to good_label.
#   # Hence, it should be as long as good_label minus the number of products minus the NA
#   residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
#   J = length(good_label)- length(residual_label) - 1
# 
#   V = list()
#   for(j in 1:J) V[[paste0("alt",j)]] =
#     get( paste0("b_", good_label[j])) +
#     get( paste0("b_", good_marque_simple[j])) +
#     get( paste0("b_", good_calibre[j])) +
#     b_price * get(paste0(j, "_price")) +
#     #b_control * get(paste0(j, "_control")) +
#     b_valqvol * get(paste0(j, "_valqvol"))
# 
#   mnl_settings = list(
#     alternatives  = setNames(1:J, names(V)),
#     avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
#     choiceVar     = choice,
#     V             = V
#   )
# 
#   P[["model"]] = apollo_mnl(mnl_settings , functionality)
#   P = apollo_panelProd(P, apollo_inputs , functionality)
# 
#   ### TO COMMENT WHEN USING MODELS WITHOUT RANDOM COEFF
#   P = apollo_avgInterDraws(P, apollo_inputs , functionality)
# 
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }


#### MODEL 1bis : LNORM, WITHOUT NOSALE 


# apollo_control = list(
#   mixing = TRUE,
#   modelName = "lnorm_nosale",
#   modelDescr = "",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# 
# apollo_beta = c(
#   "b_medium" = 0,
#   "b_low" = 0,
#   "b_indep" = 0,
#   "b_high" = 0,
#   "b_other" = 0,
#   "b_labelbio" = 0,
#   "b_labelpleinair" = 0,
#   "b_nolabel" = 0,
#   "b_calibreL" = 0,
#   "b_calibreM" = 0,
#   "b_valqvol" = 0,
#   "mu_lnorm" = 0,
#   "sigma_lnorm" = 0
# )
# 
# apollo_fixed = c("b_medium", "b_nolabel", "b_calibreM")
# 
# apollo_draws = list(
#   #interUnifDraws = c("draws_unif_exp"),
#   interNormDraws = c("draws_norm_lognorm"),
#   interDrawsType = "halton",
#   interNDraws = nb_halton_draw
# )
# 
# apollo_randCoeff = function (apollo_beta , apollo_inputs) {
#   randcoeff = list()
#   #randcoeff[["b_price"]] = - log(draws_unif_exp)
#   randcoeff[["b_price"]] = - exp(mu_lnorm + sigma_lnorm*draws_norm_lognorm)
#   return (randcoeff)
# }
# 
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
#   
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
#   
#   P = list()
#   
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
#   
#   # ALLER CHERCHER LE 2e NA POUR CALCULER CA
#   # THis is the same hack as previously, applied to good_label.
#   # Hence, it should be as long as good_label minus the number of products minus the NA
#   residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
#   J = length(good_label)- length(residual_label) - 1
#   
#   V = list()
#   for(j in 1:J) V[[paste0("alt",j)]] =
#     get( paste0("b_", good_label[j])) +
#     get( paste0("b_", good_marque_simple[j])) +
#     get( paste0("b_", good_calibre[j])) +
#     b_price * get(paste0(j, "_price")) +
#     #b_control * get(paste0(j, "_control")) +
#     b_valqvol * get(paste0(j, "_valqvol"))
#   
#   mnl_settings = list(
#     alternatives  = setNames(1:J, names(V)),
#     avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
#     choiceVar     = choice,
#     V             = V
#   )
#   
#   P[["model"]] = apollo_mnl(mnl_settings , functionality)
#   P = apollo_panelProd(P, apollo_inputs , functionality)
#   
#   ### TO COMMENT WHEN USING MODELS WITHOUT RANDOM COEFF
#   P = apollo_avgInterDraws(P, apollo_inputs , functionality)
#   
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }


###################### MODEL 2 : LATENT CLASS, CONTROL, NOSALE,  ######################


# apollo_control = list(
#   mixing = FALSE,
#   modelName = "latent_control_nosale",
#   modelDescr = "",
#   indivID = "hhid",
#   workInLogs = TRUE,
#   nCores = 3
# )
# 
# apollo_beta = c(
#   "b_medium" = 0,
#   "b_low" = 0,
#   "b_indep" = 0,
#   "b_high" = 0,
#   "b_other" = 0,
#   "b_labelbio" = 0,
#   "b_labelpleinair" = 0,
#   "b_nolabel" = 0,
#   "b_label_other" = 0,
#   "b_calibreL" = 0,
#   "b_calibreM" = 0,
#   "b_calibre_other" = 0,
#   "b_valqvol" = 0,
#   "b_price1" = 0,
#   "b_price2" = 0,
#   "b_control" = 0 #, "mu_lnorm" = 0, "sigma_lnorm" = 0,
# )
# 
# apollo_fixed = c("b_other", "b_label_other", "b_calibre_other")
# 
# 
# # Latent class section
# 
# apollo_lcPars=function(apollo_beta, apollo_inputs){
#   lcpars = list()
#   lcpars[["b_price"]] = list(b_price1, b_price2)
# 
#   V=list()
#   # On peut introduire ici des parametres sur les caracteristiques sociologiques du menage
#   V[["class1"]] = 0
#   V[["class2"]] = 0
# 
#   mnl_settings = list(
#     alternatives = c(class1=1, class1=2),
#     avail        = 1,
#     choiceVar    = NA,
#     V            = V
#   )
#   lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
# 
#   lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
# 
#   return(lcpars)
# }
# 
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# # Probability function section
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
#   
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
#   
#   P = list()
#   
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
#   
#   # ALLER CHERCHER LE 2e NA POUR CALCULER CA
#   J = 145
#   vect_names = paste0("alt",as.character(1:J))
#   
#   mnl_settings = list(
#     alternatives = setNames(1:J, vect_names),
#     avail = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], vect_names),
#     choiceVar = choice
#   )
#   
#   ### Loop over classes
#   for (s in 1:length(pi_values)){
#     
#     V = list()
# 
#     ### Compute class−specific utilities
#     for(j in 1:J){
#       V[[paste0("alt",j)]] =
#         get( paste0("b_", good_label[j])) +
#         get( paste0("b_", good_marque_simple[j])) +
#         get( paste0("b_", good_calibre[j])) +
#         b_price[[s]] * get(paste0(j, "_price")) +
#         b_control * get(paste0(j, "_control")) +
#         b_valqvol * get(paste0(j, "_valqvol"))
#     } 
# 
#     mnl_settings$V=V
#     mnl_settings$componentName = paste0("Class_",s)
#   
#     ### Compute within−class choice probabilities using MNL model
#     P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings,functionality)
#   
#     ### Take product across observation for same individual
#     P[[paste0("Class_",s)]]=apollo_panelProd(P[[paste0("Class_",s)]],apollo_inputs,functionality)
#   }
# 
#   ### Compute latent class model probabilities
#   lc_settings=list(inClassProb=P,classProb=pi_values)
#   P[["model"]]=apollo_lc(lc_settings,apollo_inputs,functionality)
#   
#   ### TO COMMENT WHEN USING MODELS WITHOUT RANDOM COEFF
#   # P = apollo_avgInterDraws(P, apollo_inputs , functionality)
#   
#   ###Prepare and return outputs of function
#   P=apollo_prepareProb(P, apollo_inputs, functionality)
#   
#   return(P)
# }


###################### MODEL 3 : LNORM, LATENT CLASS, CONTROL, NOSALE,  ######################


# apollo_control = list(
#   mixing = TRUE,
#   modelName = "lnorm_latent_control_nosale",
#   modelDescr = "",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = 3
# )
# 
# apollo_beta = c(
#   "b_medium" = 1.1921,
#   "b_low" = 0.7424,
#   "b_indep" = -0.1308,
#   "b_high" = 1.4504,
#   "b_other" = 0.0000,
#   "b_labelbio" = 1.8698,
#   "b_labelpleinair" = 1.0426,
#   "b_nolabel" = 0.5236,
#   "b_label_other" = 0.0000,
#   "b_calibreL" = 1.6436,
#   "b_calibreM" = 1.7301,
#   "b_calibre_other" = 0.0000,
#   "b_control" = 0.0000,
#   "b_valqvol" = -0.0677,
#   "mu_lnorm1" = -2.8257,
#   "sigma_lnorm1" = -0.6142,
#   "mu_lnorm2" = -2.8257,
#   "sigma_lnorm2" = -0.6142
# )
# 
# apollo_fixed = c("b_other", "b_label_other", "b_calibre_other")
# 
# 
# # Random coefficient section
# 
# apollo_draws = list(
#   #interUnifDraws = c("draws_unif_exp"),
#   interNormDraws = c("draws_norm_lognorm"),
#   interDrawsType = "halton",
#   interNDraws = 3
# )
# 
# apollo_randCoeff = function (apollo_beta , apollo_inputs) {
#   randcoeff = list()
#   #randcoeff[["b_price"]] = - log(draws_unif_exp)
#   randcoeff[["b_price1"]] = - exp(mu_lnorm1 + sigma_lnorm1*draws_norm_lognorm)
#   randcoeff[["b_price2"]] = - exp(mu_lnorm2 + sigma_lnorm2*draws_norm_lognorm)
#   return (randcoeff)
# }
# 
# 
# # Latent class section
# 
# apollo_lcPars=function(apollo_beta, apollo_inputs){
#   lcpars = list()
#   lcpars[["mu_lnorm"]] = list(mu_lnorm1, mu_lnorm2)
#   lcpars[["sigma_lnorm"]] = list(sigma_lnorm1, sigma_lnorm2)
#   lcpars[["b_price"]] = list(b_price1, b_price2)
#   
#   V=list()
#   # On peut introduire ici des parametres sur les caracteristiques sociologiques du menage
#   V[["class1"]] = 0
#   V[["class2"]] = 0
#   
#   mnl_settings = list(
#     alternatives = c(class1=1, class1=2),
#     avail        = 1,
#     choiceVar    = NA,
#     V            = V
#   )
#   lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
#   lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
#   
#   return(lcpars)
# }
# 
# 
# apollo_inputs=apollo_validateInputs()
# 
# 
# # Probability function section
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
#   
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
#   
#   P = list()
#   
#   # This is a hack
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   # I had to store df_product as columns in database.
#   # If df_product is written just once and followed by NAs,
#   # there may be informaiton loss when rows are split for parallel computing
#   # Thus, I have repeated blocks of df_product followed by an empty row in database last columns
#   # The hack recovers the relevant values, namely the 145 after the first NA has appeared
#   # This trick works as long as each core receives at least 2*J = 290 rows
#   # Here is an example
#   # test = c(rep(c(2,3), 45), NA_integer_, rep(c(4,5), 32), NA_integer_, 1)
#   # (test[cumsum(is.na(test)) > 0])[2:sum(cumsum(is.na(test)) > 0)]
#   
#   # ALLER CHERCHER LE 2e NA POUR CALCULER CA
#   J = 145
#   vect_names = paste0("alt",as.character(1:J))
#   
#   mnl_settings = list(
#     alternatives = setNames(1:J, vect_names),
#     avail = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], vect_names),
#     choiceVar = choice
#   )
#   
#   ### Loop over classes
#   for (s in 1:length(pi_values)){
#     
#     V = list()
#     
#     ### Compute class−specific utilities
#     for(j in 1:J){
#       V[[paste0("alt",j)]] =
#         get( paste0("b_", good_label[j])) +
#         get( paste0("b_", good_marque_simple[j])) +
#         get( paste0("b_", good_calibre[j])) +
#         b_price[[s]] * get(paste0(j, "_price")) +
#         b_control * get(paste0(j, "_control")) +
#         b_valqvol * get(paste0(j, "_valqvol"))
#     } 
#     
#     mnl_settings$V=V
#     mnl_settings$componentName = paste0("Class_",s)
#     
#     ### Compute within−class choice probabilities using MNL model
#     P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings,functionality)
#     
#     ### Take product across observation for same individual
#     P[[paste0("Class_",s)]] = apollo_panelProd(
#       P[[paste0("Class_",s)]],
#       apollo_inputs,functionality
#       )
#   
#     ### Average across inter−individual draws within classes
#     P[[paste0("Class_",s)]] = apollo_avgInterDraws(
#       P[[paste0("Class_",s)]],
#       apollo_inputs,functionality
#       )
#     }
#   
#   ### Compute latent class model probabilities
#   lc_settings=list(inClassProb=P,classProb=pi_values)
#   P[["model"]]=apollo_lc(lc_settings,apollo_inputs,functionality)
#   
#   ### Prepare and return outputs of function
#   P=apollo_prepareProb(P, apollo_inputs, functionality)
#   
#   return(P)
# }



###################### MODEL ESTIMATION ######################

estimate_settings = list(
  #maxIterations = 30,
  #bootstrapSE = 10,
# scaling = c(
#   "b_medium" = 0.1,
#   "b_low" = 0.1,
#   "b_indep" = 0.1,
#   "b_high" = 0.1,
#   #"b_other" = 0.1,
#   "b_calibreL" = 0.1,
#   #"b_calibreM" = 0.1,
#   "b_price" = 1,
#   "b_control" = 0.001,
#   "b_valqvol" = 0.01
# )
)


# il peut etre necessaire de reexecuter le code suivant 
# avant de relancer l'algorithme
# apollo_inputs=apollo_validateInputs()

start = Sys.time()

# searchStart_settings = list(
#   bfgsIter = 10
# )
# 
# apollo_beta = apollo_searchStart(apollo_beta,
#                                  apollo_fixed,
#                                  apollo_probabilities,
#                                  apollo_inputs,
#                                  searchStart_settings)

model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
  )

end = Sys.time()
print(end-start)


setwd("E:/Mémoire/Code/price-information_organic-eggs/Inputs/Apollo")
apollo_saveOutput(model)


#######################################


# Candidate for beta
# apollo_beta = c(
#   "b_medium" = 1.1921,
#   "b_low" = 0.7424,
#   "b_indep" = -0.1308,
#   "b_high" = 1.4504,
#   "b_other" = 0.0000,
#   "b_labelbio" = 1.8698,
#   "b_labelpleinair" = 1.0426,
#   "b_nolabel" = 0.5236,
#   "b_label_other" = 0.0000,
#   "b_calibreL" = 1.6436,
#   "b_calibreM" = 1.7301,
#   "b_calibre_other" = 0.0000,
#   "b_control" = 0.0000,
#   "b_valqvol" = -0.0677,
#   "mu_lnorm" = 2.8257,
#   "sigma_lnorm" = -0.6142
# )



