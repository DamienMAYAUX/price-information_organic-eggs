
if (Sys.info()["sysname"] == "Windows"){
  setwd("U:/price-information_organic-eggs")
} else {
  setwd("~/U/price-information_organic-eggs")
}

source("Scripts/0_Packages_Libraries.R")


## Parameter for all models
nb_halton_draw = 200
nb_core = 20

apollo_initialise()

base_beta = c(
  "b_medium" = 0,
  "b_low" = 0,
  "b_indep" = 0,
  "b_high" = 0,
  "b_other" = 0,
  "b_labelbio" = 0,
  "b_labelpleinair" = 0,
  "b_nolabel" = 0,
  "b_calibreL" = 0,
  "b_calibreM" = 0,
  "b_valqvol" = 0
)

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


#### ADVICE ON MODEL BUILDING

## - UNCOMMENT THE MODEL YOU WANT
## - USE THE APPROPRIATE DATABASE PREPARATION
## - CHECH THE SECTION ON RANDOM COEFFICIENT, PICK THE APPROPRIATE DISTRIBUTION
## - CHECK THE SECTION ON LATENT CLASS
## - IF RANDOM COEFF, CHECK AVGINTERDRAW AT THE END OF APOLLO_PROB


#### MODEL SUMMARY

### There are 4 models
## 0. MNL 
## 1. MNL with log-normal random coefficient
## 2. MNL with latent class
## 3. MNL with log_normal random coefficient and latent class 

### Each model exists in 4 versions :
## a) With nosales, without control
## b) Without nosales, without control
## c) With nosales, with control
## d) Without nosales, with control

## TU USE A MODEL WITHOUT NOSALE b) OR d), LOAD THE APPROPRIATE DATABASE !



#### DATABASE PREPARATION

## WITH NO SALE

database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo_20211129.rds"))

df_product = as.data.frame(readRDS("Inputs/product_with_nosale_20211129.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)

# # Information on household characteristics is useful for latent class models
# 
# household = readRDS("Inputs/household.rds")
# 
# household_wider = household%>%
#   select(
#     hhid, cycle, habi_inra, min_age, rve_num
#   )%>%
#   mutate(min_age2 = min_age*min_age)%>%
#   mutate_at(vars(cycle, habi_inra), ~as.numeric(as.factor(.)))%>%
#   # One column per possible value for cycle
#   pivot_longer(cols = c("cycle"))%>%
#   select(-name)%>%
#   pivot_wider(names_from = "value", values_from = "value", names_prefix = "cycle_")%>%
#   mutate(across(contains("cycle"), ~as.integer(!is.na(.))))%>%
#   # One column per possible value for habi_inra
#   pivot_longer(cols = c("habi_inra"))%>%
#   select(-name)%>%
#   pivot_wider(names_from = "value", values_from = "value", names_prefix = "habi_inra_")%>%
#   mutate(across(contains("habi_inra"), ~as.integer(!is.na(.))))


# All the information required to compute the probabilities must appear 
# in the dataframe called "database"
# The following trick inserts space-efficiently the content of df_product
# in this database by repeating several time the information.
# From one repetition to another, information is separated by a NA
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
      )
  )%>%
  mutate_at(vars(marque_simple, label, calibre), as.factor)
  # %>%
  # left_join(household_wider)



#############################################################################################
########################################### MODEL 0 #########################################
#############################################################################################

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
#   base_beta,
#   "b_nosale" = 0,
#   "b_price" = 0
# )
# 
# apollo_fixed = c("b_nosale", "b_nolabel", "b_calibreM", "b_other")
# 
# apollo_inputs=apollo_validateInputs()
# 
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
# 
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
# 
#   P = list()
# 
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
# 
# 
# model = apollo_estimate(
#   apollo_beta,
#   apollo_fixed,
#   apollo_probabilities,
#   apollo_inputs,
#   estimate_settings
# )
# setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs/Inputs/Apollo")
# apollo_saveOutput(model)
# setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")





#### MODEL 0c : WITH NOSALE, WITH CONTROL 

apollo_control = list(
  mixing = FALSE,
  modelName = "nosale_with_control",
  modelDescr = "A usual MNL without random coefficient. Not buying is always possible. A control function is included",
  indivID = "hhid",
  workInLogs = FALSE,
  nCores = nb_core
)

apollo_beta = c(
  base_beta,
  "b_nosale" = 0,
  "b_price" = -5,
  "b_control" = 0
)

apollo_fixed = c("b_nosale", "b_nolabel", "b_calibreM")

apollo_inputs=apollo_validateInputs()


apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){

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
    b_price * get(paste0(j, "_price")) +
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

model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs/Inputs/Apollo")
apollo_saveOutput(model)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")





#############################################################################################
########################################### MODEL 1 #########################################
#############################################################################################


#### MODEL 1a

# apollo_control = list(
#   mixing = TRUE,
#   modelName = "nosale_lnorm",
#   modelDescr = "",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# 
# apollo_beta = c(
#   "b_nosale" = 0,
#   base_beta,
#   "mu_lnorm" = -3,
#   "sigma_lnorm" = 0
# )
# 
# apollo_fixed = c("b_nosale", "b_medium", "b_nolabel", "b_calibreM")
# 
# apollo_draws = list(
#   interNormDraws = c("draws_norm_lognorm"),
#   interDrawsType = "halton",
#   interNDraws = nb_halton_draw
# )
# 
# apollo_randCoeff = function (apollo_beta , apollo_inputs) {
#   randcoeff = list()
#   randcoeff[["b_price"]] = - exp(mu_lnorm + sigma_lnorm*draws_norm_lognorm)
#   return (randcoeff)
# }
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
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
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
#   P = apollo_avgInterDraws(P, apollo_inputs , functionality)
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }
# 
# 
# model = apollo_estimate(
#   apollo_beta,
#   apollo_fixed,
#   apollo_probabilities,
#   apollo_inputs,
#   estimate_settings
# )
# setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs/Inputs/Apollo")
# apollo_saveOutput(model)
# setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")





#### MODEL 1c

apollo_control = list(
  mixing = TRUE,
  modelName = "nosale_lnorm_with_control",
  modelDescr = "",
  indivID = "hhid",
  workInLogs = FALSE,
  nCores = nb_core
)

apollo_beta = c(
  base_beta,
  "b_nosale" = 0,
  "mu_lnorm" = -3,
  "sigma_lnorm" = 0,
  "b_control" = 0
)

apollo_fixed = c("b_nosale", "b_medium", "b_nolabel", "b_calibreM")

apollo_draws = list(
  interNormDraws = c("draws_norm_lognorm"),
  interDrawsType = "halton",
  interNDraws = nb_halton_draw
)

apollo_randCoeff = function (apollo_beta , apollo_inputs) {
  randcoeff = list()
  randcoeff[["b_price"]] = - exp(mu_lnorm + sigma_lnorm*draws_norm_lognorm)
  return (randcoeff)
}


apollo_inputs=apollo_validateInputs()


apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){

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
    b_price * get(paste0(j, "_price")) +
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
  P = apollo_avgInterDraws(P, apollo_inputs , functionality)
  P = apollo_prepareProb(P, apollo_inputs , functionality)
  return(P)
}


model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs/Inputs/Apollo")
apollo_saveOutput(model)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")


fitsTest_settings = list(
  subsamples = list(
    "high_etud" = database$max_etud >= 5,
    "low_etud" = database$max_etud <=3,
    "high or middle high income" = database$max_etud %in% c("HIGH INCOME", "MIDDLE HIGH INCOME"),
    "low or middle low income" = database$max_etud %in% c("HIGH INCOME", "MIDDLE LOW INCOME")
  )
)
apollo_fitsTest(model,apollo_probabilities,apollo_inputs,fitsTest_settings)




#######################################################################################
###################### MODEL 2 : LATENT CLASS, CONTROL, NOSALE,  ######################
#######################################################################################


apollo_control = list(
  mixing = FALSE,
  modelName = "latent_control_nosale",
  modelDescr = "",
  indivID = "hhid",
  workInLogs = TRUE,
  nCores = nb_core
)

apollo_beta = c(
  base_beta,
  "b_nosale" = 0,
  "b_price1" = 1,
  "b_price2" = 3,
  "b_control" = 0,
  "b_class" = 0.5
)

apollo_fixed = c("b_nosale", "b_nolabel", "b_calibreM")

# Latent class section

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["b_price"]] = list(b_price1, b_price2)
  
  V=list()
  V[["Class_1"]]= 0
  V[["Class_2"]]= b_class

  mnl_settings=list(
    alternatives=c("Class_1"=1,"Class_2"=2),
    avail=1, choiceVar=NA, V=V
    )

  lcpars[["pi_values"]]= apollo_mnl(mnl_settings,functionality="raw")
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  

  return(lcpars)
}

apollo_inputs=apollo_validateInputs()


# Probability function section

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){

  apollo_attach( apollo_beta, apollo_inputs)
  on.exit( apollo_detach( apollo_beta, apollo_inputs) )

  P = list()

  good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
  good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
  good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
  residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
  J = length(good_label)- length(residual_label) - 1

  vect_names = paste0("alt",as.character(1:J))

  mnl_settings = list(
    alternatives = setNames(1:J, vect_names),
    avail = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], vect_names),
    choiceVar = choice
  )

  ### Loop over classes
  for (s in 1:length(pi_values)){

    V = list()

    ### Compute class−specific utilities
    for(j in 1:J){
      V[[paste0("alt",j)]] =
        get( paste0("b_", good_label[j])) +
        get( paste0("b_", good_marque_simple[j])) +
        get( paste0("b_", good_calibre[j])) +
        b_price[[s]] * get(paste0(j, "_price")) +
        b_control * get(paste0(j, "_control")) +
        b_valqvol * get(paste0(j, "_valqvol"))
    }

    mnl_settings$V=V
    mnl_settings$componentName = paste0("Class_",s)

    ### Compute within−class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings,functionality)

    ### Take product across observation for same individual
    P[[paste0("Class_",s)]]=apollo_panelProd(P[[paste0("Class_",s)]],apollo_inputs,functionality)
  }

  ### Compute latent class model probabilities
  lc_settings=list(inClassProb=P,classProb=pi_values)
  P[["model"]]=apollo_lc(lc_settings,apollo_inputs,functionality)

  ### Prepare and return outputs of function
  P=apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}


model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs/Inputs/Apollo")
apollo_saveOutput(model)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")



#########################################################################
####################### PART WITHOUT NOSALE #############################
#########################################################################

#### DATABASE PREPARATION

## WITHOUT NO SALE

database_before_merge = as.data.frame(readRDS("choice_situation_without_nosale_for_apollo_testing.rds"))

df_product = as.data.frame(readRDS("product_without_nosale.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)

household = readRDS("Inputs/household.rds")

household_wider = household%>%
  select(
    hhid, cycle, habi_inra, max_etud, rve_num
  )%>%
  mutate_at(vars(cycle, habi_inra), ~as.numeric(as.factor(.)))%>%
  # One column per possible value for cycle
  pivot_longer(cols = c("cycle"))%>%
  select(-name)%>%
  pivot_wider(names_from = "value", values_from = "value", names_prefix = "cycle_")%>%
  mutate(across(contains("cycle"), ~as.integer(!is.na(.))))%>%
  # One column per possible value for habi_inra
  pivot_longer(cols = c("habi_inra"))%>%
  select(-name)%>%
  pivot_wider(names_from = "value", values_from = "value", names_prefix = "habi_inra_")%>%
  mutate(across(contains("habi_inra"), ~as.integer(!is.na(.))))


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
  mutate_at(vars(marque_simple, label, calibre), as.factor)%>%
  left_join(household_wider)




################################################################
##################### MODEL 0 : BASIC MNL ######################
################################################################


#### MODEL 0b : WITHOUT NOSALE 

apollo_control = list(
  mixing = FALSE,
  modelName = "without_nosale",
  modelDescr = "A usual MNL without random coefficient. Not buying is impossible.",
  indivID = "hhid",
  workInLogs = FALSE,
  nCores = nb_core
)

apollo_beta = c(
  base_beta,
  "b_price" = -5
)

apollo_fixed = c("b_medium", "b_nolabel", "b_calibreM")

apollo_inputs=apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
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
    b_price * get(paste0(j, "_price")) +
    #b_control * get(paste0(j, "_control")) +
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

model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs/Inputs/Apollo")
apollo_saveOutput(model)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")




#### MODEL 0d : WITHOUT NOSALE, CONTROL

apollo_control = list(
  mixing = FALSE,
  modelName = "without_nosale_with_control",
  modelDescr = "A usual MNL without random coefficient. Not buying is impossible. A control function is included.",
  indivID = "hhid",
  workInLogs = FALSE,
  nCores = nb_core
)

apollo_beta = c(
  base_beta,
  "b_price" = -5
)

apollo_fixed = c("b_medium", "b_nolabel", "b_calibreM")

apollo_inputs=apollo_validateInputs()


apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
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
    b_price * get(paste0(j, "_price")) +
    #b_control * get(paste0(j, "_control")) +
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


model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs/Inputs/Apollo")
apollo_saveOutput(model)
setwd("C:/Users/d.mayaux/Documents/GitHub/price-information_organic-eggs")





