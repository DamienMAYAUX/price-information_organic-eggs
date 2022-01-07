

if (Sys.info()["sysname"] == "Windows"){
  wd = "U:/price-information_organic-eggs"
} else {
  wd = "~/U/price-information_organic-eggs"
}
setwd(wd)

source("Scripts/0_Packages_Libraries.R")

tic("Demand model estimation and conditionnals")


#### ON GENERE LA BASE ####

nb_halton_draw = 100
nb_core = 8

apollo_initialise()

base_beta = c(
  "b_medium" = 0,
  "b_low" = 0,
  "b_indep" = 0,
  "b_high" = 0,
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

#### DATABASE PREPARATION

## WITH NO SALE

database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo_20211222.rds"))

df_product = as.data.frame(readRDS("Inputs/product_with_nosale_20211222.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)

household = readRDS("Inputs/household.rds")

household_wider = household%>%
  select(
    hhid, cycle, habi_inra, max_age, rve_num, max_etud
  )%>%
  mutate(
    max_age2 = max_age*max_age,
    log10_rev = log10(rve_num),
    habi_inra_index = as.numeric(as.factor(habi_inra)),
    log10_pop = case_when(
      habi_inra_index == 2 ~ 3,
      habi_inra_index == 5 ~ 3.3,
      habi_inra_index == 8 ~ 3.7,
      habi_inra_index == 3 ~ 4,
      habi_inra_index == 6 ~ 4.3,
      habi_inra_index == 9 ~ 4.7,
      habi_inra_index == 4 ~ 5,
      habi_inra_index == 7 ~ 6,
      habi_inra_index == 1 ~ 7
    )
    )%>%
  select(-habi_inra_index)%>%
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
  mutate_at(vars(marque_simple, label, calibre), as.factor)%>%
  left_join(household_wider)


#### ON ESTIME LE MODELE 1c, AVEC RANDOM COEFF ET NOSALE

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
  "mu_lnorm" = -1,
  "sigma_lnorm" = 0,
  "c_log10_rev" = 0,
  "c_log10_pop" = 0,
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
  randcoeff[["b_price"]] = - exp(
    mu_lnorm +  sigma_lnorm*draws_norm_lognorm + 
      c_log10_rev * log10_rev + c_log10_pop * log10_pop 
    )
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

tic("Model estimation")
model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
)
toc()
# Avec 100 Halton, 8 cores, 2 demographics
# 42 000s = 700 min = 11h40min

setwd(paste0(wd, "/Inputs/Apollo"))
saveOutput_settings = list(
  saveCov = TRUE,
  saveCorr = TRUE
)
apollo_saveOutput(model, saveOutput_settings)
setwd(wd)

#### MATRIX ####

parameter_standard_error = read_csv("Inputs/Apollo/nosale_lnorm_with_control_covar.csv")%>%
  column_to_rownames("...1")%>%
  as.matrix()%>%
  diag()%>%
  sqrt()


df_estimate_confidence = data.frame(
  estimate = model$estimate[model$estimate!=0],
  standard_error = parameter_standard_error
  )%>%
  rownames_to_column("variable_name")%>%
  bind_rows(
    data.frame(
      variable_name = names(model$estimate[model$estimate==0]),
      estimate = 0,
      standard_error = 0
    )
  )%>%
  mutate(
    lower_bound_confidence_interval = estimate - 1.96*standard_error,
    upper_bound_confidence_interval = estimate + 1.96*standard_error,
    variable_type = case_when(
      grepl("label", variable_name) ~ "label",
      grepl("calibre", variable_name) ~ "calibre",
      variable_name %in% c("b_low", "b_high", "b_medium", "b_indep") ~ "simplified_brand",
      grepl("c_", variable_name) ~ "demographic",
      TRUE ~ variable_name
    )
  )%>%
  mutate(rank = as.numeric(as.factor(variable_type)))

ggplot(data = df_estimate_confidence)+
  geom_errorbar(
    aes(
      x = 0,
      y = estimate,
      ymin = lower_bound_confidence_interval, 
      ymax = upper_bound_confidence_interval
    )
  )+
  geom_text(
    aes(
      x = 0,
      y = upper_bound_confidence_interval + 0.3,
      label = variable_name
    )
    )+
  facet_grid(
    scales = "free_y",
    cols = vars(variable_type)
    )

#### BAYESIAN POSTERIORS

tic("Computing the conditionnals")
conditionals = apollo_conditionals(
  model,
  apollo_probabilities,
  apollo_inputs
  )
toc()

saveRDS(conditionals, paste0("Inputs/Apollo/nosale_lnorm_with_control_conditionals_20220104.rds"))




toc()

