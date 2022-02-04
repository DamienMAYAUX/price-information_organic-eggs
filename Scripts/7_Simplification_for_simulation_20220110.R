
if (Sys.info()["sysname"] == "Windows"){
  dir = "U:/price-information_organic-eggs"
} else {
  dir = "~/U/price-information_organic-eggs"
}
setwd(dir)

source("Scripts/0_Packages_Libraries.R")

###########################################
################ PARAMETER ################
###########################################

competition_hypothesis_list = c("period", "year", "perfect_competition")

###########################################

for (competition_hypothesis in competition_hypothesis_list){
  
  # competition_hypothesis = "year"
  
  ## CHOICE SITUATIONS
  
  consumption_cleaner6 = readRDS("Inputs/shopping_trips_with_nosale_20220115.rds")%>% 
    select(-dateachat, -valqvol)%>%
    mutate(
      marque = case_when(
        marque_simple == "nosale" ~ "nosale",
        marque_simple %in% c("low", "medium", "high") ~ paste(marque_simple, retailer, sep = '_'),
        TRUE ~ paste(marque, retailer, sep = '_')
      )
    )
  
  
  df_product_simplified = consumption_cleaner6%>%
    group_by(marque, marque_simple, calibre, label)%>%
    dplyr::summarise(
      nb_sales = n()
    )%>% 
    filter(nb_sales > 20)%>%
    arrange(-nb_sales)%>%
    select(-nb_sales)
  
  
  consumption_simplified = consumption_cleaner6%>%
    inner_join(df_product_simplified)
  
  
  df_choice_set_simplified = readRDS("Inputs/choice_sets_with_nosale_20220115.rds")%>%
    select(-valqvol)%>% 
    mutate(
      marque = case_when(
        marque_simple == "nosale" ~ "nosale",
        marque_simple %in% c("low", "medium", "high") ~ paste(marque_simple, retailer, sep = '_'),
        TRUE ~ paste(marque, retailer, sep = '_')
      )
    )%>%
    unique()%>%
    inner_join(df_product_simplified)
  
  
  saveRDS(df_choice_set_simplified, "Inputs/choice_set_simplified.rds")
  
  df_retailer_set = readRDS(paste0("Inputs/df_hhid_retailer_set_", competition_hypothesis, ".rds"))
  
  df_choice_situation_simplified = consumption_simplified%>%
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
                  calibre_chosen == calibre)
    )%>%
    select(-unit_price)%>%
    mutate_at(
      vars(marque, label, calibre, marque_simple, retailer),
      as.character
    )%>%
    bind_rows(
      consumption_simplified%>%
        select(-unit_price)%>%
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
        )
    )%>%
    mutate_at(
      vars(marque, label, calibre, marque_simple, retailer),
      as.factor
    )
  
  ## ON INTRODUIT LES BONS NO-SALE
  
  hhid_with_enough_period_with_visit = consumption_simplified %>%
    filter(marque_simple != "nosale")%>%
    group_by(hhid, periode)%>%
    dplyr::summarise( nb_visits = n() )%>%
    group_by(hhid)%>%
    dplyr::summarise( nb_periode_with_visit = n() )%>%
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
      consumption_simplified%>%
        filter(marque_simple != "nosale")%>%
        select(periode, hhid)%>% unique
    )
  
  consumption_completed_with_nosale = 
    # Build the dataframe of nosale to draw from
    consumption_simplified%>%
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
      consumption_simplified%>%
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
  
  df_product_simplified2 =   consumption_completed_with_nosale%>%
    group_by(marque, marque_simple, label, calibre)%>%
    summarise(nb = n())%>%
    arrange(-nb)%>%
    select(-nb)%>%
    mutate(constant = 1)%>%
    group_by(constant)%>%
    mutate(product_number = 1:n())%>%
    ungroup()%>%
    select(-constant)
  
  ## PRICES AND CONTROL FUNCTIONS
  
  df_price = consumption_simplified%>%
    group_by(label, calibre, periode)%>%
    mutate(
      avg_price_across_retailer = mean(unit_price),
      nb_sales_across_retailer = n()
    )%>%
    ungroup()%>%
    group_by(marque_simple, label, calibre, marque, retailer, periode)%>%
    dplyr::summarise(
      avg_price = ifelse(marque_simple == "nosale", 0, mean(unit_price)),
      avg_price_across_retailer = max(avg_price_across_retailer),
      nb_sales = n(),
      nb_sales_across_retailer = max(nb_sales_across_retailer),
      avg_price_other_retailer = ifelse(marque_simple == "nosale", 0,
                                        avg_price_across_retailer * nb_sales_across_retailer / (nb_sales_across_retailer-nb_sales) -
                                          avg_price * nb_sales / (nb_sales_across_retailer-nb_sales) ),
      .groups = 'drop'
    )%>% unique()
  
  saveRDS(df_price, "Inputs/df_price_simplified.rds")
  
  df_price_for_regression = consumption_completed_with_nosale%>%
    left_join(df_price)%>%
    pivot_wider(names_from = c("label", "calibre", "marque", "retailer"), values_from = "avg_price")%>%
    select(X, unit_price, avg_price_across_retailer, contains("calibre"))%>%
    mutate_all(~replace_na(., 0))%>%
    mutate_at(vars(contains("calibre")), ~as.logical(.))
  
  fit_lm_ref_price = lm(unit_price ~ . - 1, data = df_price_for_regression)
  
  control_residuals = data.frame(
    X = consumption_completed_with_nosale$X,
    v_value = fit_lm_ref_price$residuals
  )
  
  choice_situation_with_nosale_for_estimation =
    df_choice_situation_simplified%>%
    filter(X %in% X_one_per_period$X)%>%
    left_join(df_price)%>%
    left_join(control_residuals)%>%
    mutate(
      price = ifelse(marque == "nosale", 0, avg_price),
      control = ifelse(choice & marque_chosen != "nosale", v_value, 0)
    )%>%
    select(
      -avg_price, -avg_price_across_retailer, -nb_sales, 
      -nb_sales_across_retailer, -avg_price_other_retailer, -v_value
    )
  
  
  household = readRDS("Inputs/household.rds")%>%
    select(hhid, clas, cycle)%>%
    mutate(
      clas = case_when(
        clas == "HIGH INCOME" ~ "high",
        clas == "LOW INCOME" ~ "low",
        clas == "HIGH MIDDLE INCOME" ~ "high_middle",
        clas == "LOW MIDDLE INCOME" ~ "low_middle"
      ),
      cycle = case_when(
        cycle == "COUPLE (< 35 YEARS OLD)" ~ "couple_35",
        cycle == "COUPLE (> 65 YEARS OLD)" ~ "couple_65",
        cycle == "COUPLE (35-65 YEARS OLD)" ~ "couple_3565",
        cycle == "FAMILY (ELDEST CHILD 0-5 Y.O.)" ~ "family_05",
        cycle == "FAMILY (ELDEST CHILD 12-17 Y.O.)" ~ "family_1217",
        cycle == "FAMILY (ELDEST CHILD 18-24 Y.O.)" ~ "family_1824",
        cycle == "FAMILY (ELDEST CHILD 6-11 Y.O.)" ~ "family_611",
        cycle == "SINGLE (< 35 YEARS OLD)" ~ "single_35",
        cycle == "SINGLE (> 65 YEARS OLD)" ~ "single_65",
        cycle == "SINGLE (35-65 YEARS OLD)" ~ "single_3565"
      )
    )%>%
    pivot_wider(values_from = c(clas), names_from = c(clas), names_prefix = "clas_")%>%
    pivot_wider(values_from = c(cycle), names_from = c(cycle), names_prefix = "cycle_")%>%
    mutate_if(is.character, ~as.integer(!is.na(.)))
  
  household%>%
    select(-hhid)%>%
    mutate(constant = 1)%>%
    group_by(constant)%>%
    summarise_all(mean)%>%
    select(-constant)%>%
    pivot_longer(1:14, names_to = "variable", values_to = "share")%>%
    mutate(
      share = paste0(format(round(100*share,1),1),"%"),
      variable = case_when(
        variable == "cycle_couple_35" ~ "aCouple en dessous de 35 ans sans enfant",
        variable == "cycle_couple_3565" ~ "bCouple entre 35 et 65 ans sans enfant",
        variable == "cycle_couple_65" ~ "cCouple au dessus de 65 ans sans enfant",
        variable == "cycle_family_05" ~ "dFamille dont l'enfant le plus ag� a moins de 5 ans",
        variable == "cycle_family_611" ~ "eFamille dont l'enfant le plus ag� a entre 6 et 11 ans",
        variable == "cycle_family_1217" ~ "fFamille dont l'enfant le plus ag� a entre 12 et 17 ans",
        variable == "cycle_family_1824" ~ "gFamille dont l'enfant le plus ag� a entre 18 et 24 ans",
        variable == "cycle_single_35" ~ "hC�libataire en dessous de 35 ans",
        variable == "cycle_single_3565" ~ "iC�libataire entre 35 et 65 ans",
        variable == "cycle_single_65" ~ "jC�libataire au dessus de 65 ans",
        variable == "clas_low" ~ "kBas niveau de revenu",
        variable == "clas_low_middle" ~ "lAssez bas niveau de revenu",
        variable == "clas_high_middle" ~ "mAssez haut niveau de revenu",
        variable == "clas_high" ~ "nHaut niveau de revenu"
      )
    )%>% arrange(variable)%>% 
    mutate(variable = substring(variable, first = 2))%>%
    rename(
      `Part dans l'�chantillon` = share,
      `Variable socio-d�mographique` = variable
      )%>%
    kbl()%>%
    kable_paper(lightable_options = "strip", full_width = F)%>%
    pack_rows("Cycle de vie",1,10)%>%
    pack_rows("Niveau de revenu",11,14)%>%
    save_kable("Outputs/summary_statistic_demographics.png")
  
  choice_situation_with_nosale_for_apollo = choice_situation_with_nosale_for_estimation%>%
    left_join(df_product_simplified2)%>%
    group_by(X)%>%
    mutate(
      choice = max(choice * product_number)
    )%>%
    ungroup()%>%
    select(
      X, hhid, choice,
      retailer, marque_simple, label, calibre, price, control, product_number
    )%>%
    mutate_at(vars(X, hhid, choice), ~as.integer(as.character(.)))%>%
    mutate_at(vars(price, control), as.numeric)%>%
    pivot_wider(
      id_cols = c("X", "hhid", "choice"),
      names_from = "product_number", 
      values_from = c(
        price,
        control
      ),
      names_glue = "{product_number}_{.value}",
      names_sort = TRUE
    )%>%
    mutate(across(ends_with('price'), list(avl = ~as.numeric(!is.na(.))), "{.col}_{.fn}"))%>%
    arrange(hhid, X)%>%
    left_join(household)
  
  
  choice_situation_with_nosale_for_apollo_testing = 
    choice_situation_with_nosale_for_apollo%>%
    filter(
      hhid %in%
        (choice_situation_with_nosale_for_apollo$hhid%>% 
           unique()%>% .[sample(1:length(.), 200)]
        )
    )%>%
    arrange(hhid, X)
  
  saveRDS(
    df_product_simplified2, 
    "Inputs/df_product_simplified.rds"
  )
  saveRDS(
    choice_situation_with_nosale_for_apollo, 
    paste0("Inputs/choice_situation_with_nosale_for_apollo_simplified_", competition_hypothesis, ".rds")
  )
  saveRDS(
    choice_situation_with_nosale_for_apollo_testing, 
    paste0("Inputs/choice_situation_with_nosale_for_apollo_testing_simplified_", competition_hypothesis, ".rds")
  )
  
  ### DATABASE PREPARATION
  
  df_product = df_product_simplified2%>%
    select(marque_simple, calibre, label)%>%
    mutate_all(as.character)
  
  ## FULL DATABASE
  
  rep_nb = as.integer(nrow(choice_situation_with_nosale_for_apollo) / (nrow(df_product) + 1))
  res_nb = as.integer(nrow(choice_situation_with_nosale_for_apollo) %% (nrow(df_product) + 1))
  
  database = choice_situation_with_nosale_for_apollo%>%
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
  
  saveRDS(database, paste0("Inputs/database_apollo_", competition_hypothesis, ".rds"))
  
  ## DATABASE FOR TESTING APOLLO
  
  rep_nb = as.integer(nrow(choice_situation_with_nosale_for_apollo_testing) / (nrow(df_product) + 1))
  res_nb = as.integer(nrow(choice_situation_with_nosale_for_apollo_testing) %% (nrow(df_product) + 1))
  
  database = choice_situation_with_nosale_for_apollo_testing%>%
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
  
  saveRDS(database, paste0("Inputs/database_apollo_testing_", competition_hypothesis, ".rds"))
  
  ## DATABSE FOR THE SIMULATION
  
  X_period_12 = readRDS("Inputs/shopping_trips_with_nosale_20220115.rds")%>% 
    select(-dateachat, -valqvol)%>%
    mutate(
      marque = case_when(
        marque_simple == "nosale" ~ "nosale",
        marque_simple %in% c("low", "medium", "high") ~ paste(marque_simple, retailer, sep = '_'),
        TRUE ~ paste(marque, retailer, sep = '_')
      )
    )%>%
    filter(periode == 12)%>% .$X
  
  database_before_merge_12 = choice_situation_with_nosale_for_apollo%>%
    filter(X %in% X_period_12)
  
  rep_nb_12 = as.integer(nrow(database_before_merge_12) / (nrow(df_product) + 1))
  res_nb_12 = as.integer(nrow(database_before_merge_12) %% (nrow(df_product) + 1))
  
  database_12 = database_before_merge_12%>%
    mutate(
      marque_simple = 
        c(
          rep(c(df_product$marque_simple, NA_character_), rep_nb_12),
          as.character(df_product$marque_simple)[1:res_nb_12]
        ),
      label = 
        c(
          rep(c(df_product$label, NA_character_), rep_nb_12),
          df_product$label[1:res_nb_12]
        ),
      calibre = 
        c(
          rep(c(df_product$calibre, NA_character_), rep_nb_12),
          df_product$calibre[1:res_nb_12]
        ),
    )%>%
    mutate_at(vars(marque_simple, label, calibre), as.factor)
  
  saveRDS(database_12, paste0("Inputs/database_apollo_12_", competition_hypothesis, ".rds"))
  
}


##############################################################
##############################################################
################### DEMAND ESTIMATION ########################
##############################################################
##############################################################

##############################################################
################### PARAMETERS ###############################
##############################################################
# competition_hypothesis = "period"
# competition_hypothesis = "year"
# competition_hypothesis = "perfect_competition"

nb_halton_draw = 50
nb_core = 20

###############################################################

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
  "b_calibreM" = 0
)

database = readRDS(paste0("Inputs/database_apollo_testing", competition_hypothesis, ".rds"))

apollo_control = list(
  mixing = TRUE,
  modelName = paste0("nosale_lnorm_with_control_", competition_hypothesis),
  modelDescr = "",
  indivID = "hhid",
  workInLogs = FALSE,
  nCores = nb_core,
  panelData = TRUE
)

# apollo_control = list(
#   mixing = FALSE,
#   modelName = "nosale_with_control",
#   modelDescr = "",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core,
#   panelData = TRUE
# )

apollo_beta = c(
  base_beta,
  #"b_price" = 1,
  "mu_lnorm" = -3,
  "sigma_lnorm" = 0,
  # "c_couple_35" = 0,
  # "c_couple_65" = 0,
  # "c_couple_3565" = 0,
  # "c_family_05" = 0,
  # "c_family_1217" = 0,
  # "c_family_1824" = 0,
  # "c_family_611" = 0,
  # "c_single_35" = 0,
  # "c_single_65" = 0,
  # "c_single_3565" = 0,
  "c_high" = 0,
  "c_low_middle" = 0,
  "c_high_middle" = 0,
  "c_low" = 0,
  "b_nosale" = 0,
  "b_control" = 0
)

apollo_fixed = c(
  "c_low_middle", # "c_family_1217",
  "b_nosale", "b_nolabel", "b_calibreM"
  )

apollo_draws = list(
  interNormDraws = c("draws_norm_lognorm"),
  interDrawsType = "halton",
  interNDraws = nb_halton_draw
)

apollo_randCoeff = function (apollo_beta , apollo_inputs) {
  randcoeff = list()
  randcoeff[["b_price"]] = - exp(
    mu_lnorm + sigma_lnorm*draws_norm_lognorm +
      # c_couple_35 * cycle_couple_35 +
      # c_couple_65 * cycle_couple_65 +
      # c_couple_3565 * cycle_couple_3565 +
      # c_family_05 * cycle_family_05 +
      # c_family_1217 * cycle_family_1217 +
      # c_family_1824 * cycle_family_1824 +
      # c_family_611 * cycle_family_611 +
      # c_single_35 * cycle_single_35 +
      # c_single_65 * cycle_single_65 +
      # c_single_3565 * cycle_single_3565
      c_high * clas_high +
      c_high_middle * clas_high_middle +
      c_low_middle * clas_low_middle +
      c_low * clas_low

    )
  return (randcoeff)
}


apollo_inputs=apollo_validateInputs()


apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="raw"){
  
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
    b_control * get(paste0(j, "_control"))
  
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

estimate_settings = list()

tic("Demand model estimation")
model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings
)
toc()

setwd("./Inputs/Apollo")
apollo_saveOutput(model)
setwd(dir)

# model = apollo_loadModel("DemandModel_20220131/nosale_lnorm_with_control_perfect_competition")
# model$estimate
# df_model_coefficients = 
#   left_join(
#     model$estimate%>% as.data.frame()%>% rownames_to_column("coefficient"),
#     model$varcov%>% diag()%>% sqrt() %>% as.data.frame()%>% rownames_to_column("coefficient"),
#     by = ("coefficient")
#   )%>%
#   rename(
#     estimate = ..x,
#     standard_deviation = ..y
#     )%>%
#   mutate(
#     relative_error = 100* 1.96*standard_deviation/abs(estimate)
#   )%>%
#   mutate(
#     logmean_reference_demographic_group = sum( estimate * (coefficient == "mu_lnorm") ),
#     mean_reference_demographic_group = exp(logmean_reference_demographic_group),
#     ratio = ifelse(
#       str_detect(coefficient, "c_"),
#       NA,
#       estimate / mean_reference_demographic_group
#     ),
#     estimate_truncated = format(round(estimate, 3),3),
#     ratio_truncated = ifelse(
#       str_detect(coefficient,"b_"),
#       ifelse(abs(ratio)<0.00001, "Référence", as.character(format(round(ratio, 4), 4))),
#       ""),
#     estimate_truncated_star = ifelse(relative_error < 100, paste0(estimate_truncated,"*"), estimate_truncated),
#     estimate_truncated_star = replace_na(estimate_truncated_star, "Référence")
#   )%>% filter(coefficient != "b_nosale")
# 
# #df_model_coefficient_kable = 
# df_model_coefficients%>%
#   select(coefficient, estimate_truncated_star, ratio_truncated)%>%
#   mutate(
#     coefficient = case_when(
#       coefficient == "b_low" ~ "aMarque distributeur bas de gamme",
#       coefficient == "b_medium" ~ "bMarque distributeur milleu de gamme",
#       coefficient == "b_high" ~ "cMarque distributeur haut de gamme",
#       coefficient == "b_indep" ~ "dMarque nationale",
#       
#       coefficient == "b_nolabel" ~ "ePoules élevées en cage",
#       coefficient == "b_labelpleinair" ~ "fPoules élevées en plair air",
#       coefficient == "b_labelbio" ~ "gPoules en élevage biologique",
#       
#       coefficient == "b_calibreM" ~ "hCalibre moyen",
#       coefficient == "b_calibreL" ~ "iCalibre large",
#       
#       coefficient == "b_control" ~ "jFonction de contrôle",
#       
#       coefficient == "mu_lnorm" ~ "kLog-moyenne de la sensibilité au prix",
#       coefficient == "c_low" ~ "lFaible niveau de revenu",
#       coefficient == "c_low_middle" ~ "mAssez faible niveau de revenu",
#       coefficient == "c_high_middle" ~ "nAssez haut niveau de revenu",
#       coefficient == "c_high" ~ "oHaut niveau de revenu",
#   
#       coefficient == "sigma_lnorm" ~ "pLog-variance de la sensibilité au prix",
#     ),
#   )%>%
#   arrange(coefficient)%>%
#   mutate_at(vars(coefficient), ~str_sub(., start = 2))%>%
#   rename(
#     `Variable` = coefficient,
#     `Valeur monétaire (en €)` = ratio_truncated,
#     `Coefficient estimé` = estimate_truncated_star
#     )%>%
#   kbl()%>%
#   kable_paper("striped", full_width = F)%>%
#   pack_rows("Disposition à payer pour les caractéristiques du produit", 1,10)%>%
#   pack_rows("Sensibilité au prix et variables démographiques", 11,16)%>%
#   footnote(general = "* l'étoile indique que le coefficient est significativement différent de 0 avec 95% de certitude
#            On normalise l'utilité de chaque caractéristique par la sensibilité au prix d'un ménage de référence 
#            (assez faible revenu) pour obtenir la valeur monétaire d'une caractéristique", footnote_as_chunk = T)%>%
#   save_kable("Outputs/table_demand_coefficient.png")


## Result table for the product characteristic coefficients

df_model_coefficient_characteristics = df_model_coefficients%>%
  filter(str_detect(coefficient, "b_"))%>%
  filter()

## Result table for the demographic coefficients

df_model_coefficient_demographics = df_model_coefficients%>%
  filter(str_detect(coefficient, "c_") | str_detect(coefficient, "lnorm"))%>%
  mutate(
    estimate = ifelse(
      is.na(standard_deviation),
      "Référence",
      paste0(
        as.character(format(round(estimate, 3), nsmall=3)),
        "   +/- ",
        as.character(format(round(relative_error),3)),
        "%"
      )
    ),
    standard_deviation = ifelse(
      is.na(standard_deviation),
      "Référence",
      as.character(format(round(standard_deviation, 3), nsmall=3))
      ),
    coefficient = case_when(
      coefficient == "mu_lnorm" ~ "Log-moyenne de la sensibilité au prix pour un ménage de référence",
      coefficient == "sigma_lnorm" ~ "Log-variance de la sensibilité au prix",
      coefficient == "c_high" ~ "Haut niveau de revenu",
      coefficient == "c_high_middle" ~ "Assez haut niveau de revenu",
      coefficient == "c_low_middle" ~ "Assez faible niveau de revenu",
      coefficient == "c_low" ~ "Faible niveau de revenu",
      coefficient == "c_couple_35" ~ "En couple, moins de 35 ans, sans enfant",
      coefficient == "c_couple_65" ~ "En couple, plus de 65 ans, sans enfant",
      coefficient == "c_couple_3565" ~ "En couple, entre 35 et 65 ans, sans enfant",
      coefficient == "c_family_05" ~ "Famille, l'ainé-e a moins de 5 ans",
      coefficient == "c_family_611" ~ "Famille, l'ainé-e a entre 6 et 11 ans",
      coefficient == "c_family_1217" ~ "Famille, l'ainé-e a entre 12 et 17 ans",
      coefficient == "c_family_1824" ~ "Famille, l'ainé-e a entre 18 et 24 ans",
      coefficient == "c_single_35" ~ "Célibataire, moins de 35 ans, sans enfant",
      coefficient == "c_single_65" ~ "Célibataire, plus de 65 ans, sans enfant",
      coefficient == "c_single_3565" ~ "Célibataire, entre 35 et 65 ans, sans enfant",
    )
  )%>% 
  select(-relative_error, -standard_deviation)%>%
  rename(
    Variable = coefficient,
    `Coefficient estimé` = estimate
  )

df_model_coefficient_demographics_kable = df_model_coefficient_demographics%>%
  kbl()%>%
  kable_paper("striped", full_width = F)%>%
  pack_rows("Niveau de revenu", 3,6)%>%
  pack_rows("Cycle de vie", 7,16)%>%
  footnote(general = "Les intervalles de confiance sont donnés avec 95% de certitude", footnote_as_chunk = T)

df_model_coefficient_demographics_kable%>%
  save_kable("Outputs/demographic_coefficients_table.png")
  

# ## QUALITY TEST
# 
# prediction_settings <<- list(silent = TRUE)
# 
# tic("Forecast")
# forecast = apollo_prediction(
#   model,
#   apollo_probabilities,
#   apollo_inputs,
#   prediction_settings
# )
# toc()
# # BY PRODUCT
# 
# predicted_demand_product = forecast%>%
#   select(-ID, -Observation, -chosen)%>%
#   pivot_longer(
#     cols = 1:(length(forecast)-3),
#     names_to = "product_number",
#     values_to = "probability_sales" 
#   )%>%
#   group_by(product_number)%>%
#   summarize(
#     aggregate_sales = sum(probability_sales)
#   )%>%
#   mutate(
#     product_number = as.integer(gsub("alt", "", product_number))
#   )%>% 
#   arrange(product_number)
# 
# actual_demand_product = database%>%
#   count(choice)%>%
#   rename(product_number= choice, aggregate_sales = n)
# 
# # BY RETAILER
# 
# df_product_simplified2 = readRDS("Inputs/df_product_simplified.rds")
# 
# predicted_demand_retailer = predicted_demand_product%>% 
#   left_join(df_product_simplified2)%>%
#   filter(marque_simple != "nosale")%>%
#   mutate(
#     retailer = str_extract(marque, pattern = '(?<=[a-zA-Z]{1,10}_).*'),
#     total_sales = sum(aggregate_sales)
#     )%>%
#   group_by(retailer)%>%
#   summarise(
#     aggregate_sales = sum(aggregate_sales),
#     market_share_volume = sum(aggregate_sales)/max(total_sales)
#     )%>% 
#   arrange(-market_share_volume)
# 
# actual_demand_retailer = actual_demand_product%>% 
#   left_join(df_product_simplified2)%>%
#   filter(marque_simple != "nosale")%>%
#   mutate(
#     retailer = str_extract(marque, pattern = '(?<=[a-zA-Z]{1,10}_).*'),
#     total_sales = sum(aggregate_sales)
#   )%>%
#   group_by(retailer)%>%
#   summarise(
#     aggregate_sales = sum(aggregate_sales),
#     market_share_volume = sum(aggregate_sales)/max(total_sales)
#   )%>% 
#   arrange(-market_share_volume)
# 
# # full_join(
# #   predicted_demand_retailer,
# #   actual_demand_retailer,
# #   by = c("retailer")
# #   )%>% View()
# 
# # BY LABEL
# 
# predicted_demand_label = predicted_demand_product%>% 
#   left_join(df_product_simplified2)%>%
#   filter(marque_simple != "nosale")%>%
#   mutate(total_sales = sum(aggregate_sales))%>%
#   group_by(label)%>%
#   summarise(
#     aggregate_sales = sum(aggregate_sales),
#     market_share_volume = sum(aggregate_sales)/max(total_sales)
#   )%>% 
#   arrange(-market_share_volume)
# 
# actual_demand_label = actual_demand_product%>% 
#   left_join(df_product_simplified2)%>%
#   filter(marque_simple != "nosale")%>%
#   mutate(total_sales = sum(aggregate_sales))%>%
#   group_by(label)%>%
#   summarise(
#     aggregate_sales = sum(aggregate_sales),
#     market_share_volume = sum(aggregate_sales)/max(total_sales)
#   )%>% 
#   arrange(-market_share_volume)
#   
# full_join(
#   predicted_demand_label,
#   actual_demand_label,
#   by = c("label")
# )%>% View()
# 
# # BY SALE OR NOSALE
# 
# predicted_demand_egg = predicted_demand_product%>% 
#   left_join(df_product_simplified2)%>%
#   mutate(
#     total_shopping_trips = sum(aggregate_sales),
#     purchase = marque_simple != "nosale"
#     )%>%
#   group_by(purchase)%>%
#   summarise(share = sum(aggregate_sales)/max(total_shopping_trips))
# 
# actual_demand_egg = actual_demand_product%>% 
#   left_join(df_product_simplified2)%>%
#   mutate(
#     total_shopping_trips = sum(aggregate_sales),
#     purchase = marque_simple != "nosale"
#   )%>%
#   group_by(purchase)%>%
#   summarise(share = sum(aggregate_sales)/max(total_shopping_trips))
# 
# 
# full_join(
#   predicted_demand_egg,
#   actual_demand_egg,
#   by = c("purchase")
# )%>% View()
# 
# # BY HOUSEHOLD
# 
# predicted_demand_household = forecast%>%
#   select(-chosen, -Observation)%>%
#   rename(hhid = ID)%>%
#   pivot_longer(
#     cols = contains("alt"),
#     names_to = "product_number",
#     values_to = "probability_sales" 
#   )%>%
#   mutate(
#     product_number = as.integer(gsub("alt", "", product_number))
#   )%>%
#   left_join(df_product_simplified2)%>%
#   group_by(hhid)%>%
#   mutate(individual_sales = sum(probability_sales))%>%
#   group_by(hhid, label)%>%
#   summarise(
#     share = sum(probability_sales)/max(individual_sales)
#     )
# 
# actual_demand_household = database%>%
#   select(hhid, choice)%>%
#   rename(product_number = choice)%>%
#   left_join(df_product_simplified2)%>%
#   group_by(hhid)%>%
#   mutate(individual_sales = n())%>%
#   group_by(hhid, label)%>%
#   summarise(
#     share = n()/max(individual_sales)
#   )
# 
# full_join(
#   predicted_demand_household,
#   actual_demand_household,
#   by = c("hhid", "label")
#   )%>% 
#   mutate(
#     share.y = replace_na(share.y, 0)
#   )%>% View()

# Il me faudrait des metriques ou des visualisations ici

# On constate que les achats des ménages sont souvent tres polarises concernant le label
# Ils achetent rarement plus d'un label different






##############################################################
##############################################################
################### BOYCOTT SIMULATION #######################
##############################################################
##############################################################

#competition_hypothesis = "perfect_competition"
competition_hypothesis = "period"

database = readRDS(paste0("Inputs/database_apollo_12_", competition_hypothesis, ".rds"))

df_choice_set_simplified = readRDS("Inputs/choice_set_simplified.rds")

df_product = readRDS("Inputs/df_product_simplified.rds")%>%
  inner_join(
    df_choice_set_simplified %>% filter(periode == 12)
  )

df_price = readRDS("Inputs/df_price_simplified.rds")

# tic("Computing conditionals")
# conditionals = apollo_conditionals(
#   model,
#   apollo_probabilities,
#   apollo_inputs
# )%>%
#   rename(hhid = ID, conditional = post.mean)
# toc()
# saveRDS(conditionals, "Inputs/conditionals_simplified_20220131.rds")

conditionals = readRDS("Inputs/conditionals_simplified_20220131.rds")%>%
  select(-post.sd)

# df_conditional_plot = conditionals%>%
#   left_join(
#     readRDS("Inputs/household.rds")
#   )%>% 
#   mutate(
#     quant5 = quantile(conditional, probs = c(0.95)),
#     quant1 = quantile(conditional, probs = c(0.99)),
#     alpha5 = conditional >= quant5,
#     alpha1 = conditional >= quant1,
#     high_middle = (clas == "HIGH MIDDLE INCOME"),
#     high = (clas == "HIGH INCOME")
#     )
# ggplot(
#   bind_rows(
#     df_conditional_plot%>% mutate(fill_category = "pg"),
#     df_conditional_plot%>% filter(high_middle)%>% mutate(fill_category = "ahr"),
#     df_conditional_plot%>% filter(high)%>% mutate(fill_category = "hr")
#   ), 
#   aes(x = conditional)
#   )+
#   geom_histogram(aes(fill = fill_category), color = "black", position = "dodge", alpha = 0.5)+
#   geom_vline(xintercept = df_conditional_plot%>% select(quant5)%>% unique%>% .[1,1], color = "orange")+
#   geom_text(
#     x = df_conditional_plot%>% select(quant5)%>% unique%>% .[1,1],
#     y = 190, label = "95e centile", color = "orange", hjust = 1.2
#     )+
#   geom_vline(xintercept = df_conditional_plot%>% select(quant1)%>% unique%>% .[1,1], color = "red")+
#   geom_text(
#     x = df_conditional_plot%>% select(quant1)%>% unique%>% .[1,1],
#     y = 190, label = "99e centile", color = "red", hjust = -0.3
#   )+
#   ylab("Nombre de ménages") + xlab("Sensibilité au prix")+
#   scale_fill_manual(
#     breaks = c("pg", "ahr", "hr"), 
#     values = c("pg" = "grey2", "hr" = "blue", "ahr" = "green"),
#     labels = c("Population générale", "Haut revenu", "Assez haut revenu"),
#     drop = FALSE,
#     name = "Echantillon de ménages"
#   )+
#   theme_bw()+
#   theme(legend.position = "bottom")
# ggsave("Outputs/distribution_alpha.png", width = 20, height = 20, units = "cm")

# df_organic_conditional_plot = readRDS("Inputs/shopping_trips_with_nosale_20220115.rds")%>%
#   filter(label != "nosale")%>%
#   group_by(hhid)%>%
#   mutate(total_purchase = n())%>%
#   ungroup()%>%
#   group_by(hhid, label)%>%
#   summarise(share = n()/max(total_purchase))%>%
#   pivot_wider(id_cols = "hhid", names_from = "label", values_from = "share")%>%
#   select(hhid, labelbio)%>%
#   mutate_at(vars(labelbio), ~replace_na(., 0))%>%
#   left_join(conditionals)%>%
#   filter(!is.na(conditional))
# never_organic = df_organic_conditional_plot%>% filter(abs(labelbio)<0.01)%>% nrow() / nrow(df_organic_conditional_plot)
# less_than_25pc_organic = df_organic_conditional_plot%>% filter(labelbio<=0.25)%>% nrow() / nrow(df_organic_conditional_plot)
# always_organic = df_organic_conditional_plot%>% filter(abs(labelbio-1)<0.01)%>% nrow() / nrow(df_organic_conditional_plot)
# less_than_25pc_organic- never_organic
# ggplot(df_organic_conditional_plot)+
#   annotate(geom = "text", x = -28, y = 1, label = " 3% des ménages achètent toujours bio")+
#   annotate(geom = "text", x = -28, y = 0.25, label = " 13% des ménages achètent bio \nmais moins d'une fois sur quatre")+
#   annotate(geom = "text", x = -28, y = -0.05, label = " 74% des ménages n'achètent jamais bio")+
#   stat_binhex(aes(x = conditional, y = labelbio), color = "black")+ 
#   scale_fill_gradient(
#     name = "Ménages",
#     breaks = c(1, 2, 5, 10, 20, 50, 100),
#     trans = "log"
#   )+
#   scale_y_continuous(limits = c(-0.05, NA))+
#   xlab("Sensibilité au prix") + ylab("Part d'oeufs bios dans les achats")+
# theme_bw()
# ggsave("Outputs/organic_share_alpha.png", width = 25, height = 20, units = "cm")

# readRDS("Inputs/shopping_trips_with_nosale_20220115.rds")%>%
#   select(marque, marque_simple, label)%>%
#   unique()%>%
#   pivot_wider(id_cols = 1:2, names_from = label, values_from = label)%>%
#   select(-nosale)%>%
#   mutate_at( vars(nolabel, labelbio, labelpleinair), ~ifelse(is.na(.), "NON", "OUI"))%>%
#   mutate(
#     marque_simple = case_when(
#       marque_simple == "high" ~ "1Marque distributeur haut de gamme",
#       marque_simple == "medium" ~ "2Marque distributeur milieu de gamme",
#       marque_simple == "low" ~ "3Marque distributeur bas de gamme",
#       marque_simple == "indep" ~ "4Marque nationale"
#     ),
#     marque = case_when(
#       marque == "OEUFS DE NOS REGIONS LES" ~ "LES OEUFS DE NOS REGIONS",
#       marque == "GAULOIS LE" ~ "LE GAULOIS",
#       TRUE ~ as.character(marque)
#     )
#   )%>%
#   arrange(marque_simple)%>%
#   mutate(marque_simple = substring(marque_simple, first = 2))%>%
#   filter(!is.na(marque_simple))%>%
#   select(marque, nolabel, labelpleinair, labelbio)%>%
#   rename(
#     `Marques` = marque,
#     `En Cage` = nolabel,
#     `En Plein air` = labelpleinair,
#     `Biologique` = labelbio
#     )%>%
#   kbl()%>%
#   kable_paper("striped", full_width = F)%>%
#   pack_rows("Marques distributeur haut de gamme  ", 1,5)%>%
#   pack_rows("Marques distributeur milieu de gamme  ", 6,10)%>%
#   pack_rows("Marques distributeur bas de gamme  ", 11,16)%>%
#   pack_rows("Marques nationales", 17,19)%>%
#   footnote(general = "On conserve dans l'échantillon que les 3 marques nationales les plus vendues", footnote_as_chunk = T)%>%
#   save_kable("Outputs/marque_simple_label_table.png")


retailer_price_list_test = df_price%>%
  filter(periode == 12 & marque != "nosale")%>%
  left_join(df_product)%>%
  group_by(product_number)%>%
  summarise(
    avg_price = mean(avg_price)
    )%>%
  .$avg_price

hhid_activist_list_test = conditionals%>%
  mutate(percentile = quantile(conditional, probs = c(0.1)))%>%
  filter(conditional < percentile)%>% .$hhid

# hhid_activist_list_test = conditionals$hhid%>%
#   sample(100)

activist_price_list_test = rep(0.3, length(retailer_price_list_test))

database_saved = database

generate_database = function(
  retailer_price_list, 
  df_hhid_conditional = NULL, 
  activist_price_list = NULL, 
  hhid_activist_list = NULL,
  boycott_type = NULL){
  
  # TEST 1
  # retailer_price_list = retailer_price_list_test
  # activist_price_list = NULL
  # hhid_activist_list = NULL
  # hhid_list = NULL
  # boycott_type = NULL
  
  # TEST 2 
  # retailer_price_list = retailer_price_list_test
  # df_hhid_conditional = conditionals
  # activist_price_list = activist_price_list_test
  # hhid_activist_list = hhid_activist_list_test
  # boycott_type = "hard"
  
  product_number = length(retailer_price_list)
  product_number_list = df_product$product_number
  
  if (is.null(activist_price_list)){
    activist_price_list = rep(0, product_number)
  }
  
  for (j in 1:product_number){
    
    price_name = paste(product_number_list[j], "price", sep='_')
    avl_name = paste(product_number_list[j], "price_avl", sep='_')
    product_label = df_product%>% 
      filter(product_number == product_number_list[j])%>% 
      .[1,"label"]
    
    if ( (!is.null(hhid_activist_list) & is.null(boycott_type)) |
          (is.null(hhid_activist_list) & !is.null(boycott_type)) ){
      stop("Incomplete boycott argument list")

    # Hard boycott : activists can only purchase organic eggs.
    # Their price must be below the boycott price 
    } else if (!is.null(hhid_activist_list) && boycott_type == "hard"){
      
      if (product_label == "labelbio"){
        database[[avl_name]] = 
          database_saved[[avl_name]] & 
          ( !(database$hhid %in% hhid_activist_list) | 
            (retailer_price_list[j] < activist_price_list[j]) )
      } else {
        database[[avl_name]] = database_saved[[avl_name]] & !(database$hhid %in% hhid_activist_list)
      }
      
    # Soft boycott : activists can purchase all types of eggs.
    # The price of purchased organic eggs must be below the boycott price
    } else if (!is.null(hhid_activist_list) && boycott_type == "soft"){
      
      if (product_label == "labelbio"){
        database[[avl_name]] = database_saved[[avl_name]] & 
          ( !(database$hhid %in% hhid_activist_list) | 
            (retailer_price_list[j] < activist_price_list[j]) )
      } else {
        database[[avl_name]] = database_saved[[avl_name]]
      }
      
    # If none of the two boycott types is specified or the list of activists  is empty
    # Then no boycott is implemented
    } else {
      if (!is.null(boycott_type) | !is.null(hhid_activist_list)){
        stop("Incorrect boycott type")
      } else {
        database[[avl_name]] = database_saved[[avl_name]]
      }
    }
    
    # if (!is.null(hhid_activist_list) && boycott_type == "hard"){
    #   if (product_label == "labelbio"){
    #     database[[avl_name]] = 
    #       database_saved[[avl_name]] & 
    #       ( !(database$hhid %in% hhid_activist_list) | 
    #           (retailer_price_list[j] < activist_price_list[j]) )
    #   } else {
    #     database[[avl_name]] = database_saved[[avl_name]] & (database$hhid %in% hhid_activist_list)
    #   }
    # } else if (!is.null(hhid_activist_list) && boycott_type == "soft" && product_label == "labelbio") {
    #   database[[avl_name]] = database_saved[[avl_name]] & 
    #     ( !(database$hhid %in% hhid_activist_list) | (retailer_price_list[j] < activist_price_list[j]) )
    # } else {
    #   database[[avl_name]] = 
    #     database_saved[[avl_name]]
    # }
    
    database[[price_name]] = ifelse(database[[avl_name]], retailer_price_list[j], NA_real_)
  }
  
  if (!is.null(df_hhid_conditional) & !("conditional" %in% names(database))){
    database = database%>%
      left_join(
        df_hhid_conditional,
        by = c("hhid")
        )
  }
  
  return(database)
  
}

#database = generate_database(retailer_price_list_test, conditionals)

# database = generate_database(
#   retailer_price_list_test, conditionals,
#   activist_price_list_test, hhid_activist_list_test,
#   boycott_type = "hard"
#   )
# database%>% filter(hhid %in% hhid_activist_list_test) %>% View()

model = apollo_loadModel(paste0("Inputs/Apollo/nosale_lnorm_with_control_perfect_competition_20220131"))
model$apollo_control$panelData = FALSE
model$apollo_control$mixing = FALSE
model$apollo_randCoeff = NULL
model$apollo_draws = NULL
model$estimate = model$estimate%>%
  .[!grepl("c_", names(model$estimate))]%>%
  .[!grepl("lnorm", names(model$estimate))]


apollo_control <<-model$apollo_control
apollo_fixed <<- model$apollo_fixed
apollo_beta <<- model$apollo_beta


generate_cross_product_derivative_and_demand = 
  function(model, retailer_price_list,  df_hhid_conditional = NULL,
           activist_price_list = NULL, hhid_activist_list = NULL, 
           boycott_type = NULL, subsample = NULL){
    
    # model
    
    # TEST 1
    # retailer_price_list = retailer_price_list_test
    # df_hhid_conditional = conditionals
    # activist_price_list = NULL
    # hhid_activist_list = NULL
    # boycott_type = NULL
    # subsample = NULL
    
    # TEST 2 
    # retailer_price_list = retailer_price_list_test
    # df_hhid_conditional = conditionals
    # activist_price_list = activist_price_list_test
    # hhid_activist_list = hhid_activist_list_test
    # boycott_type = "hard"
    # subsample = NULL
    
    ## 1) Generate forecasts
    
    # apollo_initialise()
    
    database <<- generate_database(
      retailer_price_list, df_hhid_conditional,
      activist_price_list, hhid_activist_list, boycott_type
    )

    
    apollo_inputs <<- apollo_validateInputs(silent = TRUE)
    
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
          b_control * get(paste0(j, "_control"))
        
        mnl_settings = list(
          alternatives  = setNames(1:J, names(V)),
          avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
          choiceVar     = choice,
          V             = V
        )
        
        P[["model"]] = apollo_mnl(mnl_settings , functionality)
        P = apollo_prepareProb(P, apollo_inputs , functionality)
        return(P)
      }
    
    forecast = apollo_probabilities(
      model$estimate,
      apollo_inputs,
    )%>% .[[1]]%>% as.data.frame()
    
    if (!is.null(subsample)){
      if (subsample == "activist"){
        forecast = forecast%>%
          mutate(hhid = database$hhid)%>%
          filter(hhid %in% hhid_activist_list)
      } else if (subsample == "passive"){
        forecast = forecast%>%
          mutate(hhid = database$hhid)%>%
          filter( !(hhid %in% hhid_activist_list) )
      } else {
        stop("Unknown value for the \"subsample\"argument")
      }
    }
    
    ## Compute the product-level demand from the forecast
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
    product_share_matrix = forecast%>% 
      select(starts_with("alt"))%>% 
      mutate_all(as.numeric)%>%
      as.matrix()%>%
      Matrix(., sparse = TRUE)

    # nnzero(product_share_matrix) / (product_nb * nrow(product_share_matrix))
    
    # La formule de la derivee de la proba de choix s_i par rapport au prix p_j est - s_i(dirac_i_j - s_j) 
    # Theoriquement, on pourrait multiplier ca par la "taille du marche", 
    # ie le nb moyen d'oeuf dans le panier du menage
    # J'obtiens une matrix 302x302 pour chacune des 53601 observations
    # Il faut sommer direct pour éviter de prendre inutilement de la place en memoire
    cross_product_price_derivative_matrix = 
      t(product_share_matrix) %*% (product_share_matrix) - diag(demand_per_product)
    
    return( 
      list(
        demand = demand_per_product, 
        matrix = cross_product_price_derivative_matrix
      )
    )
    
  }

# tic("Demand estimation")
# demand_test1 = generate_cross_product_derivative_and_demand(model, retailer_price_list_test, conditionals)$demand
# toc()
# 
# demand_test2 = generate_cross_product_derivative_and_demand(
#   model, retailer_price_list_test, conditionals,
#   activist_price_list_test, hhid_activist_list_test,
#   boycott_type = "hard"
#   )%>%
#   .$demand
#
# (demand_test2 - demand_test1)/demand_test1
#
# demand_test3 = generate_cross_product_derivative_and_demand(
#   model, retailer_price_list_test, conditionals,
#   activist_price_list_test, hhid_activist_list_test,
#   boycott_type = "hard", subsample = "passive"
#   )%>%
#   .$demand
# 
# demand_test4 = generate_cross_product_derivative_and_demand(
#   model, retailer_price_list_test, conditionals,
#   activist_price_list_test, hhid_activist_list_test,
#   boycott_type = "hard", subsample = "activist"
#   )%>%
#   .$demand
#
# demand_test5 = generate_cross_product_derivative_and_demand(
#   model, retailer_price_list_test, conditionals,
#   activist_price_list_test, hhid_activist_list_test,
#   boycott_type = "hard")%>%
#   .$demand
# 
# demand_test3 + demand_test4 - demand_test5
# 
# demand_test6 = generate_cross_product_derivative_and_demand(
#   model, retailer_price_list_test, conditionals, subsample = "passive"
#   )%>%
#   .$demand
#
# (demand_test3 - demand_test6) / demand_test6



#### Computing the costs

## On determine la demande et son elasticite au prix actuel

demand_and_matrix = generate_cross_product_derivative_and_demand(model, retailer_price_list_test, conditionals)
demand = demand_and_matrix$demand
cross_product_price_derivative_matrix = demand_and_matrix$matrix

# cross_product_price_derivative_matrix%>% as.matrix()%>% View()

df_choice_set_simplified = readRDS("Inputs/choice_set_simplified.rds")

df_product_retailer = df_choice_set_simplified%>%
  filter(periode == 12)%>%
  left_join(df_product)%>%
  select(retailer, product_number)%>%
  arrange(product_number)%>%
  #arrange(retailer)%>%
  mutate(constant = 1)

relevant_product_number_list = df_product_retailer$product_number
relevant_demand = demand %>% .[relevant_product_number_list]

## On construit la matrice d'appartenance

relevant_cross_product_price_derivative_matrix = 
  cross_product_price_derivative_matrix[relevant_product_number_list, relevant_product_number_list]

# relevant_cross_product_price_derivative_matrix%>% as.matrix()%>% View()

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

# belonging_matrix%>% as.matrix()%>% View()

omega = belonging_matrix * relevant_cross_product_price_derivative_matrix   

# omega%>% as.matrix()%>% View()

## On inverse le systeme

# Omega ([1-tau]P-C) + D = 0 
# C = [1-tau]P + Omega^-1 D
absolute_margin = - solve(omega, relevant_demand) %>% as.vector()
cost_list = (1-0.055)*retailer_price_list_test - absolute_margin


## Creation of a table containing the calibration results 

# df_calibration_analysis = df_product%>%
#   mutate(
#     format = str_extract(marque, "[0-9]+(?=_)"),
#     cost = cost_list,
#     price = retailer_price_list_test,
#     margin = price - cost
#   )%>%
#   inner_join(
#     df_choice_set_simplified%>% filter(periode == 12)
#     )
# 
# df_calibration_total_analysis = df_calibration_analysis%>%
#   mutate(dummy = "Ensemble")%>%
#   group_by(dummy)%>%
#   summarise(
#     avg_cost = as.character(format(round(mean(cost), 3), nsmall = 3)),
#     avg_price = format(round(mean(price), 3), nsmall = 3),
#     avg_margin = as.character(format(round(mean(margin), 3), nsmall = 3))
#   )%>%
#   rename(
#     Catégorie = dummy,
#     avg_cost_diff = avg_cost,
#     avg_margin_diff = avg_margin
#     )%>%
#   arrange(avg_price)
# 
# df_calibration_label_analysis = df_calibration_analysis%>%
#   group_by(label)%>%
#   summarise(
#     avg_cost = mean(cost),
#     avg_price = format(round(mean(price), 3), nsmall = 3),
#     avg_margin = mean(margin)
#     )%>%
#   mutate(
#     avg_cost_diff = ifelse(
#       avg_price == min(avg_price),
#       "Référence",
#       as.character(format(round(avg_cost - sum((avg_price == min(avg_price)) * avg_cost), 3), nsmall = 3))
#       ),
#     avg_margin_diff = ifelse(
#       avg_price == min(avg_price),
#       "Référence",
#       as.character(format(round(avg_margin - sum((avg_price == min(avg_price)) * avg_margin), 3), nsmall = 3))
#     ),
#     label = case_when(
#       label == "nolabel" ~ "Oeufs de poules élevées en cage",
#       label == "labelpleinair" ~ "Oeufs de poules élevées en plein air",
#       label == "labelbio" ~ "Oeufs issus de l'agriculture biologique ",
#     )
#   )%>%
#   rename(Catégorie = label)%>%
#   arrange(avg_price)
# 
# df_calibration_marque_simple_analysis = df_calibration_analysis%>%
#   group_by(marque_simple)%>%
#   summarise(
#     avg_cost = mean(cost),
#     avg_price = format(round(mean(price), 3), nsmall = 3),
#     avg_margin = mean(margin)
#     )%>%
#   mutate(
#     avg_cost_diff = ifelse(
#       avg_price == min(avg_price),
#       "Référence",
#       as.character(format(round(avg_cost - sum((avg_price == min(avg_price)) * avg_cost), 3), nsmall = 3))
#     ),
#     avg_margin_diff = ifelse(
#       avg_price == min(avg_price),
#       "Référence",
#       as.character(format(round(avg_margin - sum((avg_price == min(avg_price)) * avg_margin), 3), nsmall = 3))
#     ),
#     marque_simple = case_when(
#       marque_simple == "low" ~ "Marque distributeur entrée de gamme",
#       marque_simple == "medium" ~ "Marque distributeur milieu de gamme",
#       marque_simple == "high" ~ "Marque distributeur haut de gamme",
#       marque_simple == "indep" ~ "Marque nationale",
#     )
#   )%>%
#   rename(Catégorie = marque_simple)%>%
#   arrange(avg_price)
# 
# df_calibration_format_analysis = df_calibration_analysis%>%
#   group_by(format)%>%
#   summarise(
#     avg_cost = mean(cost),
#     avg_price = format(round(mean(price), 3), nsmall = 3),
#     avg_margin = mean(margin)
#     )%>%
#   mutate(
#     avg_cost_diff = ifelse(
#       avg_price == min(avg_price),
#       "Référence",
#       as.character(format(round(avg_cost - sum((avg_price == min(avg_price)) * avg_cost), 3), nsmall = 3))
#     ),
#     avg_margin_diff = ifelse(
#       avg_price == min(avg_price),
#       "Référence",
#       as.character(format(round(avg_margin - sum((avg_price == min(avg_price)) * avg_margin), 3), nsmall = 3))
#     ),
#     format = case_when(
#       format == 43 ~ "Hypermarchés",
#       format == 42 ~ "Supermarchés",
#       format == 41 ~ "Supérettes",
#       format == 7 ~ "Magasins populaires"
#     )
#   )%>%
#   rename(Catégorie = format)%>%
#   arrange(avg_price)
# 
# df_calibration_analysis_table = bind_rows(
#   df_calibration_total_analysis,
#   df_calibration_label_analysis,
#   df_calibration_marque_simple_analysis,
#   df_calibration_format_analysis
#   )%>%
#   select(-avg_margin, -avg_cost)%>%
#   select(
#     Catégorie,
#     `Prix moyen` = avg_price,
#     `Coût marginal` = avg_cost_diff,
#     `Bénéfice marginal` = avg_margin_diff
#   )
# 
# if (competition_hypothesis == 'period'){
#   
#   df_calibration_analysis_table_kable = df_calibration_analysis_table%>%
#     kbl()%>%
#     kable_paper("striped", full_width = F)%>%
#     pack_rows("Label", 2,4)%>%
#     pack_rows("Marque simplifiée", 5,8)%>%
#     pack_rows("Format", 9,12)%>%
#     footnote(general = "Les prix sont en euros.
#            Les distributeurs anticipent la demande selon la configuration (3) pour la fixation des prix,
#            dans laquelle chaque ménage n'aurait pu acheter que chez les distributeurs visités durant la période
#            ", footnote_as_chunk = T)
#   
#   df_calibration_analysis_table_kable%>%
#     save_kable("Outputs/calibration_table_period.png")
#   
# } else if (competition_hypothesis == 'year'){
#   
#   df_calibration_analysis_table_kable = df_calibration_analysis_table%>%
#     kbl()%>%
#     kable_paper("striped", full_width = F)%>%
#     pack_rows("Label", 2,4)%>%
#     pack_rows("Marque simplifiée", 5,8)%>%
#     pack_rows("Format", 9,12)%>%
#     footnote(general = "Les prix sont en euros.
#            Les distributeurs anticipent la demande selon la configuration (2) pour la fixation des prix,
#            dans laquelle chaque ménage n'aurait pu acheter que chez les distributeurs visités durant l'année
#            ", footnote_as_chunk = T)
#   
#   df_calibration_analysis_table_kable%>%
#     save_kable("Outputs/calibration_table_year.png")
#   
# } else {
#   
#   df_calibration_analysis_table_kable = df_calibration_analysis_table%>%
#     kbl()%>%
#     kable_paper("striped", full_width = F)%>%
#     pack_rows("Label", 2,4)%>%
#     pack_rows("Marque simplifiée", 5,8)%>%
#     pack_rows("Format", 9,12)%>%
#     footnote(general = "Les prix sont en euros.
#            Les distributeurs anticipent la demande selon la configuration (1) pour la fixation des prix,
#            dans laquelle chaque ménage aurait pu acheter chez tous les distributeurs
#            ", footnote_as_chunk = T)
#   
#   df_calibration_analysis_table_kable%>%
#     save_kable("Outputs/calibration_table_perfect_competition.png")
#   
# }





#### DETERMINATION DES PRIX POST BOYCOTT

############################################################
####################### Parameters #########################
############################################################

boycott_price_min = 0.25
boycott_price_max = 0.35
boycott_step = 0.02
boycott_price_range = boycott_price_min + boycott_step * 0:(1+(boycott_price_max-boycott_price_min)/boycott_step)

# hhid_activist_list = hhid_activist_list_test
# boycott_type = "hard"
nb_cluster = 15

############################################################


package_loader = function(){
  library(Matrix)
  library(matlib)
  library(apollo)
  library(tidyverse)
  library(tictoc)
  library(rje)
}

compute_equilibrium_price = function(
  model, start_price = retailer_price_list_test,  df_hhid_conditional = NULL,
  activist_price_list = NULL, hhid_activist_list = NULL,
  boycott_type = NULL, subsample = NULL,
  precision = 0.000001, step = 1){

  # TEST 1
  # start_price = retailer_price_list_test
  # df_hhid_conditional = NULL
  # activist_price_list = NULL
  # hhid_activist_list = NULL
  # boycott_type = NULL
  # subsample = NULL
  # precision = 0.000001
  # step = 1

  # TEST 2
  #start_price = abs(rnorm(length(retailer_price_list_test)))

  package_loader()

  new_retailer_price = start_price
  iteration_counter = 0
  error = 1

  # On itere l'algo suivant tant que ça bouge d'au moins d'un tiers de centime sur une composante :
  while (error > precision){

    current_retailer_price = new_retailer_price
    iteration_counter = iteration_counter+1

    # On reinitialise la derniere erreur enregistree
    # Cela permet a l'algorithme d'essayer un pas plus grand
    # if (iteration_counter %% 5 == 0){
    #   error = 1
    # }

    # 1) On calcule D et Omega avec le modele de demande estime et le niveau des prix

    demand_and_matrix = generate_cross_product_derivative_and_demand(
      model, current_retailer_price, conditionals,
      activist_price_list, hhid_activist_list,
      boycott_type, subsample
    )
    demand = demand_and_matrix$demand
    cross_product_price_derivative_matrix = demand_and_matrix$matrix

    # 2) On calcule le prix optimal à partir des couts et de la demande

    relevant_demand = demand %>% .[relevant_product_number_list]
    relevant_cross_product_price_derivative_matrix =
      cross_product_price_derivative_matrix[relevant_product_number_list, relevant_product_number_list]
    omega = belonging_matrix * relevant_cross_product_price_derivative_matrix

    # On rappelle que [1-tau]P = C - Omega^-1 D
    # Pour faciler la convergence, moyenne le nouveau vecteur de prix avec le précédent
    previous_error = error
    current_step = step
    while (error >= previous_error){
      current_step = current_step/2
      new_retailer_price = (1-current_step) * current_retailer_price + current_step * (cost_list - solve(omega, relevant_demand) %>% as.vector())/(1-0.055)
      error = max(abs(current_retailer_price-new_retailer_price))
      #print(paste("Intermediary error :", error))
    }

    print(paste("Iteration", iteration_counter))
    print(paste("Step size :", current_step))
    print(paste("Error :", error))
    #print(new_retailer_price[1:10])

  }

  return(new_retailer_price)
}






# This is an auxiliary function for the one-step cross-retailer BR computation
# It computes the BR at the retailer level, 
# holding the price of some organic products at their boycott level

compute_best_response_retailer = function(
  model, 
  initial_retailer_price, 
  conditionals,
  activist_price_list,
  hhid_activist_list,
  boycott_type, subsample,
  current_retailer,
  retailer_product_rank,
  retailer_organic_product_at_boycott_price_rank,
  retailer_product_not_at_boycott_price_rank,
  precision = 0.000001,
  verbose = 4
){
  
  # TEST 1
  # current_retailer = "43_1"
  # df_produt_current_retailer = df_product%>%
  #   rowid_to_column("rank")%>%
  #   filter(retailer == current_retailer)
  # 
  # retailer_product_rank = df_produt_current_retailer$rank
  # retailer_organic_product_rank = df_produt_current_retailer%>%
  #   filter(label == "labelbio")%>%
  #   .$rank
  # retailer_organic_product_at_boycott_price_rank = df_produt_current_retailer%>%
  #   filter(label == "labelbio")%>%
  #   .$rank
  # retailer_product_not_at_boycott_price_rank = df_produt_current_retailer%>%
  #   .$rank%>%
  #   .[!(. %in% retailer_organic_product_at_boycott_price_rank)]
  # activist_price_list = activist_price_list_test
  # hhid_activist_list = hhid_activist_list_alpha_1percent
  # initial_retailer_price = retailer_price_list_test
  # boycott_type = "hard"
  # subsample = NULL
  # precision = 0.0001
  # verbose = 5
  
  new_retailer_price = initial_retailer_price
  new_retailer_price[retailer_organic_product_at_boycott_price_rank] = 
    activist_price_list[retailer_organic_product_at_boycott_price_rank] - 0.000001
  previous_price_difference = 1
  min_weight = 1
  weight = 1
  price_difference = 1
  iteration_counter = 0
  
  tic("Computing the best price took")
  while (price_difference>precision & weight > 0.0001){
    
    iteration_counter = iteration_counter+1
    current_retailer_price = new_retailer_price
    previous_price_difference = price_difference
    
    demand_and_matrix = generate_cross_product_derivative_and_demand(
      model, new_retailer_price, conditionals,
      activist_price_list, hhid_activist_list,
      boycott_type, subsample
    )
    
    relevant_demand = demand_and_matrix$demand%>%
      .[relevant_product_number_list]%>%
      .[retailer_product_not_at_boycott_price_rank]
    
    omega =
      demand_and_matrix$matrix%>%
      .[relevant_product_number_list, relevant_product_number_list]%>%
      as.matrix()

    new_retailer_price[retailer_product_not_at_boycott_price_rank] =
      (cost_list[retailer_product_not_at_boycott_price_rank] - 
         solve(
           omega[retailer_product_not_at_boycott_price_rank, retailer_product_not_at_boycott_price_rank],
           relevant_demand + 
             omega[retailer_product_not_at_boycott_price_rank,retailer_organic_product_at_boycott_price_rank] %*% 
             as.matrix(current_retailer_price[retailer_organic_product_at_boycott_price_rank] - cost_list[retailer_organic_product_at_boycott_price_rank])
           )%>% 
         as.vector()
       )/(1-0.055)
   
    price_difference = max(abs(
      new_retailer_price[retailer_product_not_at_boycott_price_rank] - 
      current_retailer_price[retailer_product_not_at_boycott_price_rank]
      ))
  
    # weight = min(0.5 * previous_price_difference/price_difference, 1)
    # weight = min(previous_price_difference/price_difference, 1) * weight
    # weight = min(previous_price_difference/price_difference, 1)
    
    if (price_difference/previous_price_difference > 1){
      weight = min(previous_price_difference/price_difference, weight)
    } else if (price_difference/previous_price_difference > 0.8){
      weight = weight * 0.9
    }
    
    min_weight = min(weight, min_weight)
    
    new_retailer_price[retailer_product_not_at_boycott_price_rank] = 
      current_retailer_price[retailer_product_not_at_boycott_price_rank] * (1-weight) + 
      new_retailer_price[retailer_product_not_at_boycott_price_rank] * weight
   
  if (verbose > 3){
    cat(paste("Iteration", iteration_counter, "with step size", weight, "\n"))
    if (verbose > 4){
      print(price_difference)
      print(new_retailer_price[retailer_product_not_at_boycott_price_rank])
    }
  } 
    
  }
  toc()
  
  if (min_weight < 0.05){
    print(paste("Convergence issue for retailer", current_retailer))
    warning(paste("The step size went as low as", min_weight, "for the price system"))
  }
  
  if (verbose > 2){
    cat(paste("Best price computed for retailer", current_retailer, "and boycotted products "))
    cat(retailer_organic_product_at_boycott_price_rank)
    cat("\n\n")
  }
  
  new_retailer_price
}

# compute_best_response_retailer(
#   initial_retailer_price,
#   retailer_product_rank,
#   retailer_organic_product_at_boycott_price_rank,
#   retailer_product_not_at_boycott_price_rank,
#   precision = 0.0001,
#   verbose = 5
# )



compute_equilibrium_price_boycott = function(
  initial_retailer_price,
  df_hhid_conditional,
  activist_price_list,
  hhid_activist_list,
  boycott_type,
  subsample = NULL,
  verbose = 2,
  precision = 0.00005,
  initial_precision = 0.00002
){
  
  package_loader()
  
  # retailer_price_list = retailer_price_list_test
  # df_hhid_conditional = conditionals
  # activist_price_list = activist_price_list_test
  # hhid_activist_list = hhid_activist_list_test
  # boycott_type = "hard"
  # subsample = NULL
  
  # verbose = 2
  # activist_price_list = activist_price_list_test
  # new_retailer_price = retailer_price_list_test
  # iteration_counter = 0
  # retailer_list = levels(df_product$retailer)
  # precision = 0.00001
  # price_difference = 0.10
  
  new_retailer_price = initial_retailer_price
  
  retailer_list = levels(df_product$retailer)
  
  price_difference = 0.10
  iteration_counter = 0
  current_precision = initial_precision
  
  # res <- NULL
  # tryCatch({
  #   res <- withTimeout({
  #     foo()
  #   }, timeout = 3600)
  # }, TimeoutException = function(ex) {
  #   message("")
  # })
  
  while (price_difference>precision){
    
    iteration_counter = iteration_counter+1
    current_retailer_price = new_retailer_price
    current_precision = min(current_precision, price_difference/100)
    
    cat(paste("Iteration number", iteration_counter, "of the cross-retailer best response function\n"))
    cat(paste0("current precision : ", current_precision, "\n\n"))
    
    new_set_of_boycotted_products = c()
    
    tic("Computing the best response for every retailer")
    for (current_retailer in retailer_list){
      # current_retailer = "43_1"
      
      df_produt_current_retailer = df_product%>%
        rowid_to_column("rank")%>%
        filter(retailer == current_retailer)
      
      retailer_product_rank = df_produt_current_retailer$rank
      retailer_organic_product_rank = df_produt_current_retailer%>%
        filter(label == "labelbio")%>%
        .$rank
      
      combination_list = powerSet(retailer_organic_product_rank)
      
      best_profit = 0
      
      tic(paste("Computing the best response for retailer", current_retailer))
      for (combination in combination_list){
        
        if (verbose > 1){
          cat(combination)
          cat("\n")
        }
        
        retailer_best_response = 
          compute_best_response_retailer(
            model, 
            current_retailer_price, 
            conditionals,
            activist_price_list, 
            hhid_activist_list,
            boycott_type, subsample,
            current_retailer,
            retailer_product_rank,
            combination,
            retailer_product_rank%>% .[!(. %in% combination)],
            precision = current_precision,
            verbose = verbose
          )
        
        retailer_relevant_demand = 
          generate_cross_product_derivative_and_demand(
            model, retailer_best_response, df_hhid_conditional,
            activist_price_list, hhid_activist_list,
            boycott_type, subsample
          )%>%
          .$demand%>%
          .[retailer_product_rank]
        
        profit = sum(
          retailer_relevant_demand * 
            ((retailer_best_response - cost_list)%>% .[retailer_product_rank])
        )
        
        if (profit > best_profit){
          best_profit = profit
          best_combination = combination
          best_new_retailer_price = retailer_best_response
        }
        
      }
      toc()
      
      if (verbose > 1){
        cat("#######################################################\n")
        cat(paste("Products at boycott price for retailer", current_retailer, ": "))
        cat(best_combination)
        cat("\n")
        cat("#######################################################\n")
        cat("\n\n\n")
      }
      
      new_retailer_price[retailer_product_rank] = best_new_retailer_price[retailer_product_rank]
      new_set_of_boycotted_products = c(new_set_of_boycotted_products, best_combination)
    }
    toc()
    
    price_difference = max(abs(new_retailer_price - current_retailer_price))
    
    if (verbose > 0){
      cat(paste("The price difference for this iteration was", price_difference, "\n"))
      cat("The products at boycott price for this iteration were : ")
      cat(new_set_of_boycotted_products)
      cat("\n")
      cat("The new retailer price list is :\n")
      print(new_retailer_price)
      cat("\n\n\n\n")
    }
    
  }
  
  new_retailer_price
}

# tic("Equilibrium price with boycott")
# converged_retailer_price =
#   compute_equilibrium_price_boycott(
#     initial_retailer_price = retailer_price_list_test,
#     df_hhid_conditional = conditionals,
#     activist_price_list = activist_price_list_test,
#     hhid_activist_list = hhid_activist_list_test,
#     boycott_type = "hard",
#     subsample = NULL,
#     verbose = 1,
#     precision = 0.005,
#     initial_precision = 0.005
#     )
# toc()
#round(100 * (converged_retailer_price-retailer_price_list_test)/retailer_price_list_test)

compute_equilibrium_price_boycott_range = function(
  initial_retailer_price = retailer_price_list_test,
  df_hhid_conditional = conditionals,
  hhid_activist_list,
  boycott_type,
  subsample = NULL,
  verbose = 1,
  precision = 0.00005,
  initial_precision = 0.0001
){
  
  cluster = makeCluster(nb_cluster)
  
  required_variable_list = list(
    "belonging_matrix",
    "relevant_product_number_list",
    "generate_database",
    "generate_cross_product_derivative_and_demand",
    "compute_best_response_retailer",
    "cost_list",
    "model",
    "df_product_retailer",
    "df_product",
    "df_price",
    "database_saved",
    "database",
    "conditionals",
    "package_loader",
    "apollo_control",
    "apollo_fixed",
    "apollo_beta"
  )
  
  tic("Broadcasting global variables in the cluster")
  clusterExport(cl = cluster, varlist = required_variable_list)
  toc()
  
  tic("Compute equilibrium price range in parallel")
  retailer_price_list_range = clusterApply(
    cl = cluster,
    x = lapply(
      X = boycott_price_range%>% as.list(),
      FUN = function(x){rep(x, length(retailer_price_list_test))}
    ),
    fun = compute_equilibrium_price_boycott,
    initial_retailer_price = retailer_price_list_test,
    df_hhid_conditional = df_hhid_conditional,
    hhid_activist_list = hhid_activist_list,
    boycott_type = boycott_type,
    subsample = subsample,
    verbose = verbose,
    precision = precision,
    initial_precision = initial_precision
  )
  toc()
  
  stopCluster(cluster)
  
  retailer_price_list_range
  
}

# tic("Compute the equilibrium price under boycott")
# retailer_price_list_range = compute_equilibrium_price_boycott_range(
#   initial_retailer_price = retailer_price_list_test,
#   df_hhid_conditional = conditionals,
#   hhid_activist_list = hhid_activist_list_test,
#   boycott_type = "hard",
#   subsample = NULL,
#   verbose = 1,
#   precision = 0.005,
#   initial_precision = 0.005
# )
# toc()
# saveRDS(retailer_price_list_range, "Inputs/price_range_hard_25_35_2.rds")


#### POUR LA NUIT #####

# - un cas sans boycott pour vérifier que tout va bien (1)
# - middle high 5% et high 5% en soft (2)
# - alpha et top consommateurs bios en hard et 0.5%, 1% et 5% (6)

nb_hhid = nrow(conditionals)

hhid_activist_list_high_middle_5percent = database%>%
  select(hhid, clas_high_middle)%>% 
  filter(clas_high_middle == 1)%>% 
  unique()%>%
  .$hhid%>%
  sample(round(5*nb_hhid/100))

hhid_activist_list_high_5percent = database%>%
  select(hhid, clas_high)%>% 
  filter(clas_high == 1)%>% 
  unique()%>%
  .$hhid%>%
  sample(round(5*nb_hhid/100))
  
hhid_activist_list_alpha_5percent = conditionals%>%
  mutate(percentile = quantile(conditional, probs = c(0.05)))%>%
  filter(conditional < percentile)%>% .$hhid

hhid_activist_list_alpha_1percent = conditionals%>%
  mutate(percentile = quantile(conditional, probs = c(0.01)))%>%
  filter(conditional < percentile)%>% .$hhid

hhid_activist_list_alpha_05percent = conditionals%>%
  mutate(percentile = quantile(conditional, probs = c(0.005)))%>%
  filter(conditional < percentile)%>% .$hhid


# # CONTROL 
# tic("Control")
# retailer_price_list_range_test_control1 = compute_equilibrium_price_boycott(
#   initial_retailer_price = retailer_price_list_test,
#   activist_price_list = rep(1, 122),
#   df_hhid_conditional = conditionals,
#   hhid_activist_list = hhid_activist_list_alpha_5percent,
#   boycott_type = "soft",
#   subsample = NULL,
#   verbose = 1,
#   precision = 0.001,
#   initial_precision = 0.005
# )
# saveRDS(
#   retailer_price_list_range_test_control1, 
#   "Inputs/retailer_price_list_range_test_control1.rds"
# )
# toc()

# 
# # SOFT, HIGH
# retailer_price_list_range_soft_high_5percent_0_70_02 = 
#   compute_equilibrium_price_boycott(
#     initial_retailer_price = retailer_price_list_test,
#     df_hhid_conditional = conditionals,
#     activist_price_list = rep(0.25, 122),
#     hhid_activist_list = hhid_activist_list_high_5percent,
#     boycott_type = "soft",
#     subsample = NULL,
#     verbose = 1,
#     precision = 0.001,
#     initial_precision = 0.005
#   )
# saveRDS(
#   retailer_price_list_range_soft_high_5percent_0_70_02_25c, 
#   "Inputs/retailer_price_list_range_soft_high_5percent_0_70_02.rds"
# )      


# # SOFT, HIGH MIDDLE
# retailer_price_list_range_soft_high_middle_5percent_0_70_02_25c = 
#   compute_equilibrium_price_boycott(
#     initial_retailer_price = retailer_price_list_test,
#     df_hhid_conditional = conditionals,
#     activist_price_list = rep(0.25, 122),
#     hhid_activist_list = hhid_activist_list_high_middle_5percent,
#     boycott_type = "soft",
#     subsample = NULL,
#     verbose = 1,
#     precision = 0.001,
#     initial_precision = 0.005
#   )
# saveRDS(
#   retailer_price_list_range_soft_high_middle_5percent_0_70_02_25c, 
#   "Inputs/retailer_price_list_range_soft_high_middle_5percent_0_70_02_25c.rds"
# )            


# # HARD, ALPHA, 5%
# retailer_price_list_range_hard_alpha_5percent_0_70_02_25c = compute_equilibrium_price_boycott(
#   initial_retailer_price = retailer_price_list_test,
#   df_hhid_conditional = conditionals,
#   activist_price_list = rep(0.25, 122),
#   hhid_activist_list = hhid_activist_list_alpha_5percent,
#   boycott_type = "hard",
#   subsample = NULL,
#   verbose = 1,
#   precision = 0.001,
#   initial_precision = 0.005
# )
# saveRDS(
#   retailer_price_list_range_hard_alpha_5percent_0_70_02_25c,
#   "Inputs/retailer_price_list_range_hard_alpha_5percent_0_70_02_25c.rds"
# )
# 
# 


# HARD, ALPHA 1%
retailer_price_list_range_hard_alpha_1percent_0_70_02 = compute_equilibrium_price_boycott(
  initial_retailer_price = retailer_price_list_test,
  df_hhid_conditional = conditionals,
  activist_price_list = rep(0.05, 122),
  hhid_activist_list = hhid_activist_list_alpha_1percent,
  boycott_type = "hard",
  subsample = NULL,
  verbose = 1,
  precision = 0.001,
  initial_precision = 0.005
)
saveRDS(
  retailer_price_list_range_hard_alpha_1percent_0_70_02,
  "Inputs/retailer_price_list_range_hard_alpha_1percent_0_70_02.rds"
)
# 
# 
# # HARD, ALPHA 0.5%
# retailer_price_list_range_hard_alpha_05percent_0_70_02 = 
#   compute_equilibrium_price_boycott_range(
#     initial_retailer_price = retailer_price_list_test,
#     df_hhid_conditional = conditionals,
#     hhid_activist_list = hhid_activist_list_alpha_05percent,
#     boycott_type = "hard",
#     subsample = NULL,
#     verbose = 1,
#     precision = 0.001,
#     initial_precision = 0.005
#   )
# saveRDS(
#   retailer_price_list_range_hard_alpha_05percent_0_70_02, 
#   "Inputs/retailer_price_list_range_hard_alpha_05percent_0_70_02.rds"
# )                      






retailer_price_list_with_hard_boycott = readRDS("Inputs/retailer_price_list_range_hard_alpha_5percent_0_70_02_25c.rds")

generate_cross_product_derivative_and_demand(
  model, retailer_price_list_with_hard_boycott, conditionals,
  rep(0.25, 122), hhid_activist_list_alpha_5percent,
  "hard"
)%>%
  .$demand%>%
  .[relevant_product_number_list]



demand_test2 = generate_cross_product_derivative_and_demand(
  model, retailer_price_list_with_hard_boycott, conditionals,
  c(0.25, 122), hhid_activist_list_alpha_5percent,
  boycott_type = "hard"
)%>%
  .$demand

round(100*(retailer_price_list_with_hard_boycott-retailer_price_list_test)/retailer_price_list_test)
boycotted_products = data.frame(
  price_after = retailer_price_list_with_hard_boycott,
  price_before = retailer_price_list_test,
  demand_after = generate_cross_product_derivative_and_demand(
    model, retailer_price_list_with_hard_boycott, conditionals,
    rep(0.25, 122), hhid_activist_list_alpha_5percent,
    "hard"
  )%>%
    .$demand%>%
    .[relevant_product_number_list],
  demand_before = generate_cross_product_derivative_and_demand(
    model, retailer_price_list_test, conditionals)%>%
    .$demand%>%
    .[relevant_product_number_list],
  product_number = df_product$product_number
)%>%
  left_join(df_product)%>%
  mutate(
    percent_change = round(100*(price_after-price_before)/price_before),
    percent_change = round(100*(demand_after-demand_before)/demand_before)
    )

boycotted_products%>%group_by(label)%>%
  # summarise_at(vars(price_before, price_after), mean)%>%
  summarise_at(vars(demand_before, demand_after), sum)%>%
  mutate(
    #percent_change = round(100*(price_after-price_before)/price_before)
    percent_change = round(100*(demand_after-demand_before)/demand_before)
  )



# compute_equilibrium_price(model, start_price = retailer_price_list_test, conditionals)
#
# tic("Compute equilibrium price")
# retailer_price_list_with_hard_boycott = compute_equilibrium_price(
#     model, retailer_price_list_test, conditionals,
#     activist_price_list_test, hhid_activist_list_test,
#     boycott_type = "hard", precision = 0.0001, step = 0.25
#     )
# toc()
#df_product%>%mutate(price = retailer_price_list_with_hard_boycott)%>% View()
# 100 * (retailer_price_list_with_hard_boycott - retailer_price_list_test)/retailer_price_list_test
#
# retailer_price_list_without_activist1 = compute_equilibrium_price(
#     model, retailer_price_list_test, conditionals,
#     activist_price_list_test, hhid_activist_list_test,
#     boycott_type = "hard", subsample = "passive"
#     )
#
# retailer_price_list_without_activist2 = compute_equilibrium_price(
#     model, retailer_price_list_test, conditionals,
#     activist_price_list_test, hhid_activist_list_test,
#     boycott_type = "soft", subsample = "passive"
#     )
#
# The retailer price without activist should be the same, whatever the boycott type
# retailer_price_list_without_activist1 - retailer_price_list_without_activist2
#
# # Overvaluation effect
# 100*(retailer_price_list_without_activist1 - retailer_price_list_test)/retailer_price_list_test
# # Boycott effet
# 100*(retailer_price_list_with_hard_boycott - retailer_price_list_without_activist1)/retailer_price_list_without_activist1
#
# tic("Compute equilibrium price range without parallel")
# retailer_price_list_range = 
#   lapply(
#     X = boycott_price_range%>% as.list(),
#     FUN = function(x){rep(x, length(retailer_price_list_test))}
#     )%>%
#   lapply(
#     FUN = compute_equilibrium_price,
#     model = model, 
#     start_price = retailer_price_list_test, 
#     df_hhid_conditional = df_hhid_conditional,
#     hhid_activist_list = hhid_activist_list
#     )
# toc()

# compute_equilibrium_price_range = function(
#   fun = compute_equilibrium_price,
#   model = model, start_price = retailer_price_list_test, 
#   df_hhid_conditional = NULL,  hhid_activist_list = NULL,
#   boycott_type = NULL, subsample = NULL,
#   precision = NULL, step = NULL
# ){
#   
#   # Found on https://stackoverflow.com/questions/52190651/how-to-shut-down-an-open-r-cluster-connection-using-parallel
#   # Allow to free memory and shut down processes once computation has finished
#   # autoStopCluster <- function(cl) {
#   #   stopifnot(inherits(cl, "cluster"))
#   #   env <- new.env()
#   #   env$cluster <- cl
#   #   attr(cl, "gcMe") <- env
#   #   reg.finalizer(env, function(e) {
#   #     message("Finalizing cluster ...")
#   #     message(capture.output(print(e$cluster)))
#   #     try(parallel::stopCluster(e$cluster), silent = FALSE)
#   #     message("Finalizing cluster ... done")
#   #   })
#   #   cl
#   # }
#   # cluster = autoStopCluster(makeCluster(10))
#   
#   cluster = makeCluster(nb_cluster)
#   
#   package_loader = function(){
#     library(Matrix)
#     library(matlib)
#     library(apollo)
#     library(tidyverse)
#   }
#   
#   required_variable_list = list(
#     "belonging_matrix",
#     "relevant_product_number_list",
#     "generate_database",
#     "generate_cross_product_derivative_and_demand",
#     "cost_list",
#     "model",
#     "df_product_retailer",
#     "df_product",
#     "df_price",
#     "database_saved",
#     "database",
#     "conditionals",
#     "package_loader",
#     "hhid_activist_list",
#     "apollo_control",
#     "apollo_fixed",
#     "apollo_beta"
#   )
#   
#   tic("Broadcasting global variables in the cluster")
#   clusterExport(cl = cluster, varlist = required_variable_list)
#   toc()
#   
#   tic("Compute equilibrium price range in parallel")
#   retailer_price_list_range = clusterApply(
#     cl = cluster,
#     x = lapply(
#       X = boycott_price_range%>% as.list(),
#       FUN = function(x){rep(x, length(retailer_price_list_test))}
#     ),
#     fun = compute_equilibrium_price,
#     model = model, 
#     start_price = retailer_price_list_test, 
#     df_hhid_conditional = conditionals,
#     hhid_activist_list = hhid_activist_list,
#     boycott_type = boycott_type,
#     precision = precision, 
#     step = step
#   )
#   toc()
#   
#   stopCluster(cluster)
#   
#   retailer_price_list_range
#   
# }


# Computing the boycott effect for different boycott prices

# retailer_price_list_range = compute_equilibrium_price_range(
#   model = model, 
#   start_price = retailer_price_list_test, 
#   df_hhid_conditional = conditionals,
#   hhid_activist_list = hhid_activist_list,
#   boycott_type = boycott_type,
#   precision = 0.00001,
#   step = 1
# )

# retailer_price_list_range%>%
#   lapply(
#     FUN = function(x){
#       round(100*(x - retailer_price_list_test)/retailer_price_list_test)
#     }
#   )
retailer_price_list_range = readRDS("Inputs/price_range_hard_25_35_2.rds")
hhid_activist_list = hhid_activist_list_alpha_5percent
boycott_type = "hard"

demand_range = 1:length(retailer_price_list_range)%>%
  lapply(
    FUN = function(n){
      demand_and_matrix = generate_cross_product_derivative_and_demand(
        model, 
        retailer_price_list_range[[n]], 
        conditionals, 
        rep(boycott_price_range[[n]], length(retailer_price_list_test)), 
        hhid_activist_list, boycott_type = boycott_type
      )%>%
        .$demand%>%
        .[relevant_product_number_list]
    }
  )

activist_demand_range = 1:length(retailer_price_list_range)%>%
  lapply(
    FUN = function(n){
      demand_and_matrix = generate_cross_product_derivative_and_demand(
        model, 
        retailer_price_list_range[[n]], 
        conditionals, 
        rep(boycott_price_range[[n]], length(retailer_price_list_test)), 
        hhid_activist_list, boycott_type = boycott_type, subsample = "activist"
      )%>%
        .$demand%>%
        .[relevant_product_number_list]
    }
  )



########## PLOT AREA #############

boycott_price = readRDS("Inputs/retailer_price_list_range_soft_high_middle_5percent_0_70_02_25c.rds")
activist_price_list = rep(0.25, 122)
hhid_activist_list = hhid_activist_list_high_middle_5percent
boycott_type = "soft"

# boycott_price = readRDS("Inputs/retailer_price_list_range_hard_alpha_5percent_0_70_02_25c.rds")
# activist_price_list = rep(0.25, 122)
# hhid_activist_list = hhid_activist_list_alpha_5percent
# boycott_type = "hard"


initial_price = retailer_price_list_test

absence_price = compute_equilibrium_price(
  model,
  retailer_price_list_test,
  conditionals,
  rep(0,122),
  hhid_activist_list,
  boycott_type,
  precision = 0.000000001
)

boycott_demand = generate_cross_product_derivative_and_demand(
  model,
  boycott_price,
  conditionals,
  activist_price_list,
  hhid_activist_list,
  boycott_type,
)$demand%>% .[relevant_product_number_list]

boycott_demand_activist = generate_cross_product_derivative_and_demand(
  model,
  boycott_price,
  conditionals,
  activist_price_list,
  hhid_activist_list,
  boycott_type,
  "activist"
)$demand%>% .[relevant_product_number_list]

initial_demand = generate_cross_product_derivative_and_demand(
  model,
  retailer_price_list_test,
  conditionals
)$demand%>% .[relevant_product_number_list]

initial_demand_activist = generate_cross_product_derivative_and_demand(
  model,
  retailer_price_list_test,
  conditionals,
  rep(1.5, 122),
  hhid_activist_list,
  "soft",
  "activist"
)$demand%>% .[relevant_product_number_list]


absence_demand = generate_cross_product_derivative_and_demand(
  model,
  absence_price,
  conditionals,
  rep(0,122),
  hhid_activist_list,
  "hard"
)$demand%>% .[relevant_product_number_list]

df_product_level_boycott_effect = data.frame(
  product_number = df_product$product_number,
  initial_demand = initial_demand,
  initial_demand_activist = initial_demand_activist,
  absence_demand = absence_demand,
  boycott_demand = boycott_demand,
  boycott_demand_activist = boycott_demand_activist,
  initial_price = initial_price,
  absence_price = absence_price,
  boycott_price = boycott_price
)%>% left_join(df_product)

df_product_level_boycott_effect%>%
  mutate(
    boycott_price_effect_demand = round(100*((boycott_demand-boycott_demand_activist) - absence_demand)/absence_demand),
    boycott_price_effect_price = round(100*(boycott_price-absence_price)/absence_price),
    surevaluation_effect_demand = round(100*(absence_demand - (initial_demand-initial_demand_activist))/(initial_demand-initial_demand_activist)),
    surevaluation_effect_price = round(100*(absence_price-initial_price)/initial_price),
    activist_effect_demand = round(100*(boycott_demand_activist-initial_demand_activist)/initial_demand),
    total_effect_demand = round(100*(boycott_demand - initial_demand)/initial_demand),
    total_effect_price = round(100*(boycott_price - initial_price)/initial_price)
  )

df_boycott_effect = df_product_level_boycott_effect%>%
  group_by(label)%>%
  summarise(
    initial_demand = sum(initial_demand),
    initial_demand_activist = sum(initial_demand_activist),
    absence_demand = sum(absence_demand),
    boycott_demand = sum(boycott_demand),
    boycott_demand_activist = sum(boycott_demand_activist),
    initial_price = mean(initial_price, weights = initial_demand),
    absence_price = mean(absence_price, weights = absence_demand),
    boycott_price = mean(boycott_price, weights = boycott_demand)
  )%>%
  mutate(
    boycott_price_effect_demand = round(100*((boycott_demand-boycott_demand_activist) - absence_demand)/absence_demand),
    boycott_price_effect_price = round(100*(boycott_price-absence_price)/absence_price),
    surevaluation_effect_demand = round(100*(absence_demand - (initial_demand-initial_demand_activist))/(initial_demand-initial_demand_activist)),
    surevaluation_effect_price = round(100*(absence_price-initial_price)/initial_price),
    activist_effect_demand = round(100*(boycott_demand_activist-initial_demand_activist)/initial_demand),
    total_effect_demand = round(100*(boycott_demand - initial_demand)/initial_demand),
    total_effect_price = round(100*(boycott_price - initial_price)/initial_price),
    rounded_initial_price = format(round(initial_price,3),3),
    rounded_absence_price = format(round(absence_price,3),3),
    rounded_boycott_price = format(round(boycott_price,3),3),
    label = case_when(
      label == "labelbio" ~ "Biologique",
      label == "labelpleinair" ~ "Plein Air",
      label == "nolabel" ~ "En Cage"
    )
  )%>%
  select(
    label,
    rounded_initial_price,
    rounded_absence_price,
    rounded_boycott_price,
    total_effect_price,
    surevaluation_effect_price,
    boycott_price_effect_price,
    total_effect_demand,
    activist_effect_demand,
    surevaluation_effect_demand,
    boycott_price_effect_demand,
    )

# df_boycott_effect1 = df_boycott_effect
# df_boycott_effect2 = df_boycott_effect

bind_rows(
  df_boycott_effect1,
  df_boycott_effect2
)%>%
  # mutate(
  #   across(contains("effect")), ~paste(.,"%")
  #   )%>%
  rename(
    Label = label,
    `Prix initial` = rounded_initial_price,
    `Prix sans activiste` = rounded_absence_price,
    `Prix avec le boycott`= rounded_boycott_price,
    `Effet total du boycott sur le prix` = total_effect_price,
    `Effet de surévaluation sur le prix` = surevaluation_effect_price,
    `Effet du prix seuil sur le prix` = boycott_price_effect_price,
    `Effet total du boycott sur la demande` = total_effect_demand,
    `Effet de la consommation activiste sur la demande` = activist_effect_demand,
    `Effet de surévaluation sur la demande` = surevaluation_effect_demand,
    `Effet du prix seuil sur la demande` = boycott_price_effect_demand
  )%>%
  kbl()%>%
  kable_paper("striped", full_width = F)%>%
  column_spec (c(1,7),border_left = F, border_right = T)%>%
  pack_rows("Cas 1 : Boycott souple, 5% d'activistes parmi les ménages à assez haut revenu, prix de boycott 0,25€", 1,3)%>%
  pack_rows("Cas 2 : Boycott strict, 5% d'activistes parmi les ménages avec la plus faible sensiblité prix, prix de boycott 0,25€", 4,6)%>%
    footnote(general = "Les prix sont en euros. Les distributeurs anticipent la demande selon la configuration (1) pour la fixation des prix, dans laquelle chaque ménage aurait pu acheter chez tous les distributeurs
           ", footnote_as_chunk = T)
 #%>%  save_kable("Outputs/table_simulation_result.png")

##################################


# Similar data for the initial setting

initial_price = retailer_price_list_test

initial_demand = generate_cross_product_derivative_and_demand(
  model, retailer_price_list_test, conditionals, 
)%>% .$demand%>% .[relevant_product_number_list]

# data.frame(demand = initial_demand, product_number = df_product$product_number)%>%
#   left_join(df_product)%>%
#   group_by(label)%>%
#   summarise(demand = sum(demand))

## J'AI DETECTE UN PB, LE MODE SUBSAMPLE NE FONCTIONNE PAS SANS BOYCOTT
# initial_demand_passive = generate_cross_product_derivative_and_demand(
#   model, retailer_price_list_test, conditionals, subsample = "passive"
# )%>% .$demand%>% .[relevant_product_number_list]
# data.frame(demand = initial_demand_passive, product_number = df_product$product_number)%>%
#   left_join(df_product)%>%
#   group_by(label)%>%
#   summarise(demand = sum(demand))

initial_organic_price = df_price%>%
  filter(periode == 12)%>%
  group_by(label)%>%
  summarise(avg_price = mean(avg_price))%>%
  filter(label == "labelbio")%>%
  .[[1,"avg_price"]]


# Similar data for the setting without activists

price_without_activist = compute_equilibrium_price(
  model, retailer_price_list_test, conditionals,
  activist_price_list_test, hhid_activist_list_test,
  boycott_type = "hard", subsample = "passive"
  )

organic_price_without_activist = 
  data.frame(
    price = price_without_activist,
    label = df_product$label
  )%>%
  group_by(label)%>%
  summarise(avg_price = mean(price))%>%
  filter(label == "labelbio")%>%
  .[[1,"avg_price"]]



df_boycott_analysis = lapply(
  X = 1:length(boycott_price_range),
  FUN = function(n){
    data.frame(
      boycott_price = boycott_price_range[[n]],
      product_number = df_product$product_number,
      retailer_price = retailer_price_list_range[[n]],
      demand = demand_range[[n]],
      activist_demand = activist_demand_range[[n]]
    )
  }
)%>%
  bind_rows()%>%
  left_join(df_product)%>%
  mutate(passive_demand = demand - activist_demand)

df_boycott_analysis_label = df_boycott_analysis%>%
  group_by(boycott_price, label)%>%
  summarise(
    retailer_price = mean(retailer_price),
    demand = sum(demand),
    activist_demand = sum(activist_demand),
    passive_demand = sum(passive_demand)
  )

df_boycott_analysis_label%>%
  filter(
    #abs(boycott_price - 0.30) < 0.001,
    label == "labelbio"
    )


# Display parameters
offset = 0.37
scaling = 6000

ggplot( 
  df_boycott_analysis_label%>%
    filter(label == "labelbio")%>%
    pivot_longer(
      cols = c("activist_demand", "passive_demand"),
      names_to = "demand_type"
      ), 
  aes(x = boycott_price)
)+
  geom_col(aes(y = value, fill = demand_type), position = "stack")+
  geom_line(aes(y = (retailer_price-offset)*scaling), size = 1)+
  geom_hline(
    yintercept = (initial_organic_price-offset)*scaling,
    linetype = "longdash"
    )+
  geom_text(
    x = quantile(boycott_price_range, probs = 0.8),
    y = (initial_organic_price-offset)*scaling - 20,
    check_overlap = TRUE,
    label = "Initial price"
    )+
  geom_hline(
    yintercept = (organic_price_without_activist-offset)*scaling,
    linetype = "dotted"
  )+
  geom_text(
    x = quantile(boycott_price_range, probs = 0.8),
    y = (organic_price_without_activist-offset)*scaling + 20,
    check_overlap = TRUE,
    label = "Hypothetical price without activist"
  )+
  scale_y_continuous(
    name = NULL,
    breaks = NULL, position = "right",
    sec.axis = sec_axis( trans=~./scaling+offset, 
      name="Average retailer price for organic eggs",
      guide = guide_axis(position = "left"))
    )+
  scale_fill_manual(
    name = NULL,
    values = c("tomato1", "lightblue"),
    breaks = c("activist_demand", "passive_demand"),
    labels = c("Activist demand", "Passive demand"),
    aesthetics = c("fill")
  )+
  theme_bw()+
  xlab("Boycott price") +
  theme(legend.position = "bottom")
  



######################################
######################################
############## ARCHIVES ##############
######################################
######################################

## RECHERCHE GLOBALE D'UN PRIX D'EQUILIBRE

# Je veux f(P) = D(P) + Omega(P) (P-C) = 0 et des prix positifs plus petits que 1

# function_to_minimize = function(retailer_price_list)
# {
#   
#   # retailer_price_list = retailer_price_list_test
#   
#   #package_loader()
#   
#   activist_price_list = activist_price_list_test
#   hhid_activist_list = hhid_activist_list_test
#   
#   demand_and_matrix = generate_cross_product_derivative_and_demand(
#     model, retailer_price_list, conditionals,
#     activist_price_list, hhid_activist_list)
#   demand = demand_and_matrix$demand
#   cross_product_price_derivative_matrix = demand_and_matrix$matrix
#   
#   relevant_demand = demand %>% .[relevant_product_number_list]
#   relevant_cross_product_price_derivative_matrix = 
#     cross_product_price_derivative_matrix[relevant_product_number_list, relevant_product_number_list]
#   
#   omega = belonging_matrix * relevant_cross_product_price_derivative_matrix   
#   
#   error = norm(relevant_demand + omega %*% ((1-0.055)*retailer_price_list - cost_list))
#   
#   return(error) 
#   
# }
# 
# number_of_products = length(retailer_price_list_test)
# 
# # Useful rule of thumb for the computation time
# tic("10 functions evaluations")
# for (i in 1:10){
#   function_to_minimize(abs(rnorm(number_of_products)))
# }
# toc()


## TENTATIVE DE DIRECTL

# control_direct = list(
#   xtol_rel = 0.02,
#   stopval = 1,
#   maxeval = 100000
# )
# 
# # Optimisation pour un prix de boycott donné
# tic("Optimization with DirectL")
# fit_direct = directL(
#   fn = function_to_minimize,
#   lower = (0.8 * retailer_price_list_test),
#   upper = (1.2 * retailer_price_list_test),
#   # lower = rep(0, number_of_products-1),
#   # upper = rep(1, number_of_products-1),
#   control = control_direct,
#   nl.info = TRUE
# )
# toc()
# # 1h30 avec 0.8 et 1.2 * retailer_price_list_test
# # 
# # retailer_price_list_almost_converged = fit_direct$par
# # (retailer_price_list_almost_converged-retailer_price_list_test)/retailer_price_list_test
# df_product_retailer%>% mutate(change = (retailer_price_list_almost_converged-retailer_price_list_test)/retailer_price_list_test)%>% View()
# 
# cluster = makeCluster(nb_cores_cluster)
# setDefaultCluster(cluster)
# 
# package_loader = function(){
#   library(Matrix)
#   library(matlib)
#   library(apollo)
#   library(optimParallel)
#   library(tidyverse)
# }
# 
# create_fn = function(boycott_price){
#   
#   fn = function(truncated_retailer_price_list)
#   {
#     package_loader()
#     retailer_price_list = c(0, truncated_retailer_price_list)
#     demand_and_matrix = generate_cross_product_derivative_and_demand(
#       model, retailer_price_list, conditionals,
#       rep(boycott_price, length(truncated_retailer_price_list) + 1), hhid_activist_list)
#     demand = demand_and_matrix$demand
#     cross_product_price_derivative_matrix = demand_and_matrix$matrix
#     relevant_retailer_price = retailer_price_list[relevant_product_number_list]
#     relevant_demand = demand %>% .[relevant_product_number_list]
#     relevant_cross_product_price_derivative_matrix = 
#       cross_product_price_derivative_matrix[relevant_product_number_list, relevant_product_number_list]
#     omega = belonging_matrix * relevant_cross_product_price_derivative_matrix   
#     error = norm(relevant_demand + omega %*% ((1-0.055)*relevant_retailer_price - cost_list))
#     
#     return(error) 
#     
#   }
# }
# 
# fn_list = lapply(boycott_price_range %>% as.list, create_fn)
# 
# required_variable_list = list(
#   "belonging_matrix",
#   "relevant_product_number_list",
#   "generate_database",
#   "generate_cross_product_derivative_and_demand",
#   "cost_list",
#   "model",
#   "df_product_retailer",
#   "df_product",
#   "df_price",
#   "database_saved",
#   "database",
#   "conditionals",
#   "package_loader",
#   "hhid_activist_list"
# )
# 
# tic("Broadcasting global variables in the cluster")
# clusterExport(cl = cluster, varlist = required_variable_list)
# toc()
# 
# tic("Global optimization with DirectL")
# directL_results = clusterApply(
#   cluster,
#   fn_list, directL,
#   # lower = (0.5 * retailer_price_list_test)[2:number_of_products],
#   # upper = (1.5 * retailer_price_list_test)[2:number_of_products],
#   lower = rep(0, number_of_products-1),
#   upper = rep(1, number_of_products-1),
#   control = control_direct,
#   nl.info = TRUE
# )
# toc()
# 
# retailer_price_list_almost_converged_range = lapply(directL_results, function(x){x$par})
# 
# retailer_price_list_fully_converged = lapply(
#   3:length(retailer_price_list_almost_converged_range),
#   function(n){
#     compute_equilibrium_price(
#       model, start_price = c(0,retailer_price_list_almost_converged_range[[n]]), df_hhid_conditional,
#       activist_price_list = rep(boycott_price_range[[n]], length(retailer_price_list_test)), hhid_activist_list
#     )
#   }
# )



## TENTATIVE DE BFGS PARALLEL

# cluster = makeCluster(20)
# setDefaultCluster(cluster)
# 
# package_loader = function(){
#   library(Matrix)
#   library(matlib)
#   library(apollo)
#   library(optimParallel)
#   library(tidyverse)
# }
# 
# required_variable_list = list(
#   "belonging_matrix",
#   "relevant_product_number_list",
#   "generate_database",
#   "generate_cross_product_derivative_and_demand",
#   "hhid_activist_list_test",
#   "cost_list",
#   "activist_price_list_test",
#   "model",
#   "df_product_retailer",
#   "df_product",
#   "df_price",
#   "database_saved",
#   "database",
#   "conditionals",
#   "package_loader"
# )
# 
# tic("Broadcasting global variables in the cluster")
# clusterExport(cl = cluster, varlist = required_variable_list)
# toc()
# # 72s with makeCluster(20)
# 
# control_optim = list(
#   trace = 6,
#   factr = 0.1,
#   REPORT = 5,
#   maxit = 20
# )
# 
# tic("Optimization with optimParallel")
# fit_optim = optimParallel(
#   par = retailer_price_list_test[2:number_of_products],
#   fn = function_to_minimize,
#   lower = rep(0, length(retailer_price_list_test)-1),
#   upper = rep(1, length(retailer_price_list_test)-1),
#   control = control_optim,
#   parallel = list(cl = cluster)
# )
# toc()



## LA COMPLEXITE DE L'ALGO SUIVANT PEUT ETRE LARGEMENT OPTIMISE
## LES PROPRIETES DE CV SEMBLENT ENCORE PIRES QUE POUR L'ALGO INITIAL

## ET SI JE NE L'UTILISAIS QUE LORSQUE LES PRIX DOIVENT TOUS DESCENDRE ?

# compute_equilibrium_price_retailer_by_retailer =
#   function(model, start_price = retailer_price_list_test,  df_hhid_conditional = NULL,
#            activist_price_list = NULL, hhid_activist_list = NULL){
#     
#     relevant_product_number_list = df_product_retailer$product_number
#     full_cost_list = rep(0, length(retailer_price_list_test))
#     full_cost_list[relevant_product_number_list] = cost_list
#     
#     
#     retailer_list = df_product_retailer$retailer%>% unique()
#     new_retailer_price = retailer_price_list_test * 0.95
#     #new_retailer_price = abs(rnorm(length(retailer_price_list_test)))
#     
#     iteration_counter = 0
#     total_error = 1
#     
#     # On itere l'algo suivant tant que ça bouge d'au moins un centime sur une composante :
#     while (total_error > 0.05){
#       
#       total_error = 0
#       
#       for (current_retailer in retailer_list){
#         
#         # df_product_retailer
#         # current_retailer = "43_3"
#         
#         df_product_current_retailer = df_product_retailer%>% filter(retailer == current_retailer)
#         relevant_product_number_list = df_product_current_retailer$product_number
#         
#         current_retailer_price = new_retailer_price # + abs(rnorm(length(retailer_price_list_test)))/(10 * log(iteration_counter+3))
#         iteration_counter = iteration_counter+1
#         
#         # 1) On calcule D et Omega avec le modele estime vu le niveau des prix
#         
#         demand_and_matrix = generate_cross_product_derivative_and_demand(model, current_retailer_price, conditionals)
#         demand = demand_and_matrix$demand
#         cross_product_price_derivative_matrix = demand_and_matrix$matrix
#         
#         # 2) On calcule le prix optimal à partir des couts et de la demande
#         
#         relevant_cost_list = full_cost_list[relevant_product_number_list]
#         
#         relevant_demand = demand %>% .[relevant_product_number_list]
#         relevant_cross_product_price_derivative_matrix =
#           cross_product_price_derivative_matrix[relevant_product_number_list, relevant_product_number_list]
#         
#         relevant_belonging_matrix =
#           full_join(
#             df_product_current_retailer,
#             df_product_current_retailer,
#             by = c("constant")
#           )%>%
#           mutate(same_retailer = (retailer.x == retailer.y))%>%
#           select(-constant, -retailer.x, -retailer.y)%>%
#           pivot_wider(names_from = product_number.y, values_from = same_retailer)%>%
#           column_to_rownames("product_number.x")%>%
#           mutate_all(as.numeric)%>%
#           as.matrix()%>%
#           Matrix(sparse = TRUE)
#         
#         omega = relevant_belonging_matrix * relevant_cross_product_price_derivative_matrix
#         
#         # On rappelle que [1-tau]P = C - Omega^-1 D
#         new_relevant_retailer_price = (relevant_cost_list - solve(omega, relevant_demand) %>% as.vector())/(1-0.055)
#         new_retailer_price = current_retailer_price
#         new_retailer_price[relevant_product_number_list] = new_relevant_retailer_price
#         
#         
#         error = max(abs(current_retailer_price-new_retailer_price))
#         total_error = total_error + error
#         
#         print(paste("Retailer:", current_retailer))
#         print(paste("Error :", error))
#         
#       }
#       
#       print(paste("Iteration", iteration_counter))
#       print(paste("Total error :", total_error))
#       print(new_retailer_price[1:10])
#       
#     }
#   }

# compute_equilibrium_price_retailer_by_retailer(model, start_price = retailer_price_list_test, conditionals)





## AUTRE ALGO DE CALCUL DU PRIX D'EQUILIBRE








# 
# 
# compute_equilibrium_price_global = function(
#   model, candidate_retailer_price = NULL,  df_hhid_conditional = NULL,
#   activist_price_list = NULL, hhid_activist_list = NULL, 
#   boycott_type = NULL, subsample = NULL){
#   
#   # TEST 1
#   # candidate_retailer_price = retailer_price_list_test
#   # df_hhid_conditional = NULL
#   # activist_price_list = NULL
#   # hhid_activist_list = NULL
#   # boycott_type = NULL
#   # subsample = NULL
#   
#   retailer_list = df_product%>% .$retailer%>% unique()
#   current_retailer_price = candidate_retailer_price
#   precision = 0.01
#   error = 1
#   
#   while (error>precision){
#     
#     new_retailer_price = current_retailer_price
#     
#     tic("One iteration")
#     for (current_retailer in retailer_list){
#       
#       # current_retailer = retailer_list[3]
#       
#       tic(paste("Analyzing the best response for retailer", current_retailer))
#       
#       current_retailer_product_rank_list = df_product%>%
#         rowid_to_column("product_rank")%>%
#         filter(retailer == current_retailer)%>%
#         .$product_rank
#       
#       retailer_specific_profit = function(current_retailer_product_price){
#         
#         test_retailer_price = candidate_retailer_price
#         test_retailer_price[current_retailer_product_rank_list] = current_retailer_product_price
#         
#         demand_and_matrix = generate_cross_product_derivative_and_demand(
#           model, test_retailer_price, df_hhid_conditional, 
#           activist_price_list, hhid_activist_list, 
#           boycott_type, subsample
#         )
#         
#         relevant_demand = demand_and_matrix$demand%>%
#           .[relevant_product_number_list]
#         
#         # The "minus" is important as the DIRECT algorithm minimizes the function 
#         -sum(
#           (relevant_demand * (test_retailer_price - cost_list))%>%
#             .[current_retailer_product_rank_list]
#         )
#         
#       }
#       
#       if (is.null(activist_price_list)){
#         activist_price_list = rep(0, length(retailer_price_list_test))
#       }
#       
#       upper_limit = 1.2 * retailer_price_list_test[current_retailer_product_rank_list]
#       lower_limit = pmin(
#         0.8 * activist_price_list[current_retailer_product_rank_list], 
#         0.8 * retailer_price_list_test[current_retailer_product_rank_list]
#       )
#       
#       direct_result = direct(
#         fn = retailer_specific_profit,
#         up = upper_limit,
#         lower = lower_limit,
#         nl.info = FALSE,
#         control = list(
#           #stopval = current_profit*1.2, # stop minimization at this value
#           xtol_rel = 1e-2, # stop on small optimization step
#           maxeval = 10*length(current_retailer_product_rank_list) # stop on this many function evaluations
#           # ftol_rel = 0.0, # stop on change times function value
#           # ftol_abs = 0.0, # stop on small change of function value
#         )
#       )
#       
#       new_retailer_price[current_retailer_product_rank_list] = direct_result$par
#       
#       
#       toc()
#       
#     }
#     print(new_retailer_price)
#     current_retailer_price = new_retailer_price
#     
#   }
#   
#   
#   
# }
# 
# 
#
# 
# 
# 
# 
# 
# compute_candidate_price = function(
#   model, start_price = retailer_price_list_test,  df_hhid_conditional = NULL,
#   activist_price_list = NULL, hhid_activist_list = NULL, 
#   boycott_type = NULL, subsample = NULL,
#   precision = 0.000001, step = 1){
#   
#   # TEST 1
#   # start_price = retailer_price_list_test
#   # df_hhid_conditional = NULL
#   # activist_price_list = NULL
#   # hhid_activist_list = NULL
#   # boycott_type = NULL
#   # subsample = NULL
#   # precision = 0.000001
#   
#   # TEST 2
#   # start_price = abs(rnorm(length(retailer_price_list_test)))
#   # df_hhid_conditional = conditionals
#   # activist_price_list = activist_price_list_test
#   # hhid_activist_list = hhid_activist_list_test
#   # boycott_type = "hard"
#   # subsample = NULL
#   
#   package_loader()
#   
#   new_retailer_price = start_price
#   iteration_counter = 0
#   error = 1
#   current_step = step
#   
#   organic_product_list = df_product%>% 
#     rownames_to_column("product_list_rank")%>%
#     mutate_at(vars(product_list_rank), as.integer)%>%
#     filter(label == "labelbio")%>%
#     .$product_list_rank
#   
#   retailer_list = df_product$retailer %>% unique()
#   retailer_product_list = list()
#   retailer_organic_product_list = list()
#   
#   for (current_retailer in retailer_list){
#     
#     retailer_product_list[[current_retailer]] = df_product%>% 
#       rownames_to_column("product_list_rank")%>%
#       mutate_at(vars(product_list_rank), as.integer)%>%
#       filter(retailer == current_retailer)%>%
#       .$product_list_rank
#     
#     retailer_organic_product_list[[current_retailer]] = df_product%>% 
#       rownames_to_column("product_list_rank")%>%
#       mutate_at(vars(product_list_rank), as.integer)%>%
#       filter(
#         retailer == current_retailer,
#         label == "labelbio"
#         )%>%
#       .$product_list_rank
#   
#   }
#   
#   # On itere l'algo suivant tant que les prix bougent d'au moins precision a chaque etape
#   while (error > precision){
#     
#     current_retailer_price = new_retailer_price
#     iteration_counter = iteration_counter+1
#     
#     # On calcule les BR distributeur par distributeur
#     for (current_retailer in retailer_list){
#       
#       combination_list = powerSet(retailer_organic_product_list[[current_retailer]])
#       best_retailer_specific_price = NULL 
#       best_profit = 0
#       
#       # On considere pour chaque produit bio l'éventualite que le prix de boycott soit applique
#       for (i in 1:length(combination_list)){
#         
#         current_retailer_organic_product_at_boycott_price = combination_list[[i]]
#         current_retailer_organic_product_not_at_boycott_price =
#           retailer_product_list[[current_retailer]][
#             !(retailer_product_list[[current_retailer]] %in% current_retailer_organic_product_at_boycott_price)
#           ]
#           
#         
#         test_retailer_price = current_retailer_price
#         test_retailer_price[current_retailer_organic_product_at_boycott_price] = 
#           activist_price_list[current_retailer_organic_product_at_boycott_price] -
#           0.0001 # Pour eviter les erreurs numeriques dues aux comparaisons d'égalite entre flottants
#         
#         for (product_rank in retailer_product_list[[current_retailer]]){
#           
#           retailer_specific_profit = function(product_price){
#             
#             test_retailer_price[product_rank] = product_price
#             
#             demand_and_matrix = generate_cross_product_derivative_and_demand(
#               model, test_retailer_price, conditionals, 
#               activist_price_list, hhid_activist_list, 
#               boycott_type, subsample
#             )
#             
#             relevant_demand = demand_and_matrix$demand%>%
#               .[relevant_product_number_list]
#             
#             profit = sum( 
#               (relevant_demand * (test_retailer_price - cost_list))%>%
#                  .[retailer_product_list[[current_retailer]]]
#             ) 
#             profit
#           }
#           
#           # retailer_specific_profit(0.4)
#           
#           test_retailer_price[product_rank] = optimize(
#             f = retailer_specific_profit,
#             lower = 0,
#             upper = 1,
#             tol = error/10
#           )%>% .$minimum
#         }
#         
#         
#         # 
#         
#         
#         
#         relevant_demand = demand_and_matrix$demand%>%
#           .[relevant_product_number_list]
# 
#         omega =
#           (belonging_matrix%>%
#              .[current_retailer_organic_product_not_at_boycott_price, current_retailer_organic_product_not_at_boycott_price]) *
#           (demand_and_matrix$matrix%>%
#              .[relevant_product_number_list, relevant_product_number_list]%>%
#              .[current_retailer_organic_product_not_at_boycott_price, current_retailer_organic_product_not_at_boycott_price])
# 
#         
#         # On rappelle que [1-tau]P = C - Omega^-1 D 
#         # On n'applique cette formule qu'aux prix qui sont determines par la FOC,
#         # pas ceux qui sont determines par le prix de boycott
#         test_retailer_price[current_retailer_organic_product_not_at_boycott_price] = 
#           (1-current_step) * test_retailer_price[current_retailer_organic_product_not_at_boycott_price] +
#           current_step *
#           (cost_list[current_retailer_organic_product_not_at_boycott_price] - 
#              solve(omega, relevant_demand[current_retailer_organic_product_not_at_boycott_price])%>% as.vector())/(1-0.055)
#         
#         profit = sum( (relevant_demand * (new_retailer_price - cost_list))%>% .[retailer_product_list[[current_retailer]]] ) 
#         
#         if (profit > best_profit){
#           best_retailer_specific_price = test_retailer_price[retailer_product_list[[current_retailer]]]
#           best_profit = profit
#         }
#         
#       }
#     
#       new_retailer_price[retailer_product_list[[current_retailer]]] = test_retailer_price[retailer_product_list[[current_retailer]]]
#         
#     }
#     
#     previous_error = error
#     error = max(abs(current_retailer_price-new_retailer_price))
#     
#     print(paste("Iteration", iteration_counter))
#     print(paste("Step size :", current_step))
#     print(paste("Error :", error))
#     print(new_retailer_price[organic_product_list])
#     
#   }
#   
#   
#   return(new_retailer_price)
# }








# 
# 
# test_nash_equilibrium = function(
#   model, candidate_retailer_price = NULL,  df_hhid_conditional = NULL,
#   activist_price_list = NULL, hhid_activist_list = NULL, 
#   boycott_type = NULL, subsample = NULL){
#   
#   # TEST 1
#   # candidate_retailer_price = retailer_price_list_test
# df_hhid_conditional = NULL
# activist_price_list = NULL
# hhid_activist_list = NULL
# boycott_type = NULL
# subsample = NULL
#   
#   retailer_list = df_product%>% .$retailer%>% unique()
#   
#   for (current_retailer in retailer_list){
#     
#     current_retailer = retailer_list[3]
#     
#     tic(paste("Analyzing the best response for retailer", current_retailer))
#     
#     current_retailer_product_rank_list = df_product%>%
#       rowid_to_column("product_rank")%>%
#       filter(retailer == current_retailer)%>%
#       .$product_rank
#       
#     retailer_specific_profit = function(current_retailer_product_price){
#       
#       test_retailer_price = candidate_retailer_price
#       test_retailer_price[current_retailer_product_rank_list] = current_retailer_product_price
#         
#       demand_and_matrix = generate_cross_product_derivative_and_demand(
#           model, test_retailer_price, df_hhid_conditional, 
#           activist_price_list, hhid_activist_list, 
#           boycott_type, subsample
#         )
#         
#       relevant_demand = demand_and_matrix$demand%>%
#         .[relevant_product_number_list]
#        
#       # The "minus" is important as the DIRECT algorithm minimizes the function 
#       -sum(
#         (relevant_demand * (test_retailer_price - cost_list))%>%
#           .[current_retailer_product_rank_list]
#       )
#       
#     }
#     
#     if (is.null(activist_price_list)){
#       activist_price_list = rep(0, length(retailer_price_list_test))
#     }
#     
#     # start_test_profit_function = Sys.time()
#     # for (i in 1:10){
#     #   retailer_specific_profit(abs(rnorm(length(current_retailer_product_rank_list))))
#     # }
#     # end_test_profit_function = Sys.time()
#     # print(paste("Estimated computation time for this retailer : 100 *", end_test_profit_function - start_test_profit_function))
#     
#     current_retailer_price = candidate_retailer_price[current_retailer_product_rank_list]
#     
#     upper_limit = 1.2 * retailer_price_list_test[current_retailer_product_rank_list]
#     lower_limit = pmin(0.8 * activist_price_list[current_retailer_product_rank_list], 0.8 * retailer_price_list_test[current_retailer_product_rank_list])
#     current_profit = retailer_specific_profit(current_retailer_price)
#     
#     direct_result = direct(
#       fn = retailer_specific_profit,
#       up = upper_limit,
#       lower = lower_limit,
#       nl.info = TRUE,
#       control = list(
#         #stopval = current_profit*1.2, # stop minimization at this value
#         xtol_rel = 1e-2, # stop on small optimization step
#         maxeval = 100 # stop on this many function evaluations
#         # ftol_rel = 0.0, # stop on change times function value
#         # ftol_abs = 0.0, # stop on small change of function value
#       )
#     )
#     
#     (direct_result$par - lower_limit) / (upper_limit - lower_limit)
#     
#     retailer_specific_profit(direct_result$par)
#     
#     print(paste0("relative_error = ", round(100*(direct_result$value - current_profit)/current_profit, digits = 2), "%"))
#     
#     candidate_retailer_price[current_retailer_product_rank_list]
#     
#     relative_error = (direct_result$value - current_profit)/current_profit
#     print(relative_error)
#       
#   }
#   toc()
#     
# }
# 
# 
# 





