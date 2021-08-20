
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")


#### MODEL TESTING

model_nosale = apollo_loadModel("Inputs/Apollo/nosale")
model_nosale_with_control = apollo_loadModel("Inputs/Apollo/nosale_with_control")
model_nosale_lnorm = apollo_loadModel("Inputs/Apollo/nosale_lnorm")
model_nosale_lnorm_with_control = apollo_loadModel("Inputs/Apollo/nosale_lnorm_with_control")

## Summary

apollo_modelOutput(model_nosale)
apollo_modelOutput(model_nosale_with_control)
apollo_modelOutput(model_nosale_lnorm)
apollo_modelOutput(model_nosale_lnorm_with_control)

-model_nosale$estimate/model_nosale$estimate[["b_price"]] # - 0.171740688

-model_nosale_with_control$estimate/model_nosale_with_control$estimate[["b_price"]] # - 0.175589617

median_price_sensibility = exp(model_nosale_lnorm$estimate[["mu_lnorm"]])
model_nosale_lnorm$estimate/median_price_sensibility # - 0.277500738

median_price_sensibility = exp(model_nosale_lnorm_with_control$estimate[["mu_lnorm"]])
model_nosale_lnorm_with_control$estimate/median_price_sensibility # - 0.289264380

value_organic_egg_lnorm = 
  model_nosale_lnorm$estimate[["b_labelbio"]]+
  model_nosale_lnorm$estimate[["b_medium"]]

value_organic_egg_lnorm_with_control = 
  model_nosale_lnorm_with_control$estimate[["b_labelbio"]]+
  model_nosale_lnorm_with_control$estimate[["b_medium"]]

ggplot(
  data = data.frame(price = 1:100/200)%>%
    mutate(
      density_nosale_lnorm = 
        dlnorm(
          x = value_organic_egg_lnorm/price, 
          meanlog = model_nosale_lnorm$estimate[["mu_lnorm"]], 
          sdlog = model_nosale_lnorm$estimate[["sigma_lnorm"]]
          ) * value_organic_egg_lnorm / price^2,
      density_nosale_lnorm_with_control = 
        dlnorm(
          x = value_organic_egg_lnorm_with_control /price, 
          meanlog = model_nosale_lnorm_with_control$estimate[["mu_lnorm"]], 
          sdlog = model_nosale_lnorm_with_control$estimate[["sigma_lnorm"]]
        ) * value_organic_egg_lnorm_with_control / price^2
    ),
  aes(x = price)
)+
  geom_line(aes(y = density_nosale_lnorm, color = "Without control"), size = 1)+
  geom_line(aes(y = density_nosale_lnorm_with_control, color = "With control"))+
  geom_vline(xintercept = 0.345)+
  geom_vline(xintercept = 0.15, linetype = "dashed")+
  scale_colour_discrete("Model specification")+
  xlab("Willingness to pay for organic product")+
  ylab("Estimated consumer density")+
  labs(
    caption = "The dashed vertical line indicates the production cost
    The plain vertical line indicates the average retail price")

ggsave(
  "Outputs/plot_density_wtp.png", device = "png",
  width = 15, height = 9, unit = "cm"
  )


## With VS without controls

apollo_lrTest(model_nosale_lnorm, model_nosale_lnorm_with_control)
apollo_basTest(model_nosale_lnorm, model_nosale_lnorm_with_control)


## With VS without random coefficient

apollo_lrTest(model_nosale_with_control, model_nosale_lnorm_with_control)
apollo_basTest(model_nosale_with_control, model_nosale_lnorm_with_control)



#### BAYESIAN POSTERIORS ANALYSIS

# ## Loading model inputs
# nb_halton_draw = 50
# nb_core = 3
# apollo_initialise()
# base_beta = c(
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
#   "b_valqvol" = 0
# )
# database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo.rds"))
# df_product = as.data.frame(readRDS("Inputs/product_with_nosale.rds"))%>%
#   select(marque_simple, calibre, label)%>%
#   mutate_all(as.character)
# rep_nb = as.integer(nrow(database_before_merge) / (nrow(df_product) + 1))
# res_nb = as.integer(nrow(database_before_merge) %% (nrow(df_product) + 1))
# database = database_before_merge%>%
#   mutate(
#     marque_simple = 
#       c(
#         rep(c(df_product$marque_simple, NA_character_), rep_nb),
#         as.character(df_product$marque_simple)[1:res_nb]
#       ),
#     label = 
#       c(
#         rep(c(df_product$label, NA_character_), rep_nb),
#         df_product$label[1:res_nb]
#       ),
#     calibre = 
#       c(
#         rep(c(df_product$calibre, NA_character_), rep_nb),
#         df_product$calibre[1:res_nb]
#       ),
#   )%>%
#   mutate_at(vars(marque_simple, label, calibre), as.factor)
# apollo_control = list(
#   mixing = TRUE,
#   modelName = "nosale_lnorm_with_control",
#   modelDescr = "",
#   indivID = "hhid",
#   workInLogs = FALSE,
#   nCores = nb_core
# )
# apollo_beta = c(
#   base_beta,
#   "b_nosale" = 0,
#   "mu_lnorm" = -3,
#   "sigma_lnorm" = 0,
#   "b_control" = 0
# )
# apollo_fixed = c("b_nosale", "b_medium", "b_nolabel", "b_calibreM")
# apollo_draws = list(
#   interNormDraws = c("draws_norm_lognorm"),
#   interDrawsType = "halton",
#   interNDraws = nb_halton_draw
# )
# apollo_randCoeff = function (apollo_beta , apollo_inputs) {
#   randcoeff = list()
#   randcoeff[["b_price"]] = - exp(mu_lnorm + sigma_lnorm*draws_norm_lognorm)
#   return (randcoeff)
# }
# apollo_inputs=apollo_validateInputs()
# apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
#   apollo_attach( apollo_beta, apollo_inputs)
#   on.exit( apollo_detach( apollo_beta, apollo_inputs) )
#   P = list()
#   good_label = (label[cumsum(is.na(label)) > 0])[2:sum(cumsum(is.na(label)) > 0)]
#   good_marque_simple = (marque_simple[cumsum(is.na(marque_simple)) > 0])[2:sum(cumsum(is.na(marque_simple)) > 0)]
#   good_calibre = (calibre[cumsum(is.na(calibre)) > 0])[2:sum(cumsum(is.na(calibre)) > 0)]
#   residual_label = (good_label[cumsum(is.na(good_label)) > 0])[2:sum(cumsum(is.na(good_label)) > 0)]
#   J = length(good_label)- length(residual_label) - 1
#   V = list()
#   for(j in 1:J) V[[paste0("alt",j)]] =
#     get( paste0("b_", good_label[j])) +
#     get( paste0("b_", good_marque_simple[j])) +
#     get( paste0("b_", good_calibre[j])) +
#     b_price * get(paste0(j, "_price")) +
#     b_control * get(paste0(j, "_control")) +
#     b_valqvol * get(paste0(j, "_valqvol"))
#   mnl_settings = list(
#     alternatives  = setNames(1:J, names(V)),
#     avail         = setNames(apollo_inputs$database[,paste0(1:J, "_price_avl")], names(V)),
#     choiceVar     = choice,
#     V             = V
#   )
#   P[["model"]] = apollo_mnl(mnl_settings , functionality)
#   P = apollo_panelProd(P, apollo_inputs , functionality)
#   P = apollo_avgInterDraws(P, apollo_inputs , functionality)
#   P = apollo_prepareProb(P, apollo_inputs , functionality)
#   return(P)
# }
# 
# ## Conditionals
# 
# conditionals = apollo_conditionals(model_nosale_lnorm_with_control, apollo_probabilities, apollo_inputs)
# 
# posterior = conditionals%>%
#   mutate(
#     price_sensibility = 
#    wtp = - model_nosale_lnorm_with_control$estimate["b_labelbio"] / post.mean
#   )%>%
#   left_join(
#     household_extended,
#     by = c("ID"=  "hhid")
#   )
# 
# saveRDS(posterior, "Inputs/posterior_nosale_lnorm_with_control.rds")

conditionnals = readRDS("Inputs/conditionnals_lnorm.rds")

posterior = conditionnals%>%
  mutate(
    wtp = value_organic_egg_lnorm_with_control/post.mean,
    min_age2 = min_age * min_age 
    )

fit_posterior_demographics = lm(
  -post.mean ~ clas + cycle + habi_inra,
  data = posterior
)
stargazer(
  fit_posterior_demographics,
  type = "text",
  #type = "html"
  )
# A EXPORTER, CORRIGER, INTEGRER


ggplot(
  posterior,
  aes(x = -wtp)
)+
  geom_density()+
  scale_x_continuous(limits = c(0, 0.5))






ggplot()+
  geom_density(
    data= posterior, 
    aes(x=wtp, color = "TOTAL")
    )+
  # geom_density(
  #   data = posterior %>% filter(young_highly_educated_single), 
  #   aes(x=wtp, color = "YHES")
  #   )+
  # geom_density(
  #   data = posterior %>% filter(educated_parents_middle_high_income), 
  #   aes(x=wtp, color = "EPMHI")
  # )+
  geom_density(
    data = posterior %>% filter(clas == "HIGH INCOME"), 
    aes(x=wtp, color = "HIGH INCOME")
  )+
  geom_density(
    data = posterior %>% filter(clas == "LOW MIDDLE INCOME"), 
    aes(x=wtp, color = "LOW MIDDLE INCOME")
  )+
  geom_density(
    data = posterior %>% filter(clas == "HIGH MIDDLE INCOME"), 
    aes(x=wtp, color = "HIGH MIDDLE INCOME")
  )+
  geom_density(
    data = posterior %>% filter(clas == "LOW INCOME"), 
    aes(x=wtp, color = "LOW INCOME")
  )+
  scale_x_continuous(limits = c(0,0.3))
  
posterior%>% filter(wtp > 0.12)
# 0.12 c'est à peu près le troisième quartile


price_sensitive_consumers = conditionals%>%
  arrange(post.mean)%>%
  .[1:5,]

price_insensitive_consumers = conditionals%>%
  arrange(-post.mean)%>%
  .[1:5,]

database%>%
  left_join(price_sensitive_consumers, by = c("hhid" = "ID"))%>%
  left_join(household)


####################################################


#### EFFECT OF A CHANGE IN PRICE

database_before_merge = as.data.frame(readRDS("Inputs/choice_situation_with_nosale_for_apollo_testing.rds"))

df_product = as.data.frame(readRDS("Inputs/product_with_nosale.rds"))%>%
  select(marque_simple, calibre, label)%>%
  mutate_all(as.character)

## RECONSTRUCT A MODIFIED VERSION OF THE DATABASE (IN THE NOSALE CASE)

percent_price_decrease = 0
organic_product_list = df_product%>%
  rownames_to_column("product_nb")%>%
  filter(label == "labelbio")%>%
  .$product_nb

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

apollo_inputs = apollo_validateInputs()


## FORECAST MARKET SHARES

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



#### WHO CONSUMES ORGANIC PRODUCTS ?

household = readRDS("Inputs/household.rds")
consumption = readRDS("Inputs/shopping_trips_without_nosale.rds")

household_extended = consumption%>%
  group_by(hhid)%>%
  summarise(share_bio = sum(label == "labelbio")/n() )%>%
  left_join(household, by = "hhid")%>%
  left_join(conditionals, by = c("hhid" = "ID"))%>%
  rename(posterior = post.mean)%>%
  drop_na(posterior)%>%
  mutate(wtp = -coeff_bio/posterior)

hist(
  household_extended%>% 
    filter(share_bio>0)%>% 
    .$share_bio
  )  

ggplot(
  household_extended%>%
    filter(wtp < 0.5)
  )+
  geom_point(
    aes(x = wtp, y = share_bio)
  )+
  geom_smooth(
    formula = "y ~ x",
    aes(x = wtp, y = share_bio)
  )

ggplot(
  household_extended,
  aes(x = share_bio, y = wtp)
  )+
  geom_point()+
  geom_smooth(formula = "y ~ x", method = "loess")

## Share of organic consumption in the population



## Probit to predict share of organic consumption based on demographics


## Correlation between WTP and share of organic consumption



     