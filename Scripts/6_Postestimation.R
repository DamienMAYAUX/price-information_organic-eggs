
if (Sys.info()["sysname"] == "Windows"){
  setwd("U:/price-information_organic-eggs")
} else {
  setwd("~/U/price-information_organic-eggs")
}

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
model_nosale_lnorm$estimate/median_price_sensibility # - 0.2720228153

median_price_sensibility = exp(model_nosale_lnorm_with_control$estimate[["mu_lnorm"]])
model_nosale_lnorm_with_control$estimate/median_price_sensibility # - 0.284381017

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
  xlab("Willingness to pay for organic eggs")+
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

conditionnals = readRDS("Inputs/conditionals_lnorm_with_control.rds")
household = readRDS("Inputs/household.rds")
consumption = readRDS("Inputs/shopping_trips_without_nosale.rds")

household_organic_share = consumption%>%
  group_by(hhid)%>%
  summarise(share_bio = sum(label == "labelbio")/n() )

posterior = conditionnals%>%
  left_join(household, by = c("ID" = "hhid"))%>%
  rename(
    hhid = ID,
    price_sensibility = post.mean
    )%>%
  mutate(
    wtp = -value_organic_egg_lnorm_with_control/price_sensibility,
    min_age2 = max_age * max_age 
    )%>%
  left_join(household_organic_share)
  

# Regressions

fit_posterior_demographics = lm(
  -price_sensibility ~ rve + habi_inra,
  data = posterior
)

stargazer(
  fit_posterior_demographics,
  type = "latex"
  )


# Figures

ggplot(
  posterior,
  aes(x = wtp)
)+
  geom_density()+
  scale_x_continuous(limits = c(0, 0.5))

table_segment = posterior%>%
  mutate(
    yhes_dummy = young_highly_educated_single,
    epmhi_dummy = educated_parents_middle_high_income,
    peak2_dummy = (wtp > 2),
    peak1_dummy = (wtp > 1),
    share80_dummy = (share_bio > 0.8),
    share10_dummy = (share_bio > 0.1),
    all_dummy = TRUE
  )%>%
  group_by(all_dummy)%>%
  mutate(
    all_mass = n()/3899,
    all_wtp_mean = mean(wtp),
    all_wtp_logmean = mean(log(wtp)),
    all_wtp_sd = sd(wtp),
    all_wtp_logsd = sd(log(wtp)),
    all_organic_share = mean(share_bio)
  )%>%
  ungroup()%>%
  group_by(peak2_dummy)%>%
  mutate(
    peak2_mass = n()/3899,
    peak2_wtp_mean = mean(wtp),
    peak2_wtp_logmean = mean(log(wtp)),
    peak2_wtp_sd = sd(wtp),
    peak2_wtp_logsd = sd(log(wtp)),
    peak2_organic_share = mean(share_bio)
  )%>%
  ungroup()%>%
  group_by(peak1_dummy)%>%
  mutate(
    peak1_mass = n()/3899,
    peak1_wtp_mean = mean(wtp),
    peak1_wtp_logmean = mean(log(wtp)),
    peak1_wtp_sd = sd(wtp),
    peak1_wtp_logsd = sd(log(wtp)),
    peak1_organic_share = mean(share_bio)
  )%>%
  ungroup()%>%
  group_by(share80_dummy)%>%
  mutate(
    share80_mass = n()/3899,
    share80_wtp_mean = mean(wtp),
    share80_wtp_logmean = mean(log(wtp)),
    share80_wtp_sd = sd(wtp),
    share80_wtp_logsd = sd(log(wtp)),
    share80_organic_share = mean(share_bio)
  )%>%
  ungroup()%>%
  group_by(share10_dummy)%>%
  mutate(
    share10_mass = n()/3899,
    share10_wtp_mean = mean(wtp),
    share10_wtp_logmean = mean(log(wtp)),
    share10_wtp_sd = sd(wtp),
    share10_wtp_logsd = sd(log(wtp)),
    share10_organic_share = mean(share_bio)
  )%>%
  ungroup()%>%
  group_by(yhes_dummy)%>%
  mutate(
    yhes_mass = n()/3899,
    yhes_wtp_mean = mean(wtp),
    yhes_wtp_logmean = mean(log(wtp)),
    yhes_wtp_sd = sd(wtp),
    yhes_wtp_logsd = sd(log(wtp)),
    yhes_organic_share = mean(share_bio)
  )%>%
  ungroup()%>%
  group_by(epmhi_dummy)%>%
  mutate(
    epmhi_mass = n()/3899,
    epmhi_wtp_mean = mean(wtp),
    epmhi_wtp_logmean = mean(log(wtp)),
    epmhi_wtp_sd = sd(wtp),
    epmhi_wtp_logsd = sd(log(wtp)),
    epmhi_organic_share = mean(share_bio)
  )%>%
  ungroup()%>%
  select(
    contains(c("all", "share80", "share10", "peak2", "peak1", "yhes", "epmhi"))
    )%>%
  # Creates an appropriate table
  unique()%>%
  drop_na()%>%
  pivot_longer(
    cols = contains("dummy"),
    names_to = "subset",
    values_to = "dummy"
  )%>%
  mutate(
    subset = gsub("_dummy", "", subset)
  )%>% 
  pivot_longer(
    cols = !contains(c("dummy", "subset", "dummy")),
    names_to = "statistic"
    )%>% 
  filter(str_detect(statistic, subset))%>%
  unique()%>%
  mutate(
    statistic = gsub("^[^_]*_", "", statistic)
  )%>%
  arrange(subset, dummy, statistic)%>%
  select(subset, dummy, statistic, value)%>%
  pivot_wider(names_from = statistic, values_from = value)

stargazer(table_segment, summary = FALSE)



ggplot(
  posterior,
)+
  geom_histogram(aes(x = share_bio), binwidth = 0.05)+
  scale_x_continuous(name = "Share of organic egg purchases by the household")+
  scale_y_continuous(name = "Number of households", limits = c(0,250))+
  labs(caption = "2732 households (70%) that never purchase organic eggs are not represented here")

posterior%>%
  filter(share_bio < 0.05)%>%
  nrow()
2864/3899

ggsave(
  device = "png", 
  file = "Outputs/share_bio_household.png", 
  units = "cm",
  width = 15,
  height = 15
)

ggplot(
  posterior,
)+
  geom_histogram(aes(x = wtp), binwidth = 0.1)+
  scale_x_continuous(name = "Willingness to pay for organic eggs", limits = c(0,4))+
  scale_y_log10(name = "Number of households")+
  labs(caption = "The vertical scale is logarithmic")

ggsave(
  device = "png", 
  file = "Outputs/wtp_household.png", 
  units = "cm",
  width = 20,
  height = 15
)



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



     
     
     