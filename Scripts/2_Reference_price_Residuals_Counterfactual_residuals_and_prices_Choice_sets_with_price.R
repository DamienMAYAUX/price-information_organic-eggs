
library(tidyverse)
library(quantreg)
library(stargazer)

directory_path = "C:/Users/d.mayaux/Desktop/PSE/Mémoire/Code/Scripts/Microeconometrics"
setwd(directory_path)


consumption_filtered_again = readRDS("Inputs/data_fully_filtered.rds")
choice_situation_df = readRDS("Inputs/choice_situation_without_price.rds")



#### WE NOTICE THE EXISTENCE OF A BASELINE PRICE ####

ggplot(
  consumption_filtered_again%>%
    group_by(retailer, product_label_chosen)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    filter(sales > 1300)%>%
    mutate(product_label_retailer = paste(product_label_chosen, retailer, sep = '_'))%>%
    rename(
      `eggs per box` = valqvol,
      `sales date` = dateachat,
      `price per egg` = unit_price
      )
  )+
  geom_point(aes(x = `sales date`, y = `price per egg` , color = `eggs per box`))+
  facet_grid(rows = "product_label_retailer", scales = "free")

ggsave(
  "Outputs/reference_price/Price_patterns_most_common_couple_product_retailer_1.png", 
  device = "png", width = 10, height = 10)

ggplot(
  consumption_filtered_again%>%
    group_by(retailer, product_label_chosen)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    filter(sales < 1300, sales > 1000, valqvol != 1, valqvol != 6)%>%
    mutate(product_label_retailer = paste(product_label_chosen, retailer, sep = '_'))%>%
    rename(
      `eggs per box` = valqvol,
      `sales date` = dateachat,
      `price per egg` = unit_price
    )
)+
  geom_point(aes(x = `sales date`, y = `price per egg`, color = `eggs per box`))+
  facet_grid(rows = "product_label_retailer", scales = "free")

ggsave(
  "Outputs/reference_price/Price_patterns_most_common_couple_product_retailer_2.png", 
  device = "png", width = 10, height = 15)



#### WE COMPUTE THE REFERENCEPRICE FOR EVERY ####
#### PRODUCT, RETAILER AND PERIOD ####


price = consumption_filtered_again%>%
  select(periode, hhid, product_label_chosen, retailer, unit_price, valqvol)%>%
  mutate(
    valqvol = as.factor(valqvol),
    product_label_retailer_valqvol = 
      paste(product_label_chosen, retailer, valqvol, sep = '_')
  )

nb_quantile = 10


# Calcul du prix de référence pour tous les couples produits / retailers
# Un peu long
ref_price = consumption_filtered_again%>%
  select(periode, hhid, product_label_chosen, retailer, unit_price, valqvol)%>%
  mutate(
    valqvol = as.factor(valqvol),
    product_label_retailer_valqvol = 
      paste(product_label_chosen, retailer, valqvol, sep = '_')
  )%>%
  select(product_label_retailer_valqvol, periode, unit_price)%>%
  group_by(product_label_retailer_valqvol, periode)%>%
  mutate(nb_product_sold = n())%>%
  ungroup()%>%
  filter(nb_product_sold > 20)%>%
  group_by(product_label_retailer_valqvol, periode)%>%
  summarise_at(vars(unit_price), lapply(1:(nb_quantile-1)/nb_quantile, function(p){~quantile(.,p)}))%>%
  pivot_longer(cols = 3:(nb_quantile+1), names_to = "order", values_to = "quantile")%>%
  mutate(
    order = as.numeric(substring(order, 11)),
    quantile_shifted = c(0, quantile[1:(length(quantile)-1)]),
    diff = quantile - quantile_shifted
  )%>%
  filter(diff == 0)%>%
  group_by(product_label_retailer_valqvol, periode)%>%
  summarise(ref_price = min(quantile))

# Visualisation du résultat sur les exemples du début

ggplot(
  consumption_filtered_again%>%
    group_by(retailer, product_label_chosen)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    filter(sales > 1300, valqvol == 10)%>%
    mutate(
      product_label_retailer_valqvol = paste(product_label_chosen, retailer, valqvol, sep = '_'),
      product_label_retailer = paste(product_label_chosen, retailer, sep = '_'))%>%
    left_join(ref_price)%>%
    rename(
      `eggs per box` = valqvol,
      `sales date` = dateachat,
      `price per egg` = unit_price
    )
)+
  geom_point(aes(x = `sales date`, y = `price per egg`, color = 'actual price'))+
  geom_point(aes(x = `sales date`, y = ref_price, color  = 'reference price'), size = 0.5)+
  facet_grid(row = vars(`product_label_retailer`), scales = "free")

ggsave(
  "Outputs/reference_price/Reference_price_most_common_couple_product_retailer_1.png", 
  device = "png", width = 10, height = 10)


ggplot(
  consumption_filtered_again%>%
    group_by(retailer, product_label_chosen)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    filter(sales < 1300, sales > 1000, valqvol == 12)%>%
    mutate(
      product_label_retailer_valqvol = paste(product_label_chosen, retailer, valqvol, sep = '_'),
      product_label_retailer = paste(product_label_chosen, retailer, sep = '_'))%>%
    left_join(ref_price)%>%
    rename(
      `eggs per box` = valqvol,
      `sales date` = dateachat,
      `price per egg` = unit_price
    )
)+
  geom_point(aes(x = `sales date`, y = `price per egg`, color = 'actual price'))+
  geom_point(aes(x = `sales date`, y = ref_price, color = 'reference price'), size = 0.5)+
  facet_grid(row = vars(`product_label_retailer`), cols = vars(`eggs per box`), scales = "free")
# On notera qu'il manque un prix de référence 
# pour une des dernières périodes du troisième élément

ggsave(
  "Outputs/reference_price/Reference_price_most_common_couple_product_retailer_2.png", 
  device = "png", width = 15, height = 20)


# On compare le nombre de produit x retailer pour lesquels 
# on a un prix de référence pour certaines periodes
# avec le nombre de produit x retailer total
nrow(ref_price%>% group_by(product_label_retailer_valqvol)%>%
       summarise(nb = n()))
nrow(consumption_filtered_again%>%
       group_by(product_label_chosen, retailer, valqvol)%>%
       summarise(nb = n()))
# Envrion 6%

# Même chose au niveau produit x retailer x periode
nrow(ref_price)
nrow(consumption_filtered_again%>%
       group_by(product_label_chosen, retailer, valqvol, periode)%>%
       summarise(nb = n()))
# Environ 6%

# On regarde la part des ventes que cela représente
product_label_retailer_valqvol_periode_df = ref_price%>%
  select(-ref_price)%>%
  unique()
nrow(consumption_filtered_again%>%
  mutate(product_label_retailer_valqvol = 
           paste(product_label_chosen, retailer, valqvol, sep = '_'))%>%
  inner_join(product_label_retailer_valqvol_periode_df))
nrow(consumption_filtered_again)
# Environ 50% des actes de ventes ont une prix de reference bien defini


# Pour la moitié restante, on prend le 3e décile comme prix de référence
ref_price_completed = consumption_filtered_again%>%
  select(periode, hhid, product_label_chosen, retailer, unit_price, valqvol)%>%
  mutate(
    valqvol = as.factor(valqvol),
    product_label_retailer_valqvol = 
      paste(product_label_chosen, retailer, valqvol, sep = '_')
  )%>%
  select(product_label_retailer_valqvol, periode, unit_price)%>%
  group_by(product_label_retailer_valqvol, periode)%>%
  summarise(decile_3 = quantile(unit_price, probs = 0.3))%>%
  left_join(ref_price)%>%
  mutate(
    ref_price_completed = ifelse(!is.na(ref_price), ref_price, decile_3)
    )

# Est-ce que ces deux valeurs se ressemblent quand elles sont bien définies ?
ref_price_completed%>%
  filter(!is.na(ref_price))%>%
  mutate(same = (ref_price == decile_3))%>%
  .$same%>%
  sum()
ref_price_completed%>%
  filter(!is.na(ref_price))%>%
  nrow()

# On peut regarder les 70 cas pour lesquels c'est different en se demandant 
# si ça valait la peine de prendre une définition compliquee 
# alors que le 3e decile a l'air de suffire
ref_price_completed%>%
  right_join(
    consumption_filtered_again%>%
      mutate(product_label_retailer_valqvol = paste(product, label, retailer, valqvol, sep = "_"))
    )%>%
  filter(!is.na(ref_price), ref_price != decile_3)%>%
  nrow()
# Il y a environ 2600 achats pour lesquels ça fait une difference
# On garde donc la definition compliquee, au cas ou  


#### WE CORRECT THE PRICE OF EGGS FOR ####
#### VARIATIONS DUE TO EGG BOX SIZE ####

# On ajuste la valeur des prix dans le cas 
# où il y a plusieurs nombre d'oeufs possibles
ref_price_adjusted = consumption_filtered_again%>%
  mutate(
    product_label_retailer_valqvol = paste(product,label,retailer,valqvol, sep = '_')
  )%>%
  group_by(product_label_retailer_valqvol, product, label, retailer, valqvol, periode)%>%
  summarise(nb_product_sold = n())%>%
  group_by(product, label, retailer, periode)%>%
  mutate(max_nb_product_sold = max(nb_product_sold))%>%
  ungroup()%>%
  left_join(ref_price_completed)%>%
  group_by(retailer, product, label, periode)%>%
  mutate(
    ref_price_adjusted = max(ref_price_completed * (max_nb_product_sold == nb_product_sold))
  )%>%
  select(
    -product_label_retailer_valqvol, 
    -nb_product_sold, 
    -max_nb_product_sold,
    -decile_3,
    -ref_price
    )

saveRDS(ref_price_adjusted, "Inputs/ref_price_adjusted.rds")

consumption_adjusted = consumption_filtered_again%>%
  left_join(ref_price_adjusted)%>%
  mutate(
    unit_price_adjusted = unit_price * ref_price_adjusted / ref_price_completed
  )%>%
  select(-ref_price_completed)

saveRDS(consumption_adjusted, "Inputs/consumption_with_adjusted_price.rds")



ggplot(
  consumption_adjusted%>%
    group_by(retailer, product_label_chosen)%>%
    mutate(sales = n())%>%
    ungroup()%>%
    left_join(
      consumption_adjusted%>%
        select(retailer, product_label_chosen, valqvol)%>%
        filter(valqvol != 1, valqvol != 20)%>%
        unique()%>%
        group_by(retailer,product_label_chosen)%>%
        summarise(nb_valqvol = n())
    )%>%
    filter(sales > 1000, valqvol != 20, valqvol != 1, nb_valqvol > 1)%>%
    mutate(
      product_label_retailer_valqvol = paste(product_label_chosen, retailer, valqvol, sep = '_'),
      product_label_retailer = paste(product_label_chosen, retailer, sep = '_'))%>%
    left_join(ref_price)%>%
    rename(
      `eggs per box` = valqvol,
      `sales date` = dateachat,
      `price per egg` = unit_price
    )
)+
  geom_point(aes(x = `sales date`, y = `price per egg`, color = 'actual price'), size = 2)+
  geom_point(aes(x = `sales date`, y = ref_price, color  = 'reference price'), size = 1.5)+
  geom_point(aes(x = `sales date`, y = unit_price_adjusted, color  = 'adjusted price'), size = 1)+
  geom_point(aes(x = `sales date`, y = ref_price_adjusted, color  = 'adjusted reference price'), size = 0.5)+
  facet_grid(rows = vars(product_label_retailer), cols = vars(`eggs per box`), scales = 'free')

ggsave(
  "Outputs/reference_price/Adjusted_price_most_common_couple_product_retailer.png", 
  device = "png", width = 10, height = 10)





#### WE COMPUTE THE TENDENCY TO OVERPRICE ####
### THE COEFFICIENTS MU_JK IN THE PDF DOCUMENT ####

qr_ref_price_df = consumption_adjusted%>%
  mutate(product_label_retailer = as.factor(paste(product, label, retailer, sep = '_')))%>%
  pivot_wider(names_from = "product_label_retailer", values_from = "ref_price_adjusted")%>%
  select(
    -X, -hhid, -product, -valqvol, -label, -unit_price, -retailer, 
    -periode, -dateachat, -product_label_chosen
    )%>%
  mutate_all(~replace_na(., 0))
  
# TRES LONG A EXECUTER
# fit_qr_ref_price = rq(unit_price_adjusted ~ . - 1,
#                       data = qr_ref_price_df)
# summary(fit_qr_ref_price)

fit_lm_ref_price = lm(unit_price_adjusted ~ . - 1, 
                      data = qr_ref_price_df)
summary(fit_lm_ref_price)
stargazer(fit_lm_ref_price, type = "html", out = "Outputs/summary_statistics/first_stage.htm")

mu_df = data.frame(mu_value = fit_lm_ref_price$coefficient)%>%
  rownames_to_column("mu_name")
# Les valeurs de mu obtenues semblent cohérentes
# Elles sont entre 0.9 et 1.5

#### WE COMPUTE THE RESIDUALS AND COUNTERFACTUAL RESIDUALS ####

residuals_v_df = consumption_adjusted%>%
  mutate(
    v_value = fit_lm_ref_price$residuals,
    #mu_value = (unit_price_adjusted - v_value)/ref_price_adjusted
  )

residuals_ind_df = residuals_v_df%>%
  group_by(product, label, retailer, hhid)%>%
  summarise(med_v = median(v_value))

residuals_distrib_df = residuals_v_df%>%
  group_by(product, label, retailer)%>%
  summarise_at(vars(v_value), lapply(1:100, function(x){~quantile(., probs = x/100)}))%>%
  pivot_longer(cols = 4:103, names_to = "centile_nb", values_to = "centile_value")%>%
  mutate(
    centile_nb = case_when(
      str_detect(centile_nb, "[0-9][0-9][0-9]") ~ str_extract(centile_nb, "[0-9][0-9][0-9]"),
      str_detect(centile_nb, "[0-9][0-9]") ~ str_extract(centile_nb, "[0-9][0-9]"),
      str_detect(centile_nb, "[0-9]") ~ str_extract(centile_nb, "[0-9]")
    ),
    centile_nb = as.numeric(centile_nb)
      )

residuals_med_centile_ind_df = residuals_distrib_df%>%
  right_join(residuals_ind_df)%>%
  filter(centile_value < med_v)%>%
  group_by(product, label, retailer, hhid)%>%
  summarise(centile_v = max(centile_nb))%>%
  group_by(hhid)%>%
  summarise(med_centile = median(centile_v))


#### WE COMPUTE THE COUNTERFACTUAL PRICES ####
#### INSERT THEM IN THE CHOICE SITUATION #### 
#### AND CONSUMPTION DATAFRAME ####

choice_situation_with_counterfactual_residuals_df = choice_situation_df%>%
  left_join(residuals_v_df)%>%
  select(-valqvol, -ref_price_adjusted)%>%
  left_join(residuals_med_centile_ind_df)%>%
  left_join(residuals_distrib_df, by = c("product", "label", "retailer", "med_centile" = "centile_nb"))%>%
  left_join(ref_price_adjusted, by = c("retailer", "product", "label", "periode"))%>%
  left_join(ref_price_adjusted%>%
              group_by(product, label, retailer)%>%
              summarise(med_ref_price_adjusted = median(ref_price_adjusted)))%>%
  mutate(
    ref_price_counterfactual = ifelse(is.na(ref_price_adjusted), med_ref_price_adjusted, ref_price_adjusted),
    v_value_counterfactual = ifelse(choice, v_value, centile_value),
    unit_price_counterfactual = ifelse(choice, unit_price_adjusted, ref_price_counterfactual + v_value_counterfactual)
  )%>%
  select(
    -price, -unit_price, -unit_price_adjusted, -ref_price_adjusted, -v_value, -med_centile, -valqvol,
    -centile_value, -ref_price_completed, -product_label_chosen, -product_label, -med_ref_price_adjusted
  )

# Je ne conserve qu'un seul achat par periode par individu
X_chosen_df = consumption_adjusted%>%
  select(X, periode, hhid)%>%
  unique()%>%
  group_by(periode, hhid)%>%
  mutate(drawn = sample(X, size = 1))%>%
  ungroup()%>%
  filter(X == drawn)%>%
  select(X, periode)%>%
  unique()

choice_situation_with_counterfactual_residuals_smaller_df = choice_situation_with_counterfactual_residuals_df%>%
  semi_join(X_chosen_df)
# J'ai gagné environ un facteur 2 !

saveRDS(choice_situation_with_counterfactual_residuals_smaller_df, "Inputs/choice_situation_with_counterfactual_prices_v.rds")

