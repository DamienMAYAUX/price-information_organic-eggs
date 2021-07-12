library("tidyverse")


directory_path = "D:/Users/Louise/Desktop/PSE/Mémoire/Code/Scripts/Microeconometrics"
setwd(directory_path)

choice_situation_without_prices_df = readRDS("Inputs/choice_situation_without_price.rds")%>%
  mutate_at(vars(valqvol), as.numeric)

df_product_price = choice_situation_without_prices_df%>%
  group_by(marque, marque_simple, calibre, label, valqvol, periode, retailer)%>%
  summarise(avg_price = median(price, na.rm = TRUE))


df_for_fit = choice_situation_without_prices_df%>%
  left_join(df_product_price)

fit = lm(
  avg_price ~ calibre + marque_simple + label + valqvol,
  choice_situation_without_prices_df%>%
    left_join(df_product_price)
  )

# df_residuals = df_for_fit%>%
#   mutate(control = 100 * fit$residuals)%>%
#   select(-price)

choice_situation_with_average_prices_df = choice_situation_without_prices_df%>%
  left_join(df_product_price)%>%
  mutate(control = 100 * fit$residuals)%>%
  select(-price)%>%
  rename(price = avg_price)
# %>%
#   mutate(
#     price = ifelse(marque_simple == "other", 0, price),
#     calibre = ifelse(marque_simple == "other", "calibreM", calibre),
#     label = ifelse(marque_simple == "other", "nolabel", label),
#     valqvol = ifelse(marque_simple == "other", 0, valqvol),
#     control = ifelse(marque_simple == "other", 0, control)
#     )

df_product = choice_situation_with_average_prices_df%>%
  count(marque, marque_simple, calibre, label, valqvol)%>%
  select(-"n")%>%
  mutate(product_number = 1:n())

saveRDS(df_product, "df_product.rds")


df_shopping_visits = choice_situation_with_average_prices_df%>%
  select(X, hhid, periode)%>%
  unique()%>%
  group_by(hhid, periode)%>%
  mutate(
    rank = 1:n(),
    keep = sample(1:n(), 1)
    )%>%
  ungroup()%>%
  filter(rank == keep)

choice_situation_for_optimization = choice_situation_with_average_prices_df%>%
  # On passe de 76000 à 43000 valeurs de X environ
  semi_join(df_shopping_visits)%>%
  left_join(df_product)%>%
  select(X, hhid, choice, product_number, control, valqvol, price, label, calibre, marque_simple)%>%
  group_by(X)%>%
  mutate(
    choice = max(choice * product_number)
  )%>%
  ungroup()%>%
  mutate_at(vars(X, hhid, choice, valqvol), as.integer)%>%
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
  mutate(across(ends_with('price'), list(avl = ~as.numeric(!is.na(.))), "{.col}_{.fn}"))

saveRDS(choice_situation_for_optimization, "dataforApollo.rds")


hhid_list = choice_situation_for_optimization$hhid%>% unique()
hhid_subsample_list = sample(hhid_list, 200)

choice_situation_for_optimization_lighter = choice_situation_for_optimization%>%
  filter(hhid %in% hhid_subsample_list)

saveRDS(choice_situation_for_optimization_lighter, "dataforApollolighter.rds")






