
if (Sys.info()["sysname"] == "Windows"){
  setwd("U:/price-information_organic-eggs")
} else {
  setwd("~/U/price-information_organic-eggs")
}

source("Scripts/0_Packages_Libraries.R")

start = Sys.time()

consumption = readRDS("Inputs/shopping_trips_without_nosale_20211222.rds")

#### DEFINING A REFERENCE PRICE AT THE PRODUCT LEVEL

## FOR THE MOST CONSUMED PRODUCTS, TAKE THE FIRST VALUE THAN SPANS OVER TWO QUANTILES OF THE DATA

nb_quantile = 10


df_price1 = consumption%>%
  group_by(label, calibre, marque, retailer, valqvol, periode)%>%
  mutate(nb_product_sold = n())%>%
  ungroup()%>%
  filter(nb_product_sold > 20)%>%
  group_by(label, calibre, marque, retailer, valqvol, periode)%>%
  summarise_at(vars(unit_price), lapply(1:(nb_quantile-1)/nb_quantile, function(p){~quantile(.,p)}))%>%
  pivot_longer(cols = 7:(nb_quantile+5), names_to = "order", values_to = "quantile")%>%
  mutate(
    order = as.numeric(substring(order, 11)),
    quantile_shifted = c(0, quantile[1:(length(quantile)-1)]),
    diff = quantile - quantile_shifted
  )%>%
  filter(diff == 0)%>%
  group_by(label, calibre, marque, retailer, valqvol, periode)%>%
  summarise(ref_price = min(quantile))


## FOR THE OTHER PRODUCTS, USE THE 3RD DECILE

df_price2 = consumption%>%
  group_by(label, calibre, marque, retailer, valqvol, periode)%>%
  summarise(decile_3 = quantile(unit_price, probs = 0.3))%>%
  left_join(df_price1)%>%
  mutate(
    ref_price = ifelse(!is.na(ref_price), ref_price, decile_3)
  )


## ALIGN REFERENCE PRICES FOR A GIVEN PRODUCT ACROSSS NUMBERS OF EGGS PER BOX

df_price3 = consumption%>%
  group_by(label, calibre, marque, retailer, valqvol, periode)%>%
  summarise(nb_product_sold = n())%>%
  group_by(label, calibre, retailer, periode)%>%
  mutate(max_nb_product_sold = max(nb_product_sold))%>%
  ungroup()%>%
  left_join(df_price2)%>%
  group_by(label, calibre, marque, retailer, periode)%>%
  mutate(
    ref_price_vol_adjusted = max(ref_price * (max_nb_product_sold == nb_product_sold))
  )%>%
  select(
    -nb_product_sold,
    -max_nb_product_sold,
    -decile_3
  )

## ADD SIMPLE PRICE AVERAGE AS AN ALTERNATIVE TO THE PREVIOUS PRICES

df_price4 = consumption%>%
  group_by(label, calibre, marque, marque_simple, retailer, valqvol, periode)%>%
  summarise(
    avg_price = mean(unit_price)
  )%>%
  ungroup() %>% left_join(df_price3)


saveRDS(df_price4,"Inputs/product_price_20211222.rds")


#### BUILDING A CONTROL FUNCTION AT THE INDIVIDUAL-PRODUCT LEVEL

## FIRST-STAGE EQUATION FOR THE INSTRUMENT REF PRICE (COEFF MU_JK) ####

df_price_for_regression = consumption%>%
  left_join(df_price4)%>%
  pivot_wider(names_from = c("label", "calibre", "marque", "retailer"), values_from = "ref_price")%>%
  select(unit_price, contains("calibre"))%>%
  mutate_all(~replace_na(., 0))

fit_lm_ref_price = lm(unit_price ~ . - 1, data = df_price_for_regression)

df_mu = data.frame(mu_value = fit_lm_ref_price$coefficient)%>%
  rownames_to_column("mu_name")%>%
  mutate(
    label = str_replace(str_extract(mu_name, "^[^_]*(?=_)"),"`",""),
    calibre = str_extract(mu_name, "calibre[A-Z]"),
    retailer = str_extract(mu_name, "[0-9]{1,2}_[0-9]{1,2}"),
    marque = str_replace(str_extract(mu_name, paste0("_[^_]+(?=_", retailer, ")")),"_","")
  )%>%
  select(-mu_name)

saveRDS(df_mu, "Inputs/control_first_stage_coefficients_20211222.rds")


## DETERMINE TO WHAT EXTENT HOUSEHOLDS FACE EXCESSIVE PRICES


control_residuals = consumption%>%
  mutate(v_value = fit_lm_ref_price$residuals)

control_residuals_agg = control_residuals%>%
  group_by(label, calibre, marque, retailer, hhid)%>%
  summarise(med_v = median(v_value))

control_residuals_distribution = control_residuals%>%
  group_by(label, calibre, marque, retailer)%>%
  summarise_at(vars(v_value), lapply(1:100, function(x){~quantile(., probs = x/100)}))%>%
  pivot_longer(cols = 5:104, names_to = "centile_nb", values_to = "centile_value")%>%
  mutate(
    centile_nb = case_when(
      str_detect(centile_nb, "[0-9][0-9][0-9]") ~ str_extract(centile_nb, "[0-9][0-9][0-9]"),
      str_detect(centile_nb, "[0-9][0-9]") ~ str_extract(centile_nb, "[0-9][0-9]"),
      str_detect(centile_nb, "[0-9]") ~ str_extract(centile_nb, "[0-9]")
    ),
    centile_nb = as.numeric(centile_nb)
  )

control_residuals_med_centile_hhid = control_residuals_distribution%>%
  right_join(control_residuals_agg)%>%
  filter(centile_value < med_v)%>%
  group_by(label, calibre, marque, retailer, hhid)%>%
  summarise(centile_v = max(centile_nb))%>%
  group_by(hhid)%>%
  summarise(med_centile_v = as.integer(median(centile_v)))

saveRDS(control_residuals_distribution, "Inputs/control_residuals_distribution_20211222.rds")
saveRDS(control_residuals_med_centile_hhid, "Inputs/control_residuals_med_centile_hhid_20211222.rds")



end = Sys.time()

print("Duration : ")
print(end-start)

# Environ 2 minutes