
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")
setwd("E:/Mémoire/Code/price-information_organic-eggs/Inputs")

apollo_initialise()

apollo_control = list(
  mixing = TRUE,
  modelName = "mixed_lnorm_control",
  modelDescr = "",
  indivID = "hhid", 
  workInLogs = FALSE, 
  nCores = 3
)

database = as.data.frame(readRDS("dataforApollolighter.rds"))

#### PASTE BETA / FIXED HERE ####

apollo_beta = c(
  "b_medium" = 0,
  "b_low" = 0,
  "b_indep" = 0,
  "b_high" = 0,
  "b_other" = 0,
  "b_labelbio" = 0,
  "b_labelpleinair" = 0,
  "b_nolabel" = 0,
  "b_label_other" = 0,
  "b_calibreL" = 0,
  "b_calibreM" = 0,
  "b_calibre_other" = 0,
  "b_control" = 0,
  "b_valqvol" = 0,
  "mu_lnorm" = 0,
  "sigma_lnorm" = 0
)


apollo_fixed = c("b_other", "b_label_other", "b_calibre_other", "b_control")

#######################################

apollo_draws = list(
  #interUnifDraws = c("draws_unif_exp"),
  interNormDraws = c("draws_norm_lognorm"),
  interDrawsType = "halton",
  interNDraws = 3
)

apollo_randCoeff = function (apollo_beta , apollo_inputs) {
  randcoeff = list()
  #randcoeff[["b_price"]] = - log(draws_unif_exp)
  randcoeff[["b_price"]] = - exp(mu_lnorm + sigma_lnorm*draws_norm_lognorm)
  return (randcoeff)
}



apollo_inputs=apollo_validateInputs()


apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach( apollo_beta, apollo_inputs)
  on.exit( apollo_detach( apollo_beta, apollo_inputs) )
  
  P = list()
  
  #### PASTE V HERE ####

  
  V = list()
  V[["AUCHAN_calibreL_labelbio_4"]] =
    b_labelbio + b_medium +
    b_calibreL +b_price * `1_price` +
    b_control * `1_control` + b_valqvol * `1_valqvol`
  V[["AUCHAN_calibreL_labelbio_6"]] =
    b_labelbio + b_medium +
    b_calibreL +b_price * `2_price` +
    b_control * `2_control` + b_valqvol * `2_valqvol`
  V[["AUCHAN_calibreM_labelbio_10"]] =
    b_labelbio + b_medium +
    b_calibreM +b_price * `3_price` +
    b_control * `3_control` + b_valqvol * `3_valqvol`
  V[["AUCHAN_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `4_price` +
    b_control * `4_control` + b_valqvol * `4_valqvol`
  V[["AUCHAN_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `5_price` +
    b_control * `5_control` + b_valqvol * `5_valqvol`
  V[["AUCHAN_calibreM_nolabel_6"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `6_price` +
    b_control * `6_control` + b_valqvol * `6_valqvol`
  V[["AUCHAN_calibreM_nolabel_12"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `7_price` +
    b_control * `7_control` + b_valqvol * `7_valqvol`
  V[["AUCHAN.PP POUCE_calibreM_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `8_price` +
    b_control * `8_control` + b_valqvol * `8_valqvol`
  V[["BABY COQUE_calibreL_labelbio_12"]] =
    b_labelbio + b_indep +
    b_calibreL +b_price * `9_price` +
    b_control * `9_control` + b_valqvol * `9_valqvol`
  V[["BABY COQUE_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreL +b_price * `10_price` +
    b_control * `10_control` + b_valqvol * `10_valqvol`
  V[["BABY COQUE_calibreL_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `11_price` +
    b_control * `11_control` + b_valqvol * `11_valqvol`
  V[["BABY COQUE_calibreL_nolabel_10"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `12_price` +
    b_control * `12_control` + b_valqvol * `12_valqvol`
  V[["BABY COQUE_calibreL_nolabel_20"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `13_price` +
    b_control * `13_control` + b_valqvol * `13_valqvol`
  V[["BABY COQUE_calibreM_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `14_price` +
    b_control * `14_control` + b_valqvol * `14_valqvol`
  V[["BABY COQUE_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `15_price` +
    b_control * `15_control` + b_valqvol * `15_valqvol`
  V[["BABY COQUE_calibreM_nolabel_12"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `16_price` +
    b_control * `16_control` + b_valqvol * `16_valqvol`
  V[["BIEN VU_calibreM_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `17_price` +
    b_control * `17_control` + b_valqvol * `17_valqvol`
  V[["BIEN VU_calibreM_nolabel_30"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `18_price` +
    b_control * `18_control` + b_valqvol * `18_valqvol`
  V[["BIO_calibreM_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `19_price` +
    b_control * `19_control` + b_valqvol * `19_valqvol`
  V[["BIO VILLAGE_calibreL_labelbio_10"]] =
    b_labelbio + b_high +
    b_calibreL +b_price * `20_price` +
    b_control * `20_control` + b_valqvol * `20_valqvol`
  V[["BIO VILLAGE_calibreM_labelbio_6"]] =
    b_labelbio + b_high +
    b_calibreM +b_price * `21_price` +
    b_control * `21_control` + b_valqvol * `21_valqvol`
  V[["CARREFOUR_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreL +b_price * `22_price` +
    b_control * `22_control` + b_valqvol * `22_valqvol`
  V[["CARREFOUR_calibreL_nolabel_6"]] =
    b_nolabel + b_medium +
    b_calibreL +b_price * `23_price` +
    b_control * `23_control` + b_valqvol * `23_valqvol`
  V[["CARREFOUR_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `24_price` +
    b_control * `24_control` + b_valqvol * `24_valqvol`
  V[["CARREFOUR_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `25_price` +
    b_control * `25_control` + b_valqvol * `25_valqvol`
  V[["CARREFOUR_calibreM_nolabel_6"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `26_price` +
    b_control * `26_control` + b_valqvol * `26_valqvol`
  V[["CARREFOUR_calibreM_nolabel_12"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `27_price` +
    b_control * `27_control` + b_valqvol * `27_valqvol`
  V[["CARREFOUR_calibreM_nolabel_20"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `28_price` +
    b_control * `28_control` + b_valqvol * `28_valqvol`
  V[["CARREFOUR.DISCOUNT_calibreM_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `29_price` +
    b_control * `29_control` + b_valqvol * `29_valqvol`
  V[["CARREFOUR.DISCOUNT_calibreM_nolabel_20"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `30_price` +
    b_control * `30_control` + b_valqvol * `30_valqvol`
  V[["CARREFOUR.DISCOUNT_calibreM_nolabel_30"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `31_price` +
    b_control * `31_control` + b_valqvol * `31_valqvol`
  V[["CASINO_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreL +b_price * `32_price` +
    b_control * `32_control` + b_valqvol * `32_valqvol`
  V[["CASINO_calibreL_nolabel_6"]] =
    b_nolabel + b_medium +
    b_calibreL +b_price * `33_price` +
    b_control * `33_control` + b_valqvol * `33_valqvol`
  V[["CASINO_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `34_price` +
    b_control * `34_control` + b_valqvol * `34_valqvol`
  V[["CASINO_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `35_price` +
    b_control * `35_control` + b_valqvol * `35_valqvol`
  V[["CASINO_calibreM_nolabel_6"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `36_price` +
    b_control * `36_control` + b_valqvol * `36_valqvol`
  V[["CASINO_calibreM_nolabel_12"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `37_price` +
    b_control * `37_control` + b_valqvol * `37_valqvol`
  V[["CASINO_calibreM_nolabel_20"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `38_price` +
    b_control * `38_control` + b_valqvol * `38_valqvol`
  V[["COCORETTE_calibreL_labelbio_4"]] =
    b_labelbio + b_indep +
    b_calibreL +b_price * `39_price` +
    b_control * `39_control` + b_valqvol * `39_valqvol`
  V[["COCORETTE_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreL +b_price * `40_price` +
    b_control * `40_control` + b_valqvol * `40_valqvol`
  V[["COCORETTE_calibreL_labelpleinair_10"]] =
    b_labelpleinair + b_indep +
    b_calibreL +b_price * `41_price` +
    b_control * `41_control` + b_valqvol * `41_valqvol`
  V[["COCORETTE_calibreM_labelbio_4"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `42_price` +
    b_control * `42_control` + b_valqvol * `42_valqvol`
  V[["COCORETTE_calibreM_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `43_price` +
    b_control * `43_control` + b_valqvol * `43_valqvol`
  V[["COCORETTE_calibreM_labelpleinair_4"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `44_price` +
    b_control * `44_control` + b_valqvol * `44_valqvol`
  V[["COCORETTE_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `45_price` +
    b_control * `45_control` + b_valqvol * `45_valqvol`
  V[["ECO+_calibreM_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `46_price` +
    b_control * `46_control` + b_valqvol * `46_valqvol`
  V[["GAULOIS LE_calibreL_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `47_price` +
    b_control * `47_control` + b_valqvol * `47_valqvol`
  V[["GAULOIS LE_calibreL_nolabel_12"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `48_price` +
    b_control * `48_control` + b_valqvol * `48_valqvol`
  V[["GAULOIS LE_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `49_price` +
    b_control * `49_control` + b_valqvol * `49_valqvol`
  V[["GAULOIS LE_calibreM_nolabel_20"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `50_price` +
    b_control * `50_control` + b_valqvol * `50_valqvol`
  V[["GAULOIS LE_calibreM_nolabel_30"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `51_price` +
    b_control * `51_control` + b_valqvol * `51_valqvol`
  V[["LIDL_calibreL_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreL +b_price * `52_price` +
    b_control * `52_control` + b_valqvol * `52_valqvol`
  V[["LIDL_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_low +
    b_calibreM +b_price * `53_price` +
    b_control * `53_control` + b_valqvol * `53_valqvol`
  V[["LIDL_calibreM_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `54_price` +
    b_control * `54_control` + b_valqvol * `54_valqvol`
  V[["LOUE_calibreL_labelbio_4"]] =
    b_labelbio + b_indep +
    b_calibreL +b_price * `55_price` +
    b_control * `55_control` + b_valqvol * `55_valqvol`
  V[["LOUE_calibreL_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreL +b_price * `56_price` +
    b_control * `56_control` + b_valqvol * `56_valqvol`
  V[["LOUE_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreL +b_price * `57_price` +
    b_control * `57_control` + b_valqvol * `57_valqvol`
  V[["LOUE_calibreM_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `58_price` +
    b_control * `58_control` + b_valqvol * `58_valqvol`
  V[["LOUE_calibreM_labelbio_10"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `59_price` +
    b_control * `59_control` + b_valqvol * `59_valqvol`
  V[["LOUE_calibreM_labelbio_12"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `60_price` +
    b_control * `60_control` + b_valqvol * `60_valqvol`
  V[["LOUE_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `61_price` +
    b_control * `61_control` + b_valqvol * `61_valqvol`
  V[["LOUE_calibreM_labelpleinair_10"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `62_price` +
    b_control * `62_control` + b_valqvol * `62_valqvol`
  V[["LOUE_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `63_price` +
    b_control * `63_control` + b_valqvol * `63_valqvol`
  V[["LUSTUCRU_calibreL_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreL +b_price * `64_price` +
    b_control * `64_control` + b_valqvol * `64_valqvol`
  V[["LUSTUCRU_calibreL_labelbio_10"]] =
    b_labelbio + b_indep +
    b_calibreL +b_price * `65_price` +
    b_control * `65_control` + b_valqvol * `65_valqvol`
  V[["LUSTUCRU_calibreL_labelpleinair_12"]] =
    b_labelpleinair + b_indep +
    b_calibreL +b_price * `66_price` +
    b_control * `66_control` + b_valqvol * `66_valqvol`
  V[["LUSTUCRU_calibreL_nolabel_4"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `67_price` +
    b_control * `67_control` + b_valqvol * `67_valqvol`
  V[["LUSTUCRU_calibreL_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `68_price` +
    b_control * `68_control` + b_valqvol * `68_valqvol`
  V[["LUSTUCRU_calibreL_nolabel_10"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `69_price` +
    b_control * `69_control` + b_valqvol * `69_valqvol`
  V[["LUSTUCRU_calibreL_nolabel_12"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `70_price` +
    b_control * `70_control` + b_valqvol * `70_valqvol`
  V[["LUSTUCRU_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `71_price` +
    b_control * `71_control` + b_valqvol * `71_valqvol`
  V[["LUSTUCRU_calibreM_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `72_price` +
    b_control * `72_control` + b_valqvol * `72_valqvol`
  V[["LUSTUCRU_calibreM_nolabel_12"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `73_price` +
    b_control * `73_control` + b_valqvol * `73_valqvol`
  V[["LUSTUCRU_calibreM_nolabel_20"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `74_price` +
    b_control * `74_control` + b_valqvol * `74_valqvol`
  V[["LUSTUCRU_calibreM_nolabel_30"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `75_price` +
    b_control * `75_control` + b_valqvol * `75_valqvol`
  V[["marque_other_calibre_other_label_other_0"]] =
    b_label_other + b_other +
    b_calibre_other +b_price * `76_price` +
    b_control * `76_control` + b_valqvol * `76_valqvol`
  V[["MARQUES ALDI_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_low +
    b_calibreM +b_price * `77_price` +
    b_control * `77_control` + b_valqvol * `77_valqvol`
  V[["MATINES_calibreL_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreL +b_price * `78_price` +
    b_control * `78_control` + b_valqvol * `78_valqvol`
  V[["MATINES_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreL +b_price * `79_price` +
    b_control * `79_control` + b_valqvol * `79_valqvol`
  V[["MATINES_calibreL_nolabel_4"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `80_price` +
    b_control * `80_control` + b_valqvol * `80_valqvol`
  V[["MATINES_calibreL_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `81_price` +
    b_control * `81_control` + b_valqvol * `81_valqvol`
  V[["MATINES_calibreL_nolabel_10"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `82_price` +
    b_control * `82_control` + b_valqvol * `82_valqvol`
  V[["MATINES_calibreL_nolabel_12"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `83_price` +
    b_control * `83_control` + b_valqvol * `83_valqvol`
  V[["MATINES_calibreL_nolabel_20"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `84_price` +
    b_control * `84_control` + b_valqvol * `84_valqvol`
  V[["MATINES_calibreM_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `85_price` +
    b_control * `85_control` + b_valqvol * `85_valqvol`
  V[["MATINES_calibreM_labelbio_12"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `86_price` +
    b_control * `86_control` + b_valqvol * `86_valqvol`
  V[["MATINES_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `87_price` +
    b_control * `87_control` + b_valqvol * `87_valqvol`
  V[["MATINES_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `88_price` +
    b_control * `88_control` + b_valqvol * `88_valqvol`
  V[["MATINES_calibreM_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `89_price` +
    b_control * `89_control` + b_valqvol * `89_valqvol`
  V[["MATINES_calibreM_nolabel_20"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `90_price` +
    b_control * `90_control` + b_valqvol * `90_valqvol`
  V[["MATINES_calibreM_nolabel_30"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `91_price` +
    b_control * `91_control` + b_valqvol * `91_valqvol`
  V[["MOISSON_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_high +
    b_calibreL +b_price * `92_price` +
    b_control * `92_control` + b_valqvol * `92_valqvol`
  V[["MOISSON_calibreL_nolabel_4"]] =
    b_nolabel + b_high +
    b_calibreL +b_price * `93_price` +
    b_control * `93_control` + b_valqvol * `93_valqvol`
  V[["MOISSON_calibreL_nolabel_6"]] =
    b_nolabel + b_high +
    b_calibreL +b_price * `94_price` +
    b_control * `94_control` + b_valqvol * `94_valqvol`
  V[["MOISSON_calibreM_labelbio_6"]] =
    b_labelbio + b_high +
    b_calibreM +b_price * `95_price` +
    b_control * `95_control` + b_valqvol * `95_valqvol`
  V[["MOISSON_calibreM_labelbio_10"]] =
    b_labelbio + b_high +
    b_calibreM +b_price * `96_price` +
    b_control * `96_control` + b_valqvol * `96_valqvol`
  V[["MOISSON_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_high +
    b_calibreM +b_price * `97_price` +
    b_control * `97_control` + b_valqvol * `97_valqvol`
  V[["MOISSON_calibreM_nolabel_12"]] =
    b_nolabel + b_high +
    b_calibreM +b_price * `98_price` +
    b_control * `98_control` + b_valqvol * `98_valqvol`
  V[["MOISSON_calibreM_nolabel_20"]] =
    b_nolabel + b_high +
    b_calibreM +b_price * `99_price` +
    b_control * `99_control` + b_valqvol * `99_valqvol`
  V[["MONOPRIX_calibreL_nolabel_6"]] =
    b_nolabel + b_medium +
    b_calibreL +b_price * `100_price` +
    b_control * `100_control` + b_valqvol * `100_valqvol`
  V[["MONOPRIX_calibreM_labelbio_6"]] =
    b_labelbio + b_medium +
    b_calibreM +b_price * `101_price` +
    b_control * `101_control` + b_valqvol * `101_valqvol`
  V[["MONOPRIX_calibreM_labelbio_10"]] =
    b_labelbio + b_medium +
    b_calibreM +b_price * `102_price` +
    b_control * `102_control` + b_valqvol * `102_valqvol`
  V[["MONOPRIX_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `103_price` +
    b_control * `103_control` + b_valqvol * `103_valqvol`
  V[["MONOPRIX_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `104_price` +
    b_control * `104_control` + b_valqvol * `104_valqvol`
  V[["MONOPRIX_calibreM_nolabel_20"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `105_price` +
    b_control * `105_control` + b_valqvol * `105_valqvol`
  V[["MONOPRIX GOURMET_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_high +
    b_calibreM +b_price * `106_price` +
    b_control * `106_control` + b_valqvol * `106_valqvol`
  V[["NETTO_calibreL_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreL +b_price * `107_price` +
    b_control * `107_control` + b_valqvol * `107_valqvol`
  V[["NETTO_calibreM_labelbio_6"]] =
    b_labelbio + b_low +
    b_calibreM +b_price * `108_price` +
    b_control * `108_control` + b_valqvol * `108_valqvol`
  V[["NETTO_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_low +
    b_calibreM +b_price * `109_price` +
    b_control * `109_control` + b_valqvol * `109_valqvol`
  V[["NETTO_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_low +
    b_calibreM +b_price * `110_price` +
    b_control * `110_control` + b_valqvol * `110_valqvol`
  V[["NETTO_calibreM_nolabel_6"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `111_price` +
    b_control * `111_control` + b_valqvol * `111_valqvol`
  V[["NETTO_calibreM_nolabel_12"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `112_price` +
    b_control * `112_control` + b_valqvol * `112_valqvol`
  V[["NETTO_calibreM_nolabel_30"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `113_price` +
    b_control * `113_control` + b_valqvol * `113_valqvol`
  V[["NOS REGIONS ONT DU TALENT_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_high +
    b_calibreL +b_price * `114_price` +
    b_control * `114_control` + b_valqvol * `114_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreL +b_price * `115_price` +
    b_control * `115_control` + b_valqvol * `115_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreL_nolabel_4"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `116_price` +
    b_control * `116_control` + b_valqvol * `116_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreL_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `117_price` +
    b_control * `117_control` + b_valqvol * `117_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreL_nolabel_10"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `118_price` +
    b_control * `118_control` + b_valqvol * `118_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreL_nolabel_12"]] =
    b_nolabel + b_indep +
    b_calibreL +b_price * `119_price` +
    b_control * `119_control` + b_valqvol * `119_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_labelbio_6"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `120_price` +
    b_control * `120_control` + b_valqvol * `120_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_labelbio_10"]] =
    b_labelbio + b_indep +
    b_calibreM +b_price * `121_price` +
    b_control * `121_control` + b_valqvol * `121_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `122_price` +
    b_control * `122_control` + b_valqvol * `122_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_indep +
    b_calibreM +b_price * `123_price` +
    b_control * `123_control` + b_valqvol * `123_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_nolabel_6"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `124_price` +
    b_control * `124_control` + b_valqvol * `124_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_nolabel_12"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `125_price` +
    b_control * `125_control` + b_valqvol * `125_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_nolabel_20"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `126_price` +
    b_control * `126_control` + b_valqvol * `126_valqvol`
  V[["OEUF DE NOS VILLAGES_calibreM_nolabel_30"]] =
    b_nolabel + b_indep +
    b_calibreM +b_price * `127_price` +
    b_control * `127_control` + b_valqvol * `127_valqvol`
  V[["OEUFS DE NOS REGIONS LES_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_high +
    b_calibreL +b_price * `128_price` +
    b_control * `128_control` + b_valqvol * `128_valqvol`
  V[["OEUFS DE NOS REGIONS LES_calibreL_nolabel_6"]] =
    b_nolabel + b_high +
    b_calibreL +b_price * `129_price` +
    b_control * `129_control` + b_valqvol * `129_valqvol`
  V[["OEUFS DE NOS REGIONS LES_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_high +
    b_calibreM +b_price * `130_price` +
    b_control * `130_control` + b_valqvol * `130_valqvol`
  V[["OEUFS DE NOS REGIONS LES_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_high +
    b_calibreM +b_price * `131_price` +
    b_control * `131_control` + b_valqvol * `131_valqvol`
  V[["OEUFS DE NOS REGIONS LES_calibreM_nolabel_12"]] =
    b_nolabel + b_high +
    b_calibreM +b_price * `132_price` +
    b_control * `132_control` + b_valqvol * `132_valqvol`
  V[["OEUFS DE NOS REGIONS LES_calibreM_nolabel_20"]] =
    b_nolabel + b_high +
    b_calibreM +b_price * `133_price` +
    b_control * `133_control` + b_valqvol * `133_valqvol`
  V[["REFLETS DE FRANCE_calibreL_labelbio_6"]] =
    b_labelbio + b_high +
    b_calibreL +b_price * `134_price` +
    b_control * `134_control` + b_valqvol * `134_valqvol`
  V[["REFLETS DE FRANCE_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_high +
    b_calibreM +b_price * `135_price` +
    b_control * `135_control` + b_valqvol * `135_valqvol`
  V[["TOP BUDGET_calibreM_nolabel_10"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `136_price` +
    b_control * `136_control` + b_valqvol * `136_valqvol`
  V[["TOP BUDGET_calibreM_nolabel_30"]] =
    b_nolabel + b_low +
    b_calibreM +b_price * `137_price` +
    b_control * `137_control` + b_valqvol * `137_valqvol`
  V[["U_calibreL_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreL +b_price * `138_price` +
    b_control * `138_control` + b_valqvol * `138_valqvol`
  V[["U_calibreL_nolabel_6"]] =
    b_nolabel + b_medium +
    b_calibreL +b_price * `139_price` +
    b_control * `139_control` + b_valqvol * `139_valqvol`
  V[["U_calibreM_labelbio_6"]] =
    b_labelbio + b_medium +
    b_calibreM +b_price * `140_price` +
    b_control * `140_control` + b_valqvol * `140_valqvol`
  V[["U_calibreM_labelbio_10"]] =
    b_labelbio + b_medium +
    b_calibreM +b_price * `141_price` +
    b_control * `141_control` + b_valqvol * `141_valqvol`
  V[["U_calibreM_labelpleinair_6"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `142_price` +
    b_control * `142_control` + b_valqvol * `142_valqvol`
  V[["U_calibreM_labelpleinair_12"]] =
    b_labelpleinair + b_medium +
    b_calibreM +b_price * `143_price` +
    b_control * `143_control` + b_valqvol * `143_valqvol`
  V[["U_calibreM_nolabel_12"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `144_price` +
    b_control * `144_control` + b_valqvol * `144_valqvol`
  V[["U_calibreM_nolabel_20"]] =
    b_nolabel + b_medium +
    b_calibreM +b_price * `145_price` +
    b_control * `145_control` + b_valqvol * `145_valqvol`
  
  
  
  avl = list(
    "AUCHAN_calibreL_labelbio_4" = `1_price_avl`,
    "AUCHAN_calibreL_labelbio_6" = `2_price_avl`,
    "AUCHAN_calibreM_labelbio_10" = `3_price_avl`,
    "AUCHAN_calibreM_labelpleinair_6" = `4_price_avl`,
    "AUCHAN_calibreM_labelpleinair_12" = `5_price_avl`,
    "AUCHAN_calibreM_nolabel_6" = `6_price_avl`,
    "AUCHAN_calibreM_nolabel_12" = `7_price_avl`,
    "AUCHAN.PP POUCE_calibreM_nolabel_10" = `8_price_avl`,
    "BABY COQUE_calibreL_labelbio_12" = `9_price_avl`,
    "BABY COQUE_calibreL_labelpleinair_6" = `10_price_avl`,
    "BABY COQUE_calibreL_nolabel_6" = `11_price_avl`,
    "BABY COQUE_calibreL_nolabel_10" = `12_price_avl`,
    "BABY COQUE_calibreL_nolabel_20" = `13_price_avl`,
    "BABY COQUE_calibreM_labelbio_6" = `14_price_avl`,
    "BABY COQUE_calibreM_labelpleinair_12" = `15_price_avl`,
    "BABY COQUE_calibreM_nolabel_12" = `16_price_avl`,
    "BIEN VU_calibreM_nolabel_10" = `17_price_avl`,
    "BIEN VU_calibreM_nolabel_30" = `18_price_avl`,
    "BIO_calibreM_labelbio_6" = `19_price_avl`,
    "BIO VILLAGE_calibreL_labelbio_10" = `20_price_avl`,
    "BIO VILLAGE_calibreM_labelbio_6" = `21_price_avl`,
    "CARREFOUR_calibreL_labelpleinair_6" = `22_price_avl`,
    "CARREFOUR_calibreL_nolabel_6" = `23_price_avl`,
    "CARREFOUR_calibreM_labelpleinair_6" = `24_price_avl`,
    "CARREFOUR_calibreM_labelpleinair_12" = `25_price_avl`,
    "CARREFOUR_calibreM_nolabel_6" = `26_price_avl`,
    "CARREFOUR_calibreM_nolabel_12" = `27_price_avl`,
    "CARREFOUR_calibreM_nolabel_20" = `28_price_avl`,
    "CARREFOUR.DISCOUNT_calibreM_nolabel_10" = `29_price_avl`,
    "CARREFOUR.DISCOUNT_calibreM_nolabel_20" = `30_price_avl`,
    "CARREFOUR.DISCOUNT_calibreM_nolabel_30" = `31_price_avl`,
    "CASINO_calibreL_labelpleinair_6" = `32_price_avl`,
    "CASINO_calibreL_nolabel_6" = `33_price_avl`,
    "CASINO_calibreM_labelpleinair_6" = `34_price_avl`,
    "CASINO_calibreM_labelpleinair_12" = `35_price_avl`,
    "CASINO_calibreM_nolabel_6" = `36_price_avl`,
    "CASINO_calibreM_nolabel_12" = `37_price_avl`,
    "CASINO_calibreM_nolabel_20" = `38_price_avl`,
    "COCORETTE_calibreL_labelbio_4" = `39_price_avl`,
    "COCORETTE_calibreL_labelpleinair_6" = `40_price_avl`,
    "COCORETTE_calibreL_labelpleinair_10" = `41_price_avl`,
    "COCORETTE_calibreM_labelbio_4" = `42_price_avl`,
    "COCORETTE_calibreM_labelbio_6" = `43_price_avl`,
    "COCORETTE_calibreM_labelpleinair_4" = `44_price_avl`,
    "COCORETTE_calibreM_labelpleinair_12" = `45_price_avl`,
    "ECO+_calibreM_nolabel_10" = `46_price_avl`,
    "GAULOIS LE_calibreL_nolabel_6" = `47_price_avl`,
    "GAULOIS LE_calibreL_nolabel_12" = `48_price_avl`,
    "GAULOIS LE_calibreM_labelpleinair_6" = `49_price_avl`,
    "GAULOIS LE_calibreM_nolabel_20" = `50_price_avl`,
    "GAULOIS LE_calibreM_nolabel_30" = `51_price_avl`,
    "LIDL_calibreL_nolabel_10" = `52_price_avl`,
    "LIDL_calibreM_labelpleinair_6" = `53_price_avl`,
    "LIDL_calibreM_nolabel_10" = `54_price_avl`,
    "LOUE_calibreL_labelbio_4" = `55_price_avl`,
    "LOUE_calibreL_labelbio_6" = `56_price_avl`,
    "LOUE_calibreL_labelpleinair_6" = `57_price_avl`,
    "LOUE_calibreM_labelbio_6" = `58_price_avl`,
    "LOUE_calibreM_labelbio_10" = `59_price_avl`,
    "LOUE_calibreM_labelbio_12" = `60_price_avl`,
    "LOUE_calibreM_labelpleinair_6" = `61_price_avl`,
    "LOUE_calibreM_labelpleinair_10" = `62_price_avl`,
    "LOUE_calibreM_labelpleinair_12" = `63_price_avl`,
    "LUSTUCRU_calibreL_labelbio_6" = `64_price_avl`,
    "LUSTUCRU_calibreL_labelbio_10" = `65_price_avl`,
    "LUSTUCRU_calibreL_labelpleinair_12" = `66_price_avl`,
    "LUSTUCRU_calibreL_nolabel_4" = `67_price_avl`,
    "LUSTUCRU_calibreL_nolabel_6" = `68_price_avl`,
    "LUSTUCRU_calibreL_nolabel_10" = `69_price_avl`,
    "LUSTUCRU_calibreL_nolabel_12" = `70_price_avl`,
    "LUSTUCRU_calibreM_labelpleinair_6" = `71_price_avl`,
    "LUSTUCRU_calibreM_nolabel_6" = `72_price_avl`,
    "LUSTUCRU_calibreM_nolabel_12" = `73_price_avl`,
    "LUSTUCRU_calibreM_nolabel_20" = `74_price_avl`,
    "LUSTUCRU_calibreM_nolabel_30" = `75_price_avl`,
    "marque_other_calibre_other_label_other_0" = `76_price_avl`,
    "MARQUES ALDI_calibreM_labelpleinair_6" = `77_price_avl`,
    "MATINES_calibreL_labelbio_6" = `78_price_avl`,
    "MATINES_calibreL_labelpleinair_6" = `79_price_avl`,
    "MATINES_calibreL_nolabel_4" = `80_price_avl`,
    "MATINES_calibreL_nolabel_6" = `81_price_avl`,
    "MATINES_calibreL_nolabel_10" = `82_price_avl`,
    "MATINES_calibreL_nolabel_12" = `83_price_avl`,
    "MATINES_calibreL_nolabel_20" = `84_price_avl`,
    "MATINES_calibreM_labelbio_6" = `85_price_avl`,
    "MATINES_calibreM_labelbio_12" = `86_price_avl`,
    "MATINES_calibreM_labelpleinair_6" = `87_price_avl`,
    "MATINES_calibreM_labelpleinair_12" = `88_price_avl`,
    "MATINES_calibreM_nolabel_6" = `89_price_avl`,
    "MATINES_calibreM_nolabel_20" = `90_price_avl`,
    "MATINES_calibreM_nolabel_30" = `91_price_avl`,
    "MOISSON_calibreL_labelpleinair_6" = `92_price_avl`,
    "MOISSON_calibreL_nolabel_4" = `93_price_avl`,
    "MOISSON_calibreL_nolabel_6" = `94_price_avl`,
    "MOISSON_calibreM_labelbio_6" = `95_price_avl`,
    "MOISSON_calibreM_labelbio_10" = `96_price_avl`,
    "MOISSON_calibreM_labelpleinair_12" = `97_price_avl`,
    "MOISSON_calibreM_nolabel_12" = `98_price_avl`,
    "MOISSON_calibreM_nolabel_20" = `99_price_avl`,
    "MONOPRIX_calibreL_nolabel_6" = `100_price_avl`,
    "MONOPRIX_calibreM_labelbio_6" = `101_price_avl`,
    "MONOPRIX_calibreM_labelbio_10" = `102_price_avl`,
    "MONOPRIX_calibreM_labelpleinair_6" = `103_price_avl`,
    "MONOPRIX_calibreM_labelpleinair_12" = `104_price_avl`,
    "MONOPRIX_calibreM_nolabel_20" = `105_price_avl`,
    "MONOPRIX GOURMET_calibreM_labelpleinair_6" = `106_price_avl`,
    "NETTO_calibreL_nolabel_10" = `107_price_avl`,
    "NETTO_calibreM_labelbio_6" = `108_price_avl`,
    "NETTO_calibreM_labelpleinair_6" = `109_price_avl`,
    "NETTO_calibreM_labelpleinair_12" = `110_price_avl`,
    "NETTO_calibreM_nolabel_6" = `111_price_avl`,
    "NETTO_calibreM_nolabel_12" = `112_price_avl`,
    "NETTO_calibreM_nolabel_30" = `113_price_avl`,
    "NOS REGIONS ONT DU TALENT_calibreL_labelpleinair_6" = `114_price_avl`,
    "OEUF DE NOS VILLAGES_calibreL_labelpleinair_6" = `115_price_avl`,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_4" = `116_price_avl`,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_6" = `117_price_avl`,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_10" = `118_price_avl`,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_12" = `119_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_labelbio_6" = `120_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_labelbio_10" = `121_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_labelpleinair_6" = `122_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_labelpleinair_12" = `123_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_6" = `124_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_12" = `125_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_20" = `126_price_avl`,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_30" = `127_price_avl`,
    "OEUFS DE NOS REGIONS LES_calibreL_labelpleinair_6" = `128_price_avl`,
    "OEUFS DE NOS REGIONS LES_calibreL_nolabel_6" = `129_price_avl`,
    "OEUFS DE NOS REGIONS LES_calibreM_labelpleinair_6" = `130_price_avl`,
    "OEUFS DE NOS REGIONS LES_calibreM_labelpleinair_12" = `131_price_avl`,
    "OEUFS DE NOS REGIONS LES_calibreM_nolabel_12" = `132_price_avl`,
    "OEUFS DE NOS REGIONS LES_calibreM_nolabel_20" = `133_price_avl`,
    "REFLETS DE FRANCE_calibreL_labelbio_6" = `134_price_avl`,
    "REFLETS DE FRANCE_calibreM_labelpleinair_6" = `135_price_avl`,
    "TOP BUDGET_calibreM_nolabel_10" = `136_price_avl`,
    "TOP BUDGET_calibreM_nolabel_30" = `137_price_avl`,
    "U_calibreL_labelpleinair_6" = `138_price_avl`,
    "U_calibreL_nolabel_6" = `139_price_avl`,
    "U_calibreM_labelbio_6" = `140_price_avl`,
    "U_calibreM_labelbio_10" = `141_price_avl`,
    "U_calibreM_labelpleinair_6" = `142_price_avl`,
    "U_calibreM_labelpleinair_12" = `143_price_avl`,
    "U_calibreM_nolabel_12" = `144_price_avl`,
    "U_calibreM_nolabel_20" = `145_price_avl`
  )
  
  
  alt = c(
    "AUCHAN_calibreL_labelbio_4" = 1,
    "AUCHAN_calibreL_labelbio_6" = 2,
    "AUCHAN_calibreM_labelbio_10" = 3,
    "AUCHAN_calibreM_labelpleinair_6" = 4,
    "AUCHAN_calibreM_labelpleinair_12" = 5,
    "AUCHAN_calibreM_nolabel_6" = 6,
    "AUCHAN_calibreM_nolabel_12" = 7,
    "AUCHAN.PP POUCE_calibreM_nolabel_10" = 8,
    "BABY COQUE_calibreL_labelbio_12" = 9,
    "BABY COQUE_calibreL_labelpleinair_6" = 10,
    "BABY COQUE_calibreL_nolabel_6" = 11,
    "BABY COQUE_calibreL_nolabel_10" = 12,
    "BABY COQUE_calibreL_nolabel_20" = 13,
    "BABY COQUE_calibreM_labelbio_6" = 14,
    "BABY COQUE_calibreM_labelpleinair_12" = 15,
    "BABY COQUE_calibreM_nolabel_12" = 16,
    "BIEN VU_calibreM_nolabel_10" = 17,
    "BIEN VU_calibreM_nolabel_30" = 18,
    "BIO_calibreM_labelbio_6" = 19,
    "BIO VILLAGE_calibreL_labelbio_10" = 20,
    "BIO VILLAGE_calibreM_labelbio_6" = 21,
    "CARREFOUR_calibreL_labelpleinair_6" = 22,
    "CARREFOUR_calibreL_nolabel_6" = 23,
    "CARREFOUR_calibreM_labelpleinair_6" = 24,
    "CARREFOUR_calibreM_labelpleinair_12" = 25,
    "CARREFOUR_calibreM_nolabel_6" = 26,
    "CARREFOUR_calibreM_nolabel_12" = 27,
    "CARREFOUR_calibreM_nolabel_20" = 28,
    "CARREFOUR.DISCOUNT_calibreM_nolabel_10" = 29,
    "CARREFOUR.DISCOUNT_calibreM_nolabel_20" = 30,
    "CARREFOUR.DISCOUNT_calibreM_nolabel_30" = 31,
    "CASINO_calibreL_labelpleinair_6" = 32,
    "CASINO_calibreL_nolabel_6" = 33,
    "CASINO_calibreM_labelpleinair_6" = 34,
    "CASINO_calibreM_labelpleinair_12" = 35,
    "CASINO_calibreM_nolabel_6" = 36,
    "CASINO_calibreM_nolabel_12" = 37,
    "CASINO_calibreM_nolabel_20" = 38,
    "COCORETTE_calibreL_labelbio_4" = 39,
    "COCORETTE_calibreL_labelpleinair_6" = 40,
    "COCORETTE_calibreL_labelpleinair_10" = 41,
    "COCORETTE_calibreM_labelbio_4" = 42,
    "COCORETTE_calibreM_labelbio_6" = 43,
    "COCORETTE_calibreM_labelpleinair_4" = 44,
    "COCORETTE_calibreM_labelpleinair_12" = 45,
    "ECO+_calibreM_nolabel_10" = 46,
    "GAULOIS LE_calibreL_nolabel_6" = 47,
    "GAULOIS LE_calibreL_nolabel_12" = 48,
    "GAULOIS LE_calibreM_labelpleinair_6" = 49,
    "GAULOIS LE_calibreM_nolabel_20" = 50,
    "GAULOIS LE_calibreM_nolabel_30" = 51,
    "LIDL_calibreL_nolabel_10" = 52,
    "LIDL_calibreM_labelpleinair_6" = 53,
    "LIDL_calibreM_nolabel_10" = 54,
    "LOUE_calibreL_labelbio_4" = 55,
    "LOUE_calibreL_labelbio_6" = 56,
    "LOUE_calibreL_labelpleinair_6" = 57,
    "LOUE_calibreM_labelbio_6" = 58,
    "LOUE_calibreM_labelbio_10" = 59,
    "LOUE_calibreM_labelbio_12" = 60,
    "LOUE_calibreM_labelpleinair_6" = 61,
    "LOUE_calibreM_labelpleinair_10" = 62,
    "LOUE_calibreM_labelpleinair_12" = 63,
    "LUSTUCRU_calibreL_labelbio_6" = 64,
    "LUSTUCRU_calibreL_labelbio_10" = 65,
    "LUSTUCRU_calibreL_labelpleinair_12" = 66,
    "LUSTUCRU_calibreL_nolabel_4" = 67,
    "LUSTUCRU_calibreL_nolabel_6" = 68,
    "LUSTUCRU_calibreL_nolabel_10" = 69,
    "LUSTUCRU_calibreL_nolabel_12" = 70,
    "LUSTUCRU_calibreM_labelpleinair_6" = 71,
    "LUSTUCRU_calibreM_nolabel_6" = 72,
    "LUSTUCRU_calibreM_nolabel_12" = 73,
    "LUSTUCRU_calibreM_nolabel_20" = 74,
    "LUSTUCRU_calibreM_nolabel_30" = 75,
    "marque_other_calibre_other_label_other_0" = 76,
    "MARQUES ALDI_calibreM_labelpleinair_6" = 77,
    "MATINES_calibreL_labelbio_6" = 78,
    "MATINES_calibreL_labelpleinair_6" = 79,
    "MATINES_calibreL_nolabel_4" = 80,
    "MATINES_calibreL_nolabel_6" = 81,
    "MATINES_calibreL_nolabel_10" = 82,
    "MATINES_calibreL_nolabel_12" = 83,
    "MATINES_calibreL_nolabel_20" = 84,
    "MATINES_calibreM_labelbio_6" = 85,
    "MATINES_calibreM_labelbio_12" = 86,
    "MATINES_calibreM_labelpleinair_6" = 87,
    "MATINES_calibreM_labelpleinair_12" = 88,
    "MATINES_calibreM_nolabel_6" = 89,
    "MATINES_calibreM_nolabel_20" = 90,
    "MATINES_calibreM_nolabel_30" = 91,
    "MOISSON_calibreL_labelpleinair_6" = 92,
    "MOISSON_calibreL_nolabel_4" = 93,
    "MOISSON_calibreL_nolabel_6" = 94,
    "MOISSON_calibreM_labelbio_6" = 95,
    "MOISSON_calibreM_labelbio_10" = 96,
    "MOISSON_calibreM_labelpleinair_12" = 97,
    "MOISSON_calibreM_nolabel_12" = 98,
    "MOISSON_calibreM_nolabel_20" = 99,
    "MONOPRIX_calibreL_nolabel_6" = 100,
    "MONOPRIX_calibreM_labelbio_6" = 101,
    "MONOPRIX_calibreM_labelbio_10" = 102,
    "MONOPRIX_calibreM_labelpleinair_6" = 103,
    "MONOPRIX_calibreM_labelpleinair_12" = 104,
    "MONOPRIX_calibreM_nolabel_20" = 105,
    "MONOPRIX GOURMET_calibreM_labelpleinair_6" = 106,
    "NETTO_calibreL_nolabel_10" = 107,
    "NETTO_calibreM_labelbio_6" = 108,
    "NETTO_calibreM_labelpleinair_6" = 109,
    "NETTO_calibreM_labelpleinair_12" = 110,
    "NETTO_calibreM_nolabel_6" = 111,
    "NETTO_calibreM_nolabel_12" = 112,
    "NETTO_calibreM_nolabel_30" = 113,
    "NOS REGIONS ONT DU TALENT_calibreL_labelpleinair_6" = 114,
    "OEUF DE NOS VILLAGES_calibreL_labelpleinair_6" = 115,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_4" = 116,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_6" = 117,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_10" = 118,
    "OEUF DE NOS VILLAGES_calibreL_nolabel_12" = 119,
    "OEUF DE NOS VILLAGES_calibreM_labelbio_6" = 120,
    "OEUF DE NOS VILLAGES_calibreM_labelbio_10" = 121,
    "OEUF DE NOS VILLAGES_calibreM_labelpleinair_6" = 122,
    "OEUF DE NOS VILLAGES_calibreM_labelpleinair_12" = 123,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_6" = 124,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_12" = 125,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_20" = 126,
    "OEUF DE NOS VILLAGES_calibreM_nolabel_30" = 127,
    "OEUFS DE NOS REGIONS LES_calibreL_labelpleinair_6" = 128,
    "OEUFS DE NOS REGIONS LES_calibreL_nolabel_6" = 129,
    "OEUFS DE NOS REGIONS LES_calibreM_labelpleinair_6" = 130,
    "OEUFS DE NOS REGIONS LES_calibreM_labelpleinair_12" = 131,
    "OEUFS DE NOS REGIONS LES_calibreM_nolabel_12" = 132,
    "OEUFS DE NOS REGIONS LES_calibreM_nolabel_20" = 133,
    "REFLETS DE FRANCE_calibreL_labelbio_6" = 134,
    "REFLETS DE FRANCE_calibreM_labelpleinair_6" = 135,
    "TOP BUDGET_calibreM_nolabel_10" = 136,
    "TOP BUDGET_calibreM_nolabel_30" = 137,
    "U_calibreL_labelpleinair_6" = 138,
    "U_calibreL_nolabel_6" = 139,
    "U_calibreM_labelbio_6" = 140,
    "U_calibreM_labelbio_10" = 141,
    "U_calibreM_labelpleinair_6" = 142,
    "U_calibreM_labelpleinair_12" = 143,
    "U_calibreM_nolabel_12" = 144,
    "U_calibreM_nolabel_20" = 145
  )
  
  
  ######################

  mnl_settings = list(
    alternatives = alt,
    avail = avl,
    choiceVar = choice,
    V = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings , functionality)
  
  P = apollo_panelProd(P, apollo_inputs , functionality)
  P = apollo_avgInterDraws(P, apollo_inputs , functionality)
  P = apollo_prepareProb(P, apollo_inputs , functionality)
  return(P)
}

estimate_settings = list(
  #maxIterations = 30,
  #bootstrapSE = 5,
  
### COPY HERE THE SCALING PARAMETER #####
  
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


#########################################

)

# il peut etre necessaire de reexecuter le code suivant 
# avant de relancer l'algorithme
# apollo_inputs=apollo_validateInputs()

start = Sys.time()

model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
  #estimate_settings
  )

end = Sys.time()
print(end-start)



apollo_saveOutput(model)


#######################################






