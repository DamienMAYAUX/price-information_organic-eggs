
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")
setwd("E:/Mémoire/Code/price-information_organic-eggs/Inputs/Apollo")

model_lnorm_control = apollo_loadModel("mixed_lnorm_control")


# apollo_fitTests
# une colonne par categorie de consommateur
# pour chaque achat, 0 ou 1 selon si le menage appartient a la categorie

# fitsTest_settings = list()
# fitsTest_settings[["subsamples"]] = list()
# fitsTest_settings$subsamples[["business"]] = database$business==1
# fitsTest_settings$subsamples[["leisure"]] = database$business==0
# apollo_fitsTest(model,apollo_probabilities,apollo_inputs,fitsTest_settings)




# apollo_lrTest(model1, model2)
# apollo_basTest(model1, model2)


# Use apollo_prediction to simulate the effect of a change in the price of the organic products
# This could serve as a complement to the theoretical model

model_lnorm_control$estimate["b_labelbio"]

conditionals = apollo_conditionals(model_lnorm_control, apollo_probabilities, apollo_inputs)

df_household = readRDS("Inputs/full_household_sample.rds")

posterior = conditionals%>%
  mutate(
   wtp = - model_lnorm_control$estimate["b_labelbio"] / post.mean
  )%>%
  left_join(
    df_household,
    by = c("ID"=  "hhid")
  )
  

ggplot()+
  geom_density(
    data= posterior, 
    aes(x=wtp)
    )+
  geom_density(
    data = posterior %>% filter(clas == "AISEE"), 
    aes(x=wtp, color = "HIGH-INCOME")
    )+
  geom_density(
    data = posterior %>% filter(clas == "MODESTE"), 
    aes(x=wtp, color = "LOW-INCOME")
  )
  

price_sensitive_consumers = conditionals%>%
  arrange(post.mean)%>%
  .[1:5,]

price_insensitive_consumers = conditionals%>%
  arrange(-post.mean)%>%
  .[1:5,]

database%>%
  left_join(price_sensitive_consumers, by = c("hhid" = "ID"))%>%
  left_join(household)






  
