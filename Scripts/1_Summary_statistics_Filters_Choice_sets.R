
setwd("E:/Mémoire/Code/price-information_organic-eggs")
source("Scripts/0_Packages_Libraries.R")

#### DATA LOADING AND CLEANING ####

household = read_dta("Data/hhold2012_PF_annuel.dta")
consumption = read_dta("Data/eggs2012_PF_annuel.dta")
shopping_trips = read_dta("Data/shoppingtrips2012v2.dta")

consumption_cleaner = consumption%>%
  
  # Fusion des colonnes de label
  mutate("nolabel" = (labelpleinair + labelbio + nglabel + labelmissing == 0))%>%
  pivot_longer(cols = c("labelpleinair", "labelbio", "nglabel","labelmissing", "nolabel"), names_to = "label")%>%
  filter(!is.na(value) | label == "labelbio")%>%
  mutate(label = ifelse(is.na(value), NA, label))%>%
  filter(is.na(label) | value == 1)%>%
  select(-value)%>%
  mutate(label = as.factor(label))%>%
  
  # Fusion des colonnes de calibre
  mutate("nocalibre" = (caille + calibreS + calibreM + calibreL + calibreXL + calibremissing == 0))%>%  
  pivot_longer(cols = c("caille", "calibreS", "calibreM", "calibreL", "calibreXL","calibremissing", "nocalibre"), names_to = "calibre")%>%
  filter(!is.na(value) | calibre == "calibreS")%>%
  mutate(calibre = ifelse(is.na(value), NA, calibre))%>%
  filter(is.na(calibre) | value == 1)%>%
  select(-value)%>%
  mutate(calibre = as.factor(calibre))%>%
  
  select(
    hhid,
    periode,
    semaine,
    dateachat,
    weightPFan,
    circuit,
    surface,
    sdachat,
    fabricant,
    centraleachat,
    marque,
    mdd,
    label, 
    calibre,
    weightach,
    boite,
    qaachat,
    qatotale,
    vn_2,
    valqvol,
    #gencode
  ) %>%
  
  mutate_at(
    vars(
      fabricant,
      marque,
      circuit,
      centraleachat,
      periode,
      valqvol
    ),
    as.factor
  )%>%
  mutate(
    dateachat = as.Date(dateachat, format = "%d/%m/%Y")
  )%>%
  
  # The rows for which the week is missing correspond to period during which no egg was bought
  filter(!is.na(semaine))


saveRDS(consumption_cleaner, "Inputs/full_egg_consumption.rds")


# All the households in the data have bought some eggs
anti_join(
  consumption_cleaner,
  household, 
  by = "hhid"
  )

# Conversely, all the eggs bought came from a household in the data 
anti_join(
  household, 
  consumption_cleaner,
  by = "hhid"
)

sales_per_household_per_period = consumption_cleaner%>%
  group_by(hhid, periode)%>%
  summarise(nb_sales_per_period = n())

sales_per_household_per_period$nb_sales_per_period%>% as.factor()%>% summary()

sales_per_household = sales_per_household_per_period%>%
  filter(nb_sales_per_period > 0)%>%
  group_by(hhid)%>%
  summarise(nb_period_with_sales = n())

sales_per_household$nb_period_with_sales%>% as.factor()%>% summary()
sales_per_household%>% 
  group_by(nb_period_with_sales)%>%
  summarise(nb = n())%>%
  arrange(-nb)%>%
  mutate(
    nb_total = sum(nb),
    share = 100* nb/nb_total,
    nb_cumsum = 100* cumsum(nb)/nb_total,
    )
# If I keep all those that bought eggs more than 9 period over 13, I include 66% of the sample




#### SOME SUMMARY STATISTICS ####

### Les caractéristiques des produits

## La Taille de la Boite

summary(consumption_cleaner$valqvol)

# C'est pas bien clair quelle mesure de la quantité on doit utiliser...
# View(consumption_cleaner%>%
#   filter(vn_2 != valqvol))

# Il semblerait qu'on puisse raisonnablement interpréter valqvol
# comme le nombre d'oeufs par boite ou 1 si c'est à l'unité
# View(consumption_cleaner%>%
#        filter(valqvol == 1))

# Je propose de ne conserver que les tailles suivantes:
# 1, 4, 6, 10, 12, 20, 30


## Le Calibre

summary(consumption_cleaner$calibre)

# Qu'est qu'un oeuf sans calibre ?
# View(consumption_cleaner%>%
#        filter(calibre == "nocalibre"))
# C'est souvent que l'information est manquante,
# En effet il n'y a généralement pas de label ni de fabricant
# Après inspeciton des données, 
# ce sont sans doute des oeufs de très grande taille

# Quelle est la différence avec un oeuf dont le calibre manque ?
# View(consumption_cleaner%>%
#        filter(calibre == "calibremissing"))
# Au contraire, dans ce cas le fabricant et le label sont souvent présents.

# Je propose d'enlever dans un premier temps "calibreS" et "calibremissing"


## Le Label

summary(consumption_cleaner$label)

# Comment traiter le cas des oeufs sans code barre ?
# View(consumption_cleaner%>%
#        filter(label == "nglabel"))
# Ils sont pas très nombreux mais en général les autres infos sont disponibles

# Je propose d'enlever les nglabel


## Le Prix

summary(consumption_cleaner$sdachat)
# Il semblerait que ce soit le prix d'une unité,
# qui correspond à la quantité valqvol

# Est-ce que les prix les plus élevés ont un sens ?
# View(consumption_cleaner%>%
#        filter(sdachat>10))
# A part les deux achats dont le prix est supérieur à 100,
# toutes les valeurs paraissent crédibles

# Je propose d'enlever les deux prix absurdes
# Et plyus généralement tous les prix à l'oeuf supérieurs à 1€



### Les caractéristiques du lieu de vente


## La Centrale d'Achat

summary(consumption_cleaner$centraleachat)
# Je propose de ne conserver que les plus grandes,
# cela revient à limiter le nombre de distributeurs différents

# View(consumption_cleaner%>%
#        group_by(centraleachat)%>%
#        mutate(nb_achat = n())%>%
#        ungroup()%>%
#        filter(nb_achat>1000)%>%
#        count(centraleachat, fabricant)
# )

# Je propose de ne conserver que les distributeurs suivants:
# - Leclerc (centraleachat == 1)
# - Intermarche (centraleachat == 2)
# - Auchan (centraleachat == 3)
# - Carrefour (centraleachat == 4)
# - Systeme U (centraleachat == 8)
# - Casino & MOnoprix (centraleachat == 10 | centraleachat = 30)
# - Aldi (centraleachat == 18)
# - Lidl (centraleachat == 21)


## Le Circuit de Vente

summary(consumption_cleaner$circuit)

# View(consumption_cleaner%>%
#   count(circuit, centraleachat))
# Il me semblerait que les valeurs 
# 7, 41, 42 et 43 regroupent l'essentiel des ventes des grands groupes
#    41  ALIMENTATION TRADI.SS CHARI RA
#42           SUPERMARCHES CHARIOT
#43           HYPERMARCHES CHARIOT
#52           MARCHES,FOIRES
#7           MAGASINS POPULAIRES SUPER CHAR
#890         CADEAU
# https://www.lsa-conso.fr/classement-lsa-les-100-premieres-enseignes-en-france,182807
# Leur unité de base correspond à peu prêt à circuit x centraleachat


# Regardons le lien entre circuit et surface
# ggplot(
#   data = consumption_cleaner %>% filter(circuit %in% c(7, 41, 42, 43))
#   ,
#   aes(x = log10(2+surface))
# )+
#   geom_histogram(
#     breaks = c(-2,0,2, 100,200,500,1000,2000,5000,10000,20000, 50000)%>% 
#       map_dbl(function(x){log10(3+x)})
#   )+
#   facet_grid(rows = "circuit", scale = "free")
# Il semblerait que chacun de nos circuits 
# corresponde à une certaine taille de magasin


## Fabricant et MDD pour le choix des produits

consumption%>%
  filter(mdd == "Oui")%>%
  .$marque%>%
  as.factor()%>%
  summary()
  
# Il faut faire des regroupements dans cette liste

consumption%>%
  .$fabricant%>%
  as.factor()%>%
  summary()

# Et en croisant les deux :

consumption_cleaner%>%
  filter(mdd == "Oui")%>%
  count(marque, fabricant)%>%
  pivot_wider(names_from = "fabricant", values_from = "n")

#### SOME DATA CLEANING AGAIN ####

# On applique les filtres proposés jusqu'à présent
consumption_filtered = consumption_cleaner%>%
  filter(
    is.na(sdachat) | valqvol %in% c(1, 4, 6, 10, 12, 20, 30),
    is.na(sdachat) | calibre != "calibreS",
    is.na(sdachat) | calibre != "calibremissing",
    is.na(sdachat) | calibre != "nocalibre",
    is.na(sdachat) | label != "labelmissing",
    is.na(sdachat) | label != "nglabel",
    is.na(sdachat) | sdachat < 100,
    is.na(sdachat) | centraleachat %in% c(1,2,3,4,8,10,18,21,30),
    is.na(sdachat) | circuit %in% c(7,41,42,43)
  )%>%
  drop_na()

# On passe de 131 000 à 81 185 achats
# Le drop_na est responsable de 20 000 pertes environ
# Cela correspond aux gens qui se sont renuds en magasin mais n'ont pas acheté d'oeufs


consumption_simplified = consumption_filtered%>%
  mutate(
    unit_price = sdachat / ( qaachat * as.numeric(as.character(valqvol))),
    #product = paste(marque, calibre, sep = "_"),
    surface_discrete = round(2*log10(abs(surface)))/2,
    retailer = paste(circuit, centraleachat, sep = "_")
  )%>%
  rownames_to_column("X")%>%
  select(
    X, # Définit la choice situation
    hhid, # Definit l'individu
    marque, calibre, valqvol, label, # Definit le produit,
    unit_price, # Definit le prix
    retailer, # Definit le retailer
    periode,
    dateachat
  )%>%
  filter(unit_price < 1)
# Ce dernier filtre enlève une trentaine d'achats


retailer_df = consumption_simplified%>% 
  group_by(retailer)%>% 
  summarise(
    total_sales = n(),
    )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

retailer_list = retailer_df%>%
  filter(total_sales>1000)%>%
  .$retailer


## Je définis pour chaque label la liste des marques les plus importantes
# j'essaye d'avoir les différents labels pour une même marques
marque_bio_df = consumption_simplified%>%
  filter(
    retailer %in% retailer_list,
    label == "labelbio"
    )%>%
  group_by(marque)%>% 
  summarise(
    total_sales = n()
  )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

marque_bio_list = marque_bio_df%>%
  filter(total_sales>95)%>%
  .$marque%>%
  as.character()%>%
  c(., "LUSTUCRU", "NETTO", "BABY COQUE", "REFLETS DE FRANCE", " OEUF DE NOS VILLAGES")%>%
  .[.!= "PETITJOUR"]%>%
  .[.!= "DUCLAIR"]

marque_pleinair_df = consumption_simplified%>%
  filter(
    retailer %in% retailer_list,
    label == "labelpleinair"
  )%>%
  group_by(marque)%>% 
  summarise(
    total_sales = n()
  )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

marque_pleinair_list = marque_pleinair_df%>%
  filter(total_sales>180)%>%
  .$marque%>%
  as.character()%>%
  c(., "LUSTUCRU", "NETTO", "MONOPRIX", "MONOPRIX GOURMET")%>%
  .[.!= "PETITJOUR"]%>%
  .[.!= "DUCLAIR"]

marque_nolabel_df = consumption_simplified%>%
  filter(
    retailer %in% retailer_list,
    label == "nolabel"
  )%>%
  group_by(marque)%>% 
  summarise(
    total_sales = n()
  )%>% 
  arrange(-total_sales)%>%
  mutate(
    share = total_sales/ sum(total_sales),
    cumul = cumsum(share)
  )

marque_nolabel_list = marque_nolabel_df%>%
  filter(total_sales>341)%>%
  .$marque%>%
  as.character()%>%
  c(., "LUSTUCRU LES SELECTIONS", "MONOPRIX", "BABY COQUE")%>%
  .[.!= "PETITJOUR"]%>%
  .[.!= "DUCLAIR"]
  
# A partir des marques sélectionnées ci-dessus, je construis ma liste des marques
marque_list = c(
  marque_bio_list,
  marque_pleinair_list,
  marque_nolabel_list
)%>%
  unique()

marques_mdd_low = c(
  "AUCHAN.PP POUCE",
  "BIEN VU",
  "CARREFOUR.DISCOUNT",
  "ECO+",
  "LIDL",
  "MARQUES ALDI",
  "NETTO",
  "TOP BUDGET"
)

marques_mdd_medium = c(
  "AUCHAN",
  "CARREFOUR",
  "CASINO",
  "MONOPRIX",
  "U"
)

marques_mdd_high = c(
  "MONOPRIX GOURMET",
  "BIO VILLAGE",
  "MOISSON",
  "OEUFS DE NOS REGIONS LES",
  "REFLETS DE FRANCE",
  "NOS REGIONS ONT DU TALENT"
)

marques_indep = c(
  "BABY COQUE",
  "COCORETTE",
  "OEUF DE NOS VILLAGES",
  "GAULOIS LE",
  "LOUE",
  "LUSTUCRU",
  "MATINES",
  "BIO"
)

df_marques = data.frame(marque = marque_list, marque_simple = NA)%>%
  mutate(
    marque_simple = case_when(
      marque %in% marques_indep ~ "indep",
      marque %in% marques_mdd_low ~ "low",
      marque %in% marques_mdd_medium ~ "medium",
      marque %in% marques_mdd_high ~ "high",
      TRUE ~ "other"
    )
  )

# Tous les produits qui ne sont pas dans les marques sélectionnées finissent dans "Marque non trouvée"
# Je regroupe entre elles certaines marque quand la différence correspond juste à un label
# Je regroupe entre elles les MDD selon le niveau de qualité associé
consumption_simplified_further = consumption_simplified%>%
  mutate_at(vars(marque), as.character)%>%
  filter(
    retailer %in% retailer_list
  )%>%
  mutate(
    marque = ifelse(marque %in% marque_list, marque, "Marque Non Trouvee"),
    marque = ifelse(marque == "AUCHAN.BIO", "AUCHAN", marque),
    marque = ifelse(marque == "U.BIO U", "U", marque),
    marque = ifelse(marque == "MONOPRIX BIO", "MONOPRIX", marque),
    marque = ifelse(marque == "CARREFOUR.BIO", "MONOPRIX", marque),
    marque = ifelse(marque == "CASINO.BIO", "MONOPRIX", marque)
  )%>%
  left_join(df_marques)%>%
  mutate_at(vars(retailer, marque_simple, marque), as.factor)

# consumption_simplified_further%>%
#   count(marque, calibre, label)%>%
#   pivot_wider(names_from = "label", values_from = "n")%>%
#   View()

consumption_simplified_further%>%
  count(marque, calibre, label)
# ça me fait 28 produits au sens marque_simple, calibre, label
# et 89 produits au sens marque, calibre, label

consumption_simplified_further%>%
  count(marque_simple, calibre, label, valqvol)
# il y en a 87 au sens (marque_simple, calibre, label, valqvol)
# il y en a 172 au sens (marque, calibre, label, valqvol)


# product_list = c("OTHER_PRODUCT", product_df%>% filter(kept_as_is)%>% .$product)
# other_product_list = product_df%>% filter(!kept_as_is)%>% .$product


# # Je ne garde que les couples produits/distributeurs
# # pour lesquels il y a au moins 11 ventes 
# # Je ne garde que les distributeurs 
# # pour lesquels il y a au moins 11 éléments dans le choice set
# 
# retailer_product_df = consumption_simplified_further%>%
#   # mutate(
#   #   product = ifelse(product %in% other_product_list,
#   #                    "OTHER_PRODUCT", product),
#   #   product_label = paste(product, label, sep = "_"),
#   # )%>%
#   count(retailer, marque, label, calibre)%>%
#   pivot_wider(
#     names_from = "retailer",
#     values_from = "n"
#   )
# 
# 
#   filter(nb_product_choice >= 10)%>%
#   group_by(retailer)%>%
#   mutate(
#     retailer_product_nb = n()
#   )%>%
#   filter(retailer_product_nb>10)%>%
#   ungroup()
# 
# 
# consumption_filtered_again = consumption_simplified%>%
#   mutate(
#     product = ifelse(product %in% other_product_list,
#                      "OTHER_PRODUCT", product),
#     product_label_chosen = paste(product, label, sep = "_")
#   )%>%
#   semi_join(retailer_product_df, by = c("retailer", "product_label_chosen" = "product_label"))%>%
#   mutate(
#     X = as.factor(X),
#     retailer = as.factor(retailer),
#     product_label_chosen = as.factor(product_label_chosen),
#     hhid = as.factor(hhid)
#   )

summary(consumption_simplified_further)


# On remplace les calibres et marques non reconnus par 

consumption_simplified_further_with_other_product = consumption_simplified_further%>%
  mutate_at(vars(label, calibre, marque), as.character)%>%
  mutate(
    calibre = ifelse(marque_simple == "other", "calibre_other", calibre),
    valqvol = ifelse(marque_simple == "other", 0, as.numeric(as.character(valqvol))),
    label = ifelse(marque_simple == "other", "label_other", label),
    marque = ifelse(marque_simple == "other", "marque_other", marque),
    unit_price = ifelse(marque_simple == "other", 0, unit_price),
  )%>%
  mutate_at(vars(label, calibre, marque), as.character)

saveRDS(consumption_simplified_further, "Inputs/data_fully_filtered.rds")


# # Pour chaque choice situation X
# # On récupère tous les produits possibles pour le retailer en question
# # On note celui qui a été choisi et son prix
# # Les autres prix valent NA pour le moment
# 
# 
# #### CONSTRUCTION OF THE CHOICE SETS AND CHOICE SITUATIONS ####

consumption_simplified_further_with_other_product%>%
  filter(marque_simple == "other")%>%
  count(retailer,marque, marque_simple, calibre, label, valqvol, periode)%>%
  count(retailer)
# Le produit other apparaitra bien dans tous les choice sets

retailer_product_df = consumption_simplified_further_with_other_product%>%
  count(retailer,marque, marque_simple, calibre, label, valqvol, periode)%>%
  select(-n)


retailer_product_df%>%
  count(retailer, periode)


choice_situation_df = consumption_simplified_further_with_other_product%>%
  rename(
    marque_chosen = marque,
    label_chosen = label,
    calibre_chosen = calibre,
    valqvol_chosen = valqvol,
    marque_simple_chosen = marque_simple
  )%>%
  left_join(retailer_product_df, by = c("retailer", "periode"))%>%
  mutate(
    choice = (marque_chosen == marque & 
                label_chosen == label & 
                calibre_chosen == calibre &
                valqvol_chosen == valqvol),
    price = ifelse(choice, unit_price, NA)
  )%>%
  select(-unit_price)%>%
  mutate_at(vars(retailer), as.factor)

saveRDS(choice_situation_df, "Inputs/choice_situation_without_price.rds")


