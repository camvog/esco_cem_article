##--------------------------------------------------------------------------------------------------------
## SCRIPT : Chapter 5 - Selection articles RSL
##
## Authors : Camille Vogel, Matthieu Authier,  Sean Heighton, 
## Last update : 2026-02-17
##
## R version 4.5.1 
## Copyright (C) 2024 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64
##--------------------------------------------------------------------------------------------------------

what_u_need <- c("lubridate", "tidyverse", "readxl", "ggbeeswarm", "skimr",
                 "ggalluvial", "RColorBrewer","patchwork","readxl")
cran_packages <- what_u_need[!(what_u_need %in% installed.packages())]
if(length(cran_packages) != 0) {
  lapply(cran_packages, install.packages, dependencies = TRUE)
}
lapply(what_u_need, library, character.only = TRUE)

rm(list = ls())
source("./common_functions/esco_functions.r")

mergedata <- read.csv(file = "./data/output/Final_Coding_Tool_CHAP_5_GeneralCoding_merged.csv")%>%
                        #"./output/Final_Coding_Tool_CHAP_5_CEM_only_GeneralCoding_merged.csv") %>%
  filter(match_status == "merged")



## 1. Bilan des publications selectionnees poissons et benthos
##--------------------------------------------------------------------------------------------------------

# correction complementaire (filtre de 2 refs a exclure)
mergedata_fb_2 <- mergedata %>% filter(is.na(Pressure_comment) | !str_detect(Pressure_comment, regex("EXCLUDED", ignore_case = TRUE)))

# Total nb d'articles de recherche

length(unique(mergedata_fb_2$AI_Key))
dim(mergedata_fb_2)


# nb d'articles poissons et benthos
mergedata_fb_2 %>% filter(Target == Fish_and_Benthos[2]) %>% dim() # benthos
mergedata_fb_2 %>% filter(Target == Fish_and_Benthos[1]) %>% dim() # poissons



## 2. Bilan des publications au cours du temps
##--------------------------------------------------------------------------------------------------------

publi_annee <- mergedata_fb_2 %>% group_by(Target, AI_Publication_Year) %>% 
  summarise(nb_pub= length(unique(AI_Key))
  )
publi_annee %>% group_by(Target)%>%
  summarise(total = sum(nb_pub),
            mean= mean(nb_pub),
            sd= sd(nb_pub))

## 3. Bilan in situ in vitro
##--------------------------------------------------------------------------------------------------------

mergedata_fb_2 <- mergedata_fb_2 %>% mutate(exsitu = case_when(Empirical..ex.situ..laboratory.studies. == "Yes" ~"in vitro",
                                             Empirical..ex.situ..laboratory.studies. == "No" ~ "in situ",
                                             .default= NA))
# en nombre d'article
mergedata_fb_2 %>% group_by(exsitu) %>%
              summarise(n_ra=length(unique(AI_Key)))
# en nombre de cas d'étude
mergedata_fb_2 %>% group_by(exsitu) %>%
  summarise(n_cs=n())

# en nombre d'article par Target
mergedata_fb_2 %>% group_by(Target,exsitu) %>%
  summarise(n_ra=length(unique(AI_Key)))

# en nombre de cas d'étude par Target
mergedata_fb_2 %>% group_by(Target, exsitu) %>%
  summarise(n_cs=n())


## 4. Bilan par indicateur générique
##--------------------------------------------------------------------------------------------------------

mergedata_fb_2 %>% 
  group_by(OICM_generic_indices) %>%
  summarise(n_cs=n())

mergedata_fb_2 %>% 
  group_by(Target, OICM_generic_indices) %>%
  summarise(n_cs=n())

## 4. Bilan des espèces
mergedata_fb_2 %>% 
  mutate(species = AIP_Taxonomicposition) %>%
  group_by(Target) %>%
  summarise(n_sp=length(unique(species)),
            n_cs=n()
  )

bilan_especes <- mergedata_fb_2 %>% 
  mutate(species = AIP_Taxonomicposition) %>%
  group_by(Target,species) %>%
  summarise(n_rp= length(unique(AI_Key)),
            n_cs=n()
  )

## 5. Le status des espèces (conservation/commercial)
##--------------------------------------------------------------------------------------------------------

bilan_status <- mergedata_fb_2 %>% 
  mutate(species = AIP_Taxonomicposition) %>%
  mutate(
    Sensitivity = pmap_chr(
      list(Sp_sensitive, Sp_migratory, Sp_conservation, Sp_commercial),
      function(sens, mig, cons, comm){
        labels <- c()
        if (!is.na(sens) && sens %in% c("sensitvity known","senstitivity suspected")) labels <- c(labels, "Sensitive")
        if (!is.na(mig)  && mig  == "yes") labels <- c(labels, "Migratory")
        if (!is.na(cons) && cons == "yes") labels <- c(labels, "Conservation")
        if (!is.na(comm) && comm == "yes") labels <- c(labels, "Commercial")
        if (length(labels) == 0) "Other" else str_c(labels, collapse = " + ")
        
      }
    )
  )%>%
  group_by(Target,species,Sensitivity) %>%
  summarise(n_rp= length(unique(AI_Key)),
            n_cs=n()
  )

# 6. Bilan target/stade de vie/statistical signif
##--------------------------------------------------------------------------------------------------------

bilan_stade_vie_indic <- mergedata_fb_2 %>% 
  
  mutate(
    # Toutes les valeurs ≠ "Fish" deviennent "Benthic invertebrates"
    Target = case_when(
      Target != "Fish" ~ "Benthic invertebrates",
      TRUE ~ Target
    ),
    
    # Remplacer les NA par "NS"
    OICIE_statistical_significance = case_when(
      is.na(OICIE_statistical_significance) ~ "NS",
      TRUE ~ OICIE_statistical_significance
    ),
    
    # Reordonner les niveaux de significativité
    OICIE_statistical_significance = factor(
      OICIE_statistical_significance,
      levels = c("yes", "no", "NS")
    ),
    
    # Reordonner les niveaux de stades de vie
    PLSS_lifestage1 = factor(
      PLSS_lifestage1,
      levels = c("larval", "juvenile", "adult", "NS")
    )
  ) %>%
  
  group_by(
    Target,
    PLSS_lifestage1,
    OICM_generic_indices,
    OICIE_statistical_significance,
    OICIE_impact_valence
  ) %>%
  
  summarise(n = n(), .groups = "drop")

# valeurs
bilan_stade_vie_tot <- bilan_stade_vie_indic %>%
  group_by(Target, PLSS_lifestage1) %>%
  summarise(total_cases = sum(n), .groups = "drop")



## 7. Les conditions experimentales
##--------------------------------------------------------------------------------------------------------

# reformatage du jeu de données
magneto <- mergedata_fb_2 %>%
  filter(!is.na(EXPMG_Type)) %>%
  select(AI_Key, EXPMG_Type, EXPEL_Intensity, 
         EXPMG_Max_Intensity_T, 
         starts_with("EXPCH"),
         starts_with("EXPAL"),
         Target, PLSS_lifestage1 , Empirical..ex.situ..laboratory.studies.,
         OICM_generic_indices,
         CVEOWF_cable_power_kW
  ) %>%
  rename(exsitu = Empirical..ex.situ..laboratory.studies.,
         chronic = EXPCH_chronicorponctual,
         continuous = EXPCH_continuousordiscontinuous,
         steady = EXPCH_steadyorvariable,
         cable = CVEOWF_cable_power_kW
  ) %>%
  mutate(exsitu = case_when(exsitu == "No" ~ "In vivo",
                            exsitu == "Yes" ~ "In vitro",
                            .default = exsitu
  ),
  cable = case_when(cable %in% c("330000", "118000") ~ ">10 000",
                    cable %in% c("34.5", "145") ~ "<10 000",
                    .default = cable
  ))



# 7.1. types de courant
bilan_ACDC <- magneto %>% group_by(Target,EXPMG_Type) %>%
  summarise(n_rp= length(unique(AI_Key)),
            n_cs=n()
  )
sum(bilan_ACDC$n_rp)
sum(bilan_ACDC$n_cs)

# 7.2. distance d'exposition

magneto_dist<- magneto %>% group_by(Target,exsitu, EXPMG_Type) %>%
               summarise(all_dist = unique(cbind(AI_Key, EXPCH_max_distancetosource_M)),
                         min_dist = min(EXPCH_max_distancetosource_M),
                         max_dist = max(EXPCH_max_distancetosource_M)
               )
  

# 7.3. durée d'exposition

boxplot(as.numeric(magneto$EXPCH_durationofexposure_days)
        ~ magneto$PLSS_lifestage1)

ggplot(magneto, aes(x = Target,
               y = as.numeric(EXPCH_durationofexposure_days))) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~ PLSS_lifestage1) +
  labs(x = "Type d'animal",
       y = "Durée d'exposition") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Duration_over_50_days <- magneto %>% 
  mutate(EXPCH_durationofexposure_days = as.numeric(EXPCH_durationofexposure_days),
                                                    .default= "NA")%>%
  filter(EXPCH_durationofexposure_days>50)
unique(Duration_over_50_days$AI_Key)




#----------------------------------------------------------------------------#
#
# Figures 
#
#----------------------------------------------------------------------------#

## Figure 1 : alluvial 
##--------------------------------------------------------------------------------------------------------

bilan_stade_vie_indic <- bilan_stade_vie_indic %>% mutate(
                                                    Target = case_when(Target == "Benthic invertebrates" ~ "Benthic\n invertebrates",
                                                                             .default = Target),
                                                    OICIE_statistical_significance = case_when(OICIE_statistical_significance =="NS" ~"unr.",
                                                                              .default = OICIE_statistical_significance)
                                                   )%>%
                                                   mutate(
                                                     OICIE_statistical_significance  = factor(OICIE_statistical_significance , 
                                                                                              levels = c("unr.",
                                                                                                         "no",
                                                                                                         "yes"),
                                                                                              ordered = TRUE)
                                                       )



# graph alluviaux - Ready for submission
ggplot(bilan_stade_vie_indic,
       aes(axis1 = Target,
           axis2 = PLSS_lifestage1,
           axis3 = OICIE_statistical_significance,
           y = n)) +
  
  # Alluvium in greyscale
  geom_alluvium(aes(fill = OICIE_statistical_significance),
                width = 1/12, alpha = 0.85) +
  
  # Strata in light grey with darker borders
  geom_stratum(width = 1/12, fill = "grey90", color = "grey30") +
  
  # Labels
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 4, color = "black") +
  
  # Axis labels
  scale_x_discrete(
    limits = c("Population", "Life stage", "Statistical\n significance"),
    expand = c(.01, .20)
  ) +
  
  # Greyscale fill for significance categories
  scale_fill_manual(
    values = c(
      "yes" = "darkseagreen1",
      "no"  = "seashell3",
      "unr."  = "grey40"
    ),
    name = "Statistical significance"
  ) +
  
  # Black‑and‑white publication theme
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size = 11)
  ) +
  
  labs(
    y = "Total number of case studies",
    fill = "Statistical significance"
  )

 ggsave(
   filename = "./figures/Fig_1_Sankey_Target_lifestage_statsignif.png",
   units = "cm", width = 25, height = 10, dpi = 600
 )





## Figure 2 : exposure duration VS magnetic field intensity
##--------------------------------------------------------------------------------------------------------

magneto <- mergedata_fb_2 %>%
  filter(!is.na(EXPMG_Type)) %>%
  select(AI_Key, EXPMG_Type, EXPEL_Intensity, 
         EXPMG_Max_Intensity_T, 
         starts_with("EXPCH"),
         starts_with("EXPAL"),
         starts_with("OIC"),
         Target, AIP_Taxonomicposition, PLSS_lifestage1 , Empirical..ex.situ..laboratory.studies.,
         
         
         CVEOWF_cable_power_kW
  ) %>%
  rename(exsitu = Empirical..ex.situ..laboratory.studies.,
         chronic = EXPCH_chronicorponctual,
         continuous = EXPCH_continuousordiscontinuous,
         steady = EXPCH_steadyorvariable,
         cable = CVEOWF_cable_power_kW
  ) %>%
  mutate(exsitu = case_when(exsitu == "No" ~ "In vivo",
                            exsitu == "Yes" ~ "In vitro",
                            .default = exsitu
  ),
  cable = case_when(cable %in% c("330000", "118000") ~ ">10 000",
                    cable %in% c("34.5", "145") ~ "<10 000",
                    .default = cable
  )
  )


# suppression des cas sans info sur l'intensité du CM (4 cas) ou de la duree d'expo (3 cas), 
# soit 6 cas au total (102 -> 96)
df_clean <- magneto %>%
  filter(!is.na(steady),
         EXPMG_Max_Intensity_T !=  "NS"
  ) %>%
  mutate(chronic = case_when(chronic == "chronical" ~ "chronique",
                             chronic == "ponctual" ~ "ponctuel",
                             .default = chronic
  ),
  steady = case_when(steady == "steady" ~ "constant",
                     .default = steady
  ),
  steady = factor(steady, levels = c("constant", "variable", "NS")),
  EXPMG_Max_Intensity_T = as.numeric(EXPMG_Max_Intensity_T),
  life_stage = case_when (PLSS_lifestage1 == "adult" ~ "adulte",
                          PLSS_lifestage1 == "juvenile" ~ "juvénile",
                          PLSS_lifestage1 == "larval" ~ "larvaire",
                          PLSS_lifestage1 == "NS" ~"NS")
  ) %>%
  mutate(exsitu = case_when(exsitu == "In vivo" ~ "In situ",
                            .default = exsitu)
  ) %>%
  mutate(exsitu = factor(exsitu, levels = c("In vitro", "In situ"))
  ) %>%
  filter(!is.na(EXPMG_Max_Intensity_T),
         !is.na(EXPCH_durationofexposure_days),
         EXPMG_Max_Intensity_T != "NS") %>%
  mutate(
    Intensite = as.numeric(EXPMG_Max_Intensity_T)*1000, # passage du CM en mT
    Duree = as.numeric(EXPCH_durationofexposure_days),
    Chronicite = chronic,
    Milieu = exsitu
  )

# Bubble plot

df_bubble_lifestage <- df_clean %>%
  mutate(
    life_stage = case_when(life_stage == "NS" ~"NS",
                           life_stage == "juvénile" ~ "juvenile",
                           life_stage == "adulte" ~ "adult",
                           life_stage == "larvaire" ~ "larvae",
                           .default=life_stage)
      
  ) %>%
  mutate(
    life_stage = factor(
      life_stage,
      levels = c("NS", "larvae", "juvenile", "adult")  # adapter si besoin
    )
  ) %>%
  group_by(Duree, Intensite, life_stage, Milieu) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Intensite_µT = Intensite * 1000)

ggplot(df_bubble_lifestage, aes(x = Duree, y = Intensite_µT)) +
  annotate("rect",
           xmin = 0, xmax = 333,
           ymin = 45, ymax = 55,
           fill = "grey85", alpha = 0.4) +
  geom_point(aes(color = life_stage,
                 shape = Milieu,
                 size = n),
             alpha = 0.85, stroke = 0.7) +
  scale_x_log10(
    name = "Exposure duration (days)",
    breaks = c(1, 10, 100, 1000)
  ) +
  scale_y_log10(
    name = "Magnetic field intensity (µT)",
    breaks = c(0.1, 1, 10, 100, 1000, 10000),
    labels = c("0.1", "1", "10", "100", "1000", "10000")
  )+
  scale_size_continuous(
    name = "Number of\ncase studies",
    range = c(2, 6),
    breaks = c(1, 4, 8, 12)
  ) +
  scale_color_manual(
    name = "Life stage",
    values = c(
      "NS"      = "grey60",
      "larvae"    = "lightpink",
      "juvenile" = "turquoise2",
      "adult"    = "seagreen4"
    )
  ) +
  scale_shape_manual(
    name = "Environment",
    values = c("In vitro" = 16, "In situ" = 2)
  ) +
  annotate("text",
           x = max(df_bubble_lifestage$Duree) * 0.95,
           y = 50,
           label = "Geomagnetic field (~50 µT)",
           hjust = 1, vjust = -0.3,
           size = 3.5, color = "grey20") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  ) +
  # labs(
  #   title = "Exposure duration and magnetic field intensity across life stages",
  #   subtitle = "Life stage structures the distribution of exposure conditions",
  #   caption = "Source: ESCo"
  # ) +
  guides(
    color = guide_legend(override.aes = list(size = 3), order = 2),
    shape = guide_legend(override.aes = list(size = 3), order= 1),
    size = guide_legend(order = 3)
  )


ggsave( filename = "./figures/Figure_2_20260925.png",
                       units = "cm", width = 20, height = 15, dpi = 600
               )

## Figure 3 : chartplots of indicators
##--------------------------------------------------------------------------------------------------------

### 1. définition du tableau de donnees 
#     (il contient  l'identification de l'article  
#                   , le target, la classe taxo
#                   , les indicateurs
#                   , les effets, leur sens et leur significativité,
#                   , le décompte du nombre de cas d'étude pour chaque combinaison)
df_signif <- mergedata_fb_2 %>%
  
  # Création d'une variable indiquant la présence d'une pression CEM
  mutate(
    Pressure_EXPMG = ifelse(EXPMG_Type != "NA", "CEM", NA)
  ) %>%
  
  # Binarisation de la valence d’impact : effet / pas d’effet
  mutate(OICIE_impact_valence_binary = case_when(
    OICIE_impact_valence == "no impact/negligible" ~ "no effect",
    is.na(OICIE_impact_valence) ~ NA,
    .default = "effect"
  )) %>%
  
  # Classification des études : in vivo / in vitro
  mutate(exsitu = case_when(
    Empirical..ex.situ..laboratory.studies. == "No"  ~ "In vivo",
    Empirical..ex.situ..laboratory.studies. == "Yes" ~ "In vitro",
    .default = "NA"
  )) %>%
  
  # Passage en format long pour les colonnes Pressure_*
  pivot_longer(
    cols = starts_with("Pressure_"),   # Sélection des colonnes Pressure_*
    names_to = "Pressure_Type",        # Nom de la colonne contenant le nom de la pression
    values_to = "Pressure"             # Valeur de la pression
  ) %>%
  
  # Suppression des pressions manquantes
  filter(!is.na(Pressure)) %>%
  
  # Conservation uniquement des pressions CEM
  filter(Pressure == "CEM") %>%
  
  # Renommage des colonnes pour simplifier la suite
  rename(
    Indicateur     = OICM_generic_indices,
    Significativite = OICIE_statistical_significance
  ) %>%
  
  # Harmonisation de la valence d’effet
  mutate(Effet_sens = case_when(
    OICIE_impact_valence == "mixed" ~ "variable",
    .default = OICIE_impact_valence
  )) %>%
  
  # Harmonisation des indicateurs et de la significativité
  mutate(
    Indicateur = case_when(
      Indicateur == "mortality" ~ "survival",
      .default = Indicateur
    ),
    Significativite = case_when(
      Significativite == "yes" ~ "significant",
      Significativite == "no"  ~ "not significant",
      is.na(Significativite)   ~ "NS"
    )
  ) %>%
  
  # Regroupement par combinaison de facteurs
  group_by(
    AI_Key,
    Target,
    exsitu,
    PLSS_lifestage1,
    Indicateur,
    OICM_indices,
    Effet_sens,
    OICIE_impact_valence_binary,
    Significativite
  ) %>%
  
  # Création d'une colonne regroupant toutes les valeurs taxonomiques
  # correspondant à chaque combinaison de regroupement
  summarise(
    weight = n(),   # Comptage des occurrences
    AIP_Taxonomicposition_grouped = paste(unique(AIP_Taxonomicposition), collapse = ", "),
    .groups = "drop"   # Suppression du group_by après summarise
  )


df_cible_significativite<- df_signif%>%
  group_by(AI_Key, Target, Indicateur, Significativite, Effet_sens)%>%
  tally(name="count", wt=weight)%>%
  ungroup()%>%
  group_by(Target)%>%
  mutate(percent= count/sum(count)*100)%>%
  mutate(percent=round(percent, 1))%>%
  mutate(fill_cat = case_when(
    Significativite == "NS" ~ "NS",
    Significativite == "not significant" ~ paste0("gris_", Effet_sens),
    Significativite == "significant" ~ paste0("bleu_", Effet_sens)
  ))%>%
  ungroup()


df_pie <- df_cible_significativite %>%
  mutate(
    signif_bin = case_when(
      Significativite == "not significant" ~ 0,
      Significativite == "NS" ~ 0
      , .default = 1
    )
    ,
    effet_cat = case_when(
      signif_bin == 0 ~ "Not significant",
      signif_bin == 1 & Effet_sens == "increase" ~ "Increase",
      signif_bin == 1 & Effet_sens == "decrease" ~ "Decrease",
      signif_bin == 1 & Effet_sens == "variable"   ~ "Variable"
    )
  ) %>%
  group_by(Target, Indicateur
  ) %>% 
  mutate(nb_cas = sum(count)
  )

 ### 2. Palette de couleurs
#------------------------------------------------------------------------------

cols <- c(
  "Not significant" = "grey70",
  "Increase" = "#c6dbef",   # bleu clair
  "Variable"   = "#6baed6",   # bleu moyen
  "Decrease" = "#2171b5"    # bleu foncé
)


 
### 3. Graphique en camembert par cible et par indicateur
#------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Ordre souhaité des indicateurs  (groupes de 2 x 3 indicateurs se rapportant
# à des effets similaires)
ordre_indicateurs <- c( "fitness"
                        , "activity"
                        , "spatial distribution"
                        , "swimming behaviour"
                        , "early life-stages development"
                        , "survival"
                        # , "community biomass"
                        # , "community diversity"
                        )

df_pie_indic <- df_pie %>% 
  # Regrouper les non significatifs 
  mutate( effet_cat2 = case_when( 
    signif_bin == 0 ~ "Not significant"
    , signif_bin == 1 & effet_cat == "Increase" ~ "Increase"
    , signif_bin == 1 & effet_cat == "Decrease" ~ "Decrease"
    , signif_bin == 1 & effet_cat == "Variable" ~ "Variable" )
    # ,
    # Target = case_when(
    #   Target == "Fish" ~ "Poissons",
    #   .default = "Invertébrés benthiques"
    # )
  ) %>% 
  
  # Exclure biomasse et diversité 
   filter(Indicateur %in% ordre_indicateurs) %>% 
  
  # Agrégation par Target × indicateur × catégorie
  group_by(Target, Indicateur, effet_cat2) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  
  # Proportions par Target × indicateur
  group_by(Target, Indicateur) %>%
  mutate(prop = count / sum(count),
         total_cases = sum(count)
  )%>%
  ungroup() %>%
  # Reordonner les indicateurs
  mutate(Indicateur = factor(Indicateur
                             , levels = ordre_indicateurs)
  ) %>% 
  # Reordonner les catégories d’effet 
  mutate(effet_cat2 = factor(effet_cat2
                             , levels = c("Not significant"
                                          , "Increase"
                                          , "Decrease"
                                          , "Variable"))
  ) 


ggplot(df_pie_indic, aes(
  x = "",
  y = prop,
  fill = effet_cat2)) +
  
  geom_col(width = 1, color = "white") +
  
  # --- Labels dans chaque portion --- 
  geom_text( aes(label = scales::percent(prop, accuracy = 1))
             , position = position_stack(vjust = 0.5)
             , colour = "black"
             , fontface = "bold"
             , size = 5 ) +
  
  # --- Label n=XX en bas à droite du panneau --- 
  geom_text( data = df_pie_indic |> distinct(Target, Indicateur, total_cases)
             , aes( x = Inf
                    , y = -Inf
                    , label = paste0("n=", total_cases) )
             , hjust = -2 # léger décalage vers l’intérieur 
             , vjust = -0.3 # léger décalage vers l’intérieur 
             , colour = "grey20"
             # , fontface = "bold"
             , size = 5
             , inherit.aes = FALSE ) +
  
  
  coord_polar(theta = "y") +
  scale_fill_manual(values = cols) +
  
  facet_grid(Target ~ Indicateur) +
  
  my_theme() +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank())+
  
  labs(
    #title = "Répartition des effets par cible et par indicateur",
    fill = "Effect type"
  )

# ggsave( filename = "./figures/Figure_3_20260618.png",
#         units = "cm", width = 40, height = 20, dpi = 1200
# )




##--------------------------------------------------------------------------------------------------------

# informations complémentaires pour rédaction

##--------------------------------------------------------------------------------------------------------


## 1. pourcentages signif par indices

df_pct_indic <- df_pie_indic %>%
  mutate(Signif = effet_cat2 != "Not significant") %>%
  group_by(Indicateur) %>%
  summarise(
    n_signif = sum(count[Signif], na.rm = TRUE),
    n_total  = sum(count, na.rm = TRUE),
    pct_signif = n_signif / n_total
  )


df_pct_indic_target <- df_pie_indic %>%
  mutate(Signif = effet_cat2 != "Not significant") %>%
  group_by(Indicateur, Target) %>%
  summarise(
    n_signif = sum(count[Signif], na.rm = TRUE),
    n_total  = sum(count, na.rm = TRUE),
    pct_signif = n_signif / n_total
)





## 2. Les conditions experimentales des cas significatifs

names(magneto)

magneto <- magneto %>% mutate(OICIE_impact_valence = case_when( 
                                            OICIE_impact_valence == 'mixed' ~ "variable",
                                            .default = OICIE_impact_valence),
                              OICM_generic_indices = case_when(
                                            OICM_generic_indices == "mortality" ~ "survival",
                                            .default = OICM_generic_indices)
                              )

magneto_signif <- magneto %>% filter(OICIE_statistical_significance== "yes")



ggplot(magneto_signif , aes(y = (as.numeric(EXPMG_Max_Intensity_T)*10^6)
                               , x = EXPMG_Type)) +
  geom_boxplot() +
  geom_point(aes(shape = Target,
                 color = PLSS_lifestage1,
                 ),
    position = position_jitter(width = 0.15), alpha = 0.6) +
    scale_y_log10()

 
magneto_signif_invitro <- magneto_signif %>% filter(exsitu == "In vitro")
summary(as.numeric(magneto_signif_invitro$EXPMG_Max_Intensity_T))

ggplot(magneto_signif_invitro , aes(y = (as.numeric(EXPMG_Max_Intensity_T)*10^6)
                            , x = EXPMG_Type)) +
  geom_boxplot() +
  geom_point(aes(shape = Target,
                 color = PLSS_lifestage1,
  ),
  position = position_jitter(width = 0.15), alpha = 0.6) +
  scale_y_log10()

mergedata_signif[as.numeric(mergedata_signif$EXPMG_Max_Intensity_T)== 0.0064, ]
mergedata_signif[as.numeric(mergedata_signif$EXPMG_Max_Intensity_T)<= 0.001, ]



ggplot(magneto_signif_invitro , aes(y = (as.numeric(EXPCH_durationofexposure_days))
                                    , x = EXPMG_Type)) +
  geom_boxplot() +
  geom_point(aes(shape = Target,
                 color = PLSS_lifestage1,
  ),
  position = position_jitter(width = 0.15), alpha = 0.6) +
  scale_y_log10()

summary(as.numeric(magneto_signif_invitro$EXPCH_durationofexposure_days))


# l'effort d'échantillonnage
mergedata_signif <- mergedata_fb_2 %>% filter(exsitu == "in vitro",
                          OICIE_statistical_significance == "yes")

ggplot(mergedata_signif , aes(y = CVESTE_sampling_nb*as.numeric(CVESTE_replicate_nb)
                                    , x = EXPMG_Type)) +
  geom_boxplot() +
  geom_point(aes(shape = Target,
                 color = PLSS_lifestage1,
  ),
  position = position_jitter(width = 0.15), alpha = 0.6) #+
#  scale_y_log10()

# la distance max à la source
ggplot(mergedata_signif , aes(y = EXPCH_max_distancetosource_M
                              , x = EXPMG_Type)) +
  geom_boxplot() +
  geom_point(aes(shape = Target,
                 color = PLSS_lifestage1,
  ),
  position = position_jitter(width = 0.15), alpha = 0.6) #+
#  scale_y_log10()

max_value <- max(mergedata_signif$EXPCH_max_distancetosource_M)
mergedata_signif$AI_Key[which(mergedata_signif$EXPCH_max_distancetosource_M ==
                                      max_value)]

min_value <- min(mergedata_signif$EXPCH_max_distancetosource_M[
                          mergedata_signif$EXPCH_max_distancetosource_M > 0
                             ], na.rm = TRUE)
AI_Key_min_dist <- mergedata_signif$AI_Key[which(mergedata_signif$EXPCH_max_distancetosource_M ==
                                                   min_value)]

mergedata_signif_authors <- mergedata_signif %>%
  filter(AI_Key %in% AI_Key_min_dist) %>%
  distinct(AI_Key, AI_Short_Author, AI_Publication_Year)

         
# en in situ
ai_key_signif_insitu <- mergedata_fb_2 %>% filter(exsitu == "in situ", OICIE_statistical_significance=="yes")%>% distinct(AI_Key)

Pour_Discussion <- mergedata_fb_2  %>%
  filter(!AI_Key %in% ai_key_signif_insitu$AI_Key) %>%
  distinct(AI_Key, AI_Short_Author, AI_Publication_Year
           , Target
           , AIP_Taxonomicposition
           , PLSS_lifestage1
           , EXPCH_max_distancetosource_M
           , EXPCH_durationofexposure_days
           , CVESTE_sampling_nb
           , CVESTE_replicate_nb
           , CVESTE_control
           , exsitu)


library(ggplot2)
library(ggrepel)

Pour_Discussion%>% 
  filter(exsitu== "in vitro")%>%
  filter(as.numeric(CVESTE_replicate_nb)!=500 )%>%
ggplot(aes(
  x = as.numeric(CVESTE_replicate_nb),
  y = CVESTE_sampling_nb,
  color = PLSS_lifestage1
)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = AI_Key),
    size = 2,
    vjust = -0.5,   # léger décalage vertical
    hjust = 0.5     # léger décalage horizontal
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()




#----------------------------------------------------------------------------#
#
# Supplementary material
#
#----------------------------------------------------------------------------#


# Table S.2: indicators

table_s2_indicators <- mergedata_fb_2 %>% 
  group_by(OICM_indices, OICM_generic_indices) %>% 
  summarise(
    OICM_units = paste(unique(OICM_units), collapse = "; "),
    .groups = "drop"
  )%>%
  rename (Indicator = OICM_indices,
          Generic_indicator = OICM_generic_indices,
          Units = OICM_units)%>%
  mutate(Generic_indicator = case_when(Generic_indicator == "mortality" ~ "survival",
                   .default= Generic_indicator)
         )%>%
  arrange(Generic_indicator, Indicator, Units)


# Export sous différents formats
write.csv(
  table_s2_indicators,
  "./SuppMat/Table_S2_Indicators.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

library(writexl)
write_xlsx(
  table_s2_indicators,
  "./SuppMat/Table_S2_Indicators.xlsx"
)

install.packages("flextable")
library(flextable)
library(dplyr)
table_s2_formatted <- flextable(table_s2_indicators)
table_s2_formatted <- table_s2_formatted %>%
  autofit() %>%
  set_caption("Table S2. Indicators, general indicator categories, and associated measurement units.") %>%
  theme_booktabs()

#export en word formaté
install.packages("officer")
library(flextable)
library(officer)
library(dplyr)

# --- Création du flextable ---
ft_s2 <- flextable(table_s2_indicators)

# Largeurs de colonnes adaptées au format A4 portrait (≈ 16 cm utilisables)
ft_s2 <- width(ft_s2, j = "Generic_indicator", width = 5.5) %>%  # conversion cm → inches
  width(j = "Indicator", width = 4) %>%
  width(j = "Units", width = 6.5)

# Style ICES : sobre, lisible, sans couleurs
ft_s2 <- ft_s2 %>% autofit()%>%
  theme_booktabs() %>%        # style propre, lignes fines
  fontsize(size = 10) %>%     # taille standard pour suppléments
  align(align = "left", part = "all") # %>%
  # autofit()

# Titre du tableau (ICES style)
caption_text <- "Table S2. Generic indicators, specific indicators, and associated measurement units used in the analysis."

ft_s2 <- set_caption(ft_s2, caption = caption_text)

# --- Export Word ---
doc <- read_docx()
doc <- body_add_par(doc, caption_text, style = "heading 1")
doc <- body_add_flextable(doc, ft_s2)

# Impression du document Word
print(doc, target = ".//SuppMat/Table_S2_Indicators.docx")

