
theme_set(theme_bw())
my_theme <- function() {
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size = 12), #change legend title font size
        legend.text = element_text(size = 8) #change legend text font size
        )
}

# targets
esco_target <- c("Birds", "Marine mammals", "Reptiles", "Fish",
                 "Chiroptera", "Cephalopods", "Pelagic macro-\ninvertebrates", 
                 "Benthic macro-\ninvertebrates", 
                 "Marine plants\nphytoplankton\ncyaonobacteria & algae", 
                 "Other micro-organisms", "Broader than\ncategories above", "Other"
                 )

esco_level1 <- c("Biological", "Chemical", "Physical", "Human activities", "Other", "Not described")

# a function to extract sp list
extract_sp_list <- function(x) {
  if(is.na(x)) {
    out <- NA
  } else {
    if(!str_detect(x, pattern = ";") && !str_detect(x, pattern = ",") && !str_detect(x, pattern = " and ")) {
      out0 <- x
    } else {
      out0 <- NULL
    }
    if(str_detect(x, pattern = ";")) {
      out1 <- str_split(x, pattern = ";")
    } else {
      out1 <- NULL
    }
    if(str_detect(x, pattern = ",")) {
      out2 <- str_split(x, pattern = ",")
    } else {
      out2 <- NULL
    }
    if(str_detect(x, pattern = " and ")) {
      out3 <- str_split(x, pattern = " and ")
    } else {
      out3 <- NULL
    }
    
    out <- c(out0, out1, out2, out3) %>%
      unlist() %>%
      trimws() %>%
      tolower() %>%
      gsub("\\s+", " ", .) %>% # remove double space
      unique() %>%
      sapply(., recap) %>%
      as.vector()
  }
  return(out)
}

# a function to capitalize the first letter of Latin names
recap <- function(x) {
  out <- x %>%
    str_sub(., start = 1, end = 1) %>%
    toupper() %>%
    paste0(.,
           x %>%
             str_sub(., start = 2, end = -1)
    ) %>%
    as.character()
  return(out)
}

# function to do some recoding for easier plotting
cleanup <- function(df, what = c("target", "level 1", "level 2", "EBV"), species_name = TRUE) {
  if(what == "target") {
    out <- df %>%
      mutate(Target = trimws(Target),
             Target = case_when(Target == "Benthic macro-invertebrates " ~ "Benthic macro-\ninvertebrates", # why is this not working?!
                                Target == "Pelagic macro-invertebrates" ~ "Pelagic macro-\ninvertebrates",
                                Target == "Other - Please comment" ~ "Other",
                                Target == "Other micro-organisms (except phytoplankton & cyanobacteria)" ~ "Other micro-organisms",
                                Target == "Marine plants, phytoplankton, cyaonobacteria & algae" ~ "Marine plants\nphytoplankton\ncyaonobacteria & algae",
                                Target == "Broader than categories above" ~ "Broader than\ncategories above",
                                .default = Target
                                )
             ) %>%
      filter(!Target %in% c("Not described - EXCLUDED")) %>%
      mutate(Target = factor(Target, levels = esco_target),
             Target = ifelse(is.na(Target), "Benthic macro-\ninvertebrates", as.character(Target)),
             Target = factor(Target, levels = esco_target)
             )
    
    if(species_name) {
      out <- out %>%
        mutate(species_name = str_replace(species_name, pattern = " ", replacement = "\n"))
    }
  }
  if(what == "level 2") {
    out <- df %>%
      mutate(`Pressure (level 2)` = case_when(`Pressure (level 2)` == "Input or spread of indigenous species" ~ "Input or spread of\nindigenous species",
                                              `Pressure (level 2)` == "Input or spread of non-indigenous species" ~ "Input or spread of\nnon-indigenous species",
                                              `Pressure (level 2)` == "Introduction of artifical reef (new habitat)" ~ "Introduction of\nartifical reef",
                                              `Pressure (level 2)` == "Input of chemical pollutants or change in chemical conditions" ~ "Input of\nchemical pollutants" ,
                                              `Pressure (level 2)` == "Artificial reserve effect / exclusion zone" ~ "Reserve effect, \n exclusion zone",
                                              `Pressure (level 2)` == "Human activities around OWF" ~ "Human activities\naround OWF",
                                              `Pressure (level 2)` == "Change in conditions (hydrological/atmospheric)" ~ "Change in conditions\n(hydrological/atmospheric)",
                                              `Pressure (level 2)` == "Input of anthropogenic sound" ~ "Input of\nanthropogenic sound",
                                              `Pressure (level 2)` == "Input of litter, nutrients, organic matter and water" ~ "Input of litter, nutrients,\norganic matter and water",
                                              `Pressure (level 2)` == "Input of other forms of energy" ~ "Input of other\nforms of energy",
                                              `Pressure (level 2)` == "Structures as obstacles" ~ "Structures\nas obstacles",
                                              `Pressure (level 2)` == "Physical destruction or disturbance of natural structures or modifcation of substrates" ~ "Physical destruction/disturbance\nof natural structures/substrates",
                                              .default = `Pressure (level 2)`
                                              )
      )
  }
  if(what == "level 1") {
    out <- df %>%
      mutate(`Pressure (level 1)` = ifelse(`Pressure (level 1)` == "Other - Please Comment", "Other", `Pressure (level 1)`))
  }
  if(what == "EBV") {
    out <- df %>%
      mutate(EBV = case_when(EBV == "Intraspecific genetic diversity" ~ "Intraspecific\ngenetic diversity",
                             EBV == "Genetic differentiation" ~ "Genetic\ndifferentiation",
                             EBV == "Effective population size" ~ "Effective\npopulation size",
                             EBV == "Population migration" ~ "Population\nmigration",
                             EBV == "Distributions and abundances" ~ "Distributions\n& abundances",
                             EBV == "Demographic structure" ~ "Demographic\nstructure",
                             EBV == "Swimming characteristics (individual level)" ~ "Swimming characteristics\n(individual level)",
                             EBV == "Community abundance" ~ "Community\nabundance",
                             EBV == "Trait diversity" ~ "Trait\ndiversity",
                             EBV == "Interaction diversity" ~ "Interaction\ndiversity",
                             EBV == "Taxonomic/phylogenetic diversity" ~ "Taxonomic/phylogenetic\ndiversity",
                             EBV == "Other Ecosystem functions (biotic)" ~ "Other Ecosystem\nfunctions (biotic)",
                             EBV == "Other Ecosystem functions (abiotic)" ~ "Other Ecosystem\nfunctions (abiotic)",
                             EBV == "Ecosystem type horizontal distribution" ~ "Ecosystem type\nhorizontal distribution",
                             EBV == "Social-ecological-system (across ecosystem including humans)" ~ "Social-ecological\nsystem",
                             .default = EBV
                             )
             )
  }
  return(out)
}

## rarefaction curve
rarefaction <- function(target) {
  ### subset on target
  all_df <- map_dfr(.x = 1998:2024,
                    .f = function(x) {
                      out <- clean_species %>%
                        cleanup(., what = "target", species_name = FALSE) %>%
                        select(-Key) %>%
                        filter(Target == target,
                               `Publication Year` == x
                               ) %>%
                        rename(Year = `Publication Year`)
                      if(nrow(out) == 0) {
                        out <- data.frame(species_name = NA,
                                          Year = x,
                                          Target = target
                                          )
                        }
                      return(out)
                    }
                    )
  out <- map_dfr(.x = 1998:2024,
                 .f = function(x) {
                   df <- data.frame(Target = target,
                                    Year = x,
                                    cumul = all_df %>%
                                      filter(Year <= x,
                                             !is.na(species_name)
                                             ) %>%
                                      pull(species_name) %>%
                                      unique() %>%
                                      length(),
                                    param = "n_species"
                                    )
                   }
                 )
  return(out)
}
