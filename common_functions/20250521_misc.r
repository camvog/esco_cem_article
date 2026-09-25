# labels
pressure <- data.frame(name = c("Chemical",
                                "Physical",
                                "Biological",
                                "Human activities",                                   
                                "Other/Not Described",
                                "Other/Not Described", 
                                "Artificial reserve effect / exclusion zone", 
                                "Human activities around OWF",
                                "Introduction of artifical reef (new habitat)", 
                                "Input or spread of non-indigenous species", 
                                "Input or spread of indigenous species",
                                "Structures as obstacles", "Physical destruction/disturbance of substrates", 
                                "Change in conditions (hydrological/atmospheric)",
                                "Input of litter, nutrients, organic matter and water", 
                                "Input of other forms of energy", 
                                "Input of anthropogenic sound",
                                "Input of chemical pollutants"
                                ),
                        short = c("Chemical",
                                  "Physical",
                                  "Biological",
                                  "Anthropic",                                   
                                  "ND",
                                  "ND", "Reserve", "Human activities",
                                  "Artifical reef", "Non-indigenous", "Indigenous",
                                  "Obstacles", "Substrates", "Hydrological/atmospheric",
                                  "Waste", "Energy", "Sound", "Chemical"
                                  ),
                        level = c(rep(1, 5), rep(2, 13))
                        )

target <- data.frame(name = c("Birds",
                              "Chiroptera",
                              "Cephalopods",
                              "Fish",
                              "Marine mammals",
                              "Reptiles",
                              "Benthic macro-invertebrates",
                              "Pelagic macro-invertebrates",
                              "Marine plants, phytoplankton, cyaonobacteria & algae",
                              "Other micro-organisms","Broader than categories above",
                              "Other/Not Described"
                              ),
                     short = c("Bird", "Chiroptera", "Cephalopods", "Fish", "Mammals", "Reptiles", "Benthic",
                               "Pelagic", "Plants", "Micro-organisms", "Broader", "ND")
                     )

ebv <- c("Genetic composition" , 
         "Species populations", 
         "Species traits", 
         "Community composition",
         "Ecosystem structure", 
         "Ecosystem functioning", 
         "Social ecological system"
         )
