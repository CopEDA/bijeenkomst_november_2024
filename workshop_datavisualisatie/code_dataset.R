library(tidyverse)
library(glue)
# devtools::install_github("HHSK-wkl/HHSKwkl")
library(HHSKwkl)

theme_set(hhskthema())


biologie <- data_online("biologie.rds")

dataset <- 
  biologie %>% 
  filter(str_detect(mp, "VKP_[0|1]")) %>% 
  filter(year(datum) == 2023) %>% 
  select(-contains("stadium"), -tijd, -methode) %>% 
  # rename(waarde = waarde_totaal) 
  
  summarise(aantal_locs = n(),
            fractie_locs = aantal_locs / 128,
            gem_bedekking = mean(waarde_totaal),
            eenheid = "%",
            .by = naam) %>% 
  mutate(nednaam = twn::twn_localname(naam)) 


dataset %>% 
  write_csv2("workshop_datavisualisatie/dataset_visualisatie.csv") %>% 
  openxlsx::write.xlsx("workshop_datavisualisatie/dataset_visualisatie.xlsx")
