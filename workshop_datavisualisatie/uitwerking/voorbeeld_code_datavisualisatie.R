library(tidyverse)
library(glue)
# devtools::install_github("HHSK-wkl/HHSKwkl")
library(HHSKwkl)
library(ggtext)

theme_set(hhskthema())

dataset <- readxl::read_excel("workshop_datavisualisatie/dataset_visualisatie.xlsx")

# Basiscode
dataset %>% 
  ggplot(aes(gem_bedekking, nednaam)) +
  geom_col()

# Complete code
dataset %>% 
  # Aanpassingen aan de data om de juiste teksten weer te kunnen geven
  mutate(Kranswieren = ifelse(str_detect(naam, "Chara"), "Kranswier", "Andere soort")) %>% 
  mutate(kleur = ifelse(str_detect(naam, "Chara"), blauw, grijs)) %>%
  mutate(face = ifelse(str_detect(naam, "Chara"), "b", "span")) %>% 
  mutate(nednaam_label = glue("<{face} style='color: {kleur}'>{nednaam}</{face}> <br><span style='color: #999999'> n = {aantal_locs}</span>")) %>% 
  # Het plotten zelf                
  ggplot(aes(gem_bedekking, fct_reorder(nednaam_label, gem_bedekking), fill = Kranswieren)) +
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), 
                     labels = ~scales::percent(.x / 100), breaks = scales::pretty_breaks(10)) +
  scale_fill_manual(values = c("Kranswier" = blauw, "Andere soort" = grijs_m), 
                    guide = guide_legend(reverse = TRUE, title = "")) +
  # Tekst is belangrijk
  labs(title = "Kranswieren zijn dominant aanwezig in de Kralingse Plas",
       # subtitle = "2023",
       x = "Gemiddeld bedekkingspercentage",
       y = "",
       caption = "Waarnemingen zijn gedaan in 2023",
  ) +
  # Opschonen zodat er geen overbodige dingen in het plot staan
  theme(axis.text.y = element_markdown(hjust = 0),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "top")

ggsave("workshop_datavisualisatie/uitwerking/gem_bedekking.png")

# scatterplot basis
dataset %>% 
  ggplot(aes(gem_bedekking, aantal_locs, label = nednaam)) +
  geom_smooth(se = FALSE, method = "lm", colour = grijs_l, linetype = "dashed") +
  geom_point(color = grijs, size = 2) +
  ggrepel::geom_text_repel(hjust = 0, color = grijs_m) +
  scale_x_log10(breaks = scales::breaks_log(n = 8), labels = ~scales::percent(.x / 100)) +
  scale_y_log10(breaks = scales::breaks_log(n = 8)) +
  labs(title = "Soorten met een hogere bedekking komen ook op meer plaatsen voor",
       x = "Gemiddelde bedekking (logaritmisch)",
       y = "Aantal locaties (logaritmisch)") +
  theme(plot.title.position = "plot"        )

ggsave("workshop_datavisualisatie/uitwerking/bedekking_vs_locs.png")


