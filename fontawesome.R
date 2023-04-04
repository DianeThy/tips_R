library(extrafont)
extrafont::font_import (path="~/Downloads", pattern = "fa-", prompt =  FALSE)

#install.packages("grDevices", force = TRUE)
library(grDevices)
loadfonts(device = "pdf")

library(tidyverse)
extrafont::fonttable() %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(grepl("Awesom", FamilyName)) %>% 
  select(FamilyName, FontName, fontfile)



library(showtext)
font_add(family = "FontAwesome5Free-Solid", regular = "~/Downloads/fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", regular = "~/Downloads/fa-regular-400.ttf")
font_add(family = "FontAwesome5Brands-Regular", regular = "~/Downloads/fa-brands-400.ttf")
showtext_auto()

library(waffle)
waffle(
  c(`Poor=10` =10, `Average=18` = 18, `Excellent=7` =7), rows = 5, colors = c("#FD6F6F", "#93FB98", "#D5D9DD"),
  use_glyph = "female", glyph_size = 12 ,title = 'Girls Performance', legend_pos="bottom"
)
