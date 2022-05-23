#--------------------------------------------------------------------
# Script Name:   map_naris.R
# Description:   Map NARI locations on world map
# Author:        Brandon Monier
# Created:       2022-05-23 at 13:52:36
# Last Modified: 2022-05-23 at 15:11:24
#--------------------------------------------------------------------

library(ggplot2)
library(googlesheets4)
library(magrittr)


## Parameters ----
sheetUrl <- "https://docs.google.com/spreadsheets/d/1zE1zjkSZywKHsDkp7OcBOB6dfOakEseWomz0sMmT87Y/edit?usp=sharing"
mapAes <- list(
    countryCol = "lightgrey",
    countryBorders = "lightgrey",
    nariSize = 2,
    nariCols = palette.colors()[-1]
)


## Get coordinate data ----
ilciCoords <- sheetUrl %>% read_sheet()


## Plot ----
ilciMap <- ilciCoords %>%
    ggplot() +
    borders(fill = mapAes$countryCol, colour = mapAes$countryBorders) +
    aes(x = long, y = lat, color = nari_institute, size = mapAes$nariSize) %>%
    geom_point() +
    scale_size_continuous(guide = "none") +
    scale_color_manual(name = "NARI:", values = palette.colors()[-1] %>% as.vector()) +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()
    )


## Show ----
ilciMap %>% print()


