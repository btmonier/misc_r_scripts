#--------------------------------------------------------------------
# Script Name:   map_naris.R
# Description:   Map NARI locations on world map
# Author:        Brandon Monier
# Created:       2022-05-23 at 13:52:36
# Last Modified: 2022-05-23 at 15:22:18
#--------------------------------------------------------------------

library(ggplot2)
library(googlesheets4)
library(magrittr)


## Parameters ----
## TEST
mapAes <- list(
    coordUrl       = "1zE1zjkSZywKHsDkp7OcBOB6dfOakEseWomz0sMmT87Y",
    countryCol     = "grey50",
    countryBordCol = "grey50",
    nariSize       = 2,
    nariCols       = palette.colors()[-1]
)


## Get coordinate data ----
ilciCoords <- mapAes$coordUrl %>% read_sheet()


## Plot ----
ilciMap <- ilciCoords %>%
    ggplot() +
    borders(fill = mapAes$countryCol, colour = mapAes$countryBordCol) +
    aes(x = long, y = lat, color = coi, size = mapAes$nariSize) %>%
    geom_point() +
    scale_size_continuous(guide = "none") +
    scale_color_manual(name = "NARI:", values = palette.colors()[-1] %>% as.vector()) +
    guides(color = guide_legend(override.aes = list(size=5))) +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold")
    )


## Show ----
ilciMap %>% print()


