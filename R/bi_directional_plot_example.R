#-----------------------------------------------------#
# Title:  Bi-Directional Bar Plot for ggplot2         #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   06.12.16                                    #
#-----------------------------------------------------#

#--------------
# Load packages
#--------------

# Prelim
pack.man <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c('ggplot2')
pack.man(packages)


#---------------------
# Mock data generation
#---------------------

# Data frame synthesis
myc.treatment <- factor(c('NM', 'NM', 'Myc', 'Myc'))
tissue        <- factor(c('shoot', 'root', 'shoot', 'root'))
biomass       <- c(10, -15, 20, -25)
se            <- c(2.8, 1.3, 3.4, 2.4)
exp.df        <- data.frame(myc.treatment, tissue, biomass, se)

# Inverse order of 'tissue' variable
exp.df$tissue <- factor(
  exp.df$tissue, 
  levels = rev(
    levels(
      exp.df$tissue
      )
    )
  )

exp.df


#--------------
# Visualization
#--------------

ggplot(data = exp.df) +
  aes(
    x    = as.factor(myc.treatment),
    y    = biomass,
    fill = tissue 
  ) +
  geom_bar(
    stat     = 'identity',
    position = 'identity',
    color    = 'black',
    size     = 1.0
  ) +
  geom_errorbar(
    aes(
      ymin = biomass - se,
      ymax = biomass + se
    ),
    size  = 0.9,
    width = 0.2
  ) +
  scale_fill_manual(
    values = c(
      '#454545', 
      '#F5F5F5'
    ),
    name = 'Tissue\nType'
  ) +
  xlab(
    'Mycorrhizal Treatment'
  ) +
  ylab(
    'Biomass (g)'
  ) +
  ggtitle(
    'Average Biomass between Mycorrhizal and Non-Mycorrhizal Treatments'
  ) +
  theme_bw()
  