
# Miscellaneous R Scripts

Random R script ideas throughout the ages...

## Layout

| Folder | What is in it |
| --- | --- |
| `R/bioinformatics/` | Genomics, rTASSEL, GFF, and sequencing scripts |
| `R/games/` | Game of Life, mazes, and console games |
| `R/generative_art/` | Fractals, attractors, chaos games, avatars, and other art |
| `R/learning/` | Practice scripts for R basics, OOP, and coding puzzles |
| `R/plotting/` | Plot examples, themes, and color palettes |
| `R/shiny/` | Shiny app examples |
| `R/utilities/` | Small tools and one-off helpers |
| `data/` | Example data used by some scripts |

## Usage

Run scripts from the root of the repository, since some of them load
other files with paths like `R/games/game_of_life_dictionary.R`.

To install the packages the scripts use, run:

```r
source("install.R")
```
