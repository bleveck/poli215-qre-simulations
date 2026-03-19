# POLI 215: QRE and Noisy Learning Simulations

Companion simulation code for **POLI 215: Introduction to Game Theory**, Week 9 (Learning in Games and Behavioral Game Theory) at UC Merced.

These materials let you explore Quantal Response Equilibrium (QRE), smooth fictitious play, and how bounded rationality affects strategic behavior in classic games.

## Files

### `matching_pennies_sim.R`

Core simulation functions for matching pennies. Includes a QRE solver (logit equilibrium) and smooth fictitious play dynamics. Source this file to use the functions in your own scripts, or run it directly to see example output.

```r
source("matching_pennies_sim.R")
```

### `qre_learning_notebook.Rmd`

Interactive RMarkdown notebook with guided exercises on QRE and learning. Work through the cells to build intuition for how the rationality parameter (lambda) affects equilibrium behavior and how fictitious play converges over time.

Open in RStudio and run chunks interactively, or knit the full document:

```r
rmarkdown::render("qre_learning_notebook.Rmd")
```

### `qre_shiny_app.R`

Interactive Shiny web app that lets you adjust parameters (lambda, number of rounds, game payoffs) and see QRE and learning dynamics update in real time.

```r
shiny::runApp("qre_shiny_app.R")
```

### `volunteers_dilemma_sim.R`

QRE solver for the volunteer's dilemma, an n-player public goods game. Explores how the number of players and the rationality parameter jointly determine the probability of volunteering.

```r
source("volunteers_dilemma_sim.R")
```

## Requirements

- **R** (>= 4.0)
- R packages: `shiny`, `rmarkdown`, `knitr`

Install the packages with:

```r
install.packages(c("shiny", "rmarkdown", "knitr"))
```

## Quick Start

```bash
git clone https://github.com/bleveck/poli215-qre-simulations.git
cd poli215-qre-simulations

# Run the Shiny app
Rscript -e 'shiny::runApp("qre_shiny_app.R")'

# Or open qre_learning_notebook.Rmd in RStudio
```

## Author

Brad LeVeck, UC Merced
