# Carbon pricing for the energy transition (prototype)

This repository contains an R code and a shiny app for understanding the impacts of carbon pricing for revenue generation and spending. It is a work in progress.

## Installation

Clone this repository, navigate to the `carpentr` directory, then run:

```
devtools::install()
library(carpentr)
```

## Front end development

A shiny app is located in `inst/shiny/carpentr`, and can be edited from within there. To run the app, run:

```
shiny::runApp('inst/shiny/carpentr')
```

## Back end development

R functions are located in the `R` directory, and can be run and tested within the R markdown file `inst/rmd/backend_development.Rmd`

To edit an existing function, edit the R script, then run `devtools::install()` to build the package. If you have changed the documentation, also run `devtools::document()` to update the documentation.

To add a new function, create a new R script in the R directory, using one of the existing functions as a template. Be sure edit the roxygen comments so that the documentation is correct and any imports are included.
