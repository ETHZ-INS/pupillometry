# pupillometry

A shiny app for the secondary analysis of pupillometry data.

A live version of the app can be accessed on [shinyapps.io](https://plger.shinyapps.io/pupillometry_app).

To use on your computer, first install the package with
```{r}
devtools::install_github("ETHZ-INS/pupillometry")
```

Then run the app with:
```{r}
library(pupillometry)
pupillometry.app()
```

This app is has been tested and is compatible with following operating systems:
windows 10
linux ???

Following package versions have been tested and are compatible with this app

data.table v. 1.12.6
R.matlab v. 3.6.2
shiny v. 1.4.0
plotly v.4.9.0 
shinydashboard v. 0.7.1
shinycssloaders v. 0.3
colourpicker v. 1.0
MASS v. 7.3-51.4
lme4 v.1.1-21
lmerTest v.3.1-0
emmeans v. 1.4.2
stringr v. 1.4.0
DT v. 0.9
randomcoloR v. 1.1.0.1
imputeTS v. 3.0
