<img src="inst/shiny/ACMCalculator/www/WHO-WPRO_Logo_PMS_2925.png" width = 300 alt="WHO WPRO Logo"/>[<img src="inst/shiny/ACMCalculator/www/UCLADepartmentofStatisticsSmall.png" align="right" width=500 alt="UCLA STAT Logo"/>](http://statistics.ucla.edu/)

World Health Organization (WHO) WPRO all cause of mortality and excess death calculator
==========

Welcome to the All Cause of mortality and Excess Death Monitoring calculator!

This calculator has been developed by the World Health Organization, Western Pacific Region in conjunction with the Department of Statistics at
UCLA .

This web application is written with the Shiny framework and development is via GitHub. More information on Shiny and our GitHub repository can
be found in the resource links on the right.

The app can be run in two ways:  

* Online, through the [WHO WPRO shinyapps.io server](https://worldhealthorg.shinyapps.io/WPRO-all-cause-of-mortality-and-excess-death-calculator/). 

* Offline by first installing it on your local machine using the ACMCalculator R package. First install `R` (if you have not already). Then open `R` and type in the `R` Console:

```r
source("https://faculty.stat.ucla.edu/handcock/ACMCalculator.R")`
```

then `R` should install the app and close. There will then be an app called "ACMCalculator" in your "Documents" folder. If you double-click on that, it will run the calculator in the browser (no R/Rstudio involved).

* Alternatively, you can install it on your local machine using the ACMCalculator R package. First install it for `R`:
```r
install.packages("devtools")
devtools::install_github("WorldHealthOrganization/ACMcalculator")    
```
then run it from `R`:
```r
ACMCalculator::run()
```

More info on the ACMCalculator wiki:   
https://github.com/WorldHealthOrganization/ACMcalculator/wiki

Please use the GitHub repository to report bugs or request features:
https://github.com/WorldHealthOrganization/ACMcalculator
