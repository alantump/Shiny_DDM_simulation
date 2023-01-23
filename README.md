# Shiny_DDM_simulation

This code allows you to run an App showcasing the most important features of the drift-diffusion model. 
To run the app on R install the necessary packages: 
````
install.packages(c("shiny", "dplyr", "tidyr","RWiener", "reshape2", "ggplot2","cowplot"))
````
And run the app by entering
````

library(shiny)
library(cowplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RWiener)
library(reshape2)


runGitHub( "Shiny_DDM_simulation", "alantump")
````
