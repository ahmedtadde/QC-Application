# Install and Load Pacman library
if(!require("pacman")) install.packages("pacman",dependencies = T)
library(pacman)

# Install OR Load shiny & tm libraries
p_load('shiny')
p_load('tm')

# Run Application
runGitHub("QC-Application","ahmedtadde")