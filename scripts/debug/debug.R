detach("package:mapspam2globiom", unload = T)
library(here)

# Set root folder, which is defined by RStudio project
root <- here()

source(file.path(root, "scripts/01_model_setup/01_model_setup.r"))

