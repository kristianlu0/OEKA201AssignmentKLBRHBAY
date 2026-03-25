# Inititate or create a data-raw folder inside the package project
# usethis::use_data_raw()
# One way (among many ways) to edit the file is by in the console of RStudio execute
file.edit("data-raw/data_transformation.R")
#devtools::install_github("dickoa/gretlReadWrite")
usedvw <- gretlReadWrite::read.gdt("gdt/usedvw.gdt")
newcars <- gretlReadWrite::read.gdt("gdt/newcars.gdt")
usethis::use_data(usedvw, overwrite = TRUE)
usethis::use_data(newcars, overwrite = TRUE)



