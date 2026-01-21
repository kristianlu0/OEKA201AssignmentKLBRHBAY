#devtools::install_github("dickoa/gretlReadWrite")
usedvw <- gretlReadWrite::read.gdt("gdt/usedvw.gdt")
newcars <- gretlReadWrite::read.gdt("gdt/newcars.gdt")
usethis::use_data(usedvw, overwrite = TRUE)
usethis::use_data(newcars, overwrite = TRUE)



