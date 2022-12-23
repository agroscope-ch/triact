
# change file extension to .Rmd.orig

library(knitr)

# Execute the code from the vignette

knitr::knit("vignettes/triact.Rmd.orig", output = "vignettes/triact.Rmd")

# make sure figures are in vignette (not figures) and path is correct in vignette

# add rendered pdf, html to inst/doc
