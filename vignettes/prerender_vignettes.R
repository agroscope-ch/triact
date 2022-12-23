
# change file extension to .Rmd.orig

library(knitr)

# Execute the code from the vignette

knitr::knit("vignettes/triact.Rmd.orig", output = "vignettes/triact.Rmd")
