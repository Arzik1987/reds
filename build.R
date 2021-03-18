library(devtools)
library(roxygen2)

check()
document()
build()
install(getwd(), upgrade = "never")

