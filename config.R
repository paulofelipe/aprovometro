if(!require(checkpoint)){
  install.packages('checkpoint')
}

library(checkpoint)

checkpoint('2017-12-07')

if(!require(devtools)){
  install.packages('devtools')
}

library(devtools)
install_github("Microsoft/LightGBM", subdir = "R-package")
