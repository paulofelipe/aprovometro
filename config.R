if(!require(checkpoint)){
  install.packages('checkpoint')
}

library(checkpoint)

checkpoint('2017-12-07')

if(!require(devtools)){
  install.packages('devtools')
}

library(devtools)

# Precisa de softwares auxiliares para a instalação
# Ver https://github.com/Microsoft/LightGBM/tree/master/R-package
install_github("Microsoft/LightGBM", subdir = "R-package")
