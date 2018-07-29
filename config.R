# sudo apt-get install libssl-dev 
# sudo apt-get instal cmake
# sudo apt-get install libxml2-dev 
if(!require(checkpoint)){
  install.packages('checkpoint')
}

library(checkpoint)

dir.create('~/.checkpoint')

checkpoint('2018-07-07')

if(!require(devtools)){
  install.packages('devtools')
}


library(devtools)

# Precisa de softwares auxiliares para a instalação
# Ver https://github.com/Microsoft/LightGBM/tree/master/R-package
install_github("Microsoft/LightGBM", subdir = "R-package")
