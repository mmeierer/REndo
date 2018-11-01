# REndo 
Endogeneity arises when the independence assumption between an explanatory variable and the error in a statistical model is violated. Among its most common causes are omitted variable bias (e.g. like ability in the returns to education estimation) measurement error (e.g. survey response bias) or simultaneity (e.g. advertising and sales). 

Instrumental variable estimation is a common treatment when endogeneity is of concern. However valid, strong external instruments are difficcult to find. Consequently, statistical methods to correct for endogeneity without external instruments have been advanced and they are generically called **internal instrumental variable models (IIV)**. 

REndo implements the following instrument-free methods: 
(1) latent instrumental variables approach (Ebbes, Wedel, Boeckenholt, and Steerneman 2005), 
(2) higher moments estimation(Lewbel 1997), 
(3) heteroskedastic error approach (Lewbel 2012), 
(4) joint estimation using copula (Park and Gupta 2012) 
(5) multilevel GMM (Kim and Frees 2007). 
              
Install stable version from CRAN:
```
install.packages("REndo")
```

Install development version from GitHub:
```
library(devtools)
install_github("mmeierer/REndo")
```



