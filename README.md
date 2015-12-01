# REndo 
R Package implementing state of the art methods to controll for endogeneity when no instrumental variables are available.

The first version of the package comprises two methods, the latent instrumental variables (LIV) method proposed by Ebbes et al. (2005), and the higher moments approach proposed by Lewbel (1997). 

A second verion of the pacakge will incorporate the joint estimation method using Gaussian copulas (Park and Gupta, 2012) and the mixed generalized method of moments proposed by Kim and Frees (2007). The later method can be used to treat endogeneity in multilevel models.

Proof of concept for this project 
              
Install by using the following R commands

```
library(devtools)
install_github("mmeierer/REndo", auth_token = "INSERT YOUR TOKEN HERE")
```

To create your token, see here:
https://help.github.com/articles/creating-an-access-token-for-command-line-use/


