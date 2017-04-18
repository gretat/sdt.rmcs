# sdt.rmcs
### Signal Detection Analysis with simulated data, or with data input for 1 or 2 groups

This package, allows running the basic signal detection analysis, 
by calculating the hit rate, false alarm rate, miss rate and correct rejection rate, from 
alreaddy pulled data. i.e. one row per participant, and a column for each 
total of hits, misses, false alarms, and correct rejections. It calculates 
the **d\'** and **bias** <sup>1</sup> for each individual person and a mean **d\'** and **bias** with standard deviations. 

It produces box plots to graphically represent the distributions of **d\'** and **bias** and density plots
for the signal and noise distributions based on the mean **d\'** and its standard deviation. The noise distributions, 
on the other hand, have a mean of 0 and sd=1 as suggested by  *Thomas Wickens* <sup>2</sup> ( the noise distribution is assumed to 
have a normal distributions in comparison to the Signal + Noise one). And the doted line represents the mean **criterion** (bias). 

Reseiver Operating Characteristic (ROC) curves are also displayed and the area under the curve (AUC) is calculated.
ROC is plotted based on mean **d'** and a **criterion** dot based on the mean **bias** is placed on the curve. 


The package allows to run the signal deection functions functions without any input. This will automatically invoke 
the appropriate data simulation functions which will be used to generate example data. 

Each signal detectin analysis function returns a list of the graphs, and calculated values, as well as the datasets containing the 
calculated values for rates, z-trasnorm hitr rate and false alarm rate and d'prime and bias for further calculations if required. 

### Installation
``` r
# From GitHub
# install.packages("devtools")
devtools::install_github("gretat/sdt.rmcs")
```
### Functions
* Simulate data for one group (1000 participants) **sdt.data**
* Simulate data for two groups (1000 participants) **sdt.data2**
* Perform SDT analysis for one group **rate.statistics**
* Perform SDT analysis for two groups **rate.statistics2**


Read the vignette for step-by-step instructions
``` r
vignette(sdt.rmcs)
```

#### NOTE
It is important to note that small or big SD for the d'Prime will have an effect on the signal distribution, which will be seen in the 
density functions. Those will have an effect on the ROC curve and might have an effect  on the AUC calculation. If too big violations are observed either from the numbers themselves or in the density plots, tthe calculations returned by these function of the AUC should be reviewed and possible alternatives considered.


### Reference
<sup>1</sup> Macmillan, N. A., & Creelman, C. D. (2004). Detection theory: A user's guide. Psychology press.


<sup>2</sup> Wickens, T. D. (2002). *Elementary signal detection theory*. Oxford University Press, USA.
