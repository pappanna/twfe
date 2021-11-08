# TWFE Recent Literature
Data and code for TWFE/DID recent literature discussion, Columbia SDev Colloquium on November 5, 2021. 

--- 
This repository contains the following: 
- **Data**: Data from Stevenson and Wolfers (2006) and Cheng and Hoekstra (2013) to test different DiD methods.
- **Code**: Stata and R code to run TWFE and event-study for data, as well as _Bacon Decomposition_ and alternative TWFE methods: _Sun and Abraham (2021)_, _Callaway and Sant'Anna (2021)_, _Borusyak et al. (2021)_ and _Wooldridge (2021)_ (only in Stata). The Stata code is nicer (for now).
- **Literature**: Papers for new methods used in the code.
- **Output**: Folder for saving results.
- **Simulation**: Rmd file and corresponding html file to run simulations to illustrate the issues with TWFE. The code enables to generate fake data and implements quick analyses (most probably somehow flawed) to understand the contribution of different parameters to the issue.
- **Summary**: Very short summary of methods and datasets.

--- 
Notes: 
- Potential issue with Sun and Abraham event study method in Stata, but runs well in R. 
- Callaway and Sant'anna a lot faster in R. 

