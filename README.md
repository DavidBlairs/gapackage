## GA-Package

![enter image description here](https://raw.githubusercontent.com/DavidBlairs/gapackage_submit/main/docs/ga_package_logo.png)

The GA-Package is a package designed for the R programming language which allows the user to quickly construct, tune and analyse a genetic algorithm. It contains a number of built in operators and examples. 

The documentation can be found [here](https://davidblairs.github.io/) 

###   Installation

You will need to install the following pieces of software: 
 1. R version 4.1.3 from [here](https://cran.r-project.org/bin/windows/base/)
 2. R Studio (Recommended) from [here](https://www.rstudio.com/products/rstudio/download/#download)
 3. RTools4 from [here](https://cran.r-project.org/bin/windows/Rtools/rtools40.html)

After this is done, you need to install the devtools package using the following command from the R terminal: 

    install.packages("devtools")
Then you can install the package using the following command: 

    devtools::install_github("DavidBlairs/gapackage_submit", force = TRUE)
