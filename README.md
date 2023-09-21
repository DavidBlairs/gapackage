# Final Year Project
This is the R package I created as part of the final year project. This was a requirement in the bachelor degree at Brunel University London. The project was written in $LaTeX$ and all the code was written in R. The write up can be found in `Final_Year_Project.pdf` along with all the package documentation [here](https://davidblairs.github.io/). The abstract:

><em>In this paper, we will discuss a number of different applications of genetic algorithms as well as their theoretical and historical background. We have built a package for the programming language R which makes the construction and subsequent parameter optimisation easier and quicker to accomplish. While this package is ideal in terms of its design, it is not without its limitations, most notably its long execution time compared to low-level implementations. With this package, a variety of examples are explored. We have looked at how one may minimise distances between two points in Euclidean and Non-Euclidean space. For the Non-Euclidean case, we have used a matrix function to represent the relative transformation to the space at any point. As well as this, we have provided a way of deriving this matrix from a multivariant function which represents the relative transformation.  We then looked at a number of alternative ways to measure distance such as the Manhattan and Minkowski metrics to further test the usability of the package. Lastly, we explored how someone may integrate a physics simulation as an alternative way of calculating the fitness score for each genotype. This is very much a ``proof of concept” but does demonstrate our initial intent and provides a starting point for further research into this area. </em>

The R code has the capability to view a graph of the fitness function over time and modify parameters live during the genetic algorithms execution using the R Shiny package. We also expored how you can use a physics engine, **matter.js**, embedded inside of an R Shiny window in order to act as a simulated fitness function for solving the brachistochrone problem. You can find all this information in `Final_Year_Project.pdf` along with all the package documentation [here](https://davidblairs.github.io/) 

---
## GA-Package (Old README)

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
