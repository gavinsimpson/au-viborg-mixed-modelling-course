# Applied mixed modelling with R

### Aarhus University PhD Course

* Autumn 2024 running: August 13<sup>th</sup> &ndash; 15<sup>th</sup>

* ECTS credits: 1.5 ECTS

* Language: English

* Fee: 350 DKK

## Name of course leader

Gavin Simpson, Assistant Professor, Department of Animal and Veterinary Sciences, Aarhus University gavin@anivet.au.dk

### Registration

To register for the course, please contact Julie Jensen on jsj@anivet.au.dk.

<!-- ### Slides

* [Monday](https://gavinsimpson.github.io/au-multivariate-stats/slides/01-dissimilarity-clustering-diversity/slides.html)

* [Tuesday](https://gavinsimpson.github.io/au-multivariate-stats/slides/02-unconstrained-ordination/slides.html)

* [Wednesday](https://gavinsimpson.github.io/au-multivariate-stats/slides/03-constrained-ordination/slides.html)

* [Thursday](https://gavinsimpson.github.io/au-multivariate-stats/slides/04-permutation-tests/slides.html)

* [Friday](https://gavinsimpson.github.io/au-multivariate-stats/slides/05-other-stuff/slides.html)

### Computing

* [Monday](https://gavinsimpson.github.io/au-multivariate-stats/computing/01-cluster-analysis/cluster-analysis.html)

* [Tuesday](https://gavinsimpson.github.io/au-multivariate-stats/computing/02-unconstrained-ordination/unconstrained-ordination.html)

* [Wednesday](https://gavinsimpson.github.io/au-multivariate-stats/computing/03-constrained-ordination/constrained-ordination.html)

* [Thursday](https://gavinsimpson.github.io/au-multivariate-stats/computing/04-permutation-tests/permutation-tests.html)

-->

## Objectives of the course

The course will provide an applied introduction to generalized linear mixed modelling in R for biologists. The course will equip participants to fit appropriate models to data using R and the lme4 and glmmTMB packages, how to test the assumptions of the fitted model and assess the adequacy of fit, and how to use the model to estimate quantities of interest or test hypotheses of interest using the marginaleffects package.

## Learning outcomes and competences

After completing the course, participants will

1. have a good introductory understanding of the concepts of fixed and random effects and mixed or hierarchical modelling in general,
2. be able to choose an appropriate method to use to analyse a data set,
3. know how to diagnose problems with fitted models,
4. be able to use the R statistical software to analyse multivariate data
5. be able to use the R statistical software and in particular the *lme4*, *glmmTMB*, and *marginaleffects* packages to fit and analyse generalized linear mixed effects models.

## Compulsory programme

Active participation in the course including attendance at lectures and completion of computer-based classes and exercises. Completion of short, computer-based assessments testing their understanding of a topic and the practical skills taught. For credit, students must complete a data analysis exercise to be submitted one week after the end of the course (23rd August).

## Course content

The course is based on a series of lectures and computer-based practical classes led by an international expert in biological data analysis, who has expertise in mixed and hierarchical modelling.

The course covers the following topics:

* Generalized linear models for data that are not Gaussian
* Fixed and random effects in Generalized linear mixed models (GLMMs)
* Fitting GLMMs with the lme4 and glmmTMB packages
* Model diagnostics and assessment
* Estimating marginal effects and adjusted predictions with GLMMs
* Hypothesis testing using GLMMs
* Displaying model estimates and reporting results

## Prerequisites

This course is suitable for Phd students (including senior thesis-based masters students) and researchers working with biological data where observations are correlated or grouped in some way, such as longitudinal data, or experimental data with blocking. The course will be of particular interest to PhD candidates and researchers in inter alia biology, animal science, ecology, agriculture. Some prior knowledge of R is required, and some prior knowledge of generalized linear modelling in R would be an advantage.

<!-- ## Computing requirements

Participants need to bring their own laptop with the latest version of R installed (version 4.4.0 or later), as well as the current version of RStudio. If you use another editor for your R code feel free to use it instead of Rstudio, but we cannot help you if you encounter problems with it.

You can download R from [cloud.r-project.org](https://cloud.r-project.org/) and select from the three links at the top of the page as required for your operating system.

You can download RStudio from [www.rstudio.com](https://www.rstudio.com/products/rstudio/download/#download) and choose from the list of **installers** as appropriate for your operating system.

If you have already installed R and RStudio, please check that they are both up-to-date. Within R you can run:

```r
version
```

and look at the entry next to `version.string`:

```
r$> version                                                                     
               _                           
platform       x86_64-pc-linux-gnu         
arch           x86_64                      
os             linux-gnu                   
system         x86_64, linux-gnu           
status                                     
major          4                           
minor          2.1                         
year           2022                        
month          06                          
day            23                          
svn rev        82513                       
language       R                           
version.string R version 4.2.1 (2022-06-23)
nickname       Funny-Looking Kid
```

This should include `4.4.x` if you are running the latest release, but should be no lower than `4.2.0`. If the installed version of R is < 4.4.0, install a newver version of R by downloading and running one of the installers from [cloud.r-project.org](https://cloud.r-project.org/) as mentioned above.

To check that RStudio is up-to-date, open RStudio, open the Help menu, and choose *Check for Updates*. RStudio will then check to see if there is a newer version available and if there is it will give you the option to download the newer version.

Prior to arriving at AU Viborg on the 19th of September, make sure you have updated your installed R packages and that you have installed the following packages: tidyverse, vegan, mvabund, boral, ecoCopula, and cocorresp. To do this, open RStudio (or R) and in the console window (usually lower left, with a prompt that looks like `>`) run

```r
parallel::detectCores(logical = FALSE)
```

This checks to see how many CPU cores you have available, which we use in the next chunk. 

```r
update.packages(ask = FALSE, checkBuilt = TRUE, Ncpus = 4)
```

Change the value of `Ncpus` to the number cores you have on your computer as this will speed up package updates if you have many packages installed that require updating. If you want to work while this is being done, set `Ncpus` to a number less than that returned by `parallel::detectCores(logical = FALSE)`.

Now we can install the required packages

```r
install.packages(c("tidyverse", "vegan"))
```

-->

## Name of course leader

Gavin Simpson, Assistant Professor, Department of Animal and Veterinary Sciences, Aarhus University gavin@anivet.au.dk
