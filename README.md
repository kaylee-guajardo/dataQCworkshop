# dataQCtools scripts and example data

This repository (repo) contains the three main dataQCtools scripts as well as example data for the purposes of practicing with or demonstrating the code. The data QC code here is drawn from [dataQCtools](https://github.com/tribalxg/dataQCtools), which has more documentation and an R package made from an older version of this code.

## Necessary packages

You will need to have the following five R packages installed on your computer (`tidyverse` is actually a suite of multiple R packages): `tidyverse`, `readxl`, `plotly`, `htmlwidgets`, `runner`. If you don't have any of the packages, you can run the following code:

```{r}
install.packages(c("tidyverse", "readxl", "plotly", "htmlwidgets", "runner"))
```

Otherwise, you can use install.packages just on the packages you need. For instance, if you just need `htmlwidgets` and `runner`, you can run the following code:

```{r}
install.packages(c("htmlwidgets", "runner"))
```

Or if you just need `htmlwidgets`, you can run the following code:

```{r}
install.packages("htmlwidgets")
```

## A note about using functions from packages

In this code, most or all functions from packages are written `packagename::functionname`, such as `dplyr::mutate`. Usually you probably see the function without the packagename specified, like `mutate`. If you load the package using `library(dplyr)`, then you can use `mutate` without specifying that it is from `dplyr`. Therefore, you often see code in which the packages are loaded using `library()`, and then the package name is omitted when calling the functions from those packages. Even though I have loaded the packages at the beginning of these scripts so it's clear what packages are being used, here are two reasons I have opted to still show the package name in each function call as in `dplyr::mutate`:

1. I adapted this code into a dataQCtools R package, and in R packages these package dependencies are made explicit.
2. It shows you what package each function comes from.


## Past and upcoming workshops using this repo

- PNWTCG in-person meeting, Mar 2026
- NAWM Advanced R workshop, Nov 2025

## Quarto references (shared as part of the NAWM workshop)

- https://r4ds.hadley.nz/quarto.html
- https://quarto.org/docs/get-started/hello/rstudio.html
- https://pierre-navaro.quarto.pub/hello-quarto/#/hello-quarto-title
- Automated solution for including all images from a folder in your quarto document: https://www.cynthiahqy.com/posts/layout-folder-images/
