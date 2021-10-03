---
title: "Homework 2 Report"
author: "Ian Effendi"
date: September 20, 2021
output:
  html_notebook:
    default
  html_document:
    theme: united
    highlight: tango
    toc: true
    toc_depth: 2
    keep_md: yes
  rmarkdown::html_vignette:
    theme: united
    highlight: tango
    toc: true
    toc_depth: 2
    toc_float: true
  pdf_document:
    toc: true
    toc_depth: 2
    keep_tex: true
vignette: >
  %\VignetteIndexEntry{Homework 2 Report}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
knit: (function(inputFile, encoding){
  rmarkdown::render(inputFile, encoding = encoding, 
  output_dir = "inst/doc", 
  output_format="all") })
---

## Overview

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 


```r
library(ggplot2)

ggplot(data = iris, aes( x = Sepal.Length, y = Petal.Length, colour = Species)) +
  geom_point()
```

![](C:\Users\effen\OneDrive\DOCUME~1\RIT\STAT74~1\Homework\HW2\inst\doc\HW2_RE~1/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Content {.tabset}

### Tab 1

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

### Tab 2

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
