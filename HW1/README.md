---
title: "HW 1: "
author: "Ian Effendi"
course: "ISTE 780: Data Driven Knowledge Discovery"
instructor: "Professor Qi Yu"
---

## Assignment

### Part 1

You should write: "I certify that I indeed finished reading Ch. 2 from *An Introduction to Statistical Learning*, by James Gareth, Daniela Witten, Trevor Hastie, Robert Tibshirani."

...assuming you indeed read that material.

### Part 2

Perform the following tasks, which you should document in a brief report, including R code and results:

1. Read in the data from the object, which was saved with the `save(galaxies, file="galaxies.RData")` R command.
2. Perform EDA of the data.
3. Fit a linear no-intercept model (called Hubble's Law): $velocity = \beta_1 * distance + \epsilon$
4. Assess the quality of the model fit, but do not explore other models.
5. Estimate $\beta_1$ (called Hubble's constant), including units. Hubble's constant is given in $km * sec^{-1} * Mpc^{-1}$. A mega-parsec ($Mpc$) is $3.086 * 10^{19} km$. Velocity data is given in $\frac{km}{s}$. Distance is in $Mpc$.
6. Find $\beta_1^{-1}$ (which approximates the age of the universe) in `seconds` and then transform it into `years`.

## Deliverables

Homework assignments should include both the R-Markdown (`*.Rmd`) file and an output PDF (`*.pdf`) file. If you experience difficulties producing the PDF, you can instead submit a Word (`*.docx`) file output from your R-Markdown code.

When you start working on homework, and especially when doing more complex cacluations, you should program in a regular R script file: not in an R-markdown file, to make debugging easier.
