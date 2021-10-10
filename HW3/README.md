---
title: "HW 3: Liver Analysis (LDA)"
author: "Ian Effendi"
course: "ISTE 780: Data Driven Knowledge Discovery"
instructor: "Professor Qi Yu"
---

## Package Details

<!-- HW3 -->

This package contains 2 datasets:

* `liver_data`: Liver malfunction data on 345 men from an undisclosed experiement. (Source: From course instructor).

* `severity_map`: Encoding map of severity groups to the appropriate `severity` labels for classification. (Source: `data-raw/severity_map.R`)

## Installation

Project dependencies are managed with `renv`. Simply call `renv::restore()` to install the missing project dependencies.

```R
# Install the dependencies.
renv::restore()    # Called from the project root directory.
```

Requirements are stored in the [requirements.txt](requirements.txt) file.

## Assignment

### Part 1

You should write: "I certify that I indeed finished reading Ch. 4 from *An Introduction to Statistical Learning*, by James Gareth, Daniela Witten, Trevor Hastie, Robert Tibshirani."

...assuming you indeed read that material.

### Part 2

For this assignment, you will be using data about liver disorders, stored in `Liver.txt`.

#### `Liver.txt`

- Data on 345 men.
- Variables 1:5 are quantitative results of blood tests sensitive to liver disorders.
- Variable 6: Number of alcoholic beverages drunk per day.
- Variable 7: Severity of liver malfunctioning, in two groups:
  - Group 1: `"severe"`,
  - Group 2: `"not severe"`,
- Task: Predict Variable 7 (severity).

#### Assignment Details

Perform the following tasks, which you should document in a brief report, including R code and results (use modified versions of functions used in lectures):

1. Read in the data from the text file, `Liver.txt`. Classify variable 7 (`severity`) into the two groups using linear discriminant analysis (LDA).
   1. Assume prior probabilities based on the available sample (dataset).
2. Minimize the total error of misclassification by changing the prior probabilities for the two populations.
   1. Show the results graphically, as we did in class (error rates vs. prior on the severe group).
   2. Show the optimal values, including the misclassification table for the optimal case.
3. Repeat `Step 1` and `Step 2` with 10-fold CV.
   1. Use 100 rounds.
   2. Calculate the means and standard errors of the minimized total error of misclassification.
5. Repeat `Step 1` and `Step 2` with 3-fold CV.
   1. Use 100 rounds.
   2. Calculate the means and standard errors of the minimized total error of misclassification.

## Deliverables

Homework assignments should include both the R-Markdown (`*.Rmd`) file and an output PDF (`*.pdf`) file. If you experience difficulties producing the PDF, you can instead submit a Word (`*.docx`) file output from your R-Markdown code.

When you start working on homework, and especially when doing more complex cacluations, you should program in a regular R script file: not in an R-markdown file, to make debugging easier.
