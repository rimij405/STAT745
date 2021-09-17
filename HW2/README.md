---
title: "HW 2: Liver Analysis"
author: "Ian Effendi"
course: "ISTE 780: Data Driven Knowledge Discovery"
instructor: "Professor Qi Yu"
---

## Assignment

### Part 1

You should write: "I certify that I indeed finished reading Ch. 3 from *An Introduction to Statistical Learning*, by James Gareth, Daniela Witten, Trevor Hastie, Robert Tibshirani."

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

Perform the following tasks, which you should document in a brief report, including R code and results (use modified versions of functions used in Lecture 3):

1. Read in the data from the text file, `Liver.txt`.
2. Predict Variable 7 (`severity`) using logistic regression.
   1. Setup `p = probability of belonging to Group 1`, with remaining variables as predictors.
3. Minimize the total error of misclassification by changing the threshold for `p`.
   1. Show the results graphically, as we did in class.
   2. Show the optimal values, including the misclassification table for the optimal case.
4. Repeat `Step 3` with 10-fold CV.
   1. Use 100 rounds.
   2. Calculate the means and standard errors of the minimized total error of misclassification.
5. Repeat `Step 3` with 3-fold CV.
   1. Use 100 rounds.
   2. Calculate the means and standard errors of the minimized total error of misclassification.

## Deliverables

Homework assignments should include both the R-Markdown (`*.Rmd`) file and an output PDF (`*.pdf`) file. If you experience difficulties producing the PDF, you can instead submit a Word (`*.docx`) file output from your R-Markdown code.

When you start working on homework, and especially when doing more complex cacluations, you should program in a regular R script file: not in an R-markdown file, to make debugging easier.
