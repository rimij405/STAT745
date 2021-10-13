# R/analysis/lda.R
#
# LDA helpers.

## ---- analysis::lda::imports ----

library(MASS)
source(here::here("R/utils.R"))
source.submodule(files = UTILS$printf)

## ---- analysis::lda::exports ----

.calc.priors <- function(.data, truth = (.data[,Y] == TRUE), ..., weight = 1) {
  n_samples <- nrow(.data)
  p <- sum(truth) / n_samples
  q <- (1 - p)
  if (weight != 1) {
    p <- weight * p
    pq <- p + q
    p <- p / pq
    q <- q / pq
  }
  return(list(
    # Keep order of categories as the response.
    n_samples = n_samples,
    priors = c(q,p),
    p = p,
    q = q,
    weight = weight
  ))
}

.calc.total.error <- function(mat) {
  ratio <- sum(diag(mat)) / sum(mat)
  return(1 - ratio)
}

.calc.total.miss <- function(mat) {
  return(mat[1,2] + mat[2,1])
}

.calc.total.cost <- function(mat, penalty = 1) {
  fp <- mat[1,2]
  fn <- mat[2,1]
  cost <- penalty * fp + fn
  # printf("Penalty (x%s), FP = %s, FN = %s cost = %s",
  #        penalty, fp, fn, cost)
  return(penalty * mat[1,2] + mat[2,1])
}

.fit.model <- function(.data, truth = (.data[,Y] == TRUE), ...,
                       validation = .data,
                       formula = Y ~ ., weight = 1, penalty = 1) {
  # .data: tbl_df/tbl/data.frame
  # truth: logi vector
  # formula: language used for the lda algorithm.
  # ...: additional params.

  # Get the extra arguments.
  arguments <- modifyList(
    list(formula = formula, data = quote(.data)),
    list(...), keep.null = FALSE)

  # Prepare model.
  model <- list()

  # Calculate priors with provided weight.
  if (exists("prior", where = arguments)) {
    model$priors <- arguments$prior
  } else {
    model$settings <- .calc.priors(.data, truth, weight = as.numeric(weight))
    model$priors <- model$settings$priors
    arguments$prior <- model$priors
  }

  # Fit model for specified parameters.
  model$obj <- do.call(MASS::lda, args = arguments)

  # Calculate the error rate for the given weight.
  model$predict <- predict(model$obj, newdata = validation)

  # Create the misclassification table.
  model$table <- table(model$predict$class, truth)
  model$penalty <- penalty
  model$cost  <- .calc.total.cost(model$table, penalty = penalty)
  model$miss <- .calc.total.miss(model$table)
  model$error <- .calc.total.error(model$table)

  # Return model results.
  return(model)
}

#' Calculate the optimal error rates
#' for a dataset, using the lda algorithm.
#'
#' @param .data Dataset to fit on.
#' @param truth Ground truth to evaluate against.
#' @param ... Additional parameters to pass to the model,
#' @param from Minimum weight possible.
#' @param to Maximum weight possible.
#' @param m Number of weights to test within range of [from, to]
#'
#' @return Named list containing optimal_error and optimal_weight.
.calc.optimal.error.rates <- function(
  .data, truth = (.data[,Y] == TRUE), ...,
  minimize_cost = FALSE,
  formula = Y ~ .,
  from = 0.001, to = 10, m = 50,
  penalty = 1) {

  # Weight vector.
  weights <- seq(from = from, to = to, length = m)
  n_weights <- length(weights)

  println("---------------------------")
  if (minimize_cost) {
    println("Finding optimal cost with weighted priors.")
  } else {
    println("Finding optimal error rates with weighted priors.")
  }
  printf("# Penalty factor applied to cost function: x%s", penalty)
  printf("# Checking %s positive class priors in range [%s, %s]:",
         n_weights, from, to)
  str(weights)

  # Prepare the error results.
  # - Weight: Tested weight for a given set of error rates.
  # - FN: False negative. Misclassification error for "Severe"
  # - FP: False positive. Misclassification error for "Not Severe".
  # - COST: Total cost of misclassification.
  # - p: Prior for the positive class. (Reminder: q = (1 - p))
  errors_df <- tibble(
    weight = double(),
    fp = integer(),
    fn = integer(),
    miss = integer(),
    cost = double(),
    error = double(),
    p = double()
  )

  # Calculate the error rates.
  for(i in 1:n_weights) {

    # Fit model with weighted priors.
    model <- .fit.model(.data, truth, formula = formula, weight = weights[[i]], penalty = penalty)

    # Pull results from the model.
    errors_row <- tibble_row(
      weight = weights[[i]],
      fp = model$table[1,2],
      fn = model$table[2,1],
      miss = model$miss,
      cost = model$cost,
      error = model$error,
      p = model$settings$p
    )

    # Save values to the data.frame.
    # - Uses dplyr::add_row()
    errors_df %<>% add_row(errors_row)

  }

  # Calculate the `q` values.
  errors_df %<>% mutate(q = 1 - p)

  # Fit model without priors, for comparison.
  baseline <- .fit.model(.data, truth, formula = formula, weight = 1, penalty = penalty)

  if (minimize_cost) {
    # If minimizing by cost:
    min_df <- errors_df %>%
      arrange(cost, error, miss, fp, fn, weight) %>%
      slice_min(cost)
  } else {
    # If minimizing by error:
    min_df <- errors_df %>%
      arrange(error, cost, miss, fp, fn, weight) %>%
      slice_min(error)
  }

  # Fit model with optimal priors, for comparison.
  n_candidates <- nrow(min_df)

  if (n_candidates > 1) {
    if (minimize_cost) {
      printf("Several 'minimum cost' candidates. Using average among %s candidates.", n_candidates)
    } else {
      printf("Several 'minimum error' candidates. Using average among %s candidates.", n_candidates)
    }
    min_df <- min_df %>%
      summarize(
        weight = mean(weight),
        fp = mean(fp),
        fn = mean(fn),
        miss = mean(miss),
        cost = mean(cost),
        error = mean(error),
        p = mean(p),
        q = mean(q)
      )
  }

  optimal <- .fit.model(.data, truth, formula = formula, weight = min_df$weight, penalty = penalty)

  optimal_priors <- list(
    p = min_df$p,
    q = min_df$q
  )

  # Prepare results.
  res <- list(
    penalty = penalty,
    results = min_df,
    baseline = baseline,
    optimal = optimal,
    optimal_weight = min_df$weight,
    optimal_error = min_df$error,
    optimal_cost = min_df$cost,
    optimal_priors = optimal_priors,
    # Provide all the calculated errors as well.
    errors = errors_df
  )

  if (minimize_cost) {
    printf("Summary of minimum cost search:")
  } else {
    printf("Summary of minimum error rate search:")
  }
  printf("Min. total cost: %s (Penalty of Misclassification = x%s)", round(min_df$cost, digits = 4), penalty)
  printf("Min. total error: %s", round(min_df$error, digits = 4))
  printf("Min. total miss: %s", min_df$miss)
  printf("Optimal q/p ratio: %s", res$optimal_weight)
  printf("Optimal weight of priors: p <- (%s * q)", round(min_df$weight, digits = 4))
  printf("Optimal priors: p = %s, q = %s",
    round(min_df$p, digits = 4),
    round(min_df$q, digits = 5)
  )
  println("Summary: ")
  str(min_df)
  println("---------------------------")

  # Return list of values.
  return(res)
}

#' Calculate the optimal error rates
#' for a dataset, using the lda algorithm.
#'
#' @param .data Dataset to fit on.
#' @param truth Ground truth to evaluate against.
#' @param ... Additional parameters to pass to the model,
#' @param from Minimum weight possible.
#' @param to Maximum weight possible.
#' @param m Number of weights to test within range of [from, to]
#'
#' @return Named list containing optimal_error and optimal_weight.
.calc.optimal.prior.error.rates <- function(
  .data, truth = (.data[,Y] == TRUE), ...,
  minimize_cost = FALSE,
  formula = Y ~ .,
  from = 0.001, to = 0.999, m = 25,
  penalty = 1) {

  # Priors table.
  priors <- tibble(p = seq(from = from, to = to, length = m))
  priors %<>% mutate(q = 1 - p)
  priors %<>% filter(if_all(everything(), ~ . < 1) & if_all(everything(), ~ . > 0))
  n_priors <- nrow(priors)

  println("---------------------------")
  if (minimize_cost) {
    println("Finding optimal cost with explicit priors.")
  } else {
    println("Finding optimal error rates with explicit priors.")
  }
  if (penalty != 1) {
    printf("# Penalty factor applied to cost function: x%s", penalty)
  }
  printf("# Checking %s pos class priors in range [%s, %s]:",
         n_priors, from, to)
  str(priors)

  # Prepare the error results.
  # - Weight: Tested weight for a given set of error rates.
  # - FN: False negative. Misclassification error for "Severe"
  # - FP: False positive. Misclassification error for "Not Severe".
  # - COST: Total cost of misclassification.
  # - p: Prior for the positive class. (Reminder: q = (1 - p))
  errors_df <- tibble(
    p = double(),
    q = double(),
    fp = integer(),
    fn = integer(),
    miss = integer(),
    cost = integer(),
    error = double()
  )

  # Calculate the error rates.
  for(i in 1:n_priors) {

    # Fit model with weighted priors.
    priors_i <- priors %>%
      slice(n = i) %>%
      relocate(q, p) # Rearrange for the model fitting.
    model <- .fit.model(.data, truth, formula = formula, prior = unlist(priors_i), penalty = penalty)

    # Pull results from the model.
    errors_row <- tibble_row(
      p = priors_i$p,
      q = priors_i$q,
      fp = model$table[1,2],
      fn = model$table[2,1],
      miss = model$miss,
      cost = model$cost,
      error = model$error
    )

    # Save values to the data.frame.
    # - Uses dplyr::add_row()
    errors_df %<>% add_row(errors_row)

  }

  # Fit model without set priors, for comparison.
  baseline <- .fit.model(.data, truth, formula = formula, penalty = penalty)

  # Select entry with minimum error (same as minimum cost).
  if (minimize_cost) {
    # If minimizing by cost:
    min_df <- errors_df %>%
      arrange(cost, error, miss, fp, fn, p) %>%
      slice_min(cost)
  } else {
    # If minimizing by error:
    min_df <- errors_df %>%
      arrange(error, cost, miss, fp, fn, p) %>%
      slice_min(error)
  }

  # Fit model with optimal priors, for comparison.
  n_candidates <- nrow(min_df)

  if (n_candidates > 1) {
    if (minimize_cost) {
      printf("Several 'minimum cost' candidates. Using average among %s candidates.", n_candidates)
    } else {
      printf("Several 'minimum error' candidates. Using average among %s candidates.", n_candidates)
    }
    min_df <- min_df %>%
      summarize(
        p = mean(p),
        q = mean(q),
        fp = mean(fp),
        fn = mean(fn),
        miss = mean(miss),
        cost = mean(cost),
        error = mean(error)
      )
  }

  optimal_priors <- c(min_df$q, min_df$p)
  optimal <- .fit.model(.data, truth, formula = formula, prior = optimal_priors, penalty = penalty)

  # Prepare results.
  res <- list(
    penalty = penalty,
    results = min_df,
    baseline = baseline,
    optimal = optimal,
    optimal_weight = min_df$q / min_df$p,
    optimal_error = min_df$error,
    optimal_cost = min_df$cost,
    optimal_priors = list(
      p = min_df$p,
      q = min_df$q
    ),
    # Provide all the calculated errors as well.
    errors = errors_df
  )

  if (minimize_cost) {
    printf("Summary of minimum cost search:")
  } else {
    printf("Summary of minimum error rate search:")
  }
  printf("Min. total cost: %s (Penalty = x%s)", round(min_df$cost, digits = 4), penalty)
  printf("Min. total error: %s", round(min_df$error, digits = 4))
  printf("Min. total miss: %s", min_df$cost)
  printf("Optimal q/p ratio: %s", res$optimal_weight)
  printf("Optimal priors: p = %s, q = %s",
         round(min_df$p, digits = 4),
         round(min_df$q, digits = 5)
  )
  println("Summary: ")
  str(min_df)
  println("---------------------------")

  # Return list of values.
  return(res)
}

.slice.min.cost <- function(df) {
  min_df <- df %>% slice_min(cost, n = 1)
  n_candidates <- nrow(min_df)
  if (n_candidates > 1) {
    printf("Calculating optimal values as average from %s candidates tied for minimized cost.", n_candidates)
    min_df %<>% summarize(
      fp = mean(fp),
      fn = mean(fn),
      error = mean(error),
      cost = mean(cost),
      miss = mean(miss),
      p = mean(p),
      q = mean(q)
    )
  }
  return(min_df)
}

.slice.min.error <- function(df) {
  min_df <- df %>% slice_min(error, n = 1)
  n_candidates <- nrow(min_df)
  if (n_candidates > 1) {
    printf("Calculating optimal values as average from %s candidates tied for minimized error rate.", n_candidates)
    min_df %<>% summarize(
      fp = mean(fp),
      fn = mean(fn),
      error = mean(error),
      cost = mean(cost),
      miss = mean(miss),
      p = mean(p),
      q = mean(q)
    )
  }
  return(min_df)
}

#' Plot the optimal error (or cost) rates.
#'
#' @param errors Dataframe containing priors, fp, fn, miss, cost, and error.
#' @param from Minimum prior probability value to display.
#' @param to Maximum prior probability value to dipslay.
#' @param minimize_cost Display optimal cost instead of optimal error.
#' @param digits Number of digits to round values to when displaying.
.plot.optimal.error.rates <- function(
  errors,
  ...,
  from = (errors %>% slice_min(p, n = 1, with_ties = FALSE))$p,
  to = (errors %>% slice_max(p, n = 1, with_ties = FALSE))$p,
  minimize_cost = FALSE, debug = FALSE,
  digits = 4, penalty = 1,
  opts_visible = list(
    points = TRUE,
    lines = TRUE,
    references = TRUE,
    annotations = TRUE
  )
) {
  # Save state.
  previous_digits <- getOption("digits", 10)

  # Set state.
  options(digits = digits)

  # Ensure from/to arguments are bounded.
  from <- if (from <= 0) 0 else from
  to <- if (to >= 1) 1 else to
  if (from > to) {
    stop("Range error. `from` cannot be greater than `to`.")
  }

  println("---------------------------")
  printf("Generating plot with priors of range [%0.3f, %0.3f]", from, to)
  # Prepare subset of features.
  errors_df <- errors %>%
    relocate(fp, fn, error, cost, miss, p, q) %>%
    select(fp, fn, error, cost, miss, p, q) %>%
    filter(across(c(p, q), ~ between(.x, from, to))) %>%
    mutate(priors = sprintf("(p = %0.2f, q = %0.2f)", p, q)) %>%
    arrange(error, cost, miss, fp, fn, p)

  # Calculate the minimum error or minimum cost.
  if (minimize_cost) {
    min_df <- errors_df %>% .slice.min.cost()
  } else {
    min_df <- errors_df %>% .slice.min.error()
  }

  if (minimize_cost) {
    printf("# Searching for optimal values by minimizing total cost.")
    printf("# Found optimal %s from dataset: %0.2f", "cost", min_df$cost)
    printf("# Total %s at optimal cost: %0.4f", "error rate", min_df$error)
    printf("# Optimal priors for minimized cost: %s", min_df$priors)
    printf("# Cost calculated with penatly = %s", penalty)
  } else {
    printf("# Searching for optimal values by minimizing total error rate.")
    printf("# Found optimal %s from dataset: %0.4f", "error rate", min_df$error)
    printf("# Total %s at optimal error rate: %0.2f", "cost", min_df$cost)
    printf("# Optimal priors for minimized total error rate: %s", min_df$priors)
    printf("# Cost calculated with penatly = %s", penalty)
  }

  # Copy and arrange dataframe for plotting.
  by_pq <- errors_df %>%
    select(!priors) %>% # Drop priors.
    mutate(across(c(p, q), ~ round(.x, digits = 2))) %>%
    group_by(p, q)  # Group by p, q.
  by_priors <- by_pq %>%
    summarize(
      fp = mean(fp),
      fn = mean(fn),
      error = mean(error),
      cost = mean(cost),
      miss = mean(miss),
    ) # Removes level of grouping.

  # Rename the categories for plotting.
  plot_df <- by_priors %>%
    rename(
      `false.pos` = fp,
      `false.neg` = fn,
      `total.err` = error,
      `total.cost` = cost,
      `total.miss` = miss,
      `prior.p` = p,
      `prior.q` = q
    )
  # Drop the appropriate column, should minimize_cost be set.
  if (minimize_cost) {
    plot_df <- plot_df %>%
      select(-c(total.err, false.pos, false.neg))
  } else {
    plot_df <- plot_df %>%
      # Turn into rate for plotting.
      mutate(
        false.pos = false.pos / total.miss,
        false.neg = false.neg / total.miss) %>%
      select(-c(total.cost, total.miss))
  }
  # In-place reshape plotting.
  plot_df %<>%
    pivot_longer(
      !c(prior.p, prior.q),
      names_to = "category",
      values_to = "value") %>%
    mutate(
      priors = sprintf("(p = %0.2f, q = %0.2f)", prior.p, prior.q),
      is_total = (category == "total.err") | (category == "total.miss"))

  # Debug information.
  # if (exists("debug", where = list(...))) {
  #  println("---- DEBUG ----")
  #  println("Errors DF:")
  #  print(errors_df)
  #  println("Summary By Priors:")
  #  print(by_priors)
  #  println("Plot DF:")
  #  print(plot_df)
  #  println("Optimal DF:")
  #  str(min_df)
  #  println("---- DEBUG ----")
  # }

  println("---------------------------")

  # Setup plot labels.
  optimal_label <- if (minimize_cost) "cost" else "error rate"
  optimal_cost <- round(min_df$cost, 2)
  optimal_error <- round(min_df$error, digits)
  optimal_value <- if (minimize_cost) optimal_cost else optimal_error
  optimal_prior.p <- round(min_df$p, 3)
  optimal_priors <- min_df$priors

  p.labs <- list(
    title = sprintf("Optimal %s is %s with priors = %s",
                    optimal_label, optimal_value, optimal_priors),
    x = "Prior Probability of Positive Class",
    y = if (minimize_cost) "Total Cost" else "Total Error Rate"
  )

  if (minimize_cost) {
    p.category <- list(
      colors = c("#ffa600", "#003f5c"),
      breaks = c("total.cost", "total.miss"),
      labels = c("Total Cost", "Total Misclassified")
    )
  } else {
    p.category <- list(
      colors = c("#7a5195", "#ef5675", "#003f5c"),
      breaks = c("false.pos", "false.neg", "total.err"),
      labels = c("False Positive Rate", "False Negative Rate", "Total Error Rate")
    )
  }

  if (minimize_cost) {
    p.annotation <- data.frame(
      x = c(optimal_prior.p - 2 * (optimal_prior.p / 10)),
      y = c(optimal_value - 5 * (optimal_value / 10)),
      label = c(sprintf("Optimal %s = %s when %s with error rate = %s", optimal_label, optimal_value, optimal_priors, optimal_error))
    )
  } else {
    p.annotation <- data.frame(
      x = c(optimal_prior.p + (optimal_prior.p / 10)),
      y = c(optimal_value - 1.5 * (optimal_value / 10)),
      label = c(sprintf("Optimal %s = %s when %s with cost = %s", optimal_label, optimal_value, optimal_priors, optimal_cost))
    )
  }
  # opts_visible <- list(
  #  points = TRUE,
  #  lines = FALSE,
  #  references = FALSE,
  #  annotations = FALSE,
  # )
  .v <- modifyList(
    # Override the default values.
    list(
      points = TRUE,
      lines = FALSE,
      references = FALSE,
      annotations = FALSE
    ), opts_visible)

  # Create the base plot and mapping.
  .p <- ggplot(data = plot_df, mapping = aes(x = prior.p))

  # Add points.
  if (.v$points) {
    .p <- .p + geom_point(mapping = aes(y = value, color = category), size = 1, alpha = 0.25)
  }

  # Add lines.
  if (.v$lines && minimize_cost) {
    .p <- .p + geom_line(mapping = aes(y = value, color = category), size = 1, alpha = 1)
  } else {
    .p <- .p + geom_line(mapping = aes(y = value, color = category, linetype = is_total), size = 1, alpha = 1)
  }

  # Add references.
  if (.v$references) {
    .p <- .p + geom_hline(yintercept = optimal_value, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
             geom_vline(xintercept = optimal_prior.p, linetype = "dashed", color = "black", size = 1, alpha = 0.5)
  }

  # Add annotations.
  if (.v$annotations) {
    .p <- .p + geom_label(
      data = p.annotation,
      mapping = aes(x = x, y = y, label = label),
      color = "black", alpha = 0.5,
      size = 3, angle = 45, fontface = "bold")
  }

  # Label the graph.
  .p <- .p + labs(title = p.labs$title) +
    scale_x_continuous(p.labs$x, n.breaks = 10) +
    scale_y_continuous(p.labs$y, n.breaks = 10) +
    scale_color_manual("Categories",
                       values = p.category$colors,
                       breaks = p.category$breaks,
                       labels = p.category$labels)

  # Add line types, if lines are drawn.
  if (.v$lines && !minimize_cost) {
    .p <- .p + scale_linetype_manual(
                            name = "Values",
                            values = c(3, 1),
                            labels = c("Summary", "Total"))
  }

  .p <- .p + theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

  # Restore state.
  options(digits = previous_digits)

  # Return plot object and summary.
  return(list(
    p = .p,
    optimal = min_df
  ))
}

#' Calculate the CV error rates.
.calc.cv.error.rates <- function(
  .data, truth = (.data[,Y] == TRUE),
  formula = Y ~ .,
  k = 2, rounds = 2, m = 2,
  from = 0.001, to = 0.999,
  penalty = 1) {

  # Calculate the possible priors.
  priors <- tibble(p = seq(from = from, to = to, length = m))
  priors %<>% mutate(q = 1 - p)
  priors %<>% filter(if_all(everything(), ~ . < 1) & if_all(everything(), ~ . > 0))
  n_priors <- nrow(priors)

  # Get the sample count.
  n_samples <- nrow(.data)
  m <- n_priors

  println("---------------------------")
  printf("(LDA) Performing %s-fold CV (%s rounds):", k, rounds)
  printf("# Samples: %s", n_samples)
  printf("# Testing %s priors in range [%0.2f, %0.2f]", m, from, to)
  printf("# Penalty factor applied to cost function: x%s", penalty)
  println("---------------------------")

  # Prepare the error results.
  # - k: Fold ID.
  # - Round: Round ID.
  # - FN: False negative. Misclassification error for "Severe"
  # - FP: False positive. Misclassification error for "Not Severe".
  # - MISS: # misclassified.
  # - COST: Total cost of misclassification.
  # - ERROR: Total error.
  # - p: Prior for the positive class. (Reminder: q = (1 - p))
  errors_df <- tibble(
    k = integer(),
    round = integer(),
    p = double(),
    q = double(),
    fp = integer(),
    fn = integer(),
    miss = integer(),
    cost = integer(),
    error = double()
  )

  # errors_df
  # $ k :

  # Process each round of CV.
  for (i in 1:rounds) {

    # Sample sequence 1 to k to make vector
    # of numbers with fold ids.
    fold <- sample(rep(1:k, length = n_samples))

    # Fit model on selected fold.
    for (j in 1:k) {

      # Setup fold-level df.
      fold_df <- tibble(
        k = integer(),
        round = integer(),
        p = double(),
        q = double(),
        fp = integer(),
        fn = integer(),
        miss = integer(),
        cost = integer(),
        error = double()
      )

      # Select the fold rows based on fold ID.
      validation_rows <- (fold == j)

      # Get the training and validation splits.
      fold_train <- .data[!validation_rows, ]
      fold_validation <- .data[validation_rows, ]

      # Get the ground truth for the selected fold.
      fold_truth <- (.data[validation_rows, ]$severity == "Severe")

      # Loop through the set of priors.
      for (h in 1:m) {

        # The LDA model needs appropriate set of priors.
        priors_h <- priors %>%
          slice(n = h) %>%
          relocate(q, p) # Rearrange for model fitting.

        # Fit the LDA model.
        model <- .fit.model(
          fold_train, fold_truth,
          validation = fold_validation,
          formula = formula,
          prior = unlist(priors_h),
          penalty = penalty)

        # Pull results from the model.
        model_row <- tibble_row(
          k = j,
          round = i,
          p = priors_h$p,
          q = priors_h$q,
          fp = model$table[1,2],
          fn = model$table[2,1],
          miss = model$miss,
          cost = model$cost,
          error = model$error
        )

        # Add fold-level row to the fold_df.
        fold_df %<>% add_row(model_row)

      }

      # Summarize fields, grouped by fold id.

      fold_row <- fold_df %>%
        group_by(k) %>%
        summarize(across(!c(round), ~ mean(.x)))

      # Add row to the round_df.
      errors_df %<>% add_row(fold_row)

    }

  }

  # Calculate aggregate mean and se for the statistics.
  # errors_df

  str(errors_df)

  stop("DONE")

  # Fit model without set priors, for comparison.
  baseline <- .fit.model(.data, truth, formula = formula, penalty = penalty)

  # Select entry with minimum error (same as minimum cost).
  if (minimize_cost) {
    # If minimizing by cost:
    min_df <- errors_df %>%
      arrange(cost, error, miss, fp, fn, p) %>%
      slice_min(cost)
  } else {
    # If minimizing by error:
    min_df <- errors_df %>%
      arrange(error, cost, miss, fp, fn, p) %>%
      slice_min(error)
  }

  # Fit model with optimal priors, for comparison.
  n_candidates <- nrow(min_df)

  if (n_candidates > 1) {
    if (minimize_cost) {
      printf("Several 'minimum cost' candidates. Using average among %s candidates.", n_candidates)
    } else {
      printf("Several 'minimum error' candidates. Using average among %s candidates.", n_candidates)
    }
    min_df <- min_df %>%
      summarize(
        p = mean(p),
        q = mean(q),
        fp = mean(fp),
        fn = mean(fn),
        miss = mean(miss),
        cost = mean(cost),
        error = mean(error)
      )
  }

  optimal_priors <- c(min_df$q, min_df$p)
  optimal <- .fit.model(.data, truth, formula = formula, prior = optimal_priors, penalty = penalty)

  # Prepare results.
  res <- list(
    penalty = penalty,
    results = min_df,
    baseline = baseline,
    optimal = optimal,
    optimal_weight = min_df$q / min_df$p,
    optimal_error = min_df$error,
    optimal_cost = min_df$cost,
    optimal_priors = list(
      p = min_df$p,
      q = min_df$q
    ),
    # Provide all the calculated errors as well.
    errors = errors_df
  )

  if (minimize_cost) {
    printf("Summary of minimum cost search:")
  } else {
    printf("Summary of minimum error rate search:")
  }
  printf("Min. total cost: %s (Penalty = x%s)", round(min_df$cost, digits = 4), penalty)
  printf("Min. total error: %s", round(min_df$error, digits = 4))
  printf("Min. total miss: %s", min_df$cost)
  printf("Optimal q/p ratio: %s", res$optimal_weight)
  printf("Optimal priors: p = %s, q = %s",
         round(min_df$p, digits = 4),
         round(min_df$q, digits = 5)
  )
  println("Summary: ")
  str(min_df)
  println("---------------------------")

  # Return list of values.
  return(res)



}

#' Perform CV analysis.
.analysis.cv <- function(
  .data, truth = (.data[,Y] == TRUE),
  ...,
  formula = Y ~ .,
  k = 2, rounds = 2, m = 2,
  from = 0.001, to = 0.999,
  penalty = 1
) {

  # Calculate the cross validation error rates.
  baseline_cv <- .calc.cv.error.rates(
    .data, truth,
    formula = formula,
    k = k, rounds = rounds, m = m,
    from = from, to = to,
    penalty = penalty
  )

  # Plot the CV error rates.
  plot_cv <- .plot.optimal.error.rates(
    baseline_cv$errors,
    penalty = penalty
  )

  # Make the CV error table.
  # table_cv <- .make.cv.confusion.mat()

  # Summarize the confusion matrices.
  # table_cv_summary <- .summarize.cv.confusion.mat(table_cv)

  # Return elements.
  return(list(
    results = baseline_cv,
    optimal_error = baseline_cv$error,
    optimal_cost = baseline_cv$cost,
    optimal_priors = baseline_cv$priors,
    plot = plot_cv
    # table = table_cv,
    # table_summary = table_cv_summary
  ))
}

# Helpers.
LDA.ANALYSIS <<- list(
  calc.priors = .calc.priors,
  calc.total.error = .calc.total.error,
  calc.total.miss = .calc.total.miss,
  calc.total.cost = .calc.total.cost,
  fit.model = .fit.model,
  calc.optimal.error.rates = .calc.optimal.error.rates,
  calc.optimal.prior.error.rates = .calc.optimal.prior.error.rates,
  plot.optimal.error.rates = .plot.optimal.error.rates,
  analysis.cv = .analysis.cv
)

## ---- analysis::lda::exec ----

println("LDA helpers imported.", message = TRUE)
