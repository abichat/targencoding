#' Mean target encoding
#'
#' `step_encode_mean` creates a *specification* of a recipe step that will
#'  encode qualitative data into numeric based on the outcome value.
#'
#' @inheritParams recipes::step_center
#' @param outcome A single outcome variable.
#' @param options A list of options to `mean`.
#' @param means A named numeric vector of means. This is
#'  `NULL` until computed by [prep.recipe()].
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
#' library(recipes)
#' library(modeldata)
#' data("penguins")
#' rec <-
#'   recipe(body_mass_g ~ ., data = penguins) %>%
#'   step_encode_mean(all_nominal(), outcome = "body_mass_g") %>%
#'   prep()
#' juice(rec)
#' tidy(rec, 1)
step_encode_mean <- function(recipe, ..., role = NA, trained = FALSE,
                             outcome = NULL, options = list(na.rm = FALSE),
                             means = NULL, skip = FALSE,
                             id = rand_id("encode_count")) {

  if (length(outcome) != 1 ) {
    abort("`outcome` should select one column.")
  }

  add_step(recipe,
           step_encode_mean_new(terms = ellipse_check(...),
                                role = role,
                                trained = trained,
                                outcome = outcome,
                                options = options,
                                means = means,
                                skip = skip,
                                id = id))
}

step_encode_mean_new <- function(terms, role, trained, outcome,
                                 options, means, skip, id) {
  step(subclass = "encode_mean",
       terms = terms,
       role = role,
       trained = trained,
       outcome = outcome,
       options = options,
       means = means,
       skip = skip,
       id = id)
}

#' @export
prep.step_encode_mean <- function(x, training, info = NULL, ...) {

  col_names <- terms_select(terms = x$terms, info = info)

  means <- map(training[, col_names],
               ~ tapply(training[x$outcome][[1]], ., mean, na.rm = TRUE))

  step_encode_mean_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    options = x$options,
    means = means,
    skip = x$skip,
    id = x$id
  )
}

#' @export
#' @importFrom purrr map2_dfc
bake.step_encode_mean <- function(object, new_data, ...) {
  vars <- names(object$means)
  new_data[, vars] <- map2_dfc(new_data[, vars], object$means, ~ unname(.y[.x]))

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}

#' @export
print.step_encode_mean <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Mean target encoding for ", sep = "")
  printer(names(x$means), x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_encode_mean
#' @param x A `step_encode_mean` object.
#' @export
tidy.step_encode_mean <- function(x, ...) {
  term_names <- sel2char(x$terms)
  if (is_trained(x)) {
    res <- tibble(terms = term_names,
                  value = x$means)
    res$value <- map(res$value, enframe, name = "group", value = "mean")
    res <- unnest(res, cols = .data$value)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = NA)
  }
  res$id <- x$id
  res
}
