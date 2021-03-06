% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_mean.R
\name{step_encode_mean}
\alias{step_encode_mean}
\alias{tidy.step_encode_mean}
\title{Mean target encoding}
\usage{
step_encode_mean(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  options = list(na.rm = TRUE),
  means = NULL,
  skip = FALSE,
  id = rand_id("encode_count")
)

\method{tidy}{step_encode_mean}(x, ...)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are affected by the step. See \code{\link[recipes:selections]{selections()}}
for more details. For the \code{tidy} method, these are not
currently used.}

\item{role}{Not used by this step since no new variables are
created.}

\item{trained}{A logical to indicate if the quantities for
preprocessing have been estimated.}

\item{outcome}{A single outcome variable.}

\item{options}{A list of options to \code{mean}.}

\item{means}{A named numeric vector of means. This is
\code{NULL} until computed by \code{\link[=prep.recipe]{prep.recipe()}}.}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \code{\link[recipes:bake.recipe]{bake.recipe()}}? While all operations are baked
when \code{\link[recipes:prep.recipe]{prep.recipe()}} is run, some operations may not be able to be
conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = TRUE} as it may affect
the computations for subsequent operations}

\item{id}{A character string that is unique to this step to identify it.}

\item{x}{A \code{step_encode_mean} object.}
}
\value{
An updated version of \code{recipe} with the new step
added to the sequence of existing steps (if any).
}
\description{
\code{step_encode_mean} creates a \emph{specification} of a recipe step that will
encode qualitative data into numeric based on the outcome value.
}
\examples{
library(recipes)
data("penguins", package = "modeldata")
rec <- recipe(body_mass_g ~ ., data = penguins) \%>\%
  step_encode_mean(all_nominal(), outcome = "body_mass_g") \%>\%
  prep()
juice(rec)
tidy(rec, 1)
}
