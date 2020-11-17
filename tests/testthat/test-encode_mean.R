library(recipes)
data("penguins", package = "modeldata")

rec <- recipe(body_mass_g ~ ., data = penguins)

means <- rbind(
  enframe(with(penguins, by(body_mass_g, species, mean, na.rm = TRUE))),
  enframe(with(penguins, by(body_mass_g, island, mean, na.rm = TRUE))),
  enframe(with(penguins, by(body_mass_g, sex, mean, na.rm = TRUE)))
  )
means$value <- as.numeric(means$value)
means_names <- dplyr::pull(means, value, name)

test_that("means and replacements are correct", {
  rec_num <- step_encode_mean(rec, all_nominal(), outcome = "body_mass_g")
  expect_true(!tidy(rec_num)$trained)
  expect_true(is.na(tidy(rec_num, 1)$value))

  prepped <- prep(rec_num)
  expect_true(tidy(prepped)$trained)

  tidied <- tidy(prepped, 1)
  expect_equal(tidied$group, means$name)
  expect_equal(tidied$mean, means$value)

  juiced <- juice(prepped)
  expect_equal(juiced$species,
               unname(means_names[as.character(penguins$species)]))
  expect_equal(juiced$island,
               unname(means_names[as.character(penguins$island)]))
  expect_equal(juiced$sex,
               unname(means_names[as.character(penguins$sex)]))
})


means_opt <- rbind(
  enframe(with(penguins, by(body_mass_g, species, mean, trim = 0.1))),
  enframe(with(penguins, by(body_mass_g, island, mean, trim = 0.1))),
  enframe(with(penguins, by(body_mass_g, sex, mean, trim = 0.1)))
)
means_opt$value <- as.numeric(means_opt$value)

test_that("options are passed correctly", {
  rec_num <- step_encode_mean(rec, all_nominal(), outcome = "body_mass_g",
                              options = list(na.rm = FALSE, trim = 0.1))

  prepped <- prep(rec_num)
  tidied <- tidy(prepped, 1)

  expect_equal(tidied$mean, means_opt$value)
})


test_that("errors are thrown", {
 expect_error(step_encode_mean(rec, all_nominal()),
              "`outcome` should select one column.")
 expect_error(step_encode_mean(rec, all_nominal(), outcome = c("body_mass_g", "bill_length_mm")),
              "`outcome` should select one column.")
})
