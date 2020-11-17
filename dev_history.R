library(devtools)
library(usethis)

# use_build_ignore("dev_history.R")
# use_gpl3_license("Antoine Bichat")
# use_git()

# use_r("encode_mean")

# use_test("encode_mean")

# use_github()
# use_readme_rmd()

# use_lifecycle_badge("experimental")
# badgecreatr::badge_packageversion()
# badgecreatr::badge_license()


document()
load_all()
attachment::att_to_description(extra.suggests = "modeldata")
use_tidy_description()

spell_check()

devtools::check()
goodpractice::goodpractice()


install(upgrade = "never")
rmarkdown::render("README.Rmd")
unlink("README.html")
