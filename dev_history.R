library(devtools)
library(usethis)

# use_build_ignore("dev_history.R")
# use_gpl3_license("Antoine Bichat")
# use_git()

# use_r("encode_mean")


document()
load_all()
attachment::att_to_description(extra.suggests = "modeldata")
use_tidy_description()

spell_check()

devtools::check()
