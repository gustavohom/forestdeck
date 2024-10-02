library(devtools)

# usethis::use_git()

# usethis::use_r('strsplit1')

#devtools::load_all()

# exists("strsplit1", where = globalenv(), inherits = FALSE)

#usethis::create_github_token()

#gitcreds::gitcreds_set()

# check()

# use_mit_license()

# document()

install()

library(strsplit1)

# ?strsplit1

check()
use_testthat()
use_test("strsplit_one")
test()

use_package("stringr")
rename_files("strsplit1", "str_split_one")

document()

load_all()

str_split_one("a, b, c", pattern = ", ")
str_split_one("a, b, c", ", ")

use_readme_rmd()
build_readme()
