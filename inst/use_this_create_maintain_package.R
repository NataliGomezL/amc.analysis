library(usethis)
# create_package("/home/matifou/Dropbox/Documents/Nati/Mat Nat SAM/stanford/AMC/amc.analysis")
# use_mit_license("Natali Stigler")

use_package("igraph")
use_package("dplyr")
use_package("tidyr")
use_package("purrr")
use_package("sf")
use_package("units")
use_package("methods")
use_package("rlang")
use_package("scales")
use_package("stringr")



use_import_from("sf", "st_make_valid")
use_import_from("sf", "st_area")
use_import_from("sf", "st_intersects")

use_import_from("tibble", "tibble")

use_import_from("ggplot2", "ggtitle")
use_import_from("ggplot2", "scale_x_continuous")
use_import_from("ggplot2", "scale_y_continuous")
use_import_from("ggplot2", "facet_wrap")
use_import_from("ggplot2", "geom_line")
use_import_from("ggplot2", "aes")
use_import_from("ggplot2", "xlab")
use_import_from("ggplot2", "ggplot")
use_import_from("ggplot2", "geom_histogram")
use_import_from("ggplot2", "labs")
use_import_from("ggplot2", "geom_sf")
use_import_from("ggplot2", "theme")
use_import_from("ggplot2", "geom_sf_text")

### git
# use_git(message = "Initial commit")


## tests
use_testthat()
use_test("ovr_get_overlap_pairs")
