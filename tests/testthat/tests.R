
# TEST DATA for tsfca() two-step floating catchment area, from Fig 3 of:
#DOI:10.1068/b29120
# Measures of spatial accessibility to health care in a GIS
# environment: synthesis and a case study in the Chicago region
# Wei Luo, Fahui Wang
# Department of Geography, Northern Illinois University, DeKalb, IL 60115-2854, USA;
# e-mail: luo@geog.niu.edu, wang@geog.niu.edu
# Received 12 December 2002; in revised form 14 March 2003


### DATA FOR TESTS
# input data in long format
test1_in <- dplyr::tribble(
  ~o, ~d, ~distance, ~supply_vol, ~demand_vol,
  1,  "a", 20, 1,1,
  2,  "a", 20, 1,1,
  3,  "a",20, 1,1,
  4,  "a",20, 1,1,
  5,  "a",40, 1,1,
  6,  "a",20, 1,1,
  7,  "a",20, 1,1,
  8,  "a",40, 1,1,
  9,  "a",20, 1,1,
  10, "a",20, 1,1,
  11, "a",40, 1,1,
  1,  "b",40, 1,1,
  2,  "b",40, 1,1,
  3,  "b",40, 1,1,
  4,  "b",20, 1,1,
  5,  "b",20, 1,1,
  6,  "b",40, 1,1,
  7,  "b",40, 1,1,
  8,  "b",20, 1,1,
  9,  "b",40, 1,1,
  10, "b",40, 1,1,
  11, "b",20, 1,1
)

##### TESTS

# expected output data in long format (this is dput output)
test1_out <- structure(list(o = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                            demand_vol = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                            accessibility = c(0.125, 0.125, 0.125, 0.375, 0.25, 0.125, 0.125, 0.25, 0.125, 0.125, 0.25)),
                       row.names = c(NA, -11L), class = c("tbl_df", "tbl", "data.frame"))

testthat::test_that("tsfca() works as expected",{
  # test supplying all column names as strings
  testthat::expect_equal(twostepr::tsfca(test1_in, "d", "o",
                                         dist = "distance",
                                         dist_threshold = 30,
                                         supply_vol = "supply_vol",
                                         demand_vol = "demand_vol"),
                         test1_out)

  # test supplying all column names without quotation marks
  testthat::expect_equal(twostepr::tsfca(test1_in, d, o,
                                         dist = distance,
                                         dist_threshold = 30,
                                         supply_vol = supply_vol,
                                         demand_vol = demand_vol),
                         test1_out)

  # test without supply_vol and demand_vol columns (i.e. assume all vols = 1)
  testthat::expect_equal(twostepr::tsfca(test1_in, "d", "o",
                                         dist = "distance",
                                         dist_threshold = 30),
                         test1_out)


}


)
