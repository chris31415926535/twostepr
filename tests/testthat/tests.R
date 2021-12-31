
# TEST DATA for tsfca() two-step floating catchment area, from Fig 3 of:
#DOI:10.1068/b29120
# Measures of spatial accessibility to health care in a GIS
# environment: synthesis and a case study in the Chicago region
# Wei Luo, Fahui Wang
# Department of Geography, Northern Illinois University, DeKalb, IL 60115-2854, USA;
# e-mail: luo@geog.niu.edu, wang@geog.niu.edu
# Received 12 December 2002; in revised form 14 March 2003


### TWO-STEP FLOATING CATCHMENT AREA TEST DATA

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

# expected output data in long format (this is dput output)
test1_out <- structure(list(o = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                            #demand_vol = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                            accessibility = c(0.125, 0.125, 0.125, 0.375, 0.25, 0.125, 0.125, 0.25, 0.125, 0.125, 0.25)),
                       row.names = c(NA, -11L), class = c("tbl_df", "tbl", "data.frame"))


## GRAVITY MODEL TEST DATA

# 3 populations, 2 docs: populations arranged in a triangle
# A is in between 1,2,3, and B is off to the side farther from 3
test2_in <- dplyr::tribble(
  ~pop_id, ~doc_id, ~distance, ~supply_vol, ~demand_vol,
  1, "A", 1, 1, 1,
  2, "A", 1, 1, 1,
  3, "A", 1, 1, 1,
  1, "B", 1, 1, 1,
  2, "B", 1, 1, 1,
  3, "B", 2,  1, 1
)

# expected output when gravitational value is 1
test2_out_g1 <- structure(list(pop_id = c(1, 2, 3),
               accessibility_index = c(0.733333333333333, 0.733333333333333, 0.533333333333333)),
          row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

# expected output when gravitational value is 2
test2_out_g2 <- structure(list(pop_id = c(1, 2, 3),
                               accessibility_index = c(0.777777777777778,0.777777777777778, 0.444444444444444)),
                          row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))


############################
##### TESTS


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
})



testthat::test_that("gravity() works as expected",{
  # test supplying all column names as strings
  testthat::expect_equal(twostepr::gravity(test2_in, "doc_id", "pop_id",
                                         dist = "distance",
                                         gravity_coeff = 1,
                                         supply_vol = "supply_vol",
                                         demand_vol = "demand_vol"),
                         test2_out_g1)

  # test supplying all column names without quotation marks
  testthat::expect_equal(twostepr::gravity(test2_in, doc_id, pop_id,
                                           dist = distance,
                                           gravity_coeff = 1,
                                           supply_vol = supply_vol,
                                           demand_vol = demand_vol),
                         test2_out_g1)

  # test not supplying supply_vol or demand_vol (supply and demand are 1)
  testthat::expect_equal(twostepr::gravity(test2_in, doc_id, pop_id,
                                           dist = distance,
                                           gravity_coeff = 1),
                         test2_out_g1)

  # test with grav coeff = 2
  testthat::expect_equal(twostepr::gravity(test2_in, doc_id, pop_id,
                                           dist = distance,
                                           gravity_coeff = 2),
                         test2_out_g2)

})
