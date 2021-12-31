#accessibility <-  demand_vol. <- dist. <- supply. <- supply_vol. <- ratio <- NULL
# demand. <-

#' Calculate Two-Step Floating Catchment Areas (2SFCA)
#'
#' Calculate the accessibility index using the two-step floating catchment area (2SFCA) method.
#'
#' Luo & Wang 2003, DOI:10.1068/b29120
#'
#' @param dist_table A data frame containing an origin-destination (OD) table, with additional columns for supply and demand volumes.
#' @param supply The name of the column in `dist_table` that uniquely identifies supply points (hospitals, physicians, etc.).
#' @param demand The name of the column in `dist_table` that uniquely identifies demand points (census tracts, residences, etc.).
#' @param dist The name of the column in `dist_table` that contains the distance between the `supply` and the `demand`.
#' @param dist_threshold The distance threshold defining catchment areas.
#' @param supply_vol The name of the column in `dist_table` that contains supply volumes (# of hospital beds, # of physicians, etc.).
#' @param demand_vol The name of the column in `dist_table` that contains demand volumes (population counts, etc.).
#'
#' @return A data frame.
#' @export
tsfca <- function(dist_table, supply, demand, dist, dist_threshold, supply_vol, demand_vol){

  # if supply_vol or demand_vol aren't specified, assume all supply/demand is 1
  if (missing(supply_vol)){
    dist_table$supply_vol <- 1
    supply_vol <- "supply_vol"
  }

  if (missing(demand_vol)){
    dist_table$demand_vol <- 1
    demand_vol <- "demand_vol"
  }

  # create a temp standardized and renamed tibble to work with
  # we'll rename things back to their original names at the end
  dist_table. <- dplyr::rename(dist_table,
                        supply. = {{supply}},
                        demand. = {{demand}},
                        supply_vol. = {{supply_vol}},
                        demand_vol. = {{demand_vol}},
                        dist. = {{dist}})

  # create temp tibble with just the demand info
  # we'll use it to add info about any population areas with zero total access

  demand_tb <- dist_table. %>%
    dplyr::select(.data$demand., .data$demand_vol.) %>%
    dplyr::distinct()

  # here we calculate the two-step floating catchment area.
  # first: discard all distance rows that are greater than dist_threshold..
  # then: for each supplier, find its individual catchment area supply/demand ratio
  # then: for each demander, find the total value of all the catchment areas it's in
  # then: the full_join brings back any empty demand areas, and we assign them 0 with mutate
  # then: we rename things back to the way they were, and put them in increasing order of demand
  dist_table. %>%
    dplyr::filter (.data$dist. <= dist_threshold) %>%
    #filter(demand_vol. != 0) %>% ####################### TESTING
    dplyr::group_by(.data$supply.) %>%
    dplyr::mutate(ratio = .data$supply_vol./sum(.data$demand_vol.)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$demand.) %>%
    dplyr::summarise(.data$demand., accessibility = sum(.data$ratio), .groups = "drop") %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::full_join(demand_tb, by = "demand.") %>%
    dplyr::distinct() %>%
    dplyr::mutate(accessibility = dplyr::if_else(is.na(.data$accessibility), 0, .data$accessibility)) %>%
    dplyr::select({{demand}} := .data$demand.,  .data$accessibility) %>% # {{demand_vol}} := demand_vol.,
    dplyr::arrange({{demand}})

}

#' Spatial Accessibility via Gravity Method
#'
#' Luo & Wang 2003, DOI:10.1068/b29120
#'
#' @param dist_table A data frame containing an origin-destination (OD) table, with additional columns for supply and demand volumes.
#' @param supply The name of the column in `dist_table` that uniquely identifies supply points (hospitals, physicians, etc.).
#' @param demand The name of the column in `dist_table` that uniquely identifies demand points (census tracts, residences, etc.).
#' @param dist The name of the column in `dist_table` that contains the distance between the `supply` and the `demand`.
#' @param gravity_exp The exponent to use for the inverse distance function.
#' @param supply_vol The name of the column in `dist_table` that contains supply volumes (# of hospital beds, # of physicians, etc.).
#' @param demand_vol The name of the column in `dist_table` that contains demand volumes (population counts, etc.).
#'
#' @return A data frame.
#' @export
gravity <- function(dist_table, supply, demand, dist, gravity_exp, supply_vol, demand_vol){
  if (missing (supply) | missing(demand) | missing (dist)){
    stop("Supply, demand, and dist are all mandatory parameters.")
  }

  if (missing(gravity_exp)){
    warning ("SETTING GRAV EXPONENT TO 1 FOR TESTING")
    gravity_exp <- 1
  }

  # if supply_vol or demand_vol aren't specified, assume all supply/demand is 1
  if (missing(supply_vol)){
    dist_table$supply_vol <- 1
    supply_vol <- "supply_vol"
  }

  if (missing(demand_vol)){
    dist_table$demand_vol <- 1
    demand_vol <- "demand_vol"
  }

  # create a temp standardized and renamed tibble to work with
  # we'll rename things back to their original names at the end
  dist_table. <- dplyr::rename(dist_table,
                               supply. = {{supply}},
                               demand. = {{demand}},
                               supply_vol. = {{supply_vol}},
                               demand_vol. = {{demand_vol}},
                               dist. = {{dist}})

# Pk is population of location k
# Sj is number of physicians at location j
# dkj is travel time between k and j

  # Vj (service competition at each location) from equation 6 p 874 in Luo & Wang
  Vj <- dist_table. %>%
    dplyr::mutate(service_competition_each = .data$demand_vol./(.data$dist.^gravity_exp)) %>%
    dplyr::group_by(.data$supply.) %>%
    dplyr::summarise(service_competition = sum(.data$service_competition_each))


  # Top of eqn 6 from p874 in Luo & Wang
  dist_table. %>%
    dplyr::mutate(weighted_supply = .data$supply_vol./ (.data$dist.^gravity_exp)) %>%
    dplyr::left_join(Vj, by = "supply.") %>%
    dplyr::mutate(term = .data$weighted_supply / .data$service_competition) %>%
    dplyr::group_by(.data$demand.) %>%
    dplyr::summarise(accessibility_index = sum(.data$term)) %>%
    dplyr::select({{demand}} := .data$demand.,  .data$accessibility_index)


}
#
# twostepr::gravity(dist_table = test1_in, supply = d, demand = o, dist = distance)
#
#
# twostepr::tsfca(dist_table = test1_in, supply = d, demand = o, dist = distance, dist_threshold = 30)
#
# gravity(dist_table = test1_in, supply = d, demand = o, dist = distance, gravity_coeff = 2)
# gravity(dist_table = test1_in)
