accessibility <- demand. <- demand_vol. <- dist. <- supply. <- supply_vol. <- ratio <- NULL


#' Calculate Two-Step Floating Catchment Areas (2SFCA)
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
    dplyr::select(demand., demand_vol.) %>%
    dplyr::distinct()

  # here we calculate the two-step floating catchment area.
  # first: discard all distance rows that are greater than dist_threshold..
  # then: for each supplier, find its individual catchment area supply/demand ratio
  # then: for each demander, find the total value of all the catchment areas it's in
  # then: the full_join brings back any empty demand areas, and we assign them 0 with mutate
  # then: we rename things back to the way they were, and put them in increasing order of demand
  dist_table. %>%
    dplyr::filter (dist. < dist_threshold) %>%
    #filter(demand_vol. != 0) %>% ####################### TESTING
    dplyr::group_by(supply.) %>%
    dplyr::mutate(ratio = supply_vol./sum(demand_vol.)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(demand.) %>%
    dplyr::summarise(demand., accessibility = sum(ratio), .groups = "drop") %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::full_join(demand_tb, by = "demand.") %>%
    dplyr::distinct() %>%
    dplyr::mutate(accessibility = dplyr::if_else(is.na(accessibility), 0, accessibility)) %>%
    dplyr::select({{demand}} := demand., {{demand_vol}} := demand_vol., accessibility) %>%
    dplyr::arrange({{demand}})

}
