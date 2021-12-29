accessibility <- demand_id. <- demand_vol. <- dist. <- supply_id. <- supply_vol. <- ratio <- NULL


#' Calculate Two-Step Floating Catchment Areas (2SFCA)
#'
#' @param dist_table A data frame containing an origin-destination (OD) table, with additional columns for supply and demand volumes.
#' @param supply_id The name of the column in `dist_table` that uniquely identifies supply points (hospitals, physicians, etc.).
#' @param demand_id The name of the column in `dist_table` that uniquely identifies demand points (census tracts, residences, etc.).
#' @param supply_vol The name of the column in `dist_table` that contains supply volumes (# of hospital beds, # of physicians, etc.).
#' @param demand_vol The name of the column in `dist_table` that contains demand volumes (population counts, etc.).
#' @param dist The name of the column in `dist_table` that contains the distance between the `supply_id` and the `demand_id`.
#' @param dist_threshold The distance threshold defining catchment areas.
#'
#' @return A data frame.
#' @export
tsfca <- function(dist_table, supply_id, demand_id, supply_vol, demand_vol, dist, dist_threshold){

  # create a temp standardized and renamed tibble to work with
  # we'll rename things back to their original names at the end
  dist_table. <- dplyr::rename(dist_table,
                        supply_id. = {{supply_id}},
                        demand_id. = {{demand_id}},
                        supply_vol. = {{supply_vol}},
                        demand_vol. = {{demand_vol}},
                        dist. = {{dist}})

  # create two temp tibbles with just the supply info and the demand info
  # we'll use these to add back info later about population areas with zero total access
  # FIXME: do we even need  the supply one? or is it just the demand one?
  # supply_tb <- dist_table. %>%
  #   select(supply_id., supply_vol.) %>%
  #   distinct()

  demand_tb <- dist_table. %>%
    dplyr::select(demand_id., demand_vol.) %>%
    dplyr::distinct()

  # here we calculate the two-step floating catchment area.
  # first: discard all distance rows that are greater than dist_threshold..
  # then: for each supplier, find its individual catchment area supply/demand ratio
  # then: for each demander, find the total value of all the catchment areas it's in
  # then: the full_join brings back any empty demand areas, and we assign them 0 with mutate
  # then: we rename things back to the way they were, and put them in increasing order of demand_id
  dist_table. %>%
    dplyr::filter (dist. < dist_threshold) %>%
    #filter(demand_vol. != 0) %>% ####################### TESTING
    dplyr::group_by(supply_id.) %>%
    dplyr::mutate(ratio = supply_vol./sum(demand_vol.)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(demand_id.) %>%
    dplyr::summarise(demand_id., accessibility = sum(ratio), .groups = "drop") %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::full_join(demand_tb, by = "demand_id.") %>%
    dplyr::distinct() %>%
    dplyr::mutate(accessibility = dplyr::if_else(is.na(accessibility), 0, accessibility)) %>%
    dplyr::select({{demand_id}} := demand_id., {{demand_vol}} := demand_vol., accessibility) %>%
    dplyr::arrange({{demand_id}})

}
