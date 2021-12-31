library(tidyverse)
library(SpatialAcc)

orig <- PWC.Municipalities %>%
  as_tibble()

hosps <- GR.Hospitals %>%
 # sf::st_as_sf() %>%
  as_tibble()
#%>%  sf::st_as_sf()


p.coords <- orig[,2:3] %>% as.matrix()
h.coords <- hosps[,4:5] %>% as.matrix()

a <- p.coords[1,]
D <- distance(p.coords, h.coords)

# need better way of getting OD table from just euclidean distance
od_table <- D %>%
  as_tibble() %>%
  rename_with(.fn = function(x) paste0("hosp_",as.character(hosps$ID))) %>%
  bind_cols(select(orig, orig_id = KallCode)) %>%
  select(orig_id, everything()) %>%
  pivot_longer(cols = -orig_id,
               names_to = "dest_id",
               values_to = "dist")



######### creating test data
library(tidyverse)
library(SpatialAcc)

num_pop_centres <- 50
pop_min <- 100
pop_max <- 1000

num_service_centres <- 15
service_min <- 100
service_max <- 200

tsfca_dist_threshold <- .2

pop_centres <- dplyr::tibble(
  pop_id = 1:num_pop_centres,
  x_pop = runif(n = num_pop_centres, min = 0, max = 1),
  y_pop = runif(n = num_pop_centres, min = 0, max = 1),
demand_vol = round(runif(n = num_pop_centres, min = pop_min, max = pop_max))
)

service_centres <- dplyr::tibble(
  service_id = LETTERS[1:num_service_centres],
  x_service = runif(n = num_service_centres, min = 0, max = 1),
  y_service = runif(n = num_service_centres, min = 0, max = 1),
  supply_vol = round(runif(n = num_service_centres, min = service_min, max = service_max))
)

# get all euclidean distances
od_long <- tidyr::crossing(pop_centres, service_centres) %>%
  mutate(dist = sqrt((x_pop - x_service)^2 + (y_pop - y_service)^2))

od_matrix <- od_long %>%
  select(pop_id, service_id, dist) %>%
  pivot_wider(names_from = service_id, values_from = dist) %>%
  select(-pop_id) %>%
  as.matrix()

demand_vec <- pop_centres$demand_vol
supply_vec <- service_centres$supply_vol


### COMPARE SpatialAcc 2SFCA function to twostepr::tsfca()
test_spatialacc <- SpatialAcc::ac(p = demand_vec,
               n = supply_vec,
               D = od_matrix,
               d0 = tsfca_dist_threshold,
               family = "2SFCA")

test_twostepr <- twostepr::tsfca(dist_table = od_long,
                supply = service_id,
                demand = pop_id,
                supply_vol = supply_vol,
                demand_vol = demand_vol,
                dist = "dist",
                dist_threshold = tsfca_dist_threshold
                )

# check for equality
testthat::expect_equal(test_spatialacc, test_twostepr$accessibility)




# plot them

od_for_plot <- pop_centres %>%
  rename(id = pop_id, x = x_pop, y = y_pop, vol = demand_vol) %>%
  mutate(type = "pop", id = as.character(id)) %>%
  bind_rows(service_centres %>%
              rename(id = service_id, x = x_service, y = y_service, vol = supply_vol)  %>%
              mutate(type = "service"))

ggplot() +
  ggforce::geom_circle(data = filter(od_for_plot, type == "service"),
                       mapping = aes(x0=x, y0=y, r = tsfca_dist_threshold ),
                       fill = "blue", alpha = 0.1) +
  geom_label(data = od_for_plot,
             mapping = aes(x=x,y=y, label = id, colour = type)) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))

