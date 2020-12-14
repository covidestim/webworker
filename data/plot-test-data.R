library(tidyverse)
library(sf)
library(ggrepel)
library(albersusa)

test_tracts <- read_csv('test-tracts.csv') %>% pull(tract)

c(
  '100_random_counties',
  'CT_and_MA_fips',
  'all_state_names',
  'ranked_by_num_failures',
  'ranked_by_pop_size',
  'ranked_by_seroprevalence'
) -> categories

ranks <- map(
  paste0('test-data-sources/', categories),
  read_csv,
  col_names = FALSE
) %>% setNames(categories) %>%
  imap(~transmute(.x, fips = X1, rank = 1:n(), type = .y)) %>%
  bind_rows

highest_rank <- group_by(ranks, fips) %>%
  filter(rank == min(rank)) %>%
  summarize(top_reason = head(type))

map <- counties_sf('laea') %>%
  mutate(
    center = st_centroid(geometry, of_largest_polygon = TRUE),
    is_test_county = fips %in% test_tracts
  ) %>%
  left_join(highest_rank, by='fips')

ggplot(map) +
  geom_sf(
    data = ~filter(., is_test_county),
    aes(fill = top_reason), color = NA
  ) +
  geom_sf(
    data = usa_sf('laea'),
    color = 'grey',
    fill = NA
  ) +
  geom_label_repel(
    data = ~filter(., is_test_county),
    aes(x = st_coordinates(center)[,"X"],
        y = st_coordinates(center)[,"Y"],
        label = name),
    label.size = 0.11,
    box.padding = 0.1,
    label.padding = 0.1,
    min.segment.length = 0.1,
    segment.alpha = 0.5,
    size = 2
  ) +
  scale_fill_discrete(
    "Reason for inclusion",
    labels = c(
      '100_random_counties' = "Randomly sampled",
      'CT_and_MA_fips' = "CT/MA",
      'ranked_by_num_failures' = "Frequent failures",
      'ranked_by_pop_size' = "Large population",
      'ranked_by_seroprevalence' = "High seroprevalence"
    )
  ) +
  theme_void() +
  labs(title = "Counties included in Dec 10-21 IFR test",
       subtitle = "Git tag: `ifr-frozen`")
