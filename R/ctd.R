library(here)
library(tidyverse)
library(oce)

ctd_282 <- read.oce(here("data/CTD_DATA/sta0282.cnv"))
ctd_282 <- as.data.frame(ctd_282@data) |>
  mutate(station = 282)

ctd_283 <- read.oce(here("data/CTD_DATA/sta0283.cnv"))
ctd_283 <- as.data.frame(ctd_283@data) |>
  mutate(station = 283)

ctd_284 <- read.oce(here("data/CTD_DATA/sta0284.cnv"))
ctd_284 <- as.data.frame(ctd_284@data) |>
  mutate(station = 284)

ctd_data <- bind_rows(list(ctd_282, ctd_283, ctd_284))

ggplot(data = ctd_data, aes(x = salinity, y = pressure, fill = temperature)) +
  geom_point(shape = 21,
             size = 4,
             alpha = 0.3) +
  scale_y_reverse(limits = c(0,80), breaks = seq(0, 80, by = 10)) +
  facet_wrap(~ station,
             labeller = as_labeller(c(
               '282' = "Darkness (st.282)",
               '283' = "Light pollution (st.283)",
               '284' = "Daylight (st.284)"
             ))
  ) +
  scale_fill_viridis_c(
    name = "Temperature (C)",
    option = "mako",
    direction = -1) +
  labs(x = "Par (umol photons/m^-2/sec)",
       y = "Depth (m)") +
  theme_bw()


