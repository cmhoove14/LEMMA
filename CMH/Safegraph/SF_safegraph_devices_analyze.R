
require(geojsonsf)
require(sf)
require(tidyverse)

sf_sfgrph <- readRDS("CMH/Safegraph/SF_devices_sum2020-01-01to2020-08-22.rds") %>% 
  mutate(pct_home = completely_home_device_count/device_count,
         pct_full_work = full_time_work_behavior_devices/device_count,
         pct_part_work = part_time_work_behavior_devices/device_count)

sf_sfgrph %>% 
  group_by(Date) %>% 
  summarise(device_tot = sum(device_count),
            home_tot = sum(completely_home_device_count),
            full_work_tot = sum(full_time_work_behavior_devices),
            part_work_tot = sum(part_time_work_behavior_devices),
            home_pct = home_tot/device_tot,
            full_work_pct = full_work_tot/device_tot,
            part_work_pct = part_work_tot/device_tot) %>% 
  pivot_longer(home_pct:part_work_pct) %>%
  ggplot() +
    geom_line(aes(x = Date, y = value, col = name)) +
    theme_bw() +
    scale_color_manual(labels = c("Full-time work", "Home", "Part-time work"),
                       values = c("Red", "darkgreen", "Orange")) +
    labs(x = "Date",
         y = "Proportion",
         col = "Activity")

sf_sfgrph_pct_home <- sf_sfgrph %>% 
  dplyr::select(Date, origin_census_block_group, pct_home)


saveRDS(sf_sfgrph_pct_home, "CMH/ABM/data/sfgrph_devices_pct_home_cbgs_2020-01-01_2020-08-22.rds")
