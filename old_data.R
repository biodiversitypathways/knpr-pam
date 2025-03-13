old_data <- read_csv("./assets/KNPR_Database_Export.csv")

old_standardized <- old_data |>
  select(Observer,`Point name`,Date,Species,Detection_hist,`Sex/Behaviour`,`Record #`,
         `Time interval`,Distance,Number,`Time start`,Noise, Mammal,`Start time`,`End time`) |>
  distinct() |>
  rename(location = `Point name`) |>
  mutate(location = str_replace(location, "^Q0", "Q")) |>
  mutate(location = str_replace(location, "^A0", "A")) |>
  rename(species_common_name = Species) |>
  rename(observer = Observer) |>
  left_join(knpr_main |> select(location, latitude, longitude) |> distinct(), by = "location") |>
  drop_na(latitude) |>
  mutate(recording_date_time = parse_date_time(Date, orders = "a, b d, Y") + seconds(as.numeric(hms(`Time start`)))) |>
  relocate(c(location,latitude, longitude, recording_date_time, observer, species_common_name)) |>
  group_by(location, latitude, longitude, recording_date_time, observer, species_common_name) |>
  mutate(individual_order = row_number(), .after = species_common_name) |>
  ungroup() |>
  select(location, latitude, longitude, recording_date_time, observer, species_common_name, individual_order) |>
  distinct() |>
  mutate(species_common_name = case_when(species_common_name == "Gray Jay" ~ "Canada Jay", 
                                         species_common_name == "Grouse" ~ "Unknown grouse",
                                         species_common_name == "Unknown" ~ "Unknown bird",
                                         TRUE ~ species_common_name)) |>
  inner_join(wt_get_species() |> select(species_common_name, species_code), by = "species_common_name") |>
  mutate(Site = case_when(grepl('^Q',location) ~ "Quill Creek", TRUE ~ "Auriol Trail")) |>
  mutate(individual_count = "1")

  
