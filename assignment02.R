library(tidyverse)
library(Stat2Data)
data("Hawks")

hSF = Hawks %>%
  filter(Species=="RT" & Weight>=1000) %>%
  select(Wing, Weight, Tail)
head(hSF, 5)
hSF%>%nrow()
head(arrange(hSF, Wing), 5)

species_code = c("CH", "RT", "SS")
species_name_full = c("Cooper's", "Red-tailed", "Sharp-shinned")
hawkSpeciesNameCodes = data.frame(species_code, species_name_full)
hawkSpeciesNameCodes

hawksFullName = Hawks %>%
  left_join(hawkSpeciesNameCodes %>% rename(Species = species_code)) %>%
  select(-Species) %>%
  rename(Species = species_name_full)

hawksFullName %>%
  select(Species,Wing,Weight) %>%
  head(7)

hawkswithBMI = Hawks %>%
  mutate(bird_BMI=1000*Weight/Wing^2) %>%
  select(Species, bird_BMI) %>%
  arrange(desc(bird_BMI))
hawkswithBMI %>% head(8)

hawksFullName %>%
  group_by(Species) %>%
  summarize(num_rows=n(),
            mn_wing=mean(Wing, na.rm = TRUE),
            md_wing=median(Wing, na.rm = TRUE),
            t_mn_wing=mean(Wing, na.rm = TRUE, trim = 0.1),
            b_wt_ratio=max(Wing/Tail, na.rm = TRUE))

hawksFullName %>%
  select(Species, Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus, Crop) %>%
  group_by(Species) %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  head()


