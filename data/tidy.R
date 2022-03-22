library(tidyverse)
library(here)

# Patient data ------------------------------------------------------------

metaRaw = readr::read_csv(here("data", "aim_meta.csv")) %>% print()

meta = metaRaw %>%
  rename(ID = patient, treatment = treatment_group, sex = gender) %>%
  mutate(ID = as.character(ID)) %>%
  rename_all(function(s) {substr(s, 1, 1) <- toupper(substr(s, 1, 1)); s}) %>%
  print


# RNA counts data ---------------------------------------------------------

xraw = readr::read_csv(here("data", "aim_counts.csv"))

x = xraw %>%
  pivot_longer(-ensemblID, names_to = c("ID", "Timepoint"), names_sep = "-", values_to = "Count") %>%
  rename(Gene = ensemblID) %>%
  print


# Merge and save complete RNA dataset -------------------------------------

rnadata = dplyr::left_join(x, meta) %>%
  mutate(Timepoint = factor(Timepoint, levels = c("screen", "100d", "1y"))) %>%
  mutate_at(c("Modic", "Treatment", "Sex"), as.factor) %>%
  print

saveRDS(rnadata, here("data/rnaData.rds"))

