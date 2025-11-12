kit_count <- 10

color_off <- simulate_kits(doe_color, buck_color)
agouti_off <- simulate_kits(doe_agouti, buck_agouti)
family_off <- simulate_kits(doe_family, buck_family)
pattern_off <- simulate_kits(doe_pattern, buck_pattern)
dilution_off <- simulate_kits(doe_dilution, buck_dilution)
spotting_off <- simulate_kits(doe_spotting, buck_spotting)
checkering_off <- simulate_kits(doe_checkering, buck_checkering)
rex_off <- simulate_kits(doe_rex, buck_rex)

df <- data.frame(
  Color_Genotype = rep(color_off, length.out = kit_count),
  Agouti_Genotype = rep(agouti_off, length.out = kit_count),
  Family_Genotype = rep(family_off, length.out = kit_count),
  Pattern_Genotype = rep(pattern_off, length.out = kit_count),
  Dilution_Genotype = rep(dilution_off, length.out = kit_count),
  Spotting_Genotype = rep(spotting_off, length.out = kit_count),
  Checkering_Genotype = rep(checkering_off, length.out = kit_count),
  Rex_Genotype = rep(rex_off, length.out = kit_count),
  stringsAsFactors = FALSE
)

df$Color_Phenotype <- dplyr::case_when(
  df$Color_Genotype == "BB" ~ "Black-based",
  df$Color_Genotype == "Bb" ~ "Black (carries chocolate)",
  df$Color_Genotype == "bb" ~ "Chocolate-based",
  TRUE ~ "Unknown"
)
df$Agouti_Phenotype <- dplyr::case_when(
  df$Agouti_Genotype %in% c("AA", "Aa") ~ "Agouti",
  df$Agouti_Genotype == "aa" ~ "Solid",
  TRUE ~ "Unknown"
)
df$Family_Phenotype <- dplyr::case_when(
  df$Family_Genotype == "CC" ~ "Full",
  df$Family_Genotype == "cchdcchd" ~ "Chinchilla",
  df$Family_Genotype == "chch" ~ "Seal",
  df$Family_Genotype == "cycy" ~ "Sable",
  df$Family_Genotype == "cccc" ~ "Himalayan",
  df$Family_Genotype == "cc" ~ "Ruby-Eyed White",
  TRUE ~ "Unknown"
)
df$Pattern_Phenotype <- dplyr::case_when(
  df$Pattern_Genotype == "EE" ~ "Charlie",
  df$Pattern_Genotype == "Ee" ~ "Broken",
  df$Pattern_Genotype == "ee" ~ "Self",
  df$Pattern_Genotype == "Esej" ~ "Harlequin",
  df$Pattern_Genotype == "ww" ~ "Wideband",
  TRUE ~ "Unknown"
)
df$Dilution_Phenotype <- dplyr::case_when(
  df$Dilution_Genotype == "DD" ~ "Normal",
  df$Dilution_Genotype == "Dd" ~ "Diluted",
  df$Dilution_Genotype == "dd" ~ "Blue",
  TRUE ~ "Unknown"
)
df$Spotting_Phenotype <- dplyr::case_when(
  df$Spotting_Genotype == "SS" ~ "Solid",
  df$Spotting_Genotype == "Ss" ~ "Spotted",
  df$Spotting_Genotype == "ss" ~ "Heavy spotted",
  TRUE ~ "Unknown"
)
df$Checkering_Phenotype <- dplyr::case_when(
  df$Checkering_Genotype == "Lc" ~ "Light checkering",
  df$Checkering_Genotype == "Mc" ~ "Medium checkering",
  df$Checkering_Genotype == "Hc" ~ "Heavy checkering",
  TRUE ~ "Unknown"
)
df$Rex_Phenotype <- dplyr::case_when(
  df$Rex_Genotype == "Rr" ~ "Rex",
  df$Rex_Genotype == "rr" ~ "Normal",
  TRUE ~ "Unknown"
)
