library(dplyr)

source("functions.R")

agouti_options <- c("  1. Agouti — A (AA)", "  2. Solid — a (aa)", "  3. Unknown")
color_options <- c("  1. Black (self) — B (BB)", "  2. Black (self, carries chocolate) — B (Bb)", "  3. Chocolate (self) — b (bb)", "  4. Chestnut (agouti) — B (BB)", "  5. Orange (agouti) — B (Bb)", "  6. Chocolate agouti — b (bb)", "  7. Unknown", "  8. None")
color_genotypes <- c("BB", "Bb", "bb", "BB", "Bb", "bb", "Unknown", "None")
family_options <- c("  1. Full — C (CC)", "  2. Chinchilla — c(chd) (cchdcchd)", "  3. Seal — ch (chch)", "  4. Sable — c(y) (cycy)", "  5. Himalayan — c(h) (cccc)", "  6. Ruby-Eyed-White — c (cc)", "  7. Unknown")
dilution_options <- c("  1. Normal — D (DD)", "  2. Diluted — D (Dd)", "  3. Blue — d (dd)", "  4. Unknown", "  5. None")
pattern_options <- c("  1. Self (solid) — en/en", "  2. Broken — En/en", "  3. Charlie — En/En", "  4. Harlequin — Esej", "  5. Wideband — ww", "  6. Unknown", "  7. None")
pattern_narrative_labels <- c("Sport (self)", "Marked (broken)", "Charlie", "Harlequin", "Wideband", "Unknown", "None")
pattern_display_labels <- c("Self" = "Sport", "Broken" = "Marked", "Charlie" = "Charlie", "Harlequin" = "Harlequin", "Wideband" = "Wideband", "Unknown" = "Unknown", "None" = "None")
spotting_options <- c("  1. Solid — S (SS)", "  2. Spotted — S (Ss)", "  3. Heavy spotted — s (ss)", "  4. Unknown", "  5. None")
checkering_options <- c("  1. Light checkering — Lc", "  2. Medium checkering — Mc", "  3. Heavy checkering — Hc", "  4. Unknown", "  5. None")
rex_options <- c("  1. Rex — R (Rr)", "  2. Normal — r (rr)", "  3. Unknown", "  4. None")

get_color_genotype <- function(choice) {
  if (is.na(choice) || choice < 1 || choice > length(color_genotypes)) {
    "??" 
  } else {
    color_genotypes[choice]
  }
}

cat("****************************************************\n")
cat("🐇 Welcome to The Rabbit Genome Calculator!🧮\n") 
cat("****************************************************\n\n")
cat("This calculator predicts the top 10 coat\n colors based on parent pairings.\n\n")

cat("Agouti: banded hairs (wild-type).\n")
cat("Solid: uniform color.\n")
cat("Helpful Hints:\n")
cat("Most like Rhinelander are Agouti.\n")
cat("For Harlequin (solid marble, no white),\n")
cat("select Agouti here, then Harlequin in pattern.\n")
display_menu("Doe", "agouti pattern", agouti_options)
doe_agouti_choice <- as.integer(readline("Type a number: "))
doe_agouti <- if (doe_agouti_choice == 1) "AA" else if (doe_agouti_choice == 2) "aa" else "Unknown"
cat("\n")

display_menu("Buck", "agouti pattern", agouti_options)
buck_agouti_choice <- as.integer(readline("Type a number: "))
buck_agouti <- if (buck_agouti_choice == 1) "AA" else if (buck_agouti_choice == 2) "aa" else "Unknown"
cat("\n")

cat("Color: B locus base (black/chocolate/agouti).\n")
display_menu("Doe", "color", color_options)
doe_color_choice <- as.integer(readline("Type a number: "))
doe_color <- get_color_genotype(doe_color_choice)
doe_color_label <- if (!is.na(doe_color_choice) && doe_color_choice %in% seq_along(color_options)) {
  strip_option_label(color_options[doe_color_choice])
} else {
  "Unknown color"
}
cat("\n")

display_menu("Buck", "color", color_options)
buck_color_choice <- as.integer(readline("Type a number: "))
buck_color <- get_color_genotype(buck_color_choice)
buck_color_label <- if (!is.na(buck_color_choice) && buck_color_choice %in% seq_along(color_options)) {
  strip_option_label(color_options[buck_color_choice])
} else {
  "Unknown color"
}
cat("\n")

display_menu("Doe", "color family", family_options)
doe_family_choice <- as.integer(readline("Type a number: "))
doe_family <- switch(doe_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Unknown", "Invalid")
cat("\n")

display_menu("Buck", "color family", family_options)
buck_family_choice <- as.integer(readline("Type a number: "))
buck_family <- switch(buck_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Unknown", "Invalid")
cat("\n")

display_menu("Doe", "dilution", dilution_options)
doe_dilution_choice <- as.integer(readline("Type a number: "))
doe_dilution <- switch(doe_dilution_choice, "DD", "Dd", "dd", "Unknown", "None", "Invalid")
cat("\n")

display_menu("Buck", "dilution", dilution_options)
buck_dilution_choice <- as.integer(readline("Type a number: "))
buck_dilution <- switch(buck_dilution_choice, "DD", "Dd", "dd", "Unknown", "None", "Invalid")
cat("\n")

display_menu("Doe", "pattern", pattern_options)
doe_pattern_choice <- as.integer(readline("Type a number: "))
doe_pattern <- switch(doe_pattern_choice, "ee", "Ee", "EE", "Esej", "ww", "Unknown", "None", "Invalid")
doe_pattern_label <- if (!is.na(doe_pattern_choice) && doe_pattern_choice %in% seq_along(pattern_narrative_labels)) {
  pattern_narrative_labels[doe_pattern_choice]
} else {
  "Unknown pattern"
}
cat("\n")

display_menu("Buck", "pattern", pattern_options)
buck_pattern_choice <- as.integer(readline("Type a number: "))
buck_pattern <- switch(buck_pattern_choice, "ee", "Ee", "EE", "Esej", "ww", "Unknown", "None", "Invalid")
buck_pattern_label <- if (!is.na(buck_pattern_choice) && buck_pattern_choice %in% seq_along(pattern_narrative_labels)) {
  pattern_narrative_labels[buck_pattern_choice]
} else {
  "Unknown pattern"
}
cat("\n")

display_menu("Doe", "spotting", spotting_options)
doe_spotting_choice <- as.integer(readline("Type a number: "))
doe_spotting <- switch(doe_spotting_choice, "SS", "Ss", "ss", "Unknown", "None", "Invalid")
cat("\n")

display_menu("Buck", "spotting", spotting_options)
buck_spotting_choice <- as.integer(readline("Type a number: "))
buck_spotting <- switch(buck_spotting_choice, "SS", "Ss", "ss", "Unknown", "None", "Invalid")
cat("\n")

display_menu("Doe", "checkering", checkering_options)
doe_checkering_choice <- as.integer(readline("Type a number: "))
doe_checkering <- switch(doe_checkering_choice, "Lc", "Mc", "Hc", "Unknown", "None", "Invalid")
cat("\n")

display_menu("Buck", "checkering", checkering_options)
buck_checkering_choice <- as.integer(readline("Type a number: "))
buck_checkering <- switch(buck_checkering_choice, "Lc", "Mc", "Hc", "Unknown", "None", "Invalid")
cat("\n")

display_menu("Doe", "rex", rex_options)
doe_rex_choice <- as.integer(readline("Type a number: "))
doe_rex <- switch(doe_rex_choice, "Rr", "rr", "Unknown", "None", "Invalid")
cat("\n")

display_menu("Buck", "rex", rex_options)
buck_rex_choice <- as.integer(readline("Type a number: "))
buck_rex <- switch(buck_rex_choice, "Rr", "rr", "Unknown", "None", "Invalid")
cat("\n")

source("simulation.R")
source("output.R")