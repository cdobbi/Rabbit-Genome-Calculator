agouti_options <- c("  1. Agouti — A (AA)", "  2. Solid — a (aa)", "  3. Unknown")
color_options <- c("  1. Black (self) — B (BB)", "  2. Black (self, carries chocolate) — B (Bb)", "  3. Chocolate (self) — b (bb)", "  4. Chestnut (agouti) — B (BB)", "  5. Orange (agouti) — B (Bb)", "  6. Chocolate agouti — b (bb)", "  7. Unknown", "  8. None")
color_genotypes <- c("BB", "Bb", "bb", "BB", "Bb", "bb", "Unknown", "None")
dilution_options <- c("  1. Normal — D (DD)", "  2. Diluted — D (Dd)", "  3. Blue — d (dd)", "  4. Unknown", "  5. None")
pattern_options <- c("  1. Self (solid) — en/en", "  2. Broken — En/en", "  3. Charlie — En/En", "  4. Harlequin — Esej", "  5. Wideband — ww", "  6. Unknown", "  7. None")
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
