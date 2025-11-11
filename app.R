library(dplyr)
library(genetics)

# Attribution Statement: I used AI assistance to help write individual lines of R code, but I worked through each one carefully—asking questions. I renamed variables, studied the syntax, naming conventions, and control flow to ensure I understood the meaning of every word and the logic behind it. The overall concept, structure, and application to animal husbandry are entirely my own design. I may not be writing code fully on my own yet, but I hope this calculator represents what I’ve learned and how I’ve applied it.

# R is designed around vectorized operations, meaning functions or operations can be applied to an entire vector at once without needing a loop which makes R code very efficient for data analysis.
# R stores the input string in RAM as soon as you type it; substr() just reads that in-memory string—no extra file required.
# Syntax quick-reference:
# <-  : assignment operator (binds a value to a name).
# <<- : assignment reaching parent environments (not used, but common in R).
# =   : also assigns inside argument lists (same binding idea).
# ()  : call operator; wraps function arguments.
# {}  : code block (groups multiple statements).
# []  : indexing operator for vectors/data frames.
# %in%: set membership test.
# ==  : equality comparison.
# !=  : not-equal comparison.
# !   : logical NOT.
# &&  : single-value logical AND (short-circuits).
# ||  : single-value logical OR (short-circuits).
# #   : comment marker; everything to the end of the line is ignored.

# simulate_kits:
# - Inputs: doe_genotype, buck_genotype (strings such as "Bb" or "EE")
# - Goal: return all possible child genotype combinations by mixing parents' alleles.
# - Alleles are the different versions of a gene carried on each chromosome copy
# - Genotype is the pair of alleles, 
# - Phenotype is the visible trait (like coat pattern) produced by that genotype.
simulate_kits <- function(doe_genotype, buck_genotype) {
  half_doe <- nchar(doe_genotype) / 2
  doe_alleles <- c(
    substr(doe_genotype, 1, as.integer(half_doe)),
    substr(doe_genotype, as.integer(half_doe) + 1, nchar(doe_genotype))
  )
  half_buck <- nchar(buck_genotype) / 2
  buck_alleles <- c(
    substr(buck_genotype, 1, as.integer(half_buck)),
    substr(buck_genotype, as.integer(half_buck) + 1, nchar(buck_genotype))
  )
  kits <- c()
  for (d in doe_alleles) {
    for (b in buck_alleles) {
      kits <- c(kits, paste0(d, b))
    }
  }
  normalize <- function(g) {
    if (nchar(g) == 2) {
      chars <- unlist(strsplit(g, ""))
      ord <- order(-as.integer(grepl("[A-Z]", chars)), chars)
      paste0(chars[ord], collapse = "")
    } else {
      g
    }
  }
  normalized_kits <- vapply(kits, normalize, FUN.VALUE = character(1))
  return(normalized_kits)
}

# display_menu: prints a clean section heading plus numbered options for the user
display_menu <- function(subject, descriptor, options) {
  header <- sprintf("%s's %s?", subject, descriptor)
  cat(paste(c(header, options), collapse = "\n"), "\n", sep = "")
}
# strip_option_label: removes the leading "  1. " style numbering so we can reuse just the descriptive label later
strip_option_label <- function(option_text) {
  sub("^\\s*\\d+\\.\\s*", "", option_text)
}
cat("****************************************************\n")
cat("🐇 Welcome to The Rabbit Genome Calculator!🧮\n")
cat("****************************************************\n\n")
cat("This calculator predicts the top 10 coat\n colors based on parent pairings.\n\n")

family_options <- c(
  "  1. Full — C (CC)",
  "  2. Chinchilla — c(chd) (cchdcchd)",
  "  3. Seal — ch (chch)",
  "  4. Sable — c(y) (cycy)",
  "  5. Himalayan — c(h) (cccc)",
  "  6. Ruby-Eyed-White — c (cc)"
)
color_options <- c(
  "  1. Black (self)",
  "  2. Black (self, carries chocolate)",
  "  3. Chocolate (self)",
  "  4. Black otter",
  "  5. Chocolate otter",
  "  6. Black tortoiseshell",
  "  7. Chocolate tortoiseshell",
  "  8. Chestnut (agouti)",
  "  9. Orange (agouti)",
  " 10. Chocolate agouti"
)
color_genotypes <- c("BB", "Bb", "bb", "BB", "bb", "BB", "bb", "BB", "Bb", "bb")

get_color_genotype <- function(choice) {
  if (is.na(choice) || choice < 1 || choice > length(color_genotypes)) {
    "??" 
  } else {
    color_genotypes[choice]
  }
}

# Genotype mappings (hardcoded)
steel_harlequin_genotypes <- c("EsEs", "Esej")
broken_genotypes <- c("EnEn", "Enen")
vienna_genotypes <- c("VV", "Vv")
dutch_genotypes <- c("DuDu", "Dudu")
silvering_genotypes <- c("SiSi", "Sisi")
wideband_genotypes <- c("WW", "Ww")
lutino_genotypes <- c("PP", "Pp")

# ----- Collect C-locus (color family) selections -----
display_menu("Doe", "color family", family_options)
doe_family_choice <- as.integer(readline("Type a number: "))
doe_family <- switch(doe_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")

display_menu("Buck", "color family", family_options)
buck_family_choice <- as.integer(readline("Type a number: "))
buck_family <- switch(buck_family_choice, "CC", "cchdcchd", "chch", "cycy", "cccc", "cc", "Invalid")

# ----- Collect B-locus (base color) selections -----
display_menu("Doe", "color", color_options)
doe_color_choice <- as.integer(readline("Type a number: "))
doe_color <- get_color_genotype(doe_color_choice)

display_menu("Buck", "color", color_options)
buck_color_choice <- as.integer(readline("Type a number: "))
buck_color <- get_color_genotype(buck_color_choice)

# ----- Collect A-locus (agouti vs solid) selections -----
display_menu("Doe", "agouti pattern", agouti_options)
doe_agouti_choice <- as.integer(readline("Type a number: "))
doe_agouti <- if (doe_agouti_choice == 1) "AA" else "aa"

display_menu("Buck", "agouti pattern", agouti_options)
buck_agouti_choice <- as.integer(readline("Type a number: "))
buck_agouti <- if (buck_agouti_choice == 1) "AA" else "aa"

# ----- Do you want to consider broken genes? -----
broken_options_special <- c("  1. Charlie — ENEN", "  2. Broken — Enen", "  3. Solid — enen", "  4. Unknown")
display_menu("Doe", "broken genes", broken_options_special)
doe_broken_special_choice <- as.integer(readline("Type a number: "))
doe_broken_special <- switch(doe_broken_special_choice, "ENEN", "Enen", "enen", "Unknown", "Invalid")

display_menu("Buck", "broken genes", broken_options_special)
buck_broken_special_choice <- as.integer(readline("Type a number: "))
buck_broken_special <- switch(buck_broken_special_choice, "ENEN", "Enen", "enen", "Unknown", "Invalid")

# ----- Consider patterns -----
cat("Select which patterns to consider (enter numbers separated by commas, e.g., 1,3,5 or 0 for none):\n")
cat("1. Steel and harlequin\n")
cat("2. Broken\n")
cat("3. Vienna\n")
cat("4. Dutch\n")
cat("5. Silvering\n")
cat("6. Wideband\n")
cat("7. Lutino\n")
pattern_choices <- readline("Your choices: ")
selected <- as.integer(unlist(strsplit(pattern_choices, ",")))

consider_steel <- 1 %in% selected
consider_broken <- 2 %in% selected
consider_vienna <- 3 %in% selected
consider_dutch <- 4 %in% selected
consider_silvering <- 5 %in% selected
consider_wideband <- 6 %in% selected
consider_lutino <- 7 %in% selected

if (consider_steel) {
  display_menu("Doe", "steel/harlequin", steel_harlequin_options)
  doe_steel_choice <- as.integer(readline("Type a number: "))
  doe_steel <- switch(doe_steel_choice, "EsEs", "Esej", "Invalid")
  
  display_menu("Buck", "steel/harlequin", steel_harlequin_options)
  buck_steel_choice <- as.integer(readline("Type a number: "))
  buck_steel <- switch(buck_steel_choice, "EsEs", "Esej", "Invalid")
}

if (consider_broken) {
  display_menu("Doe", "broken", broken_options)
  doe_broken_choice <- as.integer(readline("Type a number: "))
  doe_broken <- switch(doe_broken_choice, "EnEn", "Enen", "Invalid")
  
  display_menu("Buck", "broken", broken_options)
  buck_broken_choice <- as.integer(readline("Type a number: "))
  buck_broken <- switch(buck_broken_choice, "EnEn", "Enen", "Invalid")
}

if (consider_vienna) {
  display_menu("Doe", "vienna", vienna_options)
  doe_vienna_choice <- as.integer(readline("Type a number: "))
  doe_vienna <- switch(doe_vienna_choice, "VV", "Vv", "Invalid")
  
  display_menu("Buck", "vienna", vienna_options)
  buck_vienna_choice <- as.integer(readline("Type a number: "))
  buck_vienna <- switch(buck_vienna_choice, "VV", "Vv", "Invalid")
}

if (consider_dutch) {
  display_menu("Doe", "dutch", dutch_options)
  doe_dutch_choice <- as.integer(readline("Type a number: "))
  doe_dutch <- switch(doe_dutch_choice, "DuDu", "Dudu", "Invalid")
  
  display_menu("Buck", "dutch", dutch_options)
  buck_dutch_choice <- as.integer(readline("Type a number: "))
  buck_dutch <- switch(buck_dutch_choice, "DuDu", "Dudu", "Invalid")
}

if (consider_silvering) {
  display_menu("Doe", "silvering", silvering_options)
  doe_silvering_choice <- as.integer(readline("Type a number: "))
  doe_silvering <- switch(doe_silvering_choice, "SiSi", "Sisi", "Invalid")
  
  display_menu("Buck", "silvering", silvering_options)
  buck_silvering_choice <- as.integer(readline("Type a number: "))
  buck_silvering <- switch(buck_silvering_choice, "SiSi", "Sisi", "Invalid")
}

if (consider_wideband) {
  display_menu("Doe", "wideband", wideband_options)
  doe_wideband_choice <- as.integer(readline("Type a number: "))
  doe_wideband <- switch(doe_wideband_choice, "WW", "Ww", "Invalid")
  
  display_menu("Buck", "wideband", wideband_options)
  buck_wideband_choice <- as.integer(readline("Type a number: "))
  buck_wideband <- switch(buck_wideband_choice, "WW", "Ww", "Invalid")
}

if (consider_lutino) {
  display_menu("Doe", "lutino", lutino_options)
  doe_lutino_choice <- as.integer(readline("Type a number: "))
  doe_lutino <- switch(doe_lutino_choice, "PP", "Pp", "Invalid")
  
  display_menu("Buck", "lutino", lutino_options)
  buck_lutino_choice <- as.integer(readline("Type a number: "))
  buck_lutino <- switch(buck_lutino_choice, "PP", "Pp", "Invalid")
}

# ----- Simulation metadata -----
kit_count <- 10

# ----- Generate offspring genotypes for each locus by calling simulate_kits -----
doe_color_geno <- genotype(doe_color)
buck_color_geno <- genotype(buck_color)
color_cross <- cross(doe_color_geno, buck_color_geno)
color_probs <- summary(color_cross)$proportion
color_genos <- as.character(summary(color_cross)$genotype)
color_off <- sample(color_genos, size = kit_count, replace = TRUE, prob = color_probs)

doe_agouti_geno <- genotype(doe_agouti)
buck_agouti_geno <- genotype(buck_agouti)
agouti_cross <- cross(doe_agouti_geno, buck_agouti_geno)
agouti_probs <- summary(agouti_cross)$proportion
agouti_genos <- as.character(summary(agouti_cross)$genotype)
agouti_off <- sample(agouti_genos, size = kit_count, replace = TRUE, prob = agouti_probs)

family_off <- simulate_kits(doe_family, buck_family)
broken_special_off <- simulate_kits(doe_broken_special, buck_broken_special)
if (consider_steel) steel_off <- simulate_kits(doe_steel, buck_steel)
if (consider_broken) broken_off <- simulate_kits(doe_broken, buck_broken)
if (consider_vienna) vienna_off <- simulate_kits(doe_vienna, buck_vienna)
if (consider_dutch) dutch_off <- simulate_kits(doe_dutch, buck_dutch)
if (consider_silvering) silvering_off <- simulate_kits(doe_silvering, buck_silvering)
if (consider_wideband) wideband_off <- simulate_kits(doe_wideband, buck_wideband)
if (consider_lutino) lutino_off <- simulate_kits(doe_lutino, buck_lutino)

# ----- Build a data frame (table) representing kits 1..10 -----
df <- data.frame(
  Color_Genotype = color_off,
  Agouti_Genotype = agouti_off,
  Family_Genotype = rep(family_off, length.out = kit_count),
  Broken_Special_Genotype = rep(broken_special_off, length.out = kit_count),
  stringsAsFactors = FALSE
)
if (consider_steel) df$Steel_Genotype <- steel_off
if (consider_broken) df$Broken_Genotype <- broken_off
if (consider_vienna) df$Vienna_Genotype <- vienna_off
if (consider_dutch) df$Dutch_Genotype <- dutch_off
if (consider_silvering) df$Silvering_Genotype <- silvering_off
if (consider_wideband) df$Wideband_Genotype <- wideband_off
if (consider_lutino) df$Lutino_Genotype <- lutino_off

# Translate raw genotypes into words a breeder can read
df$Color_Phenotype <- dplyr::case_when(
  df$Color_Genotype == "B/B" & df$Agouti_Genotype %in% c("A/A", "A/a") ~ "Chestnut",
  df$Color_Genotype == "B/B" & df$Agouti_Genotype == "a/a" ~ "Black self",
  df$Color_Genotype == "B/b" & df$Agouti_Genotype %in% c("A/A", "A/a") ~ "Orange",
  df$Color_Genotype == "B/b" & df$Agouti_Genotype == "a/a" ~ "Black (carries chocolate)",
  df$Color_Genotype == "b/b" & df$Agouti_Genotype %in% c("A/A", "A/a") ~ "Fawn",
  df$Color_Genotype == "b/b" & df$Agouti_Genotype == "a/a" ~ "Chocolate self",
  TRUE ~ "Unknown"
)
df$Agouti_Phenotype <- dplyr::case_when(
  df$Agouti_Genotype %in% c("A/A", "A/a") ~ "Agouti",
  df$Agouti_Genotype == "a/a" ~ "Solid",
  TRUE ~ "Unknown"
)
df$Family_Phenotype <- dplyr::case_when(
  grepl("CC", df$Family_Genotype, ignore.case = TRUE) ~ "Full",
  grepl("cchd", df$Family_Genotype, ignore.case = TRUE) ~ "Chinchilla",
  grepl("chch", df$Family_Genotype, ignore.case = TRUE) ~ "Seal",
  grepl("cycy", df$Family_Genotype, ignore.case = TRUE) ~ "Sable",
  grepl("cccc", df$Family_Genotype, ignore.case = TRUE) ~ "Himalayan",
  grepl("cc", df$Family_Genotype, ignore.case = TRUE) ~ "Ruby-Eyed White",
  TRUE ~ "Unknown"
)
df$Broken_Special_Phenotype <- dplyr::case_when(
  df$Broken_Special_Genotype == "ENEN" ~ "Charlie",
  df$Broken_Special_Genotype == "Enen" ~ "Broken",
  df$Broken_Special_Genotype == "enen" ~ "Solid",
  TRUE ~ "Unknown"
)
if (consider_steel) df$Steel_Phenotype <- dplyr::case_when(
  df$Steel_Genotype == "EsEs" ~ "Steel",
  df$Steel_Genotype == "Esej" ~ "Harlequin",
  TRUE ~ "Unknown"
)
if (consider_broken) df$Broken_Phenotype <- dplyr::case_when(
  df$Broken_Genotype == "EnEn" ~ "Broken",
  df$Broken_Genotype == "Enen" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_vienna) df$Vienna_Phenotype <- dplyr::case_when(
  df$Vienna_Genotype == "VV" ~ "Vienna",
  df$Vienna_Genotype == "Vv" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_dutch) df$Dutch_Phenotype <- dplyr::case_when(
  df$Dutch_Genotype == "DuDu" ~ "Dutch",
  df$Dutch_Genotype == "Dudu" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_silvering) df$Silvering_Phenotype <- dplyr::case_when(
  df$Silvering_Genotype == "SiSi" ~ "Silvering",
  df$Silvering_Genotype == "Sisi" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_wideband) df$Wideband_Phenotype <- dplyr::case_when(
  df$Wideband_Genotype == "WW" ~ "Wideband",
  df$Wideband_Genotype == "Ww" ~ "Carrier",
  TRUE ~ "Unknown"
)
if (consider_lutino) df$Lutino_Phenotype <- dplyr::case_when(
  df$Lutino_Genotype == "PP" ~ "Lutino",
  df$Lutino_Genotype == "Pp" ~ "Carrier",
  TRUE ~ "Unknown"
)

# ----- Prepare table for printing -----
results <- df %>% 
  dplyr::mutate(
    Kit = seq_len(dplyr::n()),
    Family = Family_Phenotype,
    Color = Color_Phenotype,
    Agouti = Agouti_Phenotype,
    Broken_Special = Broken_Special_Phenotype
  )
if (consider_steel) results <- results %>% dplyr::mutate(Steel = Steel_Phenotype)
if (consider_broken) results <- results %>% dplyr::mutate(Broken = Broken_Phenotype)
if (consider_vienna) results <- results %>% dplyr::mutate(Vienna = Vienna_Phenotype)
if (consider_dutch) results <- results %>% dplyr::mutate(Dutch = Dutch_Phenotype)
if (consider_silvering) results <- results %>% dplyr::mutate(Silvering = Silvering_Phenotype)
if (consider_wideband) results <- results %>% dplyr::mutate(Wideband = Wideband_Phenotype)
if (consider_lutino) results <- results %>% dplyr::mutate(Lutino = Lutino_Phenotype)

results <- results %>% dplyr::select(Kit, Family, Color, Agouti, Broken_Special, everything())

# Summarize how many kits fall into each outcome and compute percentages
outcome_summary <- results %>% 
  dplyr::mutate(
    `POSSIBLE COLORS` = paste(Family, Color, Agouti, Broken_Special, if (consider_steel) Steel else "", if (consider_broken) Broken else "", if (consider_vienna) Vienna else "", if (consider_dutch) Dutch else "", if (consider_silvering) Silvering else "", if (consider_wideband) Wideband else "", if (consider_lutino) Lutino else "", sep = " ")
  ) %>% 
  dplyr::mutate(
    `POSSIBLE COLORS` = trimws(`POSSIBLE COLORS`)
  ) %>% 
  dplyr::mutate(
    PROBABILITY = "10%",
    `ASSOCIATED GENES` = "aa Bb EnEn Dd Ee"
  ) %>% 
  dplyr::select(`POSSIBLE COLORS`, PROBABILITY, `ASSOCIATED GENES`)

# Show only the summary table
print(outcome_summary)