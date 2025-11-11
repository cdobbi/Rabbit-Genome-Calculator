library(dplyr)

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

# ----- Simulation metadata -----
kit_count <- 10

# ----- Generate offspring genotypes for each locus by calling simulate_kits -----
color_off <- simulate_kits(doe_color, buck_color)
family_off <- simulate_kits(doe_family, buck_family)

# ----- Build a data frame (table) representing kits 1..10 -----
df <- data.frame(
  Color_Genotype = rep(color_off, length.out = kit_count),
  Family_Genotype = rep(family_off, length.out = kit_count),
  stringsAsFactors = FALSE
)

# Translate raw genotypes into words a breeder can read
df$Color_Phenotype <- dplyr::case_when(
  df$Color_Genotype == "BB" ~ "Black-based",
  df$Color_Genotype == "Bb" ~ "Black (carries chocolate)",
  df$Color_Genotype == "bb" ~ "Chocolate-based",
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

# ----- Prepare table for printing -----
results <- df %>% 
  dplyr::mutate(
    Kit = seq_len(dplyr::n()),
    Family = Family_Phenotype,
    Color = Color_Phenotype
  ) %>% 
  dplyr::select(Kit, Family, Color)

# Summarize how many kits fall into each outcome and compute percentages
outcome_summary <- results %>% 
  dplyr::count(Family, Color, name = "Kits") %>% 
  dplyr::mutate(
    Percentage = round((Kits / kit_count) * 100, 1),
    Associated_Genes = "aa Bb EnEn Dd Ee"
  ) %>% 
  dplyr::mutate(
    Possible_Colors = paste(Family, Color, sep = " ")
  ) %>% 
  dplyr::arrange(dplyr::desc(Kits)) %>% 
  dplyr::select(Possible_Colors, Percentage, Associated_Genes) %>% 
  head(10) %>% 
  dplyr::mutate(PROBABILITY = paste0(Percentage, "%")) %>% 
  dplyr::select(Possible_Colors, PROBABILITY, Associated_Genes) %>% 
  dplyr::rename(`POSSIBLE COLORS` = Possible_Colors, `ASSOCIATED GENES` = Associated_Genes)

# Show only the summary table
print(outcome_summary)