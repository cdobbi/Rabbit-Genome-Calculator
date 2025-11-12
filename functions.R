simulate_kits <- function(doe_genotype, buck_genotype) {
  half_doe <- nchar(doe_genotype) / 2
  doe_alleles <- c(substr(doe_genotype, 1, as.integer(half_doe)), substr(doe_genotype, as.integer(half_doe) + 1, nchar(doe_genotype)))
  half_buck <- nchar(buck_genotype) / 2
  buck_alleles <- c(substr(buck_genotype, 1, as.integer(half_buck)), substr(buck_genotype, as.integer(half_buck) + 1, nchar(buck_genotype)))
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

display_menu <- function(subject, descriptor, options) {
  header <- sprintf("%s's %s?", subject, descriptor)
  cat(paste(c(header, options), collapse = "\n"), "\n", sep = "")
}

strip_option_label <- function(option_text) {
  sub("^\\s*\\d+\\.\\s*", "", option_text)
}
