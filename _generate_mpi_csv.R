# =============================================================================
# Generate all CSV tables for report_interactive_lite
# Adapted from report_interactive/prepare_data.R
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(Hmisc)
library(intsvy)
library(labelled)
library(oaxaca)

output_dir <- "D:/report_education/report_interactive_lite/tables"

# =============================================================================
# 1. PISA DATA
# =============================================================================

cat("Loading PISA data...\n")
file_path <- "G:/My Drive/PISA/data/pisa_uzb_kaz_ger.dta"
pisa_data <- read_dta(file_path)
pisa <- pisa_data

# Convert labelled CNT to character for filtering
pisa$CNT <- as.character(haven::as_factor(pisa$CNT))

# Calculate weighted decile breakpoints
weighted_breaks <- wtd.quantile(pisa$ESCS, weights = pisa$W_FSTUWT,
                                probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Assign ESCS deciles
pisa <- pisa %>%
  mutate(
    ESCS_w_decile = cut(ESCS, breaks = weighted_breaks,
                        labels = paste0("D", 1:10), include.lowest = TRUE)
  )

# Filter for Uzbekistan
pisa_uzb <- pisa %>%
  filter(CNT == "UZB") %>%
  mutate(
    Gender = factor(ST004D01T, levels = c(1, 2), labels = c("Qiz", "O'g'il")),
    rural = ifelse(SC001Q01TA == 1 | SC001Q01TA == 2, "Qishloq", "Shahar")
  )

# --- 1a. PISA Level 2 by decile (all subjects) ---
cat("Computing PISA Level 2 attainment...\n")

mathcut <- c(420.07, 482.38, 544.68, 606.99, 669.30)
readcut <- c(407.47, 480.18, 552.89, 625.61, 698.32)
sciecut <- c(409.54, 484.14, 558.73, 633.33, 707.93)

math_prof <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "MATH"),
                                     cutoff = mathcut,
                                     by = "ESCS_w_decile",
                                     data = pisa_uzb))
math_level2 <- math_prof %>%
  filter(Benchmark == "At or above 420.07") %>%
  select(ESCS_w_decile, Percentage) %>%
  mutate(Subject = "Matematika")

read_prof <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "READ"),
                                     cutoff = readcut,
                                     by = "ESCS_w_decile",
                                     data = pisa_uzb))
read_level2 <- read_prof %>%
  filter(Benchmark == "At or above 407.47") %>%
  select(ESCS_w_decile, Percentage) %>%
  mutate(Subject = "O'qish")

scie_prof <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "SCIE"),
                                     cutoff = sciecut,
                                     by = "ESCS_w_decile",
                                     data = pisa_uzb))
scie_level2 <- scie_prof %>%
  filter(Benchmark == "At or above 409.54") %>%
  select(ESCS_w_decile, Percentage) %>%
  mutate(Subject = "Tabiiy fanlar")

level2_all <- bind_rows(math_level2, read_level2, scie_level2) %>%
  mutate(Decile_num = as.numeric(gsub("D", "", ESCS_w_decile)))

write_csv(level2_all, file.path(output_dir, "pisa_level2_all.csv"))
cat("  -> pisa_level2_all.csv\n")

# --- 1b. PISA Level 2 by rural/urban ---
cat("Computing PISA Level 2 by rural/urban...\n")
pisa_uzb_rural <- pisa_uzb %>% filter(!is.na(rural))

math_rural <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "MATH"),
                                      cutoff = mathcut,
                                      by = c("ESCS_w_decile", "rural"),
                                      data = pisa_uzb_rural))
math_level2_rural <- math_rural %>%
  filter(Benchmark == "At or above 420.07") %>%
  select(ESCS_w_decile, Percentage, rural) %>%
  mutate(Subject = "Matematika")

read_rural <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "READ"),
                                      cutoff = readcut,
                                      by = c("ESCS_w_decile", "rural"),
                                      data = pisa_uzb_rural))
read_level2_rural <- read_rural %>%
  filter(Benchmark == "At or above 407.47") %>%
  select(ESCS_w_decile, Percentage, rural) %>%
  mutate(Subject = "O'qish")

scie_rural <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "SCIE"),
                                      cutoff = sciecut,
                                      by = c("ESCS_w_decile", "rural"),
                                      data = pisa_uzb_rural))
scie_level2_rural <- scie_rural %>%
  filter(Benchmark == "At or above 409.54") %>%
  select(ESCS_w_decile, Percentage, rural) %>%
  mutate(Subject = "Tabiiy fanlar")

level2_rural_all <- bind_rows(math_level2_rural, read_level2_rural, scie_level2_rural) %>%
  filter(!is.na(rural)) %>%
  mutate(Decile_num = as.numeric(gsub("D", "", ESCS_w_decile)),
         Subject = factor(Subject, levels = c("Matematika", "O'qish", "Tabiiy fanlar")))

write_csv(level2_rural_all, file.path(output_dir, "pisa_level2_rural.csv"))
cat("  -> pisa_level2_rural.csv\n")

# --- 1c. PISA Level 2 by gender ---
cat("Computing PISA Level 2 by gender...\n")
pisa_uzb_gender <- pisa_uzb %>% filter(!is.na(Gender))

math_gender <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "MATH"),
                                       cutoff = mathcut,
                                       by = c("ESCS_w_decile", "Gender"),
                                       data = pisa_uzb_gender))
math_level2_gender <- math_gender %>%
  filter(Benchmark == "At or above 420.07") %>%
  select(ESCS_w_decile, Percentage, Gender) %>%
  mutate(Subject = "Matematika")

read_gender <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "READ"),
                                       cutoff = readcut,
                                       by = c("ESCS_w_decile", "Gender"),
                                       data = pisa_uzb_gender))
read_level2_gender <- read_gender %>%
  filter(Benchmark == "At or above 407.47") %>%
  select(ESCS_w_decile, Percentage, Gender) %>%
  mutate(Subject = "O'qish")

scie_gender <- data.frame(pisa.ben.pv(pvlabel = paste0("PV", 1:10, "SCIE"),
                                       cutoff = sciecut,
                                       by = c("ESCS_w_decile", "Gender"),
                                       data = pisa_uzb_gender))
scie_level2_gender <- scie_gender %>%
  filter(Benchmark == "At or above 409.54") %>%
  select(ESCS_w_decile, Percentage, Gender) %>%
  mutate(Subject = "Tabiiy fanlar")

level2_gender_all <- bind_rows(math_level2_gender, read_level2_gender, scie_level2_gender) %>%
  filter(!is.na(Gender)) %>%
  mutate(Decile_num = as.numeric(gsub("D", "", ESCS_w_decile)),
         Gender = as.character(Gender),
         Subject = factor(Subject, levels = c("Matematika", "O'qish", "Tabiiy fanlar")))

write_csv(level2_gender_all, file.path(output_dir, "pisa_level2_gender.csv"))
cat("  -> pisa_level2_gender.csv\n")

# --- 1d. Home conditions by decile ---
cat("Computing home conditions...\n")
vars <- c("ST250Q01JA", "ST250Q02JA", "ST250Q03JA", "ST250Q05JA")

descriptive_labels <- c(
  "ST250Q01JA" = "O'z xonasi bormi?",
  "ST250Q02JA" = "Kompyuteri bormi?",
  "ST250Q03JA" = "Ta'lim dasturlari?",
  "ST250Q05JA" = "Internet bormi?"
)

pisa_long <- pisa_uzb %>%
  select(ESCS_w_decile, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value")

percentages <- pisa_long %>%
  group_by(ESCS_w_decile, Variable) %>%
  summarise(
    Percent = mean(Value == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Variable_label = descriptive_labels[Variable],
    Decile_num = as.numeric(gsub("D", "", ESCS_w_decile))
  )

write_csv(percentages, file.path(output_dir, "pisa_home_conditions.csv"))
cat("  -> pisa_home_conditions.csv\n")

# Appendix version
descriptive_labels_appendix <- c(
  "ST250Q01JA" = "O'z xonasi bormi?",
  "ST250Q02JA" = "O'qish uchun kompyuteri bormi?",
  "ST250Q03JA" = "Ta'lim dasturiy ta'minotlaridan foydalanadimi?",
  "ST250Q05JA" = "Uyda internet bormi?"
)

percentages_appendix <- pisa_long %>%
  group_by(ESCS_w_decile, Variable) %>%
  summarise(
    Percent = mean(Value == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Variable_label = descriptive_labels_appendix[Variable],
    Decile_num = as.numeric(gsub("D", "", ESCS_w_decile))
  )

write_csv(percentages_appendix, file.path(output_dir, "pisa_home_conditions_appendix.csv"))
cat("  -> pisa_home_conditions_appendix.csv\n")

# --- 1e. Computer access by rural/urban ---
cat("Computing computer access by rural/urban...\n")
computer_rural <- pisa_uzb %>%
  group_by(ESCS_w_decile, rural) %>%
  summarise(
    Percent = mean(ST250Q02JA == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(Decile_num = as.numeric(gsub("D", "", ESCS_w_decile)))

write_csv(computer_rural, file.path(output_dir, "pisa_computer_rural.csv"))
cat("  -> pisa_computer_rural.csv\n")

# =============================================================================
# 2. MPI DATA (already generated, skip if exists)
# =============================================================================
if (!file.exists(file.path(output_dir, "mpi_contributions.csv"))) {
  cat("Processing MPI data...\n")
  # (MPI CSV already generated separately)
} else {
  cat("MPI CSV already exists, skipping.\n")
}

# =============================================================================
# 3. OAXACA-BLINDER DECOMPOSITION
# =============================================================================
cat("Running Oaxaca-Blinder decomposition (10 PVs)...\n")

breaks_20_80 <- wtd.quantile(pisa_uzb$ESCS, weights = pisa_uzb$W_FSTUWT,
                             probs = c(0.20, 0.80), na.rm = TRUE)

school_totals <- pisa_uzb %>%
  group_by(CNTSCHID) %>%
  summarise(
    School_Sum_Math = sum(PV1MATH * W_FSTUWT, na.rm = TRUE),
    School_Sum_W    = sum(W_FSTUWT, na.rm = TRUE),
    School_N        = n(),
    .groups = "drop"
  )

pv_vars <- paste0("PV", 1:10, "MATH")

pisa_uzb_oaxaca <- pisa_uzb %>%
  left_join(school_totals, by = "CNTSCHID") %>%
  filter(School_N >= 10) %>%
  mutate(
    School_Avg_Math = (School_Sum_Math - PV1MATH * W_FSTUWT) / (School_Sum_W - W_FSTUWT),
    rural_num = ifelse(rural == "Qishloq", 1, 0)
  ) %>%
  select(all_of(pv_vars), ESCS, W_FSTUWT, ST004D01T, rural_num, School_Avg_Math,
         HOMEPOS, ST059Q01TA, ST059Q02JA, SC176Q01JA, ST297Q01JA) %>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  mutate(ST297Q01JA = ifelse(!is.na(ST297Q01JA) & ST297Q01JA == 1, 1, 0)) %>%
  drop_na() %>%
  mutate(
    Rich_Group = case_when(
      ESCS >= breaks_20_80[2] ~ 1L,
      ESCS <= breaks_20_80[1] ~ 0L,
      TRUE ~ NA_integer_
    ),
    Female = ifelse(ST004D01T == 1, 1L, 0L)
  ) %>%
  filter(!is.na(Rich_Group)) %>%
  rename(
    Jins = Female,
    Qishloq = rural_num,
    Maktab_ortacha = School_Avg_Math,
    Mulk_indeksi = HOMEPOS,
    Matematika_dars_soni = ST059Q01TA,
    Barcha_dars_soati = ST059Q02JA,
    Sinf_oquvchi_soni = SC176Q01JA,
    Xususiy_repetitor = ST297Q01JA
  ) %>%
  as.data.frame()

run_oaxaca_pv <- function(pv_col, data) {
  formula_oaxaca <- as.formula(paste0(
    pv_col, " ~ Jins + Maktab_ortacha + Qishloq + Mulk_indeksi + ",
    "Matematika_dars_soni + Barcha_dars_soati + Sinf_oquvchi_soni + Xususiy_repetitor | Rich_Group"
  ))
  result <- oaxaca(formula_oaxaca, data = data, R = NULL)
  list(
    coef = result$threefold$variables[, "coef(endowments)"],
    y_diff = result$y$y.diff,
    var_names = rownames(result$threefold$variables)
  )
}

pv_results <- lapply(pv_vars, function(pv) run_oaxaca_pv(pv, pisa_uzb_oaxaca))

var_names <- pv_results[[1]]$var_names
coef_matrix <- sapply(pv_results, function(x) x$coef)
rownames(coef_matrix) <- var_names

coef_mean <- rowMeans(coef_matrix)
imputation_var <- apply(coef_matrix, 1, var)
total_var <- (1 + 1/10) * imputation_var
se <- sqrt(total_var)

gap_pv <- sapply(pv_results, function(x) x$y_diff)
gap_total <- mean(gap_pv)
gap_se <- sqrt((1 + 1/10) * var(gap_pv))

label_map <- c(
  "Jins" = "Jins",
  "Maktab_ortacha" = "Maktabdagi o'rtacha o'zlashtirish",
  "Qishloq" = "Qishloq",
  "Mulk_indeksi" = "Mulk indeksi",
  "Matematika_dars_soni" = "Bir haftada matematika darslari soni",
  "Barcha_dars_soati" = "Barcha fanlardan dars soatlari soni",
  "Sinf_oquvchi_soni" = "Matematika fanida sinfdagi o'quvchilar soni",
  "Xususiy_repetitor" = "Xususiy repetitorga borish"
)

oaxaca_df <- data.frame(
  variable = var_names,
  coef = coef_mean,
  se = se,
  ci_lower = coef_mean - 1.96 * se,
  ci_upper = coef_mean + 1.96 * se,
  gap_total = gap_total,
  gap_se = gap_se
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(label = label_map[variable],
         label = ifelse(is.na(label), variable, label))

write_csv(oaxaca_df, file.path(output_dir, "oaxaca_results.csv"))
cat("  -> oaxaca_results.csv\n")

# =============================================================================
# 4. MICS DATA
# =============================================================================
cat("Processing MICS data...\n")

data_path <- "G:/My Drive/parnet_style_paper/data/MICS_Datasets/Uzbekistan MICS6 Datasets/Uzbekistan MICS6 Datasets/Uzbekistan MICS6 SPSS Datasets"
mics_data <- read_sav(paste0(data_path, "/fs.sav"))

mics_data$PR3 <- ifelse(mics_data$PR3 == 99, NA, mics_data$PR3)
mics_data$windex10 <- ifelse(mics_data$windex10 == 0, NA, mics_data$windex10)
mics_data$PR6 <- ifelse(mics_data$PR6 == 8, NA, mics_data$PR6)
mics_data$PR8 <- ifelse(mics_data$PR8 == 8, NA, mics_data$PR8)
mics_data$PR11B <- ifelse(mics_data$PR11B == 8, NA, mics_data$PR11B)
mics_data$HL4 <- ifelse(!(mics_data$HL4 %in% c(1, 2)), NA, mics_data$HL4)

mics_data$HH6 <- factor(
  ifelse(as.numeric(mics_data$HH6) %in% c(1, 2), as.numeric(mics_data$HH6), NA),
  levels = c(1, 2),
  labels = c("Shahar", "Qishloq")
)

# --- 4a. Overall by decile ---

# Books
books_by_decile <- mics_data %>%
  transmute(windex10 = as.integer(windex10), PR3 = as.numeric(PR3), fsweight = as.numeric(fsweight)) %>%
  filter(!is.na(windex10), !is.na(PR3), !is.na(fsweight), fsweight > 0) %>%
  group_by(windex10) %>%
  summarise(avg_books = weighted.mean(PR3, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(windex10)
write_csv(books_by_decile, file.path(output_dir, "mics_books_by_decile.csv"))

# Help
help_by_decile <- mics_data %>%
  transmute(windex10 = as.integer(windex10), PR6 = as.integer(PR6), fsweight = as.numeric(fsweight)) %>%
  filter(!is.na(windex10), !is.na(PR6), !is.na(fsweight), fsweight > 0) %>%
  mutate(help_yes = PR6 == 1) %>%
  group_by(windex10) %>%
  summarise(help_rate_pct = 100 * weighted.mean(help_yes, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(windex10)
write_csv(help_by_decile, file.path(output_dir, "mics_help_by_decile.csv"))

# Meeting
meeting_by_decile <- mics_data %>%
  transmute(windex10 = as.integer(windex10), PR8 = as.integer(PR8), fsweight = as.numeric(fsweight)) %>%
  filter(!is.na(windex10), !is.na(PR8), !is.na(fsweight), fsweight > 0) %>%
  mutate(attended = PR8 == 1) %>%
  group_by(windex10) %>%
  summarise(attend_rate_pct = 100 * weighted.mean(attended, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(windex10)
write_csv(meeting_by_decile, file.path(output_dir, "mics_meeting_by_decile.csv"))

# Progress
progress_disc_by_decile <- mics_data %>%
  transmute(windex10 = as.integer(windex10), PR11B = as.integer(PR11B), fsweight = as.numeric(fsweight)) %>%
  filter(!is.na(windex10), !is.na(PR11B), !is.na(fsweight), fsweight > 0) %>%
  mutate(discussed_progress = PR11B == 1) %>%
  group_by(windex10) %>%
  summarise(progress_disc_rate_pct = 100 * weighted.mean(discussed_progress, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(windex10)
write_csv(progress_disc_by_decile, file.path(output_dir, "mics_progress_by_decile.csv"))

cat("  -> mics_*_by_decile.csv (4 files)\n")

# --- 4b. By gender ---

mics_rate_by_decile_gender <- function(df, var_name, yes_value = 1L) {
  df %>%
    transmute(
      windex10 = as.integer(windex10),
      gender = dplyr::case_when(
        as.integer(HL4) == 1L ~ "Erkak",
        as.integer(HL4) == 2L ~ "Ayol",
        TRUE ~ NA_character_
      ),
      x = as.integer(.data[[var_name]]),
      fsweight = as.numeric(fsweight)
    ) %>%
    filter(!is.na(windex10), !is.na(gender), !is.na(x), !is.na(fsweight), fsweight > 0) %>%
    mutate(yes = x == as.integer(yes_value)) %>%
    group_by(windex10, gender) %>%
    summarise(rate_pct = 100 * weighted.mean(yes, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
    arrange(windex10, gender)
}

# Books by gender
books_by_decile_gender <- mics_data %>%
  transmute(
    windex10 = as.integer(windex10),
    gender = dplyr::case_when(
      as.integer(HL4) == 1L ~ "Erkak",
      as.integer(HL4) == 2L ~ "Ayol",
      TRUE ~ NA_character_
    ),
    PR3 = as.numeric(PR3),
    fsweight = as.numeric(fsweight)
  ) %>%
  filter(!is.na(windex10), !is.na(gender), !is.na(PR3), !is.na(fsweight), fsweight > 0) %>%
  group_by(windex10, gender) %>%
  summarise(avg_books = weighted.mean(PR3, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(windex10, gender)
write_csv(books_by_decile_gender, file.path(output_dir, "mics_books_by_gender.csv"))

help_by_decile_gender <- mics_rate_by_decile_gender(mics_data, "PR6", yes_value = 1L)
write_csv(help_by_decile_gender, file.path(output_dir, "mics_help_by_gender.csv"))

meeting_by_decile_gender <- mics_rate_by_decile_gender(mics_data, "PR8", yes_value = 1L)
write_csv(meeting_by_decile_gender, file.path(output_dir, "mics_meeting_by_gender.csv"))

progress_disc_by_decile_gender <- mics_rate_by_decile_gender(mics_data, "PR11B", yes_value = 1L)
write_csv(progress_disc_by_decile_gender, file.path(output_dir, "mics_progress_by_gender.csv"))

cat("  -> mics_*_by_gender.csv (4 files)\n")

# --- 4c. By urban/rural ---

mics_rate_by_decile_ur <- function(df, var_name, yes_value = 1L) {
  df %>%
    mutate(
      windex10 = as.integer(windex10),
      hh6_num = suppressWarnings(as.numeric(HH6)),
      area = factor(
        dplyr::case_when(
          hh6_num == 1 ~ "Shahar",
          hh6_num == 2 ~ "Qishloq",
          TRUE ~ NA_character_
        ),
        levels = c("Shahar", "Qishloq")
      ),
      x = as.integer(.data[[var_name]]),
      fsweight = as.numeric(fsweight)
    ) %>%
    select(windex10, area, x, fsweight) %>%
    filter(!is.na(windex10), !is.na(area), !is.na(x), !is.na(fsweight), fsweight > 0) %>%
    mutate(yes = x == as.integer(yes_value)) %>%
    group_by(windex10, area) %>%
    summarise(rate_pct = 100 * weighted.mean(yes, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
    arrange(windex10, area)
}

# Books by urban/rural
books_by_decile_ur <- mics_data %>%
  mutate(
    windex10 = as.integer(windex10),
    hh6_num = suppressWarnings(as.numeric(HH6)),
    area = factor(
      dplyr::case_when(
        hh6_num == 1 ~ "Shahar",
        hh6_num == 2 ~ "Qishloq",
        TRUE ~ NA_character_
      ),
      levels = c("Shahar", "Qishloq")
    ),
    PR3 = as.numeric(PR3),
    fsweight = as.numeric(fsweight)
  ) %>%
  select(windex10, area, PR3, fsweight) %>%
  filter(!is.na(windex10), !is.na(area), !is.na(PR3), !is.na(fsweight), fsweight > 0) %>%
  group_by(windex10, area) %>%
  summarise(avg_books = weighted.mean(PR3, w = fsweight, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(windex10, area)
write_csv(books_by_decile_ur, file.path(output_dir, "mics_books_by_ur.csv"))

help_by_decile_ur <- mics_rate_by_decile_ur(mics_data, "PR6", yes_value = 1L)
write_csv(help_by_decile_ur, file.path(output_dir, "mics_help_by_ur.csv"))

meeting_by_decile_ur <- mics_rate_by_decile_ur(mics_data, "PR8", yes_value = 1L)
write_csv(meeting_by_decile_ur, file.path(output_dir, "mics_meeting_by_ur.csv"))

progress_disc_by_decile_ur <- mics_rate_by_decile_ur(mics_data, "PR11B", yes_value = 1L)
write_csv(progress_disc_by_decile_ur, file.path(output_dir, "mics_progress_by_ur.csv"))

cat("  -> mics_*_by_ur.csv (4 files)\n")

# =============================================================================
cat("\nDone! All CSV files saved to", output_dir, "\n")
