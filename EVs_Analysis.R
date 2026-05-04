# =============================================================================
#  EV ADOPTION, GRID CLEANLINESS & CO₂ EMISSIONS — FULL ANALYSIS
#  Stages 1–4: Discovery → Preparation → Visualisations → Regressions
# =============================================================================

# ── 0. DEPENDENCIES ──────────────────────────────────────────────────────────
required <- c("tidyverse", "ggcorrplot", "scales", "broom", "gt", "patchwork",
              "RColorBrewer", "viridis", "ggrepel")

for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggcorrplot)
  library(scales)
  library(broom)
  library(gt)
  library(patchwork)
  library(viridis)
  library(ggrepel)
})

dir.create("plots", showWarnings = FALSE)

# ── DARK THEME (shared by all plots) ─────────────────────────────────────────
theme_dark_custom <- function(base_size = 13) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background    = element_rect(fill = "#0d1117", colour = NA),
      panel.background   = element_rect(fill = "#161b22", colour = NA),
      panel.grid.major   = element_line(colour = "#30363d"),
      panel.grid.minor   = element_line(colour = "#21262d"),
      axis.text          = element_text(colour = "#8b949e"),
      axis.title         = element_text(colour = "#c9d1d9"),
      plot.title         = element_text(colour = "#e6edf3", face = "bold",
                                        size = rel(1.25), hjust = 0),
      plot.subtitle      = element_text(colour = "#8b949e", hjust = 0,
                                        margin = margin(b = 10)),
      plot.caption       = element_text(colour = "#484f58", size = rel(0.75)),
      legend.background  = element_rect(fill = "#161b22", colour = NA),
      legend.text        = element_text(colour = "#8b949e"),
      legend.title       = element_text(colour = "#c9d1d9"),
      strip.text         = element_text(colour = "#c9d1d9"),
      plot.margin        = margin(16, 16, 16, 16)
    )
}

ACCENT  <- "#58a6ff"
PALETTE <- c("#58a6ff","#3fb950","#f78166","#d2a8ff","#ffa657",
             "#79c0ff","#56d364","#ff7b72","#bc8cff","#e3b341")


# =============================================================================
#  STAGE 1 — DATA DISCOVERY
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("  STAGE 1 — DATA DISCOVERY\n")
cat(strrep("=", 70), "\n\n")

ev_raw  <- read_csv("data/ev_data.csv",          show_col_types = FALSE)
co2_raw <- read_csv("data/owid-co2-data.csv",    show_col_types = FALSE)
en_raw  <- read_csv("data/owid-energy-data.csv", show_col_types = FALSE)

summarise_file <- function(df, name, key_cols) {
  cat(sprintf("▸ %s\n", name))
  cat(sprintf("  Dimensions  : %d rows × %d cols\n", nrow(df), ncol(df)))
  cat(sprintf("  Year range  : %d – %d\n",
              min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE)))
  cat(sprintf("  Countries   : %d unique\n", n_distinct(df$country)))
  cat("  Missing (key cols):\n")
  for (col in key_cols) {
    n_miss <- sum(is.na(df[[col]]))
    cat(sprintf("    %-30s %d missing (%.1f%%)\n",
                col, n_miss, 100 * n_miss / nrow(df)))
  }
  cat("\n")
}

summarise_file(ev_raw,  "ev_data.csv",
               c("country","year","ev_sales"))
summarise_file(co2_raw, "owid-co2-data.csv",
               c("country","year","co2_per_capita","population","co2"))
summarise_file(en_raw,  "owid-energy-data.csv",
               c("country","year","renewables_share_elec",
                 "low_carbon_share_elec","carbon_intensity_elec"))


# =============================================================================
#  STAGE 2 — DATA PREPARATION
# =============================================================================
cat(strrep("=", 70), "\n")
cat("  STAGE 2 — DATA PREPARATION\n")
cat(strrep("=", 70), "\n\n")

# ── 2a. Country name harmonisation (EV file → OWID names) ────────────────────
recode_map <- c(
  "Korea"    = "South Korea",
  "Turkiye"  = "Turkey",
  "USA"      = "United States",
  "Viet Nam" = "Vietnam",
  "Czech Republic" = "Czechia"
)

ev_clean <- ev_raw %>%
  mutate(country = recode(country, !!!recode_map))

cat("Country name fixes applied:\n")
for (old in names(recode_map)) {
  cat(sprintf("  %-20s → %s\n", old, recode_map[old]))
}
cat("\n")

# ── 2b. Filter all files to 2015–2023 ────────────────────────────────────────
ev_filt <- ev_clean %>% filter(year >= 2015, year <= 2023)

co2_filt <- co2_raw %>%
  filter(year >= 2015, year <= 2023) %>%
  select(country, year, co2_per_capita, co2, population)

en_filt <- en_raw %>%
  filter(year >= 2015, year <= 2023) %>%
  select(country, year,
         renewables_share_elec,
         low_carbon_share_elec,
         carbon_intensity_elec)

# ── 2c. Merge on country + year ───────────────────────────────────────────────
panel <- ev_filt %>%
  inner_join(co2_filt, by = c("country", "year")) %>%
  inner_join(en_filt,  by = c("country", "year"))

cat(sprintf("Panel after merge: %d rows × %d cols\n\n", nrow(panel), ncol(panel)))

# ── 2d. Feature engineering ───────────────────────────────────────────────────
panel <- panel %>%
  mutate(
    ev_per_capita   = ev_sales / (population / 1e6),   # EVs per million people
    log_ev_sales    = log1p(ev_sales),
    log_ev_per_cap  = log1p(ev_per_capita),
    pct_renewable   = renewables_share_elec,
    log_co2_per_cap = log(co2_per_capita)
  )

# ── 2e. Drop rows missing on key variables ────────────────────────────────────
key_vars <- c("ev_per_capita","co2_per_capita","pct_renewable",
              "carbon_intensity_elec","population")
panel <- panel %>% drop_na(all_of(key_vars))

cat(sprintf("Final panel: %d rows × %d cols\n", nrow(panel), ncol(panel)))
cat(sprintf("Countries  : %d\n", n_distinct(panel$country)))
cat(sprintf("Years      : %d – %d\n\n", min(panel$year), max(panel$year)))

# Quick null-check on key vars
cat("Null counts after cleaning:\n")
panel %>%
  select(all_of(key_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print()
cat("\n")


# =============================================================================
#  STAGE 3 — VISUALISATIONS
# =============================================================================
cat(strrep("=", 70), "\n")
cat("  STAGE 3 — VISUALISATIONS\n")
cat(strrep("=", 70), "\n\n")

# Helper: save with dark background
save_plot <- function(p, filename, width = 12, height = 7) {
  path <- file.path("plots", filename)
  ggsave(path, plot = p, width = width, height = height,
         bg = "#0d1117", dpi = 150)
  cat(sprintf("  ✔ Saved  %s\n", path))
}

# ── Plot 01 — Global EV sales growth by country (line chart) ─────────────────
top_countries <- panel %>%
  filter(year == 2023) %>%
  arrange(desc(ev_sales)) %>%
  slice_head(n = 10) %>%
  pull(country)

p01 <- panel %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(year, ev_sales / 1e3, colour = country)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2.2, alpha = 0.85) +
  scale_colour_manual(values = PALETTE) +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(labels = comma_format(suffix = "k")) +
  labs(
    title    = "Global EV Sales Growth — Top 10 Countries",
    subtitle = "Thousands of EVs sold per year, 2015–2023",
    x = NULL, y = "EV Sales (thousands)",
    colour = NULL,
    caption = "Source: IEA EV Data"
  ) +
  theme_dark_custom()

save_plot(p01, "01_ev_sales_growth.png")

# ── Plot 02 — Top 15 countries EV sales 2023 (horizontal bar) ─────────────────
top15_2023 <- panel %>%
  filter(year == 2023) %>%
  arrange(desc(ev_sales)) %>%
  slice_head(n = 15)

p02 <- top15_2023 %>%
  mutate(country = fct_reorder(country, ev_sales)) %>%
  ggplot(aes(ev_sales / 1e3, country, fill = ev_sales)) +
  geom_col(alpha = 0.9) +
  scale_fill_viridis_c(option = "plasma", guide = "none") +
  scale_x_continuous(labels = comma_format(suffix = "k"), expand = expansion(mult = c(0, .05))) +
  labs(
    title    = "Top 15 Countries by EV Sales — 2023",
    subtitle = "Thousands of units sold",
    x = "EV Sales (thousands)", y = NULL,
    caption = "Source: IEA EV Data"
  ) +
  theme_dark_custom()

save_plot(p02, "02_top15_ev_2023.png", height = 6)

# ── Plot 03 — Renewable % trends for top 10 EV countries (line) ───────────────
p03 <- panel %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(year, pct_renewable, colour = country)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.8) +
  scale_colour_manual(values = PALETTE) +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     limits = c(0, NA)) +
  labs(
    title    = "Renewable Electricity Share — Top 10 EV Markets",
    subtitle = "% of electricity from renewables, 2015–2023",
    x = NULL, y = "Renewables Share (%)",
    colour = NULL,
    caption = "Source: OWID Energy Data"
  ) +
  theme_dark_custom()

save_plot(p03, "03_renewables_trends.png")

# ── Plot 04 — EV per capita vs CO₂ per capita (scatter) ──────────────────────
label_countries <- panel %>%
  filter(year == 2022) %>%
  arrange(desc(ev_per_capita)) %>%
  slice_head(n = 12) %>%
  pull(country)

p04 <- panel %>%
  filter(year == 2022) %>%
  ggplot(aes(ev_per_capita, co2_per_capita)) +
  geom_point(aes(colour = pct_renewable, size = population / 1e6),
             alpha = 0.8) +
  geom_smooth(method = "lm", colour = "#f78166", fill = "#f78166",
              alpha = 0.15, linewidth = 0.9, se = TRUE) +
  geom_text_repel(
    data = ~filter(., country %in% label_countries),
    aes(label = country), colour = "#c9d1d9", size = 3,
    box.padding = 0.4, max.overlaps = 15
  ) +
  scale_colour_viridis_c(option = "viridis", name = "Renewables %") +
  scale_size_continuous(name = "Pop. (M)", range = c(2, 10),
                        guide = guide_legend(override.aes = list(alpha = 0.7))) +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "EV Adoption vs CO₂ per Capita — 2022",
    subtitle = "Colour = renewable electricity share; size = population",
    x = "EVs per Million People", y = "CO₂ per Capita (tonnes)",
    caption = "Source: IEA EV Data & OWID CO₂ Data"
  ) +
  theme_dark_custom()

save_plot(p04, "04_ev_vs_co2.png")

# ── Plot 05 — Grid carbon intensity vs CO₂ per capita (scatter) ───────────────
p05 <- panel %>%
  filter(year == 2022) %>%
  ggplot(aes(carbon_intensity_elec, co2_per_capita)) +
  geom_point(aes(colour = ev_per_capita, size = population / 1e6),
             alpha = 0.8) +
  geom_smooth(method = "lm", colour = ACCENT, fill = ACCENT,
              alpha = 0.15, linewidth = 0.9, se = TRUE) +
  geom_text_repel(
    data = ~filter(., country %in% label_countries),
    aes(label = country), colour = "#c9d1d9", size = 3,
    box.padding = 0.4, max.overlaps = 15
  ) +
  scale_colour_viridis_c(option = "plasma", name = "EV/Million") +
  scale_size_continuous(name = "Pop. (M)", range = c(2, 10),
                        guide = guide_legend(override.aes = list(alpha = 0.7))) +
  labs(
    title    = "Grid Carbon Intensity vs CO₂ per Capita — 2022",
    subtitle = "Colour = EV adoption rate; size = population",
    x = "Carbon Intensity of Electricity (gCO₂/kWh)",
    y = "CO₂ per Capita (tonnes)",
    caption = "Source: OWID Energy & CO₂ Data"
  ) +
  theme_dark_custom()

save_plot(p05, "05_carbon_intensity_vs_co2.png")

# ── Plot 06 — Carbon intensity heatmap by country × year ──────────────────────
heatmap_countries <- panel %>%
  group_by(country) %>%
  summarise(avg_ev = mean(ev_per_capita, na.rm = TRUE)) %>%
  arrange(desc(avg_ev)) %>%
  slice_head(n = 25) %>%
  pull(country)

p06 <- panel %>%
  filter(country %in% heatmap_countries) %>%
  mutate(country = factor(country,
                          levels = rev(heatmap_countries))) %>%
  ggplot(aes(year, country, fill = carbon_intensity_elec)) +
  geom_tile(colour = "#0d1117", linewidth = 0.4) +
  scale_fill_viridis_c(option = "inferno", name = "gCO₂/kWh",
                       na.value = "#21262d") +
  scale_x_continuous(breaks = 2015:2023, expand = c(0, 0)) +
  labs(
    title    = "Grid Carbon Intensity — Top 25 EV Markets",
    subtitle = "gCO₂/kWh by country and year (darker = cleaner grid)",
    x = NULL, y = NULL,
    caption = "Source: OWID Energy Data"
  ) +
  theme_dark_custom() +
  theme(axis.text.y = element_text(size = 9),
        panel.grid  = element_blank())

save_plot(p06, "06_carbon_intensity_heatmap.png", height = 8)

# ── Plot 07 — Correlation matrix ──────────────────────────────────────────────
cor_vars <- panel %>%
  select(
    `EV/M` = ev_per_capita,
    `Log EV` = log_ev_sales,
    `CO₂/cap` = co2_per_capita,
    `Renewables %` = pct_renewable,
    `C-Intensity` = carbon_intensity_elec,
    `Low-C Share` = low_carbon_share_elec
  ) %>%
  drop_na()

corr_mat <- cor(cor_vars, use = "complete.obs")

p07 <- ggcorrplot(corr_mat,
                  method   = "circle",
                  type     = "lower",
                  lab      = TRUE,
                  lab_size = 3.5,
                  colors   = c("#f78166", "#161b22", "#58a6ff"),
                  outline.color = "#30363d",
                  ggtheme  = theme_dark_custom()) +
  labs(
    title   = "Correlation Matrix — Key Variables",
    caption = "Source: Merged panel 2015–2023"
  ) +
  theme(legend.position = "right")

save_plot(p07, "07_correlation_matrix.png", height = 7)

cat("\nAll 7 plots saved to plots/\n\n")


# =============================================================================
#  STAGE 4 — REGRESSION MODELS
# =============================================================================
cat(strrep("=", 70), "\n")
cat("  STAGE 4 — REGRESSION MODELS\n")
cat(strrep("=", 70), "\n\n")

print_model <- function(model, label) {
  cat(sprintf("\n%s\n%s\n", label, strrep("─", nchar(label))))
  s <- summary(model)
  print(s$coefficients)
  cat(sprintf("\n  R²: %.4f   Adj. R²: %.4f   F-stat: %.2f   p: %.4f\n\n",
              s$r.squared, s$adj.r.squared,
              s$fstatistic[1],
              pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)))
}

# Scale predictors for interpretable coefficients
panel_s <- panel %>%
  mutate(
    ev_per_cap_s  = scale(ev_per_capita)[,1],
    pct_renew_s   = scale(pct_renewable)[,1],
    ci_elec_s     = scale(carbon_intensity_elec)[,1],
    log_ev_pc_s   = scale(log_ev_per_cap)[,1],
    gdp_pc_s      = scale(co2 / (population / 1e6), center = TRUE, scale = TRUE)[,1]
  )

# ── Model A — EV adoption → CO₂ per capita ────────────────────────────────────
mod_A <- lm(co2_per_capita ~ log_ev_pc_s + factor(year), data = panel_s)
print_model(mod_A, "Model A: EV Adoption → CO₂ per Capita (year FE)")

# ── Model B — Renewable % → CO₂ per capita ────────────────────────────────────
mod_B <- lm(co2_per_capita ~ pct_renew_s + factor(year), data = panel_s)
print_model(mod_B, "Model B: Renewable % → CO₂ per Capita (year FE)")

# ── Model C — Carbon intensity → CO₂ per capita ───────────────────────────────
mod_C <- lm(co2_per_capita ~ ci_elec_s + factor(year), data = panel_s)
print_model(mod_C, "Model C: Grid Carbon Intensity → CO₂ per Capita (year FE)")

# ── Model D — Multiple regression: what predicts CO₂? ────────────────────────
mod_D <- lm(co2_per_capita ~ log_ev_pc_s + pct_renew_s + ci_elec_s +
              factor(year), data = panel_s)
print_model(mod_D, "Model D: Multiple Regression — Predictors of CO₂ per Capita")

# ── Model E — EV × Grid cleanliness interaction (key test) ────────────────────
# Hypothesis: EVs lower CO₂ MORE when grid is cleaner (negative interaction)
mod_E <- lm(co2_per_capita ~ log_ev_pc_s * pct_renew_s + ci_elec_s +
              factor(year), data = panel_s)
print_model(mod_E, "Model E ★ EV × Grid Cleanliness Interaction (KEY MODEL)")

# ── Model comparison table ────────────────────────────────────────────────────
models <- list(
  "A: EV Only"      = mod_A,
  "B: Renewables"   = mod_B,
  "C: C-Intensity"  = mod_C,
  "D: Multiple"     = mod_D,
  "E: Interaction ★"= mod_E
)

cat("\n── Model Comparison ──────────────────────────────────────────────────\n")
cat(sprintf("%-22s  %8s  %10s  %10s\n",
            "Model", "R²", "Adj. R²", "AIC"))
cat(strrep("─", 58), "\n")
for (nm in names(models)) {
  m  <- models[[nm]]
  s  <- summary(m)
  cat(sprintf("%-22s  %8.4f  %10.4f  %10.1f\n",
              nm, s$r.squared, s$adj.r.squared, AIC(m)))
}
cat(strrep("─", 58), "\n")

# ── Interaction plot: marginal effect of EV adoption across grid cleanliness ──
cat("\n── Interaction Effect — Marginal EV coef at low/med/high renewables ──\n")
q_low  <- quantile(panel_s$pct_renew_s, 0.25)
q_med  <- quantile(panel_s$pct_renew_s, 0.50)
q_high <- quantile(panel_s$pct_renew_s, 0.75)

b_ev   <- coef(mod_E)["log_ev_pc_s"]
b_int  <- coef(mod_E)["log_ev_pc_s:pct_renew_s"]

for (lbl in c("Low renewables (Q25)", "Median renewables", "High renewables (Q75)")) {
  q <- switch(lbl,
    "Low renewables (Q25)"  = q_low,
    "Median renewables"     = q_med,
    "High renewables (Q75)" = q_high)
  marginal <- b_ev + b_int * q
  cat(sprintf("  %-25s : marginal EV effect = %+.4f tonnes CO₂/cap\n",
              lbl, marginal))
}

cat("\n", strrep("=", 70), "\n")
cat("  ANALYSIS COMPLETE — all outputs in ./plots/\n")
cat(strrep("=", 70), "\n")
