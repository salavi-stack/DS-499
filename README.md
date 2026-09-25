
# DS-499 — Gender Gaps in Internet Access and Economic Outcomes
# BUILDING THE DATASET
# This script downloads the raw data from the World Bank and the
# UN.

 
library(WDI)        
library(tidyverse)
 
# Creating a folder to save the finished files into
if (!dir.exists("research")) dir.create("research")

# DOWNLOADING THE RAW DATA FROM THE WORLD BANK
# Each line below is one indicator. The name on the LEFT is what I
# want the column called; the code on the RIGHT is the World Bank's
# official indicator code.
 
indicators <- c(
  internet_female      = "IT.NET.USER.FE.ZS",  # % of women using internet (ITU)
  internet_male        = "IT.NET.USER.MA.ZS",  # % of men using internet (ITU)
  internet_total       = "IT.NET.USER.ZS",     # % of everyone (control)
  gdp_pc_ppp           = "NY.GDP.PCAP.PP.KD",  # Y1: GDP per capita, PPP
  gdp_pc_const         = "NY.GDP.PCAP.KD",     # Y1 alternative
  flfp                 = "SL.TLF.CACT.FE.ZS",  # Y2: female labor force participation
  sec_enroll_fem_gross = "SE.SEC.ENRR.FE",     # Y3: female secondary enrollment (gross)
  sec_enroll_fem_net   = "SE.SEC.NENR.FE",     # Y3 alternative (net)
  urban_pct            = "SP.URB.TOTL.IN.ZS",  # control: urbanization
  population           = "SP.POP.TOTL"         # context
)
 
# extra = TRUE also brings back
# each country's region and income group.
wb_raw <- WDI(country   = "all",
              indicator = indicators,
              start     = 2015,
              end       = 2024,
              extra     = TRUE)
 
cat("Downloaded", nrow(wb_raw), "rows\n")   # expect 2650
 
 
# NARROWING TO DEVELOPING COUNTRIES
# The World Bank labels every country by income group. "Developing"
# here = low, lower-middle, and upper-middle income.
 
panel <- wb_raw %>%
  filter(region != "Aggregates",
         income %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
 

# CALCULATING THE GENDER GAP (my independent variable)
  # gap_diff  = how many percentage points ahead men are (my main measure)
  # gap_ratio = women's rate divided by men's; 1.0 means parity (backup measure)
  mutate(gap_diff  = internet_male - internet_female,
         gap_ratio = internet_female / internet_male) %>%
 
  # the columns I care about
  select(iso3 = iso3c, country, region, income, year,
         internet_female, internet_male, gap_diff, gap_ratio,
         internet_total, gdp_pc_ppp, gdp_pc_const, flfp,
         sec_enroll_fem_gross, sec_enroll_fem_net,
         urban_pct, population) %>%
  arrange(country, year)
 
cat("Developing countries:", n_distinct(panel$iso3), "\n")
 
 
# ---- Adding the Gender Inequality Index from the UN (a control) ----
# This is the only variable that does not come from the World Bank.
 
hdr_url <- "https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv"
hdr <- read_csv(hdr_url, locale = locale(encoding = "latin1"), show_col_types = FALSE)
 
gii <- hdr %>%
  select(iso3, matches("^gii_20(1[5-9]|2[0-3])$")) %>%   # the columns gii_2015 ... gii_2023
  pivot_longer(-iso3, names_to = "var", values_to = "gii") %>%
  mutate(year = as.numeric(str_extract(var, "\\d{4}"))) %>%
  select(iso3, year, gii) %>%
  filter(!is.na(gii))
 
panel <- left_join(panel, gii, by = c("iso3", "year"))
 
# SAVING THE PANEL FILE (every country, every year)
write_csv(panel, "research/gender_gap_panel_2015_2024.csv")
cat("Panel file saved:", nrow(panel), "rows\n")
 

# BUILDING THE CROSS-SECTION (one row per country)
# THE PROBLEM: countries run their internet surveys in different
# years, so no single year has enough countries (the best year has
# only 44). THE SOLUTION: using each country's most recent year
# between 2019 and 2024 that has male AND female internet data.
 
base <- panel %>%
  filter(!is.na(internet_female), !is.na(internet_male), year >= 2019) %>%
  group_by(iso3) %>%
  slice_max(year, n = 1) %>%          
  ungroup() %>%
  rename(ref_year = year)             # "ref_year" = reference year for this country
 
cat("Cross-section countries:", nrow(base), "\n")
 
 
# ---- Filling in outcomes/controls from the nearest available year ----
# If a country's outcome data is missing in its reference year, taking
# the closest year within 3 years either side, and recording which year
# it came from so this is transparent.
 
fill_vars <- c("gdp_pc_ppp", "gdp_pc_const", "flfp",
               "sec_enroll_fem_gross", "urban_pct", "gii", "internet_total")
 
nearest_value <- function(varname) {
  panel %>%
    select(iso3, year, value = all_of(varname)) %>%
    filter(!is.na(value)) %>%
    inner_join(select(base, iso3, ref_year), by = "iso3") %>%
    filter(abs(year - ref_year) <= 3) %>%
    arrange(iso3, abs(year - ref_year), desc(year)) %>%  # closest year wins; ties -> newer
    group_by(iso3) %>%
    slice(1) %>%
    ungroup() %>%
    select(iso3, value, year) %>%
    setNames(c("iso3", varname, paste0(varname, "_year")))
}
 
cross <- base %>% select(-all_of(fill_vars))          # droping the originals
for (v in fill_vars) {                                 # adding back the filled versions
  cross <- left_join(cross, nearest_value(v), by = "iso3")
}
 
cross <- cross %>% arrange(country)
 
write_csv(cross, "research/gender_gap_crosssection_latest.csv")
cat("Cross-section file saved:", nrow(cross), "rows\n")
 
 

cat("\n--- CHECKS ---\n")
cat("Panel dimensions (expect about 1310 x 18):", dim(panel), "\n")
cat("Cross-section dimensions (expect about 59 x 25):", dim(cross), "\n")
cat("Countries where men are ahead:", sum(cross$gap_diff > 0, na.rm = TRUE), "\n")
cat("Countries where women are ahead:", sum(cross$gap_diff < 0, na.rm = TRUE), "\n\n")
 
print(summary(cross$gap_diff))
print(table(cross$income))
print(table(cross$region))

