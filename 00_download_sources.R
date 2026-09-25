# DS-499 — Gender Gaps in Internet Access and Economic Outcomes
#
# This downloads the published files from the three organisations
# and turns each one into a small CSV I can actually work with.
# It creates the data_raw folder.
#

library(tidyverse)
library(WDI)

if (!dir.exists("data_raw")) dir.create("data_raw")



# WHICH COUNTRIES COUNT AS "DEVELOPING"?
#
# I use the World Bank's income classification and keep low,
# lower-middle and upper-middle income. I also drop "Aggregates",
# which are rows like "World" or "Sub-Saharan Africa" - those are
# groups of countries, not countries.

country_info <- WDI(country = "all", indicator = "SP.POP.TOTL",
                    start = 2023, end = 2023, extra = TRUE) %>%
  filter(region != "Aggregates",
         income %in% c("Low income",
                       "Lower middle income",
                       "Upper middle income")) %>%
  select(country_code = iso3c, region, income)

cat("Developing countries:", nrow(country_info), "\n")


# SOURCE 1: INTERNET USE BY GENDER (Global Findex 2025)

# The published file is about 17 MB: 8,577 rows and 438 columns,
# covering six survey years, every country, and every breakdown
# (gender, income, age, education, urban/rural) for roughly 300
# indicators.
#
# I need one indicator ("internet"), one year (2024), and one
# breakdown (gender). Three things to notice:
#   1. Men and women are separate ROWS, so I pivot them into columns.
#   2. Values are stored as proportions (0.83), so I multiply by 100.
#   3. I keep developing countries only.

findex_url <- paste0("https://thedocs.worldbank.org/en/doc/",
                     "be6615202d1f08a25855c8ac2d615122-0050012025/related/",
                     "GlobalFindexDatabase2025.csv")

findex_raw <- read_csv(findex_url, show_col_types = FALSE)

cat("Findex file downloaded:", nrow(findex_raw), "rows,",
    ncol(findex_raw), "columns\n")

source1 <- findex_raw %>%
  filter(year == 2024,
         group == "gender",
         !is.na(internet)) %>%
  select(country_code = codewb,
         country_name = countrynewwb,
         group2, internet) %>%
  pivot_wider(names_from = group2, values_from = internet) %>%   # rows -> columns
  filter(!is.na(men), !is.na(women)) %>%
  transmute(country_code,
            country_name,
            survey_year    = 2024,
            internet_women = round(women * 100, 2),               # proportion -> percent
            internet_men   = round(men   * 100, 2)) %>%
  semi_join(country_info, by = "country_code") %>%                # developing only
  arrange(country_name)

write_csv(source1, "data_raw/source1_internet_gender_findex.csv")
cat("Source 1 saved:", nrow(source1), "countries\n")



# SOURCE 2: OUTCOMES AND CONTROLS (World Bank WDI)
# This is five separate indicator series, not one file. The WDI
# package downloads them together.
#
# For each country and each indicator I keep the most recent value
# between 2021 and 2024, and I record which year it came from.

indicators <- c(
  gdp_pc_ppp           = "NY.GDP.PCAP.PP.KD",  # GDP per capita, PPP
  flfp                 = "SL.TLF.CACT.FE.ZS",  # female labour force participation
  sec_enroll_fem_gross = "SE.SEC.ENRR.FE",     # female secondary enrollment
  urban_pct            = "SP.URB.TOTL.IN.ZS",  # urban population
  internet_total       = "IT.NET.USER.ZS"      # internet use, everyone
)

wb_raw <- WDI(country = "all", indicator = indicators,
              start = 2021, end = 2024)

cat("World Bank data downloaded:", nrow(wb_raw), "rows\n")

# Turn the five columns into rows so I can pick the latest year for each
wb_long <- wb_raw %>%
  select(country_code = iso3c, country_name = country, year,
         all_of(names(indicators))) %>%
  pivot_longer(all_of(names(indicators)),
               names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(country_code, variable) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()

# The values, back in columns
wb_values <- wb_long %>%
  select(country_code, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

# Which year each value came from
wb_years <- wb_long %>%
  select(country_code, variable, year) %>%
  mutate(variable = paste0(variable, "_year")) %>%
  pivot_wider(names_from = variable, values_from = year)

# One name per country
wb_names <- wb_long %>%
  select(country_code, country_name) %>%
  distinct(country_code, .keep_all = TRUE)

source2 <- wb_names %>%
  left_join(wb_values, by = "country_code") %>%
  left_join(wb_years,  by = "country_code") %>%
  semi_join(country_info, by = "country_code") %>%
  arrange(country_name)

write_csv(source2, "data_raw/source2_outcomes_worldbank.csv")
cat("Source 2 saved:", nrow(source2), "countries\n")



# SOURCE 3: GENDER INEQUALITY INDEX (UNDP)

# The published file is about 2 MB with hundreds of columns going
# back to 1990. The GII columns are named gii_1990, gii_1991 and
# so on, so I select the recent ones, turn them into rows, and keep
# each country's latest value.
#
# Note: this file is encoded in latin1, not UTF-8, which is why
# read_csv needs the locale argument. Without it, country names
# with accents come out wrong.

hdr_url <- paste0("https://hdr.undp.org/sites/default/files/2025_HDR/",
                  "HDR25_Composite_indices_complete_time_series.csv")

hdr_raw <- read_csv(hdr_url,
                    locale = locale(encoding = "latin1"),
                    show_col_types = FALSE)

cat("UNDP file downloaded:", nrow(hdr_raw), "rows,",
    ncol(hdr_raw), "columns\n")

source3 <- hdr_raw %>%
  select(country_code = iso3, country_name = country,
         matches("^gii_20(1[5-9]|2[0-3])$")) %>%          # gii_2015 ... gii_2023
  pivot_longer(starts_with("gii_"),
               names_to = "variable", values_to = "gender_inequality_index") %>%
  mutate(gii_year = as.numeric(str_extract(variable, "\\d{4}"))) %>%
  filter(!is.na(gender_inequality_index)) %>%
  group_by(country_code) %>%
  slice_max(gii_year, n = 1, with_ties = FALSE) %>%        # latest year available
  ungroup() %>%
  select(country_code, country_name, gender_inequality_index, gii_year) %>%
  semi_join(country_info, by = "country_code") %>%
  arrange(country_name)

write_csv(source3, "data_raw/source3_gii_undp.csv")
cat("Source 3 saved:", nrow(source3), "countries\n")


cat("\n--- data_raw now contains ---\n")
print(list.files("data_raw"))
cat("\nNext step: run 01_join_and_clean.Rmd\n")
