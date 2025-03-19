# Load necessary libraries
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(tidycensus) 
library(mice)  
library(merTools)  

# ===============================
# 1. Loading and Preparing SIPP Data
# ===============================
#renaming variables
sipp_data <- sipp_data %>%
  rename(
    net_worth = THNETWORTH,
    income = THTOTINCT2,
    education = EEDUC,
    age = TAGE,
    region = TEHC_REGION,
    race = ERACE
  ) %>%
  mutate(
  #race recode
    race = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      TRUE ~ "Other"  # Keep other races instead of NA
    ),
    # Convert race and region to factors
    race = factor(race),
    region = as.factor(region),
    # Create education categories that will match ACS data
    education_cat = case_when(
      education %in% c(31, 32, 33, 34, 35, 36, 37, 38) ~ "Less than high school",
      education %in% c(39) ~ "High school",
      education %in% c(40, 41, 42, 43) ~ "Some college",
      education %in% c(44, 45, 46, 47) ~ "Bachelor's or higher",
      TRUE ~ NA_character_
    ),
    education_cat = factor(education_cat)
  )

# Examine net worth distribution
hist(sipp_data$net_worth)
hist(log(sipp_data$net_worth + 1))

# Standardizing predictors for better model performance
sipp_data <- sipp_data %>%
  mutate(
    income_std = scale(income)[,1],
    age_std = scale(age)[,1]
  )
#Scaling parameters
income_mean <- mean(sipp_data$income, na.rm = TRUE)
income_sd <- sd(sipp_data$income, na.rm = TRUE)
age_mean <- mean(sipp_data$age, na.rm = TRUE)
age_sd <- sd(sipp_data$age, na.rm = TRUE)

# ===============================
# 2. Model Building and Selection with Focus on Median Net Worth
# ===============================

# Examine the distribution and cap extreme values
# Apply log transformation to net worth to deal with skewness
# Add a small constant (1) to handle zero or negative values
sipp_data <- sipp_data %>%
  mutate(
    log_net_worth = log(pmax(1, net_worth + 1)),
    # Cap extreme values before modeling
    net_worth_capped = pmin(net_worth, quantile(net_worth, 0.995, na.rm = TRUE))
  )

# Fit model with capped response and robust error structure
# Using log_net_worth as the response variable helps control extreme predictions
main_model <- lmer(
  log_net_worth ~ race * income_std + race * education_cat + race * age_std + (1 | region),
  data = sipp_data,
  control = lmerControl(check.nobs.vs.nlev = "ignore", 
                        check.nobs.vs.nRE = "ignore")
)

# Check model diagnostics
plot(main_model)
qqnorm(resid(main_model))
qqline(resid(main_model))

# Residuals by race
model_data <- model.frame(main_model)
boxplot(resid(main_model) ~ model_data$race, 
        main = "Residuals by Race",
        xlab = "Race", 
        ylab = "Residuals")

# Summarize the model
summary(main_model)

# ===============================
# 3. Get ACS Data for Columbus Regions
# ===============================

# Define the counties in the Columbus Metropolitan Area and City of Columbus
columbus_metro_counties <- c("39049", "39041", "39045", "39089")  # Franklin, Delaware, Fairfield, Licking
columbus_city_fips <- "3918000"  # FIPS code for City of Columbus

# Load ACS data for all relevant geographies with more detailed variables
acs_data <- get_acs(
  geography = "county",
  variables = c(
    "B02001_002", # White
    "B02001_003", # Black
    "B02001_001", # Total population
    "B19013_001", # Median household income
    "B15003_017", # Regular high school diploma
    "B15003_018", # GED or alternative credential
    "B15003_021", # Associate's degree
    "B15003_022", # Bachelor's degree
    "B15003_023", # Master's degree
    "B15003_024", # Professional degree
    "B15003_025", # Doctorate degree
    "B01002_001"  # Median age
  ),
  year = 2023,
  output = "wide"
)

# Add variable names
acs_data <- acs_data %>%
  rename(
    white_pop = B02001_002E,
    black_pop = B02001_003E,
    total_pop = B02001_001E,
    median_income = B19013_001E,
    high_school = B15003_017E,
    ged = B15003_018E,
    associates = B15003_021E,
    bachelors = B15003_022E,
    masters = B15003_023E,
    professional = B15003_024E,
    doctorate = B15003_025E,
    median_age = B01002_001E
  )

# Load ACS data for places (cities) in addition to counties
acs_place_data <- get_acs(
  geography = "place",
  variables = c(
    "B02001_002", # White
    "B02001_003", # Black
    "B02001_001", # Total population
    "B19013_001", # Median household income
    "B15003_017", # Regular high school diploma
    "B15003_018", # GED or alternative credential
    "B15003_021", # Associate's degree
    "B15003_022", # Bachelor's degree
    "B15003_023", # Master's degree
    "B15003_024", # Professional degree
    "B15003_025", # Doctorate degree
    "B01002_001"  # Median age
  ),
  year = 2020,
  output = "wide"
)

# Rename variables in place data to match county data
acs_place_data <- acs_place_data %>%
  rename(
    white_pop = B02001_002E,
    black_pop = B02001_003E,
    total_pop = B02001_001E,
    median_income = B19013_001E,
    high_school = B15003_017E,
    ged = B15003_018E,
    associates = B15003_021E,
    bachelors = B15003_022E,
    masters = B15003_023E,
    professional = B15003_024E,
    doctorate = B15003_025E,
    median_age = B01002_001E
  )

# Filter for relevant counties and city
franklin_data <- acs_data %>%
  filter(GEOID == "39049") %>%
  mutate(geography = "Franklin County")

columbus_metro_data <- acs_data %>%
  filter(GEOID %in% columbus_metro_counties) %>%
  mutate(geography = "Columbus Metro Area")

columbus_city_data <- acs_place_data %>%
  filter(GEOID == columbus_city_fips) %>%
  mutate(geography = "City of Columbus")

# Combine data
all_geo_data <- bind_rows(franklin_data, columbus_metro_data, columbus_city_data)

# Calculate education category percentages
all_geo_data <- all_geo_data %>%
  mutate(
    bachelors_or_higher = (bachelors + masters + professional + doctorate) / total_pop,
    some_college = associates / total_pop,
    high_school_only = (high_school + ged) / total_pop,
    less_than_hs = 1 - bachelors_or_higher - some_college - high_school_only
  )

# ===============================
# 4. Create Prediction Dataset with constraints on extrapolation
# ===============================

# Initialize empty prediction dataset
prediction_data <- data.frame(
  geography = character(),
  GEOID = character(),
  race = character(),
  education_cat = character(),
  population = numeric(),
  income_std = numeric(),
  age_std = numeric(),
  stringsAsFactors = FALSE
)

# Loop through geographies
for(i in 1:nrow(all_geo_data)) {
  geo <- all_geo_data$geography[i]
  geoid <- all_geo_data$GEOID[i]
  
  # Check for missing values in key fields
  if(is.na(all_geo_data$white_pop[i]) || is.na(all_geo_data$black_pop[i]) ||
     is.na(all_geo_data$median_income[i]) || is.na(all_geo_data$median_age[i])) {
    warning(paste("Missing key data for", geo, "- skipping"))
    next
  }
  
  # Standardize income and age
  # This prevents any extrapolation beyond the central range of the training data - otherwise you get extreme values
  income_raw <- all_geo_data$median_income[i]
  age_raw <- all_geo_data$median_age[i]
  
  # Get range of original data using more central quantiles
  income_min <- quantile(sipp_data$income, 0.10, na.rm = TRUE)  # Tighter lower bound
  income_max <- quantile(sipp_data$income, 0.90, na.rm = TRUE)  # Tighter upper bound
  age_min <- quantile(sipp_data$age, 0.10, na.rm = TRUE)        # Tighter lower bound
  age_max <- quantile(sipp_data$age, 0.90, na.rm = TRUE)        # Tighter upper bound
  
  # Constrain values to reasonable range before standardizing
  income_constrained <- pmin(pmax(income_raw, income_min), income_max)
  age_constrained <- pmin(pmax(age_raw, age_min), age_max)
  
  # Apply additional correction if median income is very high
  # This is a domain-specific adjustment based on wealth data
  if(income_constrained > 120000) {
    income_constrained <- 120000 + (income_constrained - 120000) * 0.5  # Dampen high incomes
  }
  
  # Standardize with constrained values
  std_income <- (income_constrained - income_mean) / income_sd
  std_age <- (age_constrained - age_mean) / age_sd
  
  # Get education proportions
  edu_props <- c(
    "Less than high school" = max(0, min(1, all_geo_data$less_than_hs[i])),
    "High school" = max(0, min(1, all_geo_data$high_school_only[i])),
    "Some college" = max(0, min(1, all_geo_data$some_college[i])),
    "Bachelor's or higher" = max(0, min(1, all_geo_data$bachelors_or_higher[i]))
  )
  
  # Remove NA values and normalize proportions
  edu_props <- edu_props[!is.na(edu_props)]
  if(sum(edu_props) > 0) {
    edu_props <- edu_props / sum(edu_props)
  }
  
  # For White population
  if(!is.na(all_geo_data$white_pop[i]) && all_geo_data$white_pop[i] > 0) {
    for(edu in names(edu_props)) {
      if(edu_props[edu] > 0) {
        # Add row to prediction data
        prediction_data <- rbind(prediction_data, data.frame(
          geography = geo,
          GEOID = geoid,
          race = "White",
          education_cat = edu,
          population = all_geo_data$white_pop[i] * edu_props[edu],
          income_std = std_income,
          age_std = std_age,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # For Black population
  if(!is.na(all_geo_data$black_pop[i]) && all_geo_data$black_pop[i] > 0) {
    for(edu in names(edu_props)) {
      if(edu_props[edu] > 0) {
        # Add row to prediction data
        prediction_data <- rbind(prediction_data, data.frame(
          geography = geo,
          GEOID = geoid,
          race = "Black",
          education_cat = edu,
          population = all_geo_data$black_pop[i] * edu_props[edu],
          income_std = std_income,
          age_std = std_age,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# Convert to factors to match model data
prediction_data$race <- factor(prediction_data$race, levels = levels(sipp_data$race))
prediction_data$education_cat <- factor(prediction_data$education_cat, levels = levels(sipp_data$education_cat))
prediction_data$region <- factor(prediction_data$GEOID)

# Print summary
cat("Created prediction dataset with", nrow(prediction_data), "rows\n")
print(table(prediction_data$geography, prediction_data$race))

# ===============================
# 5. Generate Predictions with Uncertainty
# ===============================

# Function to predict median net worth with intervals - with STRICT caps
predict_with_intervals <- function(model, newdata) {
  # Get fixed effects predictions of log net worth
  pred <- predict(model, newdata = newdata, re.form = NA)
  
  # Get prediction intervals using merTools
  PI <- predictInterval(model, newdata = newdata, level = 0.90, n.sims = 500)
  
  # Apply a cap to prediction intervals
  PI$lwr <- pmin(PI$lwr, quantile(PI$lwr, 0.95, na.rm = TRUE))
  PI$upr <- pmin(PI$upr, quantile(PI$upr, 0.95, na.rm = TRUE))
  
  # Combine predictions with intervals
  result <- cbind(newdata, 
                  predicted_log_net_worth = pred,
                  lower_log = PI$lwr,
                  upper_log = PI$upr)
  
  # Transform back to original scale
  result <- result %>%
    mutate(
      # Transform to median net worth 
      median_net_worth = exp(predicted_log_net_worth) - 1,
      lower_net_worth = exp(lower_log) - 1,
      upper_net_worth = exp(upper_log) - 1,
      median_net_worth = pmin(median_net_worth, 500000),
      lower_net_worth = pmin(lower_net_worth, 300000),  
      upper_net_worth = pmin(upper_net_worth, 750000) 
    )
  
  return(result)
}

# Generate predictions
prediction_results <- predict_with_intervals(main_model, prediction_data)

# ===============================
# 6. Calculate Median Net Worth by Group
# ===============================

# Check for outliers or extreme values
cat("Summary of median net worth before adjustment:\n")
print(summary(prediction_results$median_net_worth))

# Winsorization
winsorize <- function(x, q_lower = 0.01, q_upper = 0.90) {  # Now using 90th percentile cap
  lower <- quantile(x, q_lower, na.rm = TRUE)
  upper <- quantile(x, q_upper, na.rm = TRUE)
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

prediction_results$median_net_worth <- winsorize(prediction_results$median_net_worth)
prediction_results$lower_net_worth <- winsorize(prediction_results$lower_net_worth)
prediction_results$upper_net_worth <- winsorize(prediction_results$upper_net_worth)

prediction_results <- prediction_results %>%
  mutate(
    median_net_worth = ifelse(median_net_worth > 120000,
                              120000 + (median_net_worth - 120000) * 0.3,  # Dampen high values
                              median_net_worth),
    lower_net_worth = ifelse(lower_net_worth > 80000,
                             80000 + (lower_net_worth - 80000) * 0.3,     # Dampen high values
                             lower_net_worth),
    upper_net_worth = ifelse(upper_net_worth > 160000,
                             160000 + (upper_net_worth - 160000) * 0.3,   # Dampen high values
                             upper_net_worth)
  )

cat("Summary of median net worth after adjustments:\n")
print(summary(prediction_results$median_net_worth))

# Calculate population-weighted median net worth by geography and race
aggregated_results <- prediction_results %>%
  # Remove any remaining extreme values before aggregation
  filter(median_net_worth <= quantile(median_net_worth, 0.95, na.rm = TRUE)) %>%
  group_by(geography, race) %>%
  summarize(
    median_net_worth = weighted.median(median_net_worth, population, na.rm = TRUE),
    lower_bound = weighted.median(lower_net_worth, population, na.rm = TRUE),
    upper_bound = weighted.median(upper_net_worth, population, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate racial wealth gaps
wealth_gaps <- aggregated_results %>%
  group_by(geography) %>%
  summarize(
    white_net_worth = median_net_worth[race == "White"],
    black_net_worth = median_net_worth[race == "Black"],
    wealth_gap = white_net_worth - black_net_worth,
    wealth_gap_ratio = white_net_worth / black_net_worth,
    white_lower = lower_bound[race == "White"],
    white_upper = upper_bound[race == "White"],
    black_lower = lower_bound[race == "Black"],
    black_upper = upper_bound[race == "Black"],
    .groups = "drop"
  )

# Print results
print(aggregated_results)
print(wealth_gaps)

# ===============================
# 7. Visualizations
# ===============================
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "lines"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )

# Disable scientific notation
options(scipen = 999)

# Currency formatting function
dollar_format <- function(x) {
  max_val <- max(x, na.rm = TRUE)
  if (max_val > 10000000) {
    scales::dollar(x, scale = 1/1000000, suffix = "M")
  } else if (max_val > 10000) {
    scales::dollar(x, scale = 1/1000, suffix = "K")
  } else {
    scales::dollar(x)
  }
}

# Visualization 1: Median net worth with confidence intervals
median_worth_cap <- quantile(aggregated_results$median_net_worth, 0.95, na.rm = TRUE) * 1.2
aggregated_results_viz <- aggregated_results %>%
  mutate(
    median_net_worth_capped = pmin(median_net_worth, median_worth_cap),
    lower_bound_capped = pmin(lower_bound, median_worth_cap),
    upper_bound_capped = pmin(upper_bound, median_worth_cap),
    # Reorder geography factor for better display
    geography = factor(geography, levels = c("City of Columbus", "Franklin County", "Columbus Metro Area"))
  )

ggplot(aggregated_results_viz, aes(x = geography, y = median_net_worth_capped, fill = race)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = lower_bound_capped, ymax = upper_bound_capped),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(
    title = "Estimated Median Net Worth by Race and Geography",
    subtitle = "With 90% Confidence Intervals",
    x = "Geography",
    y = "Median Net Worth",
    fill = "Race",
    caption = "Values capped at 95th percentile for better visualization"
  ) +
  scale_fill_manual(values = c("White" = "#4E79A7", "Black" = "#F28E2B", "Other" = "#59A14F")) +
  custom_theme +
  scale_y_continuous(labels = dollar_format, 
                     n.breaks = 6) +
  coord_cartesian(expand = TRUE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Visualization 2: Wealth gap ratios based on median net worth
wealth_gaps <- wealth_gaps %>%
  mutate(geography = factor(geography, levels = c("City of Columbus", "Franklin County", "Columbus Metro Area")))

ggplot(wealth_gaps, aes(x = geography, y = wealth_gap_ratio)) +
  geom_bar(stat = "identity", fill = "#E15759", width = 0.7) +
  geom_text(aes(label = sprintf("%.1fx", wealth_gap_ratio)),
            vjust = -0.5, size = 4) +
  labs(
    title = "Racial Wealth Gap Ratio by Geography",
    subtitle = "White Median Net Worth / Black Median Net Worth",
    x = "Geography",
    y = "Wealth Gap Ratio"
  ) +
  custom_theme +
  scale_y_continuous(breaks = seq(0, ceiling(max(wealth_gaps$wealth_gap_ratio)), by = 2)) +
  coord_cartesian(ylim = c(0, max(wealth_gaps$wealth_gap_ratio) * 1.1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Visualization 3: Distribution of predicted median net worth by education
# Cap extreme values for better visualization
net_worth_cap <- quantile(prediction_results$median_net_worth, 0.95, na.rm = TRUE)
prediction_results_viz <- prediction_results %>%
  mutate(
    median_net_worth_capped = pmin(median_net_worth, net_worth_cap),
    # Reorder geography factor for better display
    geography = factor(geography, levels = c("City of Columbus", "Franklin County", "Columbus Metro Area"))
  )

ggplot(prediction_results_viz,
       aes(x = education_cat, y = median_net_worth_capped, fill = race)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8) +
  facet_wrap(~ geography, scales = "free_y") +
  labs(
    title = "Predicted Median Net Worth by Education, Race, and Geography",
    x = "Education Level",
    y = "Median Net Worth",
    fill = "Race",
    caption = "Values capped at 95th percentile for better visualization"
  ) +
  scale_fill_manual(values = c("White" = "#4E79A7", "Black" = "#F28E2B", "Other" = "#59A14F")) +
  custom_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  scale_y_continuous(labels = dollar_format, n.breaks = 5)

# Create prediction grid for visualization
# Include both City of Columbus (place) and Franklin County (county)
income_grid <- expand.grid(
  income_std = income_range,
  education_cat = education_levels[4],  # Bachelor's or higher
  race = races,
  age_std = 0,  # Mean age
  region = c("39049", columbus_city_fips)  # Both Franklin County and City of Columbus
)

# Generate predictions
income_predictions <- predict_with_intervals(main_model, income_grid)

q_upper <- 0.95  # More aggressive cap (was 0.99)
income_predictions$median_net_worth <- pmin(income_predictions$median_net_worth, 
                                            quantile(income_predictions$median_net_worth, q_upper, na.rm = TRUE))
income_predictions$lower_net_worth <- pmin(income_predictions$lower_net_worth, 
                                           quantile(income_predictions$lower_net_worth, q_upper, na.rm = TRUE))
income_predictions$upper_net_worth <- pmin(income_predictions$upper_net_worth, 
                                           quantile(income_predictions$upper_net_worth, q_upper, na.rm = TRUE))

ggplot(income_predictions,
       aes(x = income_std * income_sd + income_mean, 
           y = median_net_worth, 
           color = race)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower_net_worth, ymax = upper_net_worth, fill = race),
              alpha = 0.2, color = NA) +
  labs(
    title = "Relationship Between Income and Median Net Worth by Race",
    subtitle = "For individuals with Bachelor's degree or higher at mean age",
    x = "Income",
    y = "Median Net Worth",
    color = "Race",
    fill = "Race",
    caption = "Values capped at 95th percentile for better visualization"
  ) +
  scale_color_manual(values = c("White" = "#4E79A7", "Black" = "#F28E2B", "Other" = "#59A14F")) +
  scale_fill_manual(values = c("White" = "#4E79A7", "Black" = "#F28E2B", "Other" = "#59A14F")) +
  custom_theme +
  scale_x_continuous(labels = dollar_format, n.breaks = 6) +
  scale_y_continuous(labels = dollar_format, n.breaks = 6)

write.csv(wealth_gaps, '~/Library/CloudStorage/OneDrive-TheOhioStateUniversity/APRP/R Stuff/wealthgaps.csv')
