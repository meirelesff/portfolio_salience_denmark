# ---
# _ANALYSIS
# ---


# Packages
library(tidyverse)
library(patchwork)
library(latex2exp)
library(here)


# Exploratory plots
estimates <- readRDS(here("results", "denmark_model_summary.Rds"))


# Dictionary for ministries and years
dict_port_year <- denmark_panel %>%
  group_by(ministry, ministry_id, year, year_id) %>%
  summarise(.groups = "drop")


# Calculate portfolios ranking for 2016 and time trends for selected portfolios
p_rank <- estimates %>%
  # Extract thetas
  filter(str_detect(parameter, "theta")) %>%
  mutate(parameter = str_remove_all(parameter, "theta\\[|\\]")) %>%
  separate(parameter, into = c("year_id", "ministry_id"), sep = ",") %>%
  mutate(across(c(year_id, ministry_id), as.numeric)) %>%
  left_join(dict_port_year, by = c("year_id", "ministry_id")) %>%
  mutate(ministry = str_wrap(ministry, width = 20)) %>%
  # Remove non-existint portfolios
  filter(!is.na(ministry)) %>%
  # Filter year 2016
  filter(year == 2016) %>%
  # Plot
  ggplot(aes(x = reorder(ministry, estimate), y = estimate, ymax = up95, ymin = lo95)) +
  geom_pointrange() +
  # scale_color_manual(values = c(purple, "black")) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(x = NULL, y = "Latent portfolio salience\n(year = 2016)", color = NULL, shape = NULL) +
  coord_flip()

p_time <- estimates %>%
  filter(str_detect(parameter, "theta")) %>%
  mutate(parameter = str_remove_all(parameter, "theta\\[|\\]")) %>%
  separate(parameter, into = c("year_id", "ministry_id"), sep = ",") %>%
  mutate(across(c(year_id, ministry_id), as.numeric)) %>%
  left_join(dict_port_year, by = c("year_id", "ministry_id")) %>%
  filter(ministry %in% c(
    "Health", "Interior", "Ecclesiastical Affairs", "Justice",
    "Immigration & Integration", "Finance"
  )) %>%
  mutate(ministry = str_wrap(ministry, width = 20)) %>%
  filter(!is.na(ministry)) %>% # Remove non-existint portfolios
  ggplot(aes(x = year, y = estimate, ymax = up95, ymin = lo95)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, fill = "#8f2a43") +
  scale_y_continuous(position = "right") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 5)) +
  facet_wrap(~ministry, ncol = 2) +
  theme_minimal() +
  labs(x = NULL, y = "Latent portfolio salience\n")

p <- p_rank + p_time +
  plot_layout(widths = c(1, 2))

ggsave(plot = p, filename = here("results", "denmark_salience.pdf"),
         width = 7, height = 6)


# Government effects
p_gov <- estimates %>%
    filter(str_detect(parameter, "gamma")) %>%
    ggplot(aes(x = parameter, y = estimate, ymin = lo95, ymax = up95)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange(size = .4) +
    geom_pointrange(aes(y = estimate, ymin = lo80, ymax = up80), linewidth = 1.2, size = 0.8) +
    coord_flip() +
    theme_minimal(base_size = 11) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = TeX(r"(\gamma)"))

ggsave(plot = p_gov, filename = here("results", "denmark_gov_effects.pdf"),
         width = 6, height = 6.2)


# Validation with DW scores
comp_data <- estimates %>%
  # Extract thetas
  filter(str_detect(parameter, "theta")) %>%
  mutate(parameter = str_remove_all(parameter, "theta\\[|\\]")) %>%
  separate(parameter, into = c("year_id", "ministry_id"), sep = ",") %>%
  mutate(across(c(year_id, ministry_id), as.numeric)) %>%
  left_join(dict_port_year, by = c("year_id", "ministry_id")) %>%
  mutate(ministry = str_wrap(ministry, width = 20)) %>%
  # Remove non-existint portfolios
  filter(!is.na(ministry)) %>%
  # Filter year 2007
  filter(year == 2007) %>%

  # Join DW scores
  left_join(dw_scores, by = c("ministry")) %>%
  # Calculate upper and lower bounds for DW scores
  mutate(up_dw = mean + 1.96 * se,
         lo_dw = mean - 1.96 * se) 

r2_comp <- lm(estimate ~ mean, data = comp_data) %>%
  summary() %>%
  .$r.squared
         

p_valid <- comp_data %>%
  # Scatter plot
  ggplot(aes(x = mean, y = estimate)) +
  geom_point(size = 2.3) +
  geom_linerange(aes(ymin = lo95, ymax = up95), alpha = 0.5, linewidth = 0.3) +
  geom_smooth(method = "lm", se = F, color = "#8f2a43", alpha = 0.2) +
  annotate("text", x = 0.6, y = 8,
           label = paste0("R² = ", round(r2_comp, 2)),
           size = 4) +
  theme_minimal() +
  labs(x = "Druckman & Warwick salience scores (2000-2002)",
       y = "Estimated latent salience (2007)") +
  ggrepel::geom_text_repel(aes(label = ministry), vjust = -1, size = 3)


ggsave(plot = p_valid, filename = here("results", "denmark_validation.pdf"),
         width = 7, height = 5)
