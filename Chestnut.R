rm(list = ls())     # clear objects  
graphics.off() 
##########################
###### Pasta  ############
##########################


# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "fpc","plot3D", "cluster", "readxl", "magrittr", "purrr"
              "multipanelfigure","klaR","psych","MASS","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats",
              "dplyr","ggstatsplot", "janitor", "stringr", "forcats", "rstatix",
              "multcompView", "patchwork")
inst(packages)
theme_set(theme_minimal())


# Loading data ------------------------------------------------------------

df <- read_excel("Chestnut.xlsx", sheet = 1) %>% 
  clean_names() %>% 
  rename_with(~ .x %>%
                str_replace_all("_", " ") %>%
                str_replace_all("'", "") %>%
                str_to_lower() %>%
                str_trim())


# Preparation of data
# Exploring and wrangling -------------------------------------------------

# cleaning observations
text_cleanse <- function(x) {
  x %>%
    str_trim() %>%                          # trim extra spaces at begining/end
    str_to_lower() %>%                      # undercap convertion
    str_replace_all("cocked", "cooked") %>% # correct mispelling
    str_replace_all("_", " ") %>%           # replace "_" for space
    str_replace_all("'", "")                # remove simple '
}

# apply cleaning (except id)
clean_df <- df %>%
  mutate(across(where(is.character), text_cleanse))

# correct translation
translation_map <- c(
  "i disliked extremely" = "I disliked extremely",
  "i disliked a lot" = "I disliked very much",
  "moderately" = "I disliked moderately",
  "disadvantaged" = "I disliked slightly",
  "indifferent" = "Indifferent",
  "i liked it slightly" = "I liked slightly",
  "i liked moderately" = "I liked moderately",
  "i really liked" = "I liked very much",
  "i liked it extremely" = "I liked extremely"
)

# Function for translation
translate_values <- function(x) {
  if (is.character(x)) {
    return(replace(x, x %in% names(translation_map), translation_map[x[x %in% names(translation_map)]]))
  } else {
    return(x)
  }
}

clean_df <- clean_df %>%
  mutate(across(everything(), translate_values))


# 1. Define variable blocks -----------------------------------------------

# a) Demographic and habit-related variables (nominal)
demog_vars <- c(
  "age", "sex", "ethnicity", "last grade",
  "fresh pasta frequency", "new pasta products frequency",
  "new pasta products availability", "allergies", "allergies type"
)

# b) Ingredient flavour preferences (ordinal: dont feel < light taste < intense taste)
ingredient_vars <- c("oat", "almond", "peanut", "chestnut", "banana")
taste_levels    <- c("dont feel", "light taste", "intense taste")

# intensities converion to factors
intensity_levels <- c("cooked", "dried", "roasted")
clean_df <- clean_df %>%
  mutate(across(c(`low intensity`, `mid intensity`, `high intensity`), ~ factor(.x, levels = intensity_levels)))

# list texture, flavour, aroma, visual and global score variables
# Define sensory variable groups
visual_vars  <- c("cooked pasta visual" , "dried pasta visual" , "roasted pasta visual")
flavour_vars <- c("cooked pasta flavour", "dried pasta flavour", "roasted pasta flavour")
aroma_vars   <- c("cooked pasta aroma"  , "dried pasta aroma"  , "roasted pasta aroma")
texture_vars <- c("cooked pasta texture", "dried pasta texture", "roasted pasta texture")
sweetness_vars <- c("cooked pasta sweetness", "dried pasta sweetness", "roasted pasta sweetness")
global_vars  <- c("cooked pasta global score", "dried pasta global score", "roasted pasta global score")


# c) Extract actual sensorial levels present in the dataset
sensorial_vars <- c(visual_vars, flavour_vars, aroma_vars, texture_vars, sweetness_vars, global_vars)

sensorial_levels_check <- clean_df %>% 
  select(all_of(sensorial_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  distinct(variable, value) %>%
  arrange(variable, value)

# levels for dataset
clean_df %>%
  select(all_of(sensorial_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  distinct(value) %>%
  arrange(value) %>%
  pull(value)

# Levels sort
scale_order <- c(
  "I disliked extremely",
  "I disliked very much",
  "I disliked moderately",
  "I disliked slightly",
  "Indifferent",
  "I liked slightly",
  "I liked moderately",
  "I liked very much",
  "I liked extremely"
)

clean_df <- clean_df %>%
  mutate(across(all_of(core_sensorial_vars), ~ factor(.x, levels = scale_order)))

# Sensory perception of pasta samples (ordinal:  
# i disliked very much < ... < i really liked)
# Replace or adjust these levels
sensorial_levels <- c(
  "I disliked extremely",
  "I disliked very much",
  "I disliked moderately",
  "I disliked slightly",
  "Indifferent",
  "I liked slightly",
  "I liked moderately",
  "I liked very much",
  "I liked extremely"
)


# Group sensorial vars withouth global and purchase
core_sensorial_vars <- setdiff(sensorial_vars, c(global_vars, purchase_vars))

# Extract and sort unique levels across all sensorial columns
sensorial_levels <- clean_df %>%
  select(all_of(sensorial_vars)) %>%
  pivot_longer(everything()) %>%
  distinct(value) %>%
  pull(value) %>%
  unique() %>%
  sort()

# d) Purchase intention variables (ordinal)
purchase_vars <- c(
  "bought preference cooked pasta",
  "bought preference dried pasta",
  "bought preference roasted pasta"
)

purchase_levels <- c(
  "certainly would not buy", 
  "would probably not buy",
  "i have doubts if i would buy",
  "probably would buy",
  "certainly would buy"
)

# 2. Convert to factors ---------------------------------------------------

df_factors <- clean_df %>% 
  # a) Demographics: nominal factor
  mutate(across(all_of(demog_vars), as.factor)) %>% 
  
  # b) Ingredient preferences: ordered factor
  mutate(across(all_of(ingredient_vars), 
                ~ factor(.x, levels = taste_levels, ordered = TRUE))) %>% 
  
  # c) Sensory variables: ordered factor
  mutate(across(all_of(c(texture_vars, flavour_vars, aroma_vars, visual_vars, sweetness_vars, global_vars)),
                ~ factor(.x, levels = sensorial_levels, ordered = TRUE))) %>% 
  
  # d) Purchase intention: ordered factor
  mutate(across(all_of(purchase_vars),
                ~ factor(.x, levels = purchase_levels, ordered = TRUE))) 



# 3. Quick check ----------------------------------------------------------
glimpse(df_factors)
# and to check any frequency table:
# table(df_factors$chestnut)




# Intensity of flavour perceived
# Mosaic plot for intensity -----------------------------------------------

# Reshape df_factors to long format for analysis
df_long_m <- df_factors %>%
  select(`low intensity`, `mid intensity`, `high intensity`) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "Intensity_Felt", values_to = "Treatment_Received") %>%
  mutate(
    Intensity_Felt = factor(Intensity_Felt,
                            levels = c("low intensity", "mid intensity", "high intensity"),
                            labels = c("Low", "Mid", "High")),
    Treatment_Received = factor(Treatment_Received,
                                levels = c("cooked", "dried", "roasted"),
                                labels = c("Cooked", "Dried", "Roasted"))
  )

# Build contingency table
table_counts <- table(df_long_m$Treatment_Received, df_long_m$Intensity_Felt)

# Run Fisher's exact test
fisher_result <- fisher.test(table_counts)

# levels extracrtion
niveles_intensidad <- levels(df_long_m$Intensity_Felt)
tratamientos <- levels(df_long_m$Treatment_Received)
combs <- combn(tratamientos, 2, simplify = FALSE)

# comparison by intensity levels
resultados <- map_dfr(niveles_intensidad, function(nivel) {
  df_sub <- df_long_m %>% mutate(Intensity_Bin = Intensity_Felt == nivel)
  map_dfr(combs, function(par) {
    df_par <- df_sub %>% filter(Treatment_Received %in% par)
    tab <- table(df_par$Treatment_Received, df_par$Intensity_Bin)
    test <- fisher.test(tab)
    tibble(
      Intensity_Level = nivel,
      Group1 = par[1],
      Group2 = par[2],
      p_value = test$p.value
    )
  })
}) %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr"))


# Plot
ggbarstats(
  data = df_long_m,
  x = Intensity_Felt,
  y = Treatment_Received,
  results.subtitle = FALSE,
  title = "Chestnut Flavour Intensity Felt by All Participants",
  subtitle = paste(
    "Pairwise Fisher test (uncorrected p-values):",
    "Low: Dried vs Roasted (p = 0.008)",
    "Mid: Cooked vs Roasted (p = 0.036)",
    "High: Cooked vs Dried (p = 0.038)",
    sep = "\n"
  )
) +
  scale_fill_manual(values = c("#7B8156", "#B15137", "#9D5316")) +
  theme(axis.title.x = element_text(size = 12))



# Global panel evaluation
# General description Sensory Evaluation -----------------------------------

# Ordinal sensorial variables list
sensorial_vars <- c(
  "cooked pasta visual", "cooked pasta flavour", "cooked pasta aroma",
  "cooked pasta texture", "cooked pasta sweetness", "cooked pasta global score",
  "dried pasta visual", "dried pasta flavour", "dried pasta aroma",
  "dried pasta texture", "dried pasta sweetness", "dried pasta global score",
  "roasted pasta visual", "roasted pasta flavour", "roasted pasta aroma",
  "roasted pasta texture", "roasted pasta sweetness", "roasted pasta global score"
)

# Sensorial percentage frequency summary
sensorial_summary <- df_factors %>%
  select(all_of(sensorial_vars)) %>%
  summarise(across(everything(), ~ list(table(.) %>% prop.table() %>% round(3)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "distribution")

# Purchase percentage frequency summary
purchase_summary <- df_factors %>%
  select(all_of(purchase_vars)) %>%
  summarise(across(everything(), ~ list(table(.) %>% prop.table() %>% round(3)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "distribution")

sensorial_summary
purchase_summary

sensorial_summary$distribution[[1]]
purchase_summary$distribution[[1]]





# Pasta scoring -----------------------------------------------------------

# Factors pivoting
df_attributes_long <- df_factors %>%
  select(all_of(sensorial_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  mutate(
    attribute = case_when(
      variable %in% visual_vars ~ "Visual",
      variable %in% flavour_vars ~ "Flavour",
      variable %in% aroma_vars ~ "Aroma",
      variable %in% texture_vars ~ "Texture",
      variable %in% sweetness_vars ~ "Sweetness",
      variable %in% global_vars ~ "Global Score"
    ),
    treatment = case_when(
      str_detect(variable, "cooked") ~ "Cooked Chestnut Pasta",
      str_detect(variable, "dried") ~ "Dried Chestnut Pasta",
      str_detect(variable, "roasted") ~ "Roasted Chestnut Pasta"
    ),
    # Ensure order for faceting
    attribute = factor(attribute, levels = c("Visual", "Flavour", "Aroma", "Texture", "Sweetness", "Global Score")),
    treatment = factor(treatment, levels = c("Cooked Chestnut Pasta", "Dried Chestnut Pasta", "Roasted Chestnut Pasta")),
    response = factor(response, levels = sensorial_levels)
  )


# Plot
ggplot(df_attributes_long, aes(x = response, fill = treatment)) +
  geom_bar() +
  facet_grid(attribute ~ treatment, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Cooked Chestnut Pasta" = "#7b8156",
      "Dried Chestnut Pasta"  = "#B15137",  
      "Roasted Chestnut Pasta" = "#9d5316"
    )
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) +
  labs(x = "Response", y = "Frequency", title = "Distribution of sensory responses by attribute and treatment")


# Purchase intention summary
df_purchase_summary <- df_purchase_long %>%
  group_by(treatment, variable, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(treatment, variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    attribute = factor(str_remove(variable, "bought preference "), 
                       levels = c("cooked pasta", "dried pasta", "roasted pasta")),
    response = factor(response, levels = purchase_levels, ordered = TRUE),
    treatment = factor(treatment, levels = c("Cooked", "Dried", "Roasted"))
  )

# Plot
p1 <- ggplot(df_purchase_summary, aes(x = response, y = prop, fill = treatment)) +
  geom_col(color = "white") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Cooked" = "#7b8156", "Dried" = "#B15137", "Roasted" = "#9d5316")) +
  facet_wrap(~attribute, ncol = 3) +
  labs(
    x = "Purchase Intention",
    y = "Proportion of responses",
    title = "Purchase Preferences by Treatment"
  ) +
  theme_linedraw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )




# PCA ---------------------------------------------------------------------

# Transformation to numeric scale
df_numeric_summary <- df_factors %>%
  select(all_of(sensorial_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  mutate(
    response_num = as.numeric(factor(response, levels = sensorial_levels)),
    treatment = case_when(
      str_detect(variable, "cooked") ~ "Cooked",
      str_detect(variable, "dried") ~ "Dried",
      str_detect(variable, "roasted") ~ "Roasted"
    ),
    attribute = case_when(
      str_detect(variable, "visual") ~ "Visual",
      str_detect(variable, "flavour") ~ "Flavour",
      str_detect(variable, "aroma") ~ "Aroma",
      str_detect(variable, "texture") ~ "Texture",
      str_detect(variable, "sweetness") ~ "Sweetness",
      str_detect(variable, "global") ~ "Global"
    )
  ) %>%
  group_by(treatment, attribute) %>%
  summarise(mean_score = mean(response_num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = attribute, values_from = mean_score)

# PCA construction
pca_res <- prcomp(df_numeric_summary[, -1], scale. = TRUE)
fviz_pca_biplot(pca_res, label = "var", habillage = df_numeric_summary$treatment)


# Treatment scores extraction
treatment_coords <- as.data.frame(pca_res$x)
treatment_coords$treatment <- df_numeric_summary$treatment

# Biplot with vector to treatments
p2 <- fviz_pca_biplot(pca_res,
                label = "var",              # variable names On
                col.var = "black",          # Variable colors
                repel = TRUE,
                addEllipses = FALSE,
                geom.ind = "point",         # Obserbations points
                habillage = df_numeric_summary$treatment) +  # Tratment colors
  scale_color_manual(values = c(
    "Cooked" = "#7b8156",
    "Dried" = "#B15137",
    "Roasted" = "#9d5316"
  )) +
  theme_classic() +
  theme(legend.position = "none")  # <- remove legend



# Statistics for global responses except purchase
# a) long data transformation with numeric responses, treatment labels and attributes
df_long_numeric <- df_factors %>%
  select(all_of(sensorial_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  mutate(
    response_num = as.numeric(factor(response, levels = sensorial_levels)),
    treatment = case_when(
      str_detect(variable, "cooked") ~ "Cooked",
      str_detect(variable, "dried") ~ "Dried",
      str_detect(variable, "roasted") ~ "Roasted",
      TRUE ~ NA_character_
    ),
    attribute = case_when(
      str_detect(variable, "visual") ~ "Visual",
      str_detect(variable, "flavour") ~ "Flavour",
      str_detect(variable, "aroma") ~ "Aroma",
      str_detect(variable, "texture") ~ "Texture",
      str_detect(variable, "sweetness") ~ "Sweetness",
      str_detect(variable, "global") ~ "Global",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(response_num) & !is.na(treatment) & !is.na(attribute))  # para limpiar datos incompletos


# b) ANOVA and Tukey for attributes
results <- df_long_numeric %>%
  group_by(attribute) %>%
  anova_test(response_num ~ treatment) %>%
  ungroup()

results_df <- as_tibble(results)

sig_attributes <- results_df %>%
  filter(p < 0.05) %>%
  pull(attribute)

# Tukey for significant terms
tukey_results <- df_long_numeric %>%
  filter(attribute %in% sig_attributes) %>%
  group_by(attribute) %>%
  tukey_hsd(response_num ~ treatment) %>%
  ungroup()

# Print
print(results)
print(tukey_results)


# c) Plot prep
# C1. Long table for easy manipulation
df_long <- df_numeric_summary %>%
  pivot_longer(-treatment, names_to = "attribute", values_to = "mean_score")

# C2. p-valor adjusted comparison matrix for each attribute
letters_df <- tukey_results %>%
  filter(p.adj < 0.05) %>% # only significant terms selection
  group_by(attribute) %>%
  summarise(
    comp = list(paste(group1, group2, sep = "-")),
    pval = list(p.adj)
  ) %>%
  rowwise() %>%
  mutate(
    # Comparison matrix for multcompView construction-
    comp_mat = list({
      trts <- unique(c(tukey_results$group1, tukey_results$group2))
      mat <- matrix(1, nrow = length(trts), ncol = length(trts), dimnames = list(trts, trts))
      # rellenar con NA donde no hay comparacion
      for(i in seq_along(comp)) {
        pair <- strsplit(comp[[i]], "-")[[1]]
        mat[pair[1], pair[2]] <- pval[[i]]
        mat[pair[2], pair[1]] <- pval[[i]]
      }
      mat
    })
  ) %>%
  mutate(
    # Group letters generation
    letters = list(multcompLetters(comp_mat)$Letters)
  ) %>%
  select(attribute, letters) %>%
  unnest_wider(letters)

# C3. Re-order and join df_long
letters_tidy <- letters_df %>%
  pivot_longer(-attribute, names_to = "treatment", values_to = "label") %>%
  mutate(treatment = factor(treatment, levels = c("Cooked", "Dried", "Roasted")))

# C4. joining df_long for vertical position (mean_score) and offset sum for text
labels_plot <- df_long %>%
  left_join(letters_tidy, by = c("attribute", "treatment")) %>%
  mutate(y_pos = mean_score + 0.15) # ajusta este valor para separar texto de barra

# C5. Final Plot
p3 <- df_long %>%
  ggplot(aes(x = attribute, y = mean_score, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(
    data = labels_plot,
    aes(label = label, y = y_pos, group = treatment),
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 5
  ) +
  theme_classic() +
  labs(x = "Attribute", y = "Panel Rating Mean score", title = "Treatment Comparison by Attribute") +
  scale_fill_manual(values = c("Cooked" = "#7b8156", "Dried" = "#B15137", "Roasted" = "#9d5316"))


(dashboard <- (p2 | p3) / p1)

dashboard + plot_annotation(
  tag_levels = 'A',
  title = 'Sensory and Purchase Evaluation of Chestnut-Enriched Pasta Treatments') +
  plot_layout(guides = 'collect')
