
#Approach 1
#Within overall space, how are the clusters of zones distributed?

# 1. Load core packages
library(ggplot2)
library(dplyr)

# 2. Generate sample data (Reuse previous structure for continuity)
set.seed(42)
num_background_points <- 300
num_groups <- 14
points_per_group <- 15
group_list <- list()

# Global Background
global_background <- data.frame(
  Feature_A = runif(num_background_points, min = 0, max = 100),
  Feature_B = runif(num_background_points, min = 10, max = 50),
  Feature_C = runif(num_background_points, min = 0, max = 1),
  Feature_D = runif(num_background_points, min = 1000, max = 5000),
  Data_Type = "Global Background Context", Group_ID  = "None"
)

# 14 Sub-groups
for (i in 1:num_groups) {
  center_shift_A <- runif(1, min = 15, max = 85)
  center_shift_B <- runif(1, min = 15, max = 45)
  group_list[[i]] <- data.frame(
    Feature_A = rnorm(points_per_group, mean = center_shift_A, sd = 5),
    Feature_B = rnorm(points_per_group, mean = center_shift_B, sd = 3),
    Feature_C = runif(points_per_group, min = 0.2, max = 0.8),
    Feature_D = runif(points_per_group, min = 1500, max = 4500),
    Data_Type = "Group Observation", Group_ID  = paste("Group", sprintf("%02d", i))
  )
}
all_observations <- bind_rows(global_background, do.call(rbind, group_list))

# 3. Perform Global Ordination (PCA)
pca_matrix <- all_observations %>% select(starts_with("Feature_"))
pca_result <- prcomp(pca_matrix, scale. = TRUE)
var_explained <- round(100 * (pca_result$sdev^2 / sum(pca_result$sdev^2)), 1)

ordination_df <- as.data.frame(pca_result$x) %>%
  bind_cols(all_observations %>% select(Data_Type, Group_ID))

# Extract Group Observations (needed to draw the variance ellipses)
group_observations <- ordination_df %>% filter(Data_Type == "Group Observation")

# Collapse into Centroids (The 14 points)
group_centroids <- group_observations %>%
  group_by(Group_ID) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")

# 4. Generate the Visualization with Multivariate Variance Ellipses
ggplot() +
  # Layer 1: Global background context cloud
  geom_point(data = filter(ordination_df, Data_Type == "Global Background Context"),
             aes(x = PC1, y = PC2), color = "gray90", alpha = 0.4, size = 1.5) +

  # Layer 2: MULTIVARIATE VARIANCE ELLIPSES
  # type = "norm" assumes a multivariate normal distribution.
  # level = 0.68 approximates 1 Standard Deviation in multivariate space.
  # Change level = 0.95 for a 95% Confidence Interval (Standard Error equivalent).
  stat_ellipse(data = group_observations,
               aes(x = PC1, y = PC2, color = Group_ID, fill = Group_ID),
               type = "norm", level = 0.68, geom = "polygon", alpha = 0.05, size = 0.5) +

  # Layer 3: Plot the 14 Group Centroids
  geom_point(data = group_centroids,
             aes(x = PC1, y = PC2, color = Group_ID), size = 4, stroke = 1) +

  # Layer 4: Text Labels for Centroids
  geom_text(data = group_centroids,
            aes(x = PC1, y = PC2, label = Group_ID),
            vjust = -1.2, size = 3, fontface = "bold", show.legend = FALSE) +

  # Fine-tune scales and canvas styling
  labs(
    title = "Multivariate Ordination with Data Ellipses",
    subtitle = "Centroids overlayed with 1-Standard-Deviation (68%) multivariate ellipses",
    x = paste0("PC1 (", var_explained, "% Variance)"),
    y = paste0("PC2 (", var_explained, "% Variance)"),
    color = "Sub-Groups", fill = "Sub-Groups"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", panel.grid.minor = element_blank())










#Approach 2
#For an individual cluster, how are the individual zones distributed?
# 1. Load core packages
library(ggplot2)
library(dplyr)

# 2. Generate sample data (Re-using the exact generation architecture for consistency)
set.seed(42)
num_background_points <- 300
num_groups <- 14
points_per_group <- 20 # Increased slightly to see individual point distribution better
group_list <- list()

# Global Background
global_background <- data.frame(
  Feature_A = runif(num_background_points, min = 0, max = 100),
  Feature_B = runif(num_background_points, min = 10, max = 50),
  Feature_C = runif(num_background_points, min = 0, max = 1),
  Feature_D = runif(num_background_points, min = 1000, max = 5000),
  Data_Type = "Global Background Context", Group_ID  = "None"
)

# 14 Sub-groups
for (i in 1:num_groups) {
  center_shift_A <- runif(1, min = 15, max = 85)
  center_shift_B <- runif(1, min = 15, max = 45)
  group_list[[i]] <- data.frame(
    Feature_A = rnorm(points_per_group, mean = center_shift_A, sd = 6),
    Feature_B = rnorm(points_per_group, mean = center_shift_B, sd = 4),
    Feature_C = runif(points_per_group, min = 0.2, max = 0.8),
    Feature_D = runif(points_per_group, min = 1500, max = 4500),
    Data_Type = "Group Observation", Group_ID  = paste("Group", sprintf("%02d", i))
  )
}
all_observations <- bind_rows(global_background, do.call(rbind, group_list))

# 3. Perform Global Ordination (PCA)
pca_matrix <- all_observations %>% select(starts_with("Feature_"))
pca_result <- prcomp(pca_matrix, scale. = TRUE)
var_explained <- round(100 * (pca_result$sdev^2 / sum(pca_result$sdev^2)), 1)

ordination_df <- as.data.frame(pca_result$x) %>%
  bind_cols(all_observations %>% select(Data_Type, Group_ID))

# ==========================================
# 4. ISOLATE A SINGLE TARGET GROUP
# ==========================================
# Change "Group 03" to whichever of your 14 groups you want to inspect
target_group_name <- "Group 03"

# Extract individual points belonging to ONLY this group
single_group_individuals <- ordination_df %>%
  filter(Group_ID == target_group_name)

# Calculate the precise centroid (multivariate mean) for this single group
single_centroid <- single_group_individuals %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2))

# Calculate the univariate standard deviations for the crosshair axis markings
single_sd <- single_group_individuals %>%
  summarise(sd_PC1 = sd(PC1), sd_PC2 = sd(PC2))

# 5. Generate the Single-Group Covariance Plot
ggplot() +
  # Layer 1: Faded global background cloud to preserve total min/max reference bounds
  geom_point(data = filter(ordination_df, Data_Type == "Global Background Context"),
             aes(x = PC1, y = PC2), color = "gray92", alpha = 0.5, size = 1.5) +

  # Layer 2: Covariance Matrix Ellipse (Spread container)
  # level = 0.68 captures roughly 1 Standard Deviation of multivariate space
  stat_ellipse(data = single_group_individuals,
               aes(x = PC1, y = PC2), type = "norm", level = 0.68,
               geom = "polygon", fill = "#E41A1C", color = "#E41A1C", alpha = 0.1, size = 0.8) +

  # Layer 3: Univariate Variance Crosshairs (SD bars intersecting at the centroid)
  geom_segment(aes(x = single_centroid$PC1 - single_sd$sd_PC1, xend = single_centroid$PC1 + single_sd$sd_PC1,
                   y = single_centroid$PC2, yend = single_centroid$PC2),
               color = "#E41A1C", size = 0.6, linetype = "dashed") +
  geom_segment(aes(x = single_centroid$PC1, xend = single_centroid$PC1,
                   y = single_centroid$PC2 - single_sd$sd_PC2, yend = single_centroid$PC2 + single_sd$sd_PC2),
               color = "#E41A1C", size = 0.6, linetype = "dashed") +

  # Layer 4: Plot the INDIVIDUAL points of this group
  geom_point(data = single_group_individuals,
             aes(x = PC1, y = PC2), color = "#E41A1C", size = 3, alpha = 0.8) +

  # Layer 5: Plot the Central Centroid Point
  geom_point(data = single_centroid,
             aes(x = PC1, y = PC2), color = "black", fill = "white", shape = 21, size = 4.5, stroke = 1.5) +

  # Canvas settings and styling labels
  labs(
    title = paste("Detailed Covariance View:", target_group_name),
    subtitle = "Individual points, centroid, and 1-SD multivariate covariance ellipse",
    x = paste0("PC1 (", var_explained[1], "% Variance)"),
    y = paste0("PC2 (", var_explained[2], "% Variance)")
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())








#Approach 3
#For an individual cluster, how are the individual zones distributed?
#This adds the labels for each zone

# 1. Load core packages (and ggrepel for smart text layout)
library(ggplot2)
library(dplyr)
library(ggrepel)

# 2. Generate sample data with unique individual subject tags
set.seed(42)
num_background_points <- 300
num_groups <- 14
points_per_group <- 15
group_list <- list()

# Global Background Context
global_background <- data.frame(
  Feature_A = runif(num_background_points, min = 0, max = 100),
  Feature_B = runif(num_background_points, min = 10, max = 50),
  Feature_C = runif(num_background_points, min = 0, max = 1),
  Feature_D = runif(num_background_points, min = 1000, max = 5000),
  Data_Type = "Global Background Context",
  Group_ID  = "None",
  Subject_ID = "None"
)

# 14 Sub-groups with tagged individuals (e.g., "G03_Ind_01")
for (i in 1:num_groups) {
  center_shift_A <- runif(1, min = 15, max = 85)
  center_shift_B <- runif(1, min = 15, max = 45)

  group_name <- paste("Group", sprintf("%02d", i))

  group_list[[i]] <- data.frame(
    Feature_A = rnorm(points_per_group, mean = center_shift_A, sd = 6),
    Feature_B = rnorm(points_per_group, mean = center_shift_B, sd = 4),
    Feature_C = runif(points_per_group, min = 0.2, max = 0.8),
    Feature_D = runif(points_per_group, min = 1500, max = 4500),
    Data_Type = "Group Observation",
    Group_ID  = group_name,
    Subject_ID = paste0("G", sprintf("%02d", i), "_Ind_", sprintf("%02d", 1:points_per_group))
  )
}
all_observations <- bind_rows(global_background, do.call(rbind, group_list))

# 3. Perform Global Ordination (PCA)
pca_matrix <- all_observations %>% select(starts_with("Feature_"))
pca_result <- prcomp(pca_matrix, scale. = TRUE)
var_explained <- round(100 * (pca_result$sdev^2 / sum(pca_result$sdev^2)), 1)

ordination_df <- as.data.frame(pca_result$x) %>%
  bind_cols(all_observations %>% select(Data_Type, Group_ID, Subject_ID))

# ==========================================
# 4. ISOLATE A SINGLE TARGET GROUP
# ==========================================
target_group_name <- "Group 03"

single_group_individuals <- ordination_df %>%
  filter(Group_ID == target_group_name)

single_centroid <- single_group_individuals %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2))

single_sd <- single_group_individuals %>%
  summarise(sd_PC1 = sd(PC1), sd_PC2 = sd(PC2))

# 5. Generate the Labeled Covariance Plot
ggplot() +
  # Layer 1: Faded global background cloud
  geom_point(data = filter(ordination_df, Data_Type == "Global Background Context"),
             aes(x = PC1, y = PC2), color = "gray92", alpha = 0.5, size = 1.5) +

  # Layer 2: Covariance Matrix Ellipse (1 Standard Deviation Container)
  stat_ellipse(data = single_group_individuals,
               aes(x = PC1, y = PC2), type = "norm", level = 0.68,
               geom = "polygon", fill = "#E41A1C", color = "#E41A1C", alpha = 0.1, size = 0.8) +

  # Layer 3: Univariance crosshairs
  geom_segment(aes(x = single_centroid$PC1 - single_sd$sd_PC1, xend = single_centroid$PC1 + single_sd$sd_PC1,
                   y = single_centroid$PC2, yend = single_centroid$PC2),
               color = "#E41A1C", size = 0.6, linetype = "dashed") +
  geom_segment(aes(x = single_centroid$PC1, xend = single_centroid$PC1,
                   y = single_centroid$PC2 - single_sd$sd_PC2, yend = single_centroid$PC2 + single_sd$sd_PC2),
               color = "#E41A1C", size = 0.6, linetype = "dashed") +

  # Layer 4: Plot individual observations
  geom_point(data = single_group_individuals,
             aes(x = PC1, y = PC2), color = "#E41A1C", size = 3, alpha = 0.8) +

  # Layer 5: SMART TEXT LABELS FOR INDIVIDUALS
  # geom_text_repel pushes text tags away from points to avoid overlaps
  geom_text_repel(data = single_group_individuals,
                  aes(x = PC1, y = PC2, label = Subject_ID),
                  size = 3.5, fontface = "bold", color = "gray20",
                  box.padding = 0.4, point.padding = 0.3,
                  segment.color = "gray60", segment.size = 0.4) +

  # Layer 6: Center Centroid Marker
  geom_point(data = single_centroid,
             aes(x = PC1, y = PC2), color = "black", fill = "white", shape = 21, size = 4.5, stroke = 1.5) +

  # Canvas settings and styling
  labs(
    title = paste("Detailed Covariance View:", target_group_name),
    subtitle = "Individual labeled points mapped inside 1-SD multivariate spread",
    x = paste0("PC1 (", var_explained, "% Variance)"),
    y = paste0("PC2 (", var_explained, "% Variance)")
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())






#Approach 4
#This was one attempt to see how different individuals are with respect to features.
#Interesting, but not necessarily so informative.

# 1. Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# =========================================================================
# 2. ISOLATE THE TARGET GROUP FEATURES (Using raw, unscaled feature data)
# =========================================================================
# Grab the original unscaled features for your isolated group
# We use the raw feature values so our final text description makes physical sense
raw_group_features <- all_observations %>%
  filter(Group_ID == target_group_name) %>%
  select(Subject_ID, starts_with("Feature_"))

# =========================================================================
# 3. IDENTIFY THE MOST SIMILAR PAIR OF INDIVIDUALS
# =========================================================================
# Use our PCA coordinates to find the pair closest together in the ordination space
group_coords <- single_group_individuals %>%
  select(PC1, PC2) %>%
  `rownames<-`(single_group_individuals$Subject_ID)

dist_matrix <- as.matrix(dist(group_coords))
diag(dist_matrix) <- Inf # Ignore self-distance zeros

# Find the exact row/column indices of the absolute minimum distance
closest_pair_idx <- which(dist_matrix == min(dist_matrix), arr.ind = TRUE)
ind_A <- rownames(dist_matrix)[closest_pair_idx[1, 1]]
ind_B <- colnames(dist_matrix)[closest_pair_idx[1, 2]]

# =========================================================================
# 4. CALCULATE INDIVIDUAL FEATURE DIFFERENCES & PCA INFLUENCE
# =========================================================================
# Extract raw feature profiles for these two most similar individuals
profile_A <- raw_group_features %>% filter(Subject_ID == ind_A) %>% select(-Subject_ID)
profile_B <- raw_group_features %>% filter(Subject_ID == ind_B) %>% select(-Subject_ID)

# Absolute difference shows the raw magnitude of variation
raw_diffs <- abs(profile_A - profile_B)

# To find what 'differentiates' them in the ordination space, we multiply
# their normalized differences by the feature loading weights from the PCA.
# This reveals which feature holds the most mathematical leverage between them.
pca_loadings <- as.data.frame(pca_result$rotation) %>%
  select(PC1, PC2) %>%
  mutate(Feature = rownames(.))

# Calculate a "Differentiation Score" based on total loading impact across PC1 & PC2
differentiation_analysis <- pca_loadings %>%
  mutate(
    Raw_Value_Ind_A = as.numeric(profile_A[1, Feature]),
    Raw_Value_Ind_B = as.numeric(profile_B[1, Feature]),
    Absolute_Difference = abs(Raw_Value_Ind_A - Raw_Value_Ind_B)
  ) %>%
  # Weight the difference by how important the feature is to the overall ordination space
  mutate(Space_Differentiation_Score = Absolute_Difference * (abs(PC1) + abs(PC2))) %>%
  arrange(desc(Space_Differentiation_Score))

# Identify the #1 feature that differentiates them the most
top_differentiator <- differentiation_analysis$Feature[1]

# =========================================================================
# 5. PRINT THE NATURAL LANGUAGE MULTIVARIATE DESCRIPTION
# =========================================================================
cat("\n========================================================================\n",
    "MULTIVARIATE SIMILARITY DESCRIPTION FOR:", target_group_name, "\n",
    "========================================================================\n",
    "• In this multivariate space, '", ind_A, "' and '", ind_B, "' are the MOST SIMILAR individuals.\n",
    "• Although they cluster tightly together, what primarily differentiates them is '", top_differentiator, "'.\n\n",
    "Breakdown of feature variations between them (sorted by space impact):\n", sep="")
print(differentiation_analysis %>% select(Feature, Raw_Value_Ind_A, Raw_Value_Ind_B, Absolute_Difference, Space_Differentiation_Score))

# =========================================================================
# 6. VISUALIZE THE FEATURE-BY-FEATURE DIFFERENTIATION PROFILE
# =========================================================================
ggplot(differentiation_analysis, aes(x = reorder(Feature, Space_Differentiation_Score), y = Space_Differentiation_Score, fill = Space_Differentiation_Score)) +
  geom_col(color = "white", width = 0.6) +
  coord_flip() + # Flip for easier reading of feature names
  scale_fill_gradient(low = "#377EB8", high = "#E41A1C", name = "Differentiation\nImpact") +
  labs(
    title = paste("What Differentiates the Most Similar Pair?"),
    subtitle = paste("Comparison between", ind_A, "and", ind_B, "within the Ordination Space"),
    x = "Original Features",
    y = "Ordination Differentiation Score (Difference × PCA Loading Weight)"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())









#Approach 5
#This plots the cluster with zones more like an ordination plot.
#To the right, it gives an indication of how different each feature is for the given individual.

#Interpretation:
#Because we scaled the features locally within this specific group from 0
#(absolute minimum value in the group) to 1 (absolute maximum value in the group),
#the color gradient serves as an individual fingerprint comparison matrix:

#🔴 Dark Red / Coral Blocks (High Values / Top of Scale):  This means that an
#individual has a very high value for that specific feature compared to the
#rest of their group. For instance, if an individual's cell under Feature_A
#is deep red, they possess the highest amount of Feature_A among all 15 members.

#⚪ White Blocks (The Group Median / Average Spectrum):This signifies that the
#individual sits right in the middle of the pack (0.5 on the relative scale)
#for that specific variable. They are perfectly baseline and average for that
#feature.

#🔵 Dark Blue / Navy Blocks (Low Values / Bottom of Scale):  This indicates a
#very low value for that specific feature relative to the group. A deep blue
#tile under Feature_D means that individual has nearly the lowest recorded
#measurement for that feature in this cluster.



# 1. Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# patchwork is excellent for sticking two plots together side-by-side smoothly
if(!require(patchwork)) install.packages("patchwork")
library(patchwork)

# =========================================================================
# 2. ISOLATE GROUP OBSERVATIONS & NORMALIZE FEATURES FOR VISUAL COMPARISON
# =========================================================================
# Filter our master ordination dataset down to just our single group of interest
target_group_individuals <- ordination_df %>%
  filter(Group_ID == target_group_name)

# Extract raw feature columns for these specific individuals
group_raw_features <- all_observations %>%
  filter(Group_ID == target_group_name) %>%
  select(Subject_ID, starts_with("Feature_"))

# Standardize features (0 to 1) ONLY within this group so we can see
# relative differences at a glance (e.g., who has the highest/lowest Feature_A)
group_features_long <- group_raw_features %>%
  mutate(across(starts_with("Feature_"), ~ (. - min(.)) / (max(.) - min(.)))) %>%
  pivot_longer(cols = starts_with("Feature_"), names_to = "Feature", values_to = "Relative_Value")

# =========================================================================
# 3. PLOT A: THE ORDINATION SPACE (Where they sit & what vectors drive them)
# =========================================================================
# Isolate loading directions matching our global PCA for context arrows
group_loadings <- loadings_df

plot_ordination <- ggplot() +
  # Draw the covariance container envelope
  stat_ellipse(data = target_group_individuals, aes(x = PC1, y = PC2),
               type = "norm", level = 0.68, geom = "polygon", fill = "#E41A1C", alpha = 0.08) +
  # Plot individuals as points
  geom_point(data = target_group_individuals, aes(x = PC1, y = PC2),
             color = "#E41A1C", size = 3.5) +
  # Label the points with their Subject IDs
  geom_text(data = target_group_individuals, aes(x = PC1, y = PC2, label = gsub(".*_Ind_", "I_", Subject_ID)),
            vjust = -1.2, fontface = "bold", size = 3, color = "gray20") +
  # Overlay feature directional vectors to show WHAT drives points to their locations
  geom_segment(data = group_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.15, "cm")), color = "black", alpha = 0.6, size = 0.6) +
  geom_text(data = group_loadings, aes(x = PC1 * 1.2, y = PC2 * 1.2, label = Feature),
            color = "black", fontface = "italic", size = 3.5) +
  labs(
    title = paste("Positions in Multivariate Space"),
    x = "PC1 Coordinate", y = "PC2 Coordinate"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# =========================================================================
# 4. PLOT B: THE PROFILE GRID (What distinguishes them at-a-glance)
# =========================================================================
# Simplify the text tags on the axis to save space ("I_01" instead of "Group_03_Ind_01")
group_features_long <- group_features_long %>%
  mutate(Short_ID = gsub(".*_Ind_", "I_", Subject_ID))

plot_profiles <- ggplot(group_features_long, aes(x = Feature, y = Short_ID, fill = Relative_Value)) +
  geom_tile(color = "white", size = 0.4) +
  # High value = strong expression of feature, Low value = weak expression
  scale_fill_gradient2(low = "#377EB8", mid = "white", high = "#E41A1C", midpoint = 0.5,
                       name = "Relative Scale\n(Min to Max)") +
  labs(
    title = "Distinguishing Feature Profiles",
    x = "Original Features", y = "Individual ID"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, fontface = "bold"),
    axis.text.y = element_text(fontface = "bold"),
    panel.grid = element_blank()
  )

# =========================================================================
# 5. COMBINE BOTH VISUALS INTO A SINGLE FIGURE
# =========================================================================
# Use patchwork syntax to bind them side-by-side with a master title layout
combined_figure <- (plot_ordination | plot_profiles) +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(
    title = paste("At-a-Glance Differentiation Blueprint:", target_group_name),
    subtitle = "Left: Spatial location determined by features | Right: Underlying fingerprint explaining differences",
    theme = theme(plot_title = element_text(size = 16, face = "bold"))
  )

# Render the layout canvas
print(combined_figure)









# 1. Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# =========================================================================
# 2. PERFORM SPATIAL SORTING USING HIERARCHICAL CLUSTERING
# =========================================================================
# Extract ordination coordinates for our target group
spatial_coords <- target_group_individuals %>%
  select(PC1, PC2) %>%
  `rownames<-`(target_group_individuals$Subject_ID)

# Calculate spatial distances and cluster them
spatial_dist <- dist(spatial_coords, method = "euclidean")
spatial_cluster <- hclust(spatial_dist, method = "complete")

# Extract the sorted order of Short IDs (e.g., "I_03", "I_01")
sorted_subject_order <- spatial_cluster$labels[spatial_cluster$order]
sorted_short_order <- gsub(".*_Ind_", "I_", sorted_subject_order)

# Update our long dataframe and force the Short_ID to follow this exact order
group_features_long <- group_features_long %>%
  mutate(Short_ID = factor(Short_ID, levels = sorted_short_order))

# =========================================================================
# 3. RE-GENERATE THE PROFILE GRID PLOT (Sorted by Space)
# =========================================================================
plot_profiles_sorted <- ggplot(group_features_long, aes(x = Feature, y = Short_ID, fill = Relative_Value)) +
  geom_tile(color = "white", size = 0.4) +
  # Using a diverging red-to-blue palette (RdBu) for easier visual reading
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       name = "Relative Value\n(Within Group)",
                       guide = guide_colorbar(barwidth = 1.5, barheight = 10)) +
  labs(
    title = "Distinguishing Feature Profiles",
    subtitle = "Stacked by spatial neighborhood proximity",
    x = "Original Features", y = "Individual ID (Clustered)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, fontface = "bold"),
    axis.text.y = element_text(fontface = "bold"),
    panel.grid = element_blank()
  )

# Combine both plots side-by-side using patchwork
combined_figure_sorted <- (plot_ordination | plot_profiles_sorted) +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(
    title = paste("Spatially Ordered Differentiation Blueprint:", target_group_name),
    subtitle = "The row grid layout now matches spatial clusters from the ordination space.",
    theme = theme(plot_title = element_text(size = 16, face = "bold"))
  )

print(combined_figure_sorted)
