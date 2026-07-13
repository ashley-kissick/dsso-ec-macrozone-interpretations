library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
if(!require(patchwork)) install.packages("patchwork")
library(patchwork)



# 3. Perform Global Ordination (PCA)
pca_matrix <- all_observations %>% select(starts_with("Feature_"))
pca_result <- prcomp(pca_matrix, scale. = TRUE)
var_explained <- round(100 * (pca_result$sdev^2 / sum(pca_result$sdev^2)), 1)

ordination_df <- as.data.frame(pca_result$x) %>%
  bind_cols(all_observations %>% select(Data_Type, Group_ID))








#Approach 1
#Within overall space, how are the clusters of zones distributed?

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


# ==========================================
# 4. ISOLATE A SINGLE TARGET GROUP
# ==========================================
# Change "Group 03" to whichever of your 14 groups you want to inspect
target_group_name <- "cluster_6"

ordination_df <- as.data.frame(pca_result$x) %>%
  bind_cols(all_observations %>% select(Data_Type, Group_ID, Subject_ID))

# Extract individual points belonging to ONLY this group
single_group_individuals <- ordination_df %>%
  filter(Group_ID == target_group_name)

# Calculate the precise centroid (multivariate mean) for this single group
single_centroid <- single_group_individuals %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2))

# Calculate the univariate standard deviations for the crosshair axis markings
single_sd <- single_group_individuals %>%
  summarise(sd_PC1 = sd(PC1), sd_PC2 = sd(PC2))




# ==========================================
# 4. ISOLATE A SINGLE TARGET GROUP
# ==========================================


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
group_loadings <- target_group_individuals

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
  
  # Layer 4: Plot individual observations
  geom_point(data = single_centroid,
             aes(x = PC1, y = PC2), color = "black", fill = "white", shape = 21, size = 4.5, stroke = 1.5) +
  
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
#group_features_long <- group_features_long %>%
#  mutate(Short_ID = gsub(".*_Ind_", "I_", Subject_ID))

group_features_long$Short_ID <- group_features_long$Subject_ID



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
group_features_long$Short_ID <- group_features_long$Subject_ID
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
