setwd("C:/Users/uqfpowe1/OneDrive - The University of Queensland/Documents/PhD/Writing Documents/Study_3-Perspectives_Study/r_scripts/clean_rscripts")


library(caret)
library(dplyr)
library(cowplot)


load("data/pos_sym_dat.rda")
load("data/neg_sym_dat.rda")
load("data/pos_asym_dat.rda")
load("data/neg_asym_dat.rda")
load("data/sml_opp_asym_dat.rda")
load("data/lge_opp_asym_dat.rda")

###### Figure 3 ##############

## A.) Including Priority effects; B.) Excluding Priority effects  

library(tidyr)

pos_sym_dat$int_type <- "Pos-Sym"
neg_sym_dat$int_type <- "Neg-Sym"
pos_asym_dat$int_type <- "Pos-Asym"
neg_asym_dat$int_type <- "Neg-Asym"
sml_opp_asym_dat$int_type <- "Sml-Opp-Asym"
lge_opp_asym_dat$int_type <- "Lge-Opp-Asym"

pos_sym_1_int <- pos_sym_dat %>%
  dplyr::select(true_sp1_coef, statemod_sp1_coef,  nullmod_sp1_coef, int_type) %>% 
  rename(true_int = true_sp1_coef) %>% 
  rename(statemod_int = statemod_sp1_coef) %>% 
  rename(nullmod_int = nullmod_sp1_coef)

pos_sym_2_int <- pos_sym_dat %>%
  dplyr::select(true_sp2_coef, statemod_sp2_coef,  nullmod_sp2_coef, int_type) %>% 
  rename(true_int = true_sp2_coef) %>% 
  rename(statemod_int = statemod_sp2_coef) %>% 
  rename(nullmod_int = nullmod_sp2_coef)

neg_sym_1_int <- neg_sym_dat %>%
  dplyr::select(true_sp1_coef, statemod_sp1_coef,  nullmod_sp1_coef, int_type) %>% 
  rename(true_int = true_sp1_coef) %>% 
  rename(statemod_int = statemod_sp1_coef) %>% 
  rename(nullmod_int = nullmod_sp1_coef)

neg_sym_2_int <- neg_sym_dat %>%
  dplyr::select(true_sp2_coef, statemod_sp2_coef,  nullmod_sp2_coef, int_type) %>% 
  rename(true_int = true_sp2_coef) %>% 
  rename(statemod_int = statemod_sp2_coef) %>% 
  rename(nullmod_int = nullmod_sp2_coef)

pos_asym_1_int <- pos_asym_dat %>%
  dplyr::select(true_sp1_coef, statemod_sp1_coef,  nullmod_sp1_coef, int_type) %>% 
  rename(true_int = true_sp1_coef) %>% 
  rename(statemod_int = statemod_sp1_coef) %>% 
  rename(nullmod_int = nullmod_sp1_coef)

pos_asym_2_int <- pos_asym_dat %>%
  dplyr::select(true_sp2_coef, statemod_sp2_coef,  nullmod_sp2_coef, int_type) %>% 
  rename(true_int = true_sp2_coef) %>% 
  rename(statemod_int = statemod_sp2_coef) %>% 
  rename(nullmod_int = nullmod_sp2_coef)

neg_asym_1_int <- neg_asym_dat %>%
  dplyr::select(true_sp1_coef, statemod_sp1_coef,  nullmod_sp1_coef, int_type) %>% 
  rename(true_int = true_sp1_coef) %>% 
  rename(statemod_int = statemod_sp1_coef) %>% 
  rename(nullmod_int = nullmod_sp1_coef)

neg_asym_2_int <- neg_asym_dat %>%
  dplyr::select(true_sp2_coef, statemod_sp2_coef,  nullmod_sp2_coef, int_type) %>% 
  rename(true_int = true_sp2_coef) %>% 
  rename(statemod_int = statemod_sp2_coef) %>% 
  rename(nullmod_int = nullmod_sp2_coef)

sml_opp_asym_1_int <- sml_opp_asym_dat %>%
  dplyr::select(true_sp1_coef, statemod_sp1_coef,  nullmod_sp1_coef, int_type) %>% 
  rename(true_int = true_sp1_coef) %>% 
  rename(statemod_int = statemod_sp1_coef) %>% 
  rename(nullmod_int = nullmod_sp1_coef)

sml_opp_asym_2_int <- sml_opp_asym_dat %>%
  dplyr::select(true_sp2_coef, statemod_sp2_coef,  nullmod_sp2_coef, int_type) %>% 
  rename(true_int = true_sp2_coef) %>% 
  rename(statemod_int = statemod_sp2_coef) %>% 
  rename(nullmod_int = nullmod_sp2_coef)

lge_opp_asym_1_int <- lge_opp_asym_dat %>%
  dplyr::select(true_sp1_coef, statemod_sp1_coef,  nullmod_sp1_coef, int_type) %>% 
  rename(true_int = true_sp1_coef) %>% 
  rename(statemod_int = statemod_sp1_coef) %>% 
  rename(nullmod_int = nullmod_sp1_coef)

lge_opp_asym_2_int <- lge_opp_asym_dat %>%
  dplyr::select(true_sp2_coef, statemod_sp2_coef,  nullmod_sp2_coef, int_type) %>% 
  rename(true_int = true_sp2_coef) %>% 
  rename(statemod_int = statemod_sp2_coef) %>% 
  rename(nullmod_int = nullmod_sp2_coef)

int_dat <- bind_rows(pos_sym_1_int, pos_sym_2_int, neg_sym_1_int, neg_sym_2_int, pos_asym_1_int, pos_asym_2_int, neg_asym_1_int, neg_asym_2_int, sml_opp_asym_1_int, sml_opp_asym_2_int, lge_opp_asym_1_int, lge_opp_asym_2_int)


colors <- c("#BBDD38" ,"#6CD05E",  "#007094", "#185086", "#422C70", "#4B0055")

int_dat$int_type <- factor(int_dat$int_type, levels = c("Pos-Sym", "Neg-Sym", "Pos-Asym", "Neg-Asym", "Sml-Opp-Asym", "Lge-Opp-Asym"))

# Including Priority Effects

statemod_int_plot <- ggplot(int_dat, aes(x = true_int, y = statemod_int, color = factor(int_type))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey", size = 1) +  # x = 0 line
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", size = 1) +  # y = 0 line
  geom_point(size = 2, alpha = 0.9) +
  labs(x = "True Interaction Effects", y = "Estimated Interaction Effects") +
  xlim(-2.5, 2.5) +
  ylim(-5, 2.5) +
  ggtitle("Including Priority Effects") + 
  theme_minimal() +
  theme(legend.position = "bottom") +  # Set legend position to bottom
  scale_color_manual(values = colors) +  # Set color values manually
  # Add a legend
  guides(color = guide_legend(title = "Interaction Type")) +
  theme(legend.title = element_text(face = "bold"))


# Excluding Priority Effects

nullmod_int_plot <- ggplot(int_dat, aes(x = true_int, y = nullmod_int, color = factor(int_type))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey", size = 1) +  # x = 0 line
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", size = 1) +  # y = 0 line
  geom_point(size = 2, alpha = 0.9) +
  labs(x = "True Interaction Effects", y = "") +
  xlim(-2.5, 2.5) +
  ylim(-5, 2.5) +
  ggtitle("Excluding Priority Effects") + 
  theme_minimal() +
  theme(legend.position = "bottom") +  # Set legend position to bottom
  scale_color_manual(values = colors) +  # Set color values manually
  # Add a legend
  guides(color = guide_legend(title = "Interaction Type")) +
  theme(legend.title = element_text(face = "bold"))


state_int_plot <- statemod_int_plot + theme(legend.position = "none")
null_int_plot <- nullmod_int_plot + theme(legend.position = "none")

combined_plots <- plot_grid(state_int_plot, null_int_plot, labels = c("a.)", "b.)"), ncol = 2)

legend_combined <- get_legend(state_int_plot + theme(legend.position = "bottom"))

final_plot <- plot_grid(combined_plots, legend_combined, ncol = 1, rel_heights = c(1, 0.2))



print(final_plot)


#### Figure 4 ####


pos_sym_dat$state_diff_true <- abs(pos_sym_dat$true_sp1_coef - pos_sym_dat$true_sp2_coef)
pos_sym_dat$state_diff_predicted <- abs(pos_sym_dat$statemod_sp1_coef - pos_sym_dat$statemod_sp2_coef)
pos_sym_dat$int_type <- "Pos-Sym"

neg_sym_dat$state_diff_true <- abs(neg_sym_dat$true_sp1_coef - neg_sym_dat$true_sp2_coef)
neg_sym_dat$state_diff_predicted <- abs(neg_sym_dat$statemod_sp1_coef - neg_sym_dat$statemod_sp2_coef)
neg_sym_dat$int_type <- "Neg-Sym"

pos_asym_dat$state_diff_true <- abs(pos_asym_dat$true_sp1_coef - pos_asym_dat$true_sp2_coef)
pos_asym_dat$state_diff_predicted <- abs(pos_asym_dat$statemod_sp1_coef - pos_asym_dat$statemod_sp2_coef)
pos_asym_dat$int_type <- "Pos-Asym"

neg_asym_dat$state_diff_true <- abs(neg_asym_dat$true_sp1_coef - neg_asym_dat$true_sp2_coef)
neg_asym_dat$state_diff_predicted <- abs(neg_asym_dat$statemod_sp1_coef - neg_asym_dat$statemod_sp2_coef)
neg_asym_dat$int_type <- "Neg-Asym"

sml_opp_asym_dat$state_diff_true <- abs(sml_opp_asym_dat$true_sp1_coef - sml_opp_asym_dat$true_sp2_coef)
sml_opp_asym_dat$state_diff_predicted <- abs(sml_opp_asym_dat$statemod_sp1_coef - sml_opp_asym_dat$statemod_sp2_coef)
sml_opp_asym_dat$int_type <- "Sml-Opp-Asym"

lge_opp_asym_dat$state_diff_true <- abs(lge_opp_asym_dat$true_sp1_coef - lge_opp_asym_dat$true_sp2_coef)
lge_opp_asym_dat$state_diff_predicted <- abs(lge_opp_asym_dat$statemod_sp1_coef - lge_opp_asym_dat$statemod_sp2_coef)
lge_opp_asym_dat$int_type <- "Lge-Opp-Asym"


# Define colors for each group
colors <- c("#BBDD38" ,"#6CD05E",  "#007094", "#185086", "#422C70", "#4B0055")

# Combine the data frames
state_combined_df <- rbind(pos_sym_dat, neg_sym_dat, pos_asym_dat, neg_asym_dat, sml_opp_asym_dat, lge_opp_asym_dat)
state_combined_df$int_type <- factor(state_combined_df$int_type, levels = c("Pos-Sym", "Neg-Sym", "Pos-Asym", "Neg-Asym", "Sml-Opp-Asym", "Lge-Opp-Asym"))

# Assign colors based on the group
#group_colors <- colors[rep(1:length(colors), each = nrow(state_combined_df) / length(colors))]


state_plot <- ggplot(state_combined_df, aes(x = state_diff_true, y = state_diff_predicted, color = factor(int_type))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_point(size = 2, alpha = 0.9) +
  labs(x = "True Relative Difference", y = "Estimated Relative Difference") +
  xlim(0, 4.5) +
  ylim(0, 4.5) +
  ggtitle("Including Priority Effects") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Set legend position to bottom
  scale_color_manual(values = colors) +  # Set color values manually
  
  # Add a legend
  guides(color = guide_legend(title = "Interaction Type")) +
  theme(legend.title = element_text(face = "bold"))


### Line from GLM NOT including state info: true x axis and predicted on y axis 


pos_sym_dat$null_diff_true <- abs(pos_sym_dat$true_sp1_coef - pos_sym_dat$true_sp2_coef)
pos_sym_dat$null_diff_predicted <- abs(pos_sym_dat$nullmod_sp1_coef - pos_sym_dat$nullmod_sp2_coef)

neg_sym_dat$null_diff_true <- abs(neg_sym_dat$true_sp1_coef - neg_sym_dat$true_sp2_coef)
neg_sym_dat$null_diff_predicted <- abs(neg_sym_dat$nullmod_sp1_coef - neg_sym_dat$nullmod_sp2_coef)

pos_asym_dat$null_diff_true <- abs(pos_asym_dat$true_sp1_coef - pos_asym_dat$true_sp2_coef)
pos_asym_dat$null_diff_predicted <- abs(pos_asym_dat$nullmod_sp1_coef - pos_asym_dat$nullmod_sp2_coef)

neg_asym_dat$null_diff_true <- abs(neg_asym_dat$true_sp1_coef - neg_asym_dat$true_sp2_coef)
neg_asym_dat$null_diff_predicted <- abs(neg_asym_dat$nullmod_sp1_coef - neg_asym_dat$nullmod_sp2_coef)

sml_opp_asym_dat$null_diff_true <- abs(sml_opp_asym_dat$true_sp1_coef - sml_opp_asym_dat$true_sp2_coef)
sml_opp_asym_dat$null_diff_predicted <- abs(sml_opp_asym_dat$nullmod_sp1_coef - sml_opp_asym_dat$nullmod_sp2_coef)

lge_opp_asym_dat$null_diff_true <- abs(lge_opp_asym_dat$true_sp1_coef - lge_opp_asym_dat$true_sp2_coef)
lge_opp_asym_dat$null_diff_predicted <- abs(lge_opp_asym_dat$nullmod_sp1_coef - lge_opp_asym_dat$nullmod_sp2_coef)


null_combined_df <- rbind(pos_sym_dat, neg_sym_dat,pos_asym_dat, neg_asym_dat, sml_opp_asym_dat, lge_opp_asym_dat)
null_combined_df$int_type <- factor(null_combined_df$int_type, levels = c("Pos-Sym", "Neg-Sym", "Pos-Asym", "Neg-Asym", "Sml-Opp-Asym", "Lge-Opp-Asym"))

# Plot the scatter plot with different colors for each group
null_plot <- ggplot(null_combined_df, aes(x = null_diff_true, y = null_diff_predicted, color = factor(int_type))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", size = 1) +
  geom_point(size = 2, alpha = 0.9) +
  labs(x = "True Relative Difference", y = "") +
  xlim(0, 4.5) +
  ylim(0, 4.5) +
  ggtitle("Excluding Priority Effects") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Set legend position to bottom
  scale_color_manual(values = colors) +  # Set color values manually
  
  # Add a legend
  guides(color = guide_legend(title = "Interaction Type")) +
  theme(legend.title = element_text(face = "bold"))


state_plot <- state_plot + theme(legend.position = "none")
null_plot <- null_plot + theme(legend.position = "none")

combined_plots <- plot_grid(state_plot, null_plot, labels = c("a.)", "b.)"), ncol = 2)

# Add a common legend outside the plots to the right
legend_combined <- get_legend(state_plot + theme(legend.position = "bottom"))

# Combine the plots and legend
final_plot <- plot_grid(combined_plots, legend_combined, ncol = 1, rel_heights = c(1, 0.2))

# Display the fiNeg-Asyml plot
print(final_plot)


###### Figure 5 #########

pos_sym_dat$int_type <- "Pos-Sym"
neg_sym_dat$int_type <- "Neg-Sym"
pos_asym_dat$int_type <- "Pos-Asym"
neg_asym_dat$int_type <- "Neg-Asym"
sml_opp_asym_dat$int_type <- "Sml-Opp-Asym"
lge_opp_asym_dat$int_type <- "Lge-Opp-Asym"

statemod_brier <- c(pos_sym_dat$mean_statemod_brier, 
                    neg_sym_dat$mean_statemod_brier, 
                    pos_asym_dat$mean_statemod_brier, 
                    neg_asym_dat$mean_statemod_brier, 
                    sml_opp_asym_dat$mean_statemod_brier, 
                    lge_opp_asym_dat$mean_statemod_brier)

nullmod_brier <- c(pos_sym_dat$mean_nullmod_brier, 
                   neg_sym_dat$mean_nullmod_brier, 
                   pos_asym_dat$mean_nullmod_brier,
                   neg_asym_dat$mean_nullmod_brier, 
                   sml_opp_asym_dat$mean_nullmod_brier,  
                   lge_opp_asym_dat$mean_nullmod_brier)

int_type <- c(pos_sym_dat$int_type, 
              neg_sym_dat$int_type, 
              pos_asym_dat$int_type, 
              neg_asym_dat$int_type, 
              sml_opp_asym_dat$int_type,
              lge_opp_asym_dat$int_type)

brier_scores <- data.frame(statemod <- statemod_brier, nullmod <- nullmod_brier, int_type = int_type)

# Define colors for each group
colors <- c("#BBDD38" ,"#6CD05E",  "#007094", "#185086", "#422C70", "#4B0055")
brier_scores$int_type <- factor(brier_scores$int_type, levels = c("Pos-Sym", "Neg-Sym", "Pos-Asym", "Neg-Asym", "Sml-Opp-Asym", "Lge-Opp-Asym"))


brier_plot <- ggplot(brier_scores, aes(x = nullmod, y = statemod, color = factor(int_type))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_point(size = 2, alpha = 0.9) +
  labs(x = "Excluding Priority Effects", y = "Including Priority Effects") +
  xlim(0.00, 0.12) +
  ylim(0.00, 0.12) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Set legend position to bottom
  scale_color_manual(values = colors) +  # Set color values manually
    # Add a legend
  guides(color = guide_legend(title = "Interaction Type")) +
  theme(legend.title = element_text(face = "bold"))
# Add the annotation
brier_plot <- brier_plot +
  annotate("label", x = 0.03, y = 0.08, label = "Better predictive performance\nby co-occurrence model") +
  annotate("label", x = 0.09, y = 0.04, label = "Better predictive performance\nby priority effects model") 

# Show the plot
print(brier_plot)

