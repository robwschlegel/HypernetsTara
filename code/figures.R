# code/figures.R
# It does what it says on the tin


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Plotting functions ------------------------------------------------------

# Plot data based on wavelength group
# TODO: Consider increasing all text sizes
plot_matchup_nm <- function(df, var_name, sensor_X, sensor_Y){
  
  # TODO: Convert this into a functions that takes a single sensor as input
  # It will correct all ins itu and satellite sensor short codes to pretty names for plotting
  # Create sensor labels
  if(sensor_X == "HYPERNETS"){
    sensor_X_col <- "Hyp"
    sensor_X_lab <- "HypStar"
  } else {
    sensor_X_col <- sensor_X
    sensor_X_lab <- sensor_X
  }
  if(sensor_Y == "HYPERNETS"){
    sensor_Y_col <- "Hyp"
    sensor_X_lab <- "HypStar"
  } else {
    sensor_Y_col <- sensor_Y
    sensor_Y_lab <- sensor_Y
  }
  
  # Get max values
  max_X <- max(df[sensor_X_col], na.rm = TRUE)
  max_Y <- max(df[sensor_Y_col], na.rm = TRUE)
  max_axis <- max(max_X, max_Y)
  
  # Plot
  df |> 
    ggplot(aes_string(x = sensor_X_col, y = sensor_Y_col)) +
    geom_point(aes(colour = wavelength_group)) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = paste(sensor_X_lab, "vs", sensor_Y_lab),
         x = paste(var_name, sensor_X_lab),
         y = paste(var_name, sensor_Y_lab),
         colour = "Wavelength (nm)") +
    scale_colour_manual(values = colour_nm) +
    guides(colour = guide_legend(nrow = 1)) +
    coord_fixed(xlim = c(0, max_axis), ylim = c(0, max_axis)) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
}


# Processing functions ----------------------------------------------------

# Takes variable and Y sensor as input to automagically create global scatterplot triptych
# var_name = "RHOW"; sensor_Y = "HYPERPRO"
global_triptych <- function(var_name, sensor_Y){
  
  # Check that in situ data are being requested if variable is anything other than Rhow
  if(var_name != "RHOW" & sensor_Y != "HYPERPRO") stop("Can only use RHOW with remote data matchups")
  
  # Load data based on in situ comparisons or not
  print("Loading matchups")
  if(sensor_Y == "HYPERPRO"){
    if(var_name == "LD"){
      match_base_1 <- load_matchups_folder(var_name, "HYPERNETS", "TRIOS", long = TRUE)
    } else {
      match_base_1 <- load_matchups_folder(var_name, "HYPERNETS", "TRIOS", long = TRUE)
      match_base_2 <- load_matchups_folder(var_name, "HYPERNETS", "HYPERPRO", long = TRUE)
      match_base_3 <- load_matchups_folder(var_name, "TRIOS", "HYPERPRO", long = TRUE)
    }
  } else {
    match_base_1 <- load_matchups_folder(var_name, "HYPERNETS", sensor_Y, long = TRUE)
    match_base_2 <- load_matchups_folder(var_name, "TRIOS", sensor_Y, long = TRUE)
    match_base_3 <- load_matchups_folder(var_name, "HYPERPRO", sensor_Y, long = TRUE)
  }
  
  # Create the three figures
  print("Creating figures")
  if(sensor_Y == "HYPERPRO"){
    if(var_name == "LD"){
      match_fig_1 <- plot_matchup_nm(match_base_1, var_name, "HYPERNETS", "TRIOS")
    } else {
      match_fig_1 <- plot_matchup_nm(match_base_1, var_name, "HYPERNETS", "TRIOS")
      match_fig_2 <- plot_matchup_nm(match_base_2, var_name, "HYPERNETS", "HYPERPRO")
      match_fig_3 <- plot_matchup_nm(match_base_3, var_name, "TRIOS", "HYPERPRO")
    }
  } else {
    match_fig_1 <- plot_matchup_nm(match_base_1, var_name, "HYPERNETS", sensor_Y)
    match_fig_2 <- plot_matchup_nm(match_base_2, var_name, "TRIOS", sensor_Y)
    match_fig_3 <- plot_matchup_nm(match_base_3, var_name, "HYPERPRO", sensor_Y)
  }
  
  # Combine into final figure and save
  print("Saving and exit")
  if(var_name == "LD"){ 
    match_fig <- match_fig_1 +guides(colour = guide_legend(nrow = 2)) 
    ggsave(paste0("figures/global_scatter_all_",var_name,"_",sensor_Y,".png"), match_fig, width = 6, height = 7)
  } else {
    match_fig <- match_fig_1 + match_fig_2 + match_fig_3 + plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    ggsave(paste0("figures/global_scatter_all_",var_name,"_",sensor_Y,".png"), match_fig, width = 12, height = 5)
  }
}


# Global scatterplots -----------------------------------------------------

# In situ
global_triptych("ED", "HYPERPRO")
global_triptych("LD", "HYPERPRO")
global_triptych("LU", "HYPERPRO")
global_triptych("LW", "HYPERPRO")
global_triptych("RHOW", "HYPERPRO")
 
# Remote
global_triptych("RHOW", "PACE_V31")
global_triptych("RHOW", "AQUA")
global_triptych("RHOW", "VIIRS_N")
global_triptych("RHOW", "VIIRS_J1")
global_triptych("RHOW", "VIIRS_J2")
global_triptych("RHOW", "S3A")
global_triptych("RHOW", "S3B")

