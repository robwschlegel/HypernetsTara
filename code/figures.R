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
    coord_fixed(xlim = c(0, max_axis)) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
}


# Load data ---------------------------------------------------------------

rhow_hypernets_aqua <- load_matchups_folder("RHOW", "HYPERNETS", "AQUA", long = TRUE)


# Global scatterplots -----------------------------------------------------

test1 <- plot_matchup_nm(rhow_hypernets_aqua, "Rhow", "HYPERNETS", "AQUA")
test2 <- plot_matchup_nm(rhow_hypernets_aqua, "Rhow", "HYPERNETS", "AQUA")
test3 <- plot_matchup_nm(rhow_hypernets_aqua, "Rhow", "HYPERNETS", "AQUA")
test4 <- test1 + test2 + test3 + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("figures/test1.png", test4, width = 12, height = 5)

