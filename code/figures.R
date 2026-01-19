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

# TODO: Limit the data plotted to match the W_nm shown in the text tables - or not
# Takes variable and Y sensor as input to automagically create global scatterplot triptych
# var_name = "RHOW"; sensor_Y = "HYPERPRO"
global_triptych <- function(var_name, sensor_Y, cut_legend = NULL){
  
  # Check that in situ data are being requested if variable is anything other than Rhow
  if(var_name != "RHOW" & sensor_Y != "HYPERPRO") stop("Can only use RHOW with remote data matchups")
  
  # Load data based on in situ comparisons or not
  print("Loading matchups")
  if(var_name == "LD"){
    match_base_1 <- load_matchups_folder(var_name, "HYPERNETS", "TRIOS", long = TRUE)
  } else if(sensor_Y == "HYPERPRO"){
    match_base_1 <- load_matchups_folder(var_name, "HYPERNETS", "TRIOS", long = TRUE)
    match_base_2 <- load_matchups_folder(var_name, "HYPERNETS", "HYPERPRO", long = TRUE)
    match_base_3 <- load_matchups_folder(var_name, "TRIOS", "HYPERPRO", long = TRUE)
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
  print("Processing and exit")
  if(var_name == "LD"){ 
    match_fig <- match_fig_1 + 
      guides(colour = guide_legend(nrow = 2)) +
      theme(legend.position = "right",
            legend.direction = "horizontal") 
    # ggsave(paste0("figures/global_scatter_all_",var_name,"_",sensor_Y,".png"), match_fig, width = 5, height = 5)
  } else if(!is.null(cut_legend)){
    match_fig_1 <- match_fig_1 + guides(colour = "none")
    match_fig_2 <- match_fig_2 + guides(colour = "none")
    match_fig_3 <- match_fig_3 + guides(colour = "none")
    match_fig <- match_fig_1 + match_fig_2 + match_fig_3
  } else {
    # match_fig <- ggpubr::ggarrange(match_fig_1, match_fig_2, match_fig_3, ncol = 3, nrow = 1, 
    #                                common.legend = TRUE, legend = "bottom")
    match_fig <- match_fig_1 + match_fig_2 + match_fig_3 + plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    # ggsave(paste0("figures/global_scatter_all_",var_name,"_",sensor_Y,".png"), match_fig, width = 12, height = 5)
  }
}

# Stack the triptych output as required
# var_name = "ED"; sensor_Z = "HYPERPRO"
# var_name = "RHOW"; sensor_Z = "HYPERPRO"
# var_name = "RHOW"; sensor_Z = "MODIS"
# var_name = "RHOW"; sensor_Z = "OLCI"
# var_name = "RHOW"; sensor_Z = "OCI"
# var_name = "RHOW"; sensor_Z = "VIIRS"
global_triptych_stack <- function(var_name, sensor_Z){
  
  # Create ply grid
  ply_grid <- sensor_grid(var_name, sensor_Z) |> 
    dplyr::select(var_name, sensor_Y) |> distinct()
  
  # Cut it down
  # ply_grid_short
  
  # Get sensor variant count
  sensor_count <- length(unique(ply_grid$sensor_Y))

  # Create figures
  if(var_name != "RHOW"){
    fig_a <- global_triptych("ED", "HYPERPRO", cut_legend = "cut") #+ guides(colour = "none")
    fig_b <- global_triptych("LD", "HYPERPRO")
    # global_triptych("LU", "HYPERPRO")
    fig_c <- global_triptych("LW", "HYPERPRO", cut_legend = "cut")
    
    # Combine into special layout
    fig_stack <- ggpubr::ggarrange(fig_a, fig_b, fig_c, ncol = 1, nrow = 3, 
                                   labels = c("a)", "b)", "c)"), hjust = c(-2.2, -7.0, -1.5)) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    # fig_stack <- plot_spacer() + fig_a / fig_b / fig_c + plot_layout(ncol = 1, nrow = 3, heights = c(1, 1, 1))
    ggsave(paste0("figures/global_scatter_OTHER_in_situ.png"), fig_stack, width = 11, height = 12)
  } else if(sensor_Z == "HYPERPRO"){
    fig_stack <- global_triptych("RHOW", "HYPERPRO")
    ggsave(paste0("figures/global_scatter_",var_name,"_in_situ.png"), fig_stack, width = 12, height = 5)
    # TODO: Optimise this logic gate to make this decision automagically
  } else if(sensor_count == 1){
    fig_stack <- global_triptych(var_name, ply_grid$sensor_Y[1])
    ggsave(paste0("figures/global_scatter_",var_name,"_",sensor_Z,".png"), fig_stack, width = 12, height = 5)
  } else if(sensor_count == 2){
    fig_a <- global_triptych(var_name, ply_grid$sensor_Y[1], cut_legend = "cut")
    fig_b <- global_triptych(var_name, ply_grid$sensor_Y[2])
    fig_stack <- ggpubr::ggarrange(fig_a, fig_b, ncol = 1, nrow = sensor_count, 
                                   labels = c("a)", "b)"), heights = c(1, 1.14)) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    # fig_stack <- fig_a / fig_b + plot_layout(guides = "collect") &
    #   theme(legend.position = "bottom") #+ 
      # plot_annotation(tag_levels = "a", tag_suffix = ")")
    # fig_stack +
    #   annotate("text", x = -Inf, y = Inf, label = "a)", vjust = 0, hjust = -2) +
    #   annotate("text", x = -Inf, y = Inf, label = "b)", vjust = 0, hjust = 5) #+
      # annotate("text", x = -Inf, y = Inf, label = "c)", vjust = 2, hjust = 1.2)
    ggsave(paste0("figures/global_scatter_",var_name,"_",sensor_Z,".png"), fig_stack, width = 12, height = 9)
  } else if(sensor_count == 3){
    fig_a <- global_triptych(var_name, ply_grid$sensor_Y[1], cut_legend = "cut")
    fig_b <- global_triptych(var_name, ply_grid$sensor_Y[2], cut_legend = "cut")
    fig_c <- global_triptych(var_name, ply_grid$sensor_Y[3])
    fig_stack <- ggpubr::ggarrange(fig_a, fig_b, fig_c, ncol = 1, nrow = sensor_count, 
                                   labels = c("a)", "b)", "c)"), heights = c(1, 1, 1.13)) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    ggsave(paste0("figures/global_scatter_",var_name,"_",sensor_Z,".png"), fig_stack, width = 12, height = 13)
  }
}


# Global scatterplots -----------------------------------------------------

# In situ
global_triptych_stack("insitu", "HYPERPRO")
global_triptych_stack("RHOW", "HYPERPRO")
 
# Remote
global_triptych_stack("RHOW", "OCI")
global_triptych_stack("RHOW", "MODIS")
global_triptych_stack("RHOW", "VIIRS")
global_triptych_stack("RHOW", "OLCI")

