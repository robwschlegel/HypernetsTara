# code/figures.R
# It does what it says on the tin


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(ggtext) # For rich text labels


# Processing functions ----------------------------------------------------

# TODO: Limit the data plotted to match the W_nm shown in the text tables - or not
# Takes variable and Y sensor as input to automagically create global scatterplot triptych
# var_name = "LU"; sensor_Y = "HYPERPRO"
# var_name = "RHOW"; sensor_Y = "HYPERPRO"
# var_name = "RHOW"; sensor_Y = "S3"
global_triptych <- function(var_name, sensor_Y, cut_legend = NULL){
  
  # Check that in situ data are being requested if variable is anything other than Rhow
  if(var_name != "RHOW" & sensor_Y != "HYPERPRO") stop("Can only use RHOW with remote data matchups")
  
  # Load outliers to screen them from being plotted
  suppressMessages(
    outliers_sat <- read_csv("meta/satellite_outliers.csv")
  )
  suppressMessages(
    outliers_insitu <- read_csv("meta/in_situ_outliers.csv")
  )
  outliers_all <- bind_rows(outliers_sat, outliers_insitu)
  
  # Load data based on in situ comparisons or not
  print("Loading matchups")
  if(var_name %in% c("LU", "LD")){
    match_base_1 <- load_matchups_folder(var_name, "HYPERNETS", "TRIOS", long = TRUE)
  } else if(sensor_Y == "HYPERPRO"){
    match_base_1 <- load_matchups_folder(var_name, "TRIOS", "HYPERPRO", long = TRUE)
    match_base_2 <- load_matchups_folder(var_name, "HYPERNETS", "HYPERPRO", long = TRUE)
    match_base_3 <- load_matchups_folder(var_name, "HYPERNETS", "TRIOS", long = TRUE)
  } else if(sensor_Y == "S3"){
    match_base_1 <- bind_rows(load_matchups_folder(var_name, "HYPERPRO", "S3A", long = TRUE),
                              load_matchups_folder(var_name, "HYPERPRO", "S3B", long = TRUE))
    match_base_2 <- bind_rows(load_matchups_folder(var_name, "TRIOS", "S3A", long = TRUE),
                              load_matchups_folder(var_name, "TRIOS", "S3B", long = TRUE))
    match_base_3 <- bind_rows(load_matchups_folder(var_name, "HYPERNETS", "S3A", long = TRUE),
                              load_matchups_folder(var_name, "HYPERNETS", "S3B", long = TRUE))
  } else {
    match_base_1 <- load_matchups_folder(var_name, "HYPERPRO", sensor_Y, long = TRUE)
    match_base_2 <- load_matchups_folder(var_name, "TRIOS", sensor_Y, long = TRUE)
    match_base_3 <- load_matchups_folder(var_name, "HYPERNETS", sensor_Y, long = TRUE)
  }
  
  # Filter out outliers
  if(var_name %in% c("LU", "LD")){
    match_base_1 <- match_base_1[!match_base_1$file_name %in% outliers_all$file_name,]
  } else {
    match_base_1 <- match_base_1[!match_base_1$file_name %in% outliers_all$file_name,]
    match_base_2 <- match_base_2[!match_base_2$file_name %in% outliers_all$file_name,]
    match_base_3 <- match_base_3[!match_base_3$file_name %in% outliers_all$file_name,]
  }
  
  # Filter out wavelengths above 590 if plotting Rhow data
  if(var_name == "RHOW"){
    match_base_1 <- filter(match_base_1, wavelength < 600) 
    match_base_2 <- filter(match_base_2, wavelength < 600) 
    match_base_3 <- filter(match_base_3, wavelength < 600) 
  }
  
  # Create the three figures
  print("Creating figures")
  if(sensor_Y == "HYPERPRO"){
    if(var_name %in% c ("LU", "LD")){
      match_fig_1 <- plot_global_nm(match_base_1, var_name, "HYPERNETS", "TRIOS")
    } else {
      match_fig_1 <- plot_global_nm(match_base_1, var_name, "HYPERPRO", "TRIOS")
      match_fig_2 <- plot_global_nm(match_base_2, var_name, "HYPERPRO", "HYPERNETS")
      match_fig_3 <- plot_global_nm(match_base_3, var_name, "HYPERNETS", "TRIOS")
    }
  } else {
    match_fig_1 <- plot_global_nm(match_base_1, var_name, "HYPERPRO", sensor_Y)
    match_fig_2 <- plot_global_nm(match_base_2, var_name, "TRIOS", sensor_Y)
    match_fig_3 <- plot_global_nm(match_base_3, var_name, "HYPERNETS", sensor_Y)
  }
  
  # Combine into final figure and save
  print("Processing and exit")
  if(var_name %in% c("LU", "LD")){
    match_fig <- match_fig_1 + guides(colour = "none")
      # guides(colour = guide_legend(nrow = 2)) +
      # theme(legend.position = "right",
            # legend.direction = "horizontal")
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
  
  # Get sensor variant count
  sensor_count <- length(unique(ply_grid$sensor_Y))

  # Create figures
  if(var_name != "RHOW"){
    fig_a <- global_triptych("ED", "HYPERPRO", cut_legend = "cut")
    fig_b <- global_triptych("LU", "HYPERPRO", cut_legend = "cut")
    fig_c <- global_triptych("LD", "HYPERPRO", cut_legend = "cut")
    fig_d <- global_triptych("LW", "HYPERPRO")
    
    # Combine into special layout
    fig_mid <- ggpubr::ggarrange(fig_b, fig_c, ncol = 2, nrow = 1, 
                                 labels = c("b)", "c)"), hjust = c(-5.3, -6.0))
    fig_stack <- ggpubr::ggarrange(fig_a, fig_mid, fig_d, ncol = 1, nrow = 3, 
                                   labels = c("a)", "", "d)"), hjust = c(-2.2, 0, -1.5), heights = c(1.07, 1, 1.17)) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    ggsave(paste0("figures/global_scatter_OTHER_in_situ.png"), fig_stack, width = 11, height = 12)
  } else if(sensor_Z == "HYPERPRO"){
    fig_stack <- global_triptych("RHOW", "HYPERPRO")
    ggsave(paste0("figures/global_scatter_",var_name,"_in_situ.png"), fig_stack, width = 12, height = 4.5)
    # TODO: Optimise this logic gate to make this decision automagically
  } else if(sensor_Z == "OLCI"){
    fig_stack <- global_triptych("RHOW", "S3")
    ggsave(paste0("figures/global_scatter_",var_name,"_OLCI.png"), fig_stack, width = 12, height = 4.5)
  } else if(sensor_count == 1){
    fig_stack <- global_triptych(var_name, ply_grid$sensor_Y[1])
    ggsave(paste0("figures/global_scatter_",var_name,"_",sensor_Z,".png"), fig_stack, width = 12, height = 4.5)
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


# Hyperspectral figure ----------------------------------------------------

# Create a hyperspectral plot that shows all in situ sensors vs PACE and one other multispectral satellite
# Matchup scatterplots for everything, with statistic panels in each pane;
# Add variance bars for multispectral data
# Add intercept and R2
# Show this all as two figures:
## The first has the hyperspectral plot plus the photos from HYPERNETS
## The second shows all of the matchup scatterplots


# PACE Supp ----------------------------------------------------------------

# TODO: Change colour palette for differences to blue-red

# Load one slice of PACE v3.1 data at 413 nm
## Visualise all five days to pick the best coverage
# map_PACE(read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240812T105611.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240813T113041.L2.OC_AOP.V3_1_rrs_413.csv")) # Winner
# map_PACE(read_csv("data/PACE_OCI.20240814T120512.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240815T110624.L2.OC_AOP.V3_1_rrs_413.csv"))
PACE_swath <- read_csv("data/PACE_OCI.20240813T113041.L2.OC_AOP.V3_1_rrs_413.csv") |> 
  filter(!is.na(Rrs))

# Load the full spectra for one point
v_all_spectra <- read_delim("data/csv_pour_générer_spectres_pace_V20_V30_V31/PACE_20240809T0900.csv", delim = ";")
colnames(v_all_spectra)[1] <- "version"
v_all_spectra_long <- v_all_spectra |> 
  pivot_longer(`356`:`718`, names_to = "nm") |> 
  mutate(nm = as.integer(nm))

# Load data extracted via Python script
v2_413 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v2_Rrs = Rrs)
v3_413 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V3_0_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v3_Rrs = Rrs)
v31_413 <- read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V3_1_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v31_Rrs = Rrs)

# Combine and compare
vall_413 <- left_join(v2_413, v3_413, by = join_by(latitude, longitude)) |> 
  left_join(v31_413, by = join_by(latitude, longitude)) |> 
  mutate(v2_v3 = (v2_Rrs / v3_Rrs)*100,
         v2_v31 = (v2_Rrs / v31_Rrs)*100,
         v3_v31 = (v3_Rrs / v31_Rrs)*100) |> 
  mutate(v2_v3 = ifelse(v2_v3 > 200, 200, v2_v3),
         v2_v31 = ifelse(v2_v31 > 200, 200, v2_v31),
         v3_v31 = ifelse(v3_v31 > 200, 200, v3_v31),
         v2_v3 = ifelse(v2_v3 < -200, -200, v2_v3),
         v2_v31 = ifelse(v2_v31 < -200, -200, v2_v31),
         v3_v31 = ifelse(v3_v31 < -200, -200, v3_v31)) |>
  # mutate(v2_v3 = cut(v2_v3, seq(-200, 200, 50)),
  #        v2_v31 = cut(v2_v31, seq(-200, 200, 50)),
  #        v3_v31 = cut(v3_v31, seq(-200, 200, 50)))
  mutate(v2_v3 = cut(v2_v3, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)),
         v2_v31 = cut(v2_v31, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)),
         v3_v31 = cut(v3_v31, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)))

# Pivot longer for plotting
v_comp_long <- vall_413 |> 
  dplyr::select(longitude, latitude, v2_v3:v3_v31) |> 
  pivot_longer(v2_v3:v3_v31, names_to = "ver") |> 
  mutate(ver = factor(ver, levels = c("v2_v3", "v2_v31", "v3_v31"),
                      labels = c("v2 / v3", "v2 / v3.1", "v3 / v3.1"))) |> 
  filter(!is.na(value))

# Plot map differences
pl_top <- ggplot(data = v_comp_long) +
  # borders(fill = "grey80") +
  geom_tile(aes(x = longitude, y = latitude, fill = value)) +
  annotate(geom = "point", x = v_all_spectra$longitude[1], y = v_all_spectra$latitude[1]) +
  # geom_contour(aes(x = longitude, y = latitude, z = value),
  #              breaks = c(1.0), color = "black") +
  # scale_colour_viridis_c(aesthetics = c("colour", "fill")) +
  scale_fill_viridis_d() +
  labs(x = NULL, y = NULL, fill = "Difference (%)") +
  facet_wrap(~ver) +
  coord_quickmap(xlim = c(min(v_comp_long$longitude), max(v_comp_long$longitude)),
                 ylim = c(min(v_comp_long$latitude), max(v_comp_long$latitude))) +
  theme(panel.background = element_rect(colour = "black", fill = "grey90"),
        legend.position = "top")

# Plot percent difference as non-map
pl_left <- v_comp_long |> 
  summarise(cut_n = n(), .by = c("ver", "value")) |> 
  # complete(ver, value)
  ggplot() +
  geom_col(aes(x = value, y = cut_n, fill = ver), 
           position = "dodge", colour = "black") +
  scale_y_continuous(expand = c(0, 2000), 
                     breaks = c(0, 50000, 100000, 150000, 200000, 250000),
                     labels = c("0", "50K", "100K", "150K", "200K", "250K")) +
  scale_fill_brewer(palette = "Accent") +
  # scale_fill_viridis_d(option = "A") + # yuck
  labs(x = "Difference (%)", y = "Pixel count (n)", fill = "Comparison") +
  theme(panel.background = element_rect(colour = "black", fill = "grey90"),
        legend.position = "bottom")

# Plot spectra differences
pl_right <- ggplot(v_all_spectra_long) +
  geom_path(aes(x = nm, y = value, colour = version),
            linewidth = 2, alpha = 0.8) +
  geom_vline(xintercept = 413) +
  labs(x = "Wavelength (nm)", y = "Remote sensing reflectance (Rrs)") +
  scale_color_brewer(palette = "YlGnBu") +
  # scale_colour_viridis_d(option = "B") + # yuck
  scale_y_continuous(expand = c(-0.1, 0.005)) +
  theme(panel.background = element_rect(colour = "black", fill = "grey90"),
        legend.position = "bottom")

# Put it all together
pl_all <- (pl_top / (pl_left + pl_right)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')
ggsave("figures/fig_S1.png", pl_all, height = 9, width = 14)