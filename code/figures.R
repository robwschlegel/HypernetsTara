# code/figures.R
# It does what it says on the tin


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Processing functions ----------------------------------------------------

# Takes variable and Y sensor as input to automagically create global scatterplot triptych
# var_name = "LU"; sensor_Y = "HYPERPRO"
# var_name = "RHOW"; sensor_Y = "HYPERPRO"
# var_name = "RHOW"; sensor_Y = "VIIRS_J2"
# var_name = "RHOW"; sensor_Y = "S3"; panel_labels <- c("a)", "b)", "c)")
global_triptych <- function(var_name, sensor_Y, panel_labels, cut_legend = NULL){
  
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
  } else if (var_name == "LU"){
    match_base_1 <- filter(match_base_1, wavelength < 600) 
  }
  
  # Create the three figures
  print("Creating figures")
  if(sensor_Y == "HYPERPRO"){
    if(var_name %in% c ("LU", "LD")){
      match_fig_1 <- plot_global_nm(match_base_1, var_name, "TRIOS", "HYPERNETS")
    } else {
      match_fig_1 <- plot_global_nm(match_base_1, var_name, "HYPERPRO", "TRIOS")
      match_fig_2 <- plot_global_nm(match_base_2, var_name, "HYPERPRO", "HYPERNETS")
      match_fig_3 <- plot_global_nm(match_base_3, var_name, "TRIOS", "HYPERNETS")
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
    # match_fig <- match_fig_1 + match_fig_2 + match_fig_3
    match_fig <- ggpubr::ggarrange(match_fig_1, match_fig_2, match_fig_3, ncol = 3, nrow = 1, labels = panel_labels)
  } else {
    match_fig <- ggpubr::ggarrange(match_fig_1, match_fig_2, match_fig_3, ncol = 3, nrow = 1,
                                   common.legend = TRUE, legend = "bottom", labels = panel_labels)
    # match_fig <- match_fig_1 + match_fig_2 + match_fig_3 + plot_layout(guides = "collect") &
    #   theme(legend.position = "bottom")
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
    fig_a <- global_triptych("ED", "HYPERPRO", cut_legend = "cut", panel_labels = c("a)", "b)", "c)"))
    fig_b <- global_triptych("LU", "HYPERPRO", cut_legend = "cut", panel_labels = "")
    fig_c <- global_triptych("LD", "HYPERPRO", cut_legend = "cut", panel_labels = "")
    fig_d <- global_triptych("LW", "HYPERPRO", panel_labels = c("f)", "g)", "h)"))
    
    # Combine into special layout
    fig_mid <- ggpubr::ggarrange(fig_b, fig_c, ncol = 2, nrow = 1, 
                                 labels = c("d)", "e)"), hjust = c(-4.9, -5.7), vjust = c(0.5, 0.5))
    fig_stack <- ggpubr::ggarrange(fig_a, fig_mid, fig_d, ncol = 1, nrow = 3, 
                                   # labels = c("a)", "", "d)"), hjust = c(-2.2, 0, -1.5), 
                                   heights = c(1.09, 1, 1.17)) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    ggsave(paste0("figures/global_scatter_OTHER_in_situ.png"), fig_stack, width = 11, height = 12)
  } else if(sensor_Z == "HYPERPRO"){
    fig_stack <- global_triptych("RHOW", "HYPERPRO", panel_labels = c("a)", "b)", "c)")) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    ggsave(paste0("figures/global_scatter_",var_name,"_in_situ.png"), fig_stack, width = 12, height = 4.5)
    # TODO: Optimise this logic gate to make this decision automagically
  } else if(sensor_Z == "OLCI"){
    fig_stack <- global_triptych("RHOW", "S3", panel_labels = c("a)", "b)", "c)")) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    ggsave(paste0("figures/global_scatter_",var_name,"_OLCI.png"), fig_stack, width = 12, height = 4.5)
  } else if(sensor_count == 1){
    fig_stack <- global_triptych(var_name, ply_grid$sensor_Y[1], panel_labels = c("a)", "b)", "c)")) + 
      ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
    ggsave(paste0("figures/global_scatter_",var_name,"_",sensor_Z,".png"), fig_stack, width = 12, height = 4.5)
  } else if(sensor_count == 2){
    fig_a <- global_triptych(var_name, ply_grid$sensor_Y[1], cut_legend = "cut", panel_labels = c("a)", "b)", "c)"))
    fig_b <- global_triptych(var_name, ply_grid$sensor_Y[2], panel_labels = c("d)", "e)", "f)"))
    fig_stack <- ggpubr::ggarrange(fig_a, fig_b, ncol = 1, nrow = sensor_count, 
                                   # labels = c("a)", "b)"), 
                                   heights = c(1, 1.14)) + 
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
    fig_a <- global_triptych(var_name, ply_grid$sensor_Y[1], cut_legend = "cut", panel_labels = c("a)", "b)", "c)"))
    fig_b <- global_triptych(var_name, ply_grid$sensor_Y[2], cut_legend = "cut", panel_labels = c("d)", "e)", "f)"))
    fig_c <- global_triptych(var_name, ply_grid$sensor_Y[3], panel_labels = c("h)", "i)", "j)"))
    fig_stack <- ggpubr::ggarrange(fig_a, fig_b, fig_c, ncol = 1, nrow = sensor_count, 
                                   # labels = c("a)", "b)", "c)"), 
                                   heights = c(1, 1, 1.13)) + 
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


# Figure 3 ----------------------------------------------------------------

# Create a hyperspectral plot that shows all in situ sensors vs PACE and one other multispectral satellite
# Add variance bars for multispectral data
# Plus the photos from HYPERNETS

# Using the HYPERPRO 2024-08-12 10:53:00 measurement as a reference as this has a PACE and VIIRS_N matchup
pro_pace2 <- read_delim("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERPRO_vs_PACE_V2/HYPERPRO_vs_PACE_V2_vs_20240812T105300_RHOW.csv", delim = ";", col_types = "ccccnnic")
pro_pace3 <- read_delim("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERPRO_vs_PACE_V30/HYPERPRO_vs_PACE_V30_vs_20240812T105300_RHOW.csv", delim = ";", col_types = "ccccnnic")
pro_pace31 <- read_delim("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERPRO_vs_PACE_V31/HYPERPRO_vs_PACE_V31_vs_20240812T105300_RHOW.csv", delim = ";", col_types = "ccccnnic")
pro_viirsn <- read_delim("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERPRO_vs_VIIRS_N/HYPERPRO_vs_VIIRS_N_vs_20240812T105300_RHOW.csv", delim = ";", col_types = "ccccnnic")
pro_hyp <- read_delim("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_HYPERNETS_vs_HYPERPRO/HYPERNETS_vs_HYPERPRO_vs_20240812T104500_RHOW.csv", delim = ";", col_types = "ccccnnic")
pro_tri <- read_delim("~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/tara_matchups_results_20260203/RHOW_TRIOS_vs_HYPERPRO/TRIOS_vs_HYPERPRO_vs_20240812T104536_RHOW.csv", delim = ";", col_types = "ccccnnic")

# Load the RGB images taken by HYPERNETS at the closest available timing
hyp_LD <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data/12/SEQ20240812T104553/image/HYPERNETS_W_MAFR_IMG_20240812T1045_20240912T1030_006_140_90_v2.0.jpg"
hyp_LU <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data/12/SEQ20240812T104553/image/HYPERNETS_W_MAFR_IMG_20240812T1045_20240912T1030_009_40_90_v2.0.jpg"
hyp_ED <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data/12/SEQ20240812T104553/image/HYPERNETS_W_MAFR_IMG_20240812T1045_20240912T1030_015_180_90_v2.0.jpg"
hyp_SUN <- "~/pCloudDrive/Documents/OMTAB/HYPERNETS/Tara/Hypernets_processed_data/12/SEQ20240812T104553/image/HYPERNETS_W_MAFR_IMG_20240812T1045_20240912T1030_016_0_0_v2.0.jpg"

# Combine wide for reference
pro_all <- rbind(pro_pace2, pro_pace3, pro_pace31, pro_viirsn, pro_hyp, pro_tri) |> 
  `colnames<-`(c("sensor", colnames(pro_pace2)[2:329])) |> 
  filter(!sensor %in% c("HYPERPRO 4", "HYPERPRO 5", "HYPERPRO 6", "HYPERPRO 7")) |> 
  distinct() |> mutate(sensor = gsub(" 1$| 2$| 3$| 4$| 5$| 6$| 7$| 8$| 9$", "", sensor))

# Melt and prep for plotting
pro_all_long <- pro_all |> 
  dplyr::select(-(day:type)) |> 
  pivot_longer(`380`:`700`, names_to = "wavelength") |> 
  filter(!is.na(value)) |> 
  mutate(wavelength = as.numeric(wavelength),
         data_type = case_when(data_type == "weighted" ~ "rhow", TRUE ~ data_type),
         sensor = case_when(sensor == "HYPERPRO" ~ "HyperPRO",
                            sensor == "PACE_V2" ~ "PACE v2.0",
                            sensor == "PACE_V30" ~ "PACE v3.0",
                            sensor == "PACE_V31" ~ "PACE v3.1",
                            sensor == "VIIRS_N" ~ "VIIRS SNPP",
                            sensor == "Hyp" ~ "HYPERNETS",
                            sensor == "Hyp_nosc" ~ "HYPERNETS (nosc)",
                            sensor == "TRIOS" ~ "So-Rad")) |> 
  pivot_wider(names_from = data_type, values_from = value) |> 
  filter(wavelength >= 400, wavelength <= 600) |> 
  filter(sensor != "HYPERNETS (nosc)") |>  # Not interesting for this match-up
  mutate(sensor = factor(sensor, levels = c("HyperPRO", "So-Rad", "HYPERNETS", "PACE v2.0", "PACE v3.0", "PACE v3.1", "VIIRS SNPP")))

# The hyperspectral plot
pl_spect <- ggplot(data = pro_all_long, aes(x = wavelength, y = rhow, colour = sensor, fill = sensor)) +
  # geom_ribbon(data = filter(pro_all_long, sensor != "VIIRS SNPP"),
  # aes(ymin = std_min, ymax = std_max), alpha = 0.1, colour = NA) +
  geom_line(data = filter(pro_all_long, sensor != "VIIRS SNPP"), linewidth = 2, alpha = 0.7) +
  geom_point(data = filter(pro_all_long, sensor == "VIIRS SNPP"), size = 3, show.legend = FALSE) +
  geom_errorbar(data = filter(pro_all_long, sensor == "VIIRS SNPP"), 
                aes(ymin = std_min, ymax = std_max), width = 5) +
  # scale_colour_brewer(palette = "Dark2") +
  scale_colour_manual(values = c("darkred", "darkorange", "goldenrod", "skyblue", "dodgerblue", "royalblue", "limegreen")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1.0, linewidth = 3))) +
  labs(x = "Wavelength (nm)",
       y = "<i>ρ<sub>w</sub></i> (sr<sup>-1</sup>)",
       colour = "Sensor") +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.75),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_rect(colour = "black", fill = "white"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_markdown(size = 12),
        axis.text = element_text(size = 10))

# Prep each jpg
pl_ld <- ggplot() +
  geom_image(aes(x = 0, y = 0), image = hyp_LD, size = 1) +
  labs(title = "<i>L<sub>d</sub></i>") + theme_void() +
  theme(plot.title = element_markdown(hjust = 0.5, vjust = 0))
pl_lu <- ggplot() +
  geom_image(aes(x = 0, y = 0), image = hyp_LU, size = 1) +
  labs(title = "<i>L<sub>u</sub></i>") + theme_void() +
  theme(plot.title = element_markdown(hjust = 0.5, vjust = 0))
pl_ed <- ggplot() +
  geom_image(aes(x = 0, y = 0), image = hyp_ED, size = 1) +
  labs(title = "<i>E<sub>d</sub></i>") + theme_void() +
  theme(plot.title = element_markdown(hjust = 0.5, vjust = 0))
pl_sun <- ggplot() +
  geom_image(aes(x = 0, y = 0), image = hyp_SUN, size = 1) +
  labs(title = "<i>Sun</i>") + theme_void() +
  theme(plot.title = element_markdown(hjust = 0.5, vjust = 0))

# Combine photos into one row
fig_3_bottom <- ggpubr::ggarrange(pl_ld, pl_lu, pl_ed, pl_sun, nrow = 1, ncol = 4)
fig_3 <- ggpubr::ggarrange(pl_spect, fig_3_bottom, ncol = 1, nrow = 2, heights = c(2, 1)) +
  ggpubr::bgcolor("white") + ggpubr::border("white", size = 2)
ggsave("figures/fig_3.png", fig_3, width = 12, height = 9)


# Figure 4 ----------------------------------------------------------------

# Matchup scatterplots for everything shown in Figure 3, with statistic panels in each pane;
sensor_X <- "HyperPRO"; sensor_Y <- "PACE v2.0"
sensor_X <- "HyperPRO"; sensor_Y <- "VIIRS SNPP"

df_prep <- pro_all_long |> 
  filter(sensor %in% c(sensor_X, sensor_Y)) |> 
  filter(wavelength < 600) |> 
  mutate(wavelength_group = cut(wavelength,
                                breaks = c(350, 400, 450, 500, 550, 600, 650, 700, 750, 800),
                                labels = labels_nm,
                                include.lowest = FALSE, right = FALSE), .after = "wavelength") |> 
  pivot_wider(names_from = sensor, values_from = rhow:std_min)

# Get max values
# NB: max/min is incerted in absolute values
max_X <- max(df_prep[paste0("std_min_",sensor_X)], na.rm = TRUE)
max_Y <- max(df_prep[paste0("std_min_",sensor_Y)], na.rm = TRUE)
max_axis <- max(max_X, max_Y)

# Get global stats
x_vec <- df_prep[[paste0("rhow_",sensor_X)]]
y_vec <- df_prep[[paste0("rhow_",sensor_Y)]]

# Calculate statistics
df_stats <- base_stats(x_vec, y_vec)

# Get pretty labels
var_labs <- pretty_label_func("RHOW")

# The first points
if(grepl("VIIRS", sensor_Y)){
  pl_base <- ggplot(data = df_prep, 
                    aes_string(x = paste0("rhow_",sensor_X), y = paste0("`rhow_",sensor_Y,"`"))) +
    geom_errorbar(aes_string(xmin = paste0("std_min_",sensor_X), xmax = paste0("std_max_",sensor_X)), width = 0.0005) +
    geom_errorbar(aes_string(ymin = paste0("`std_min_",sensor_Y,"`"), ymax = paste0("`std_max_",sensor_Y,"`")), width = 0.0005) +
    geom_point(aes(colour = wavelength_group), size = 5, alpha = 0.9)
} else {
  pl_base <- ggplot(data = df_prep, 
                      aes_string(x = paste0("rhow_",sensor_X), y = paste0("`rhow_",sensor_Y,"`"))) +
    geom_point(aes(colour = wavelength_group), size = 3, alpha = 0.9)
}

pl_single <- pl_base +
  # Add 1:1 line
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid") +
  # Add global stats linear model
  geom_smooth(method = "lm", formula = y ~ x, colour = "white", linewidth = 1.5, linetype = "solid", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", linewidth = 1, linetype = "dashed", se = FALSE) +
  # Add global stats text
  annotate(geom = "text", x = 0, y = max_axis, hjust = 0, vjust = 1, size = 4,
           label = paste0("Slope: ", df_stats$Slope, "\nβ: ", df_stats$Bias,"% \nϵ: ",df_stats$Error,"%")) +
  # Make it pretty
  labs(x = paste0(sensor_X,"; ", var_labs$units_lab),
       y = paste0(sensor_Y,"; ", var_labs$units_lab),
       colour = "Wavelength (nm)") +
  scale_colour_manual(values = colour_nm) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(alpha = 1.0, size = 3))) +
  coord_fixed(xlim = c(0, max_axis), ylim = c(0, max_axis)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        axis.title.x = element_markdown(size = 12),
        axis.title.y = element_markdown(size = 12),
        axis.text = element_text(size = 10))
# pl_single


# PACE Supp ----------------------------------------------------------------

# TODO: Change colour palette for differences to blue-red

# Load one slice of PACE v3.1 data at 413 nm
## Visualise all five days to pick the best coverage
# map_PACE(read_csv("data/PACE_OCI.20240809T105059.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240812T105611.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240813T113041.L2.OC_AOP.V3_1_rrs_413.csv")) # Winner
# map_PACE(read_csv("data/PACE_OCI.20240814T120512.L2.OC_AOP.V3_1_rrs_413.csv"))
# map_PACE(read_csv("data/PACE_OCI.20240815T110624.L2.OC_AOP.V3_1_rrs_413.csv"))
PACE_swath <- read_csv("data/PACE/PACE_OCI.20240813T113041.L2.OC_AOP.V3_1_rrs_413.csv") |> 
  filter(!is.na(Rrs))

# Load the full spectra for one point
v_all_spectra <- read_delim("data/csv_pour_générer_spectres_pace_V20_V30_V31/PACE_20240809T0900.csv", delim = ";")
colnames(v_all_spectra)[1] <- "version"
v_all_spectra_long <- v_all_spectra |> 
  pivot_longer(`356`:`718`, names_to = "nm") |> 
  mutate(nm = as.integer(nm))

# Load data extracted via Python script
v2_413 <- read_csv("data/PACE/PACE_OCI.20240809T105059.L2.OC_AOP.V2_0.NRT_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v2_Rrs = Rrs)
v3_413 <- read_csv("data/PACE/PACE_OCI.20240809T105059.L2.OC_AOP.V3_0_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v3_Rrs = Rrs)
v31_413 <- read_csv("data/PACE/PACE_OCI.20240809T105059.L2.OC_AOP.V3_1_rrs_413.csv") |> 
  filter(!is.na(Rrs)) |> 
  mutate(longitude = plyr::round_any(longitude, 0.02),
         latitude = plyr::round_any(latitude, 0.02)) |>
  summarise(Rrs = mean(Rrs, na.rm = TRUE), .by = c("longitude", "latitude")) |> 
  dplyr::rename(v31_Rrs = Rrs)

# Combine and compare
vall_413 <- left_join(v2_413, v3_413, by = join_by(latitude, longitude)) |> 
  left_join(v31_413, by = join_by(latitude, longitude)) |> 
  mutate(v2_v3 = ((v2_Rrs / v3_Rrs)*100)-100,
         v2_v31 = ((v2_Rrs / v31_Rrs)*100)-100,
         v3_v31 = ((v3_Rrs / v31_Rrs)*100)-100) |> 
  mutate(v2_v3 = ifelse(v2_v3 > 200, 200, v2_v3),
         v2_v31 = ifelse(v2_v31 > 200, 200, v2_v31),
         v3_v31 = ifelse(v3_v31 > 200, 200, v3_v31),
         v2_v3 = ifelse(v2_v3 < -200, -200, v2_v3),
         v2_v31 = ifelse(v2_v31 < -200, -200, v2_v31),
         v3_v31 = ifelse(v3_v31 < -200, -200, v3_v31)) |>
  # mutate(v2_v3 = cut(v2_v3, seq(-200, 200, 50)),
  #        v2_v31 = cut(v2_v31, seq(-200, 200, 50)),
  #        v3_v31 = cut(v3_v31, seq(-200, 200, 50)))
  mutate(v2_v3_cut = cut(v2_v3, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)),
         v2_v31_cut = cut(v2_v31, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)),
         v3_v31_cut = cut(v3_v31, c(-200, -100, 0, 50, 80, 90, 100, 110, 120, 150, 200)))

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
  scale_fill_gradient() +
  # scale_fill_viridis_d() +
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

