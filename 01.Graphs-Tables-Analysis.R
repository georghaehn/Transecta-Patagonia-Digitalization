
libs <- c("tidyverse",
          "sf",
          "rnaturalearth",
          "patchwork",
          "openxlsx")

invisible(lapply(libs, library, character.only = TRUE))

output_path <- "../Data/Output-data"

load(file.path(output_path, "R-Image-Transecta-Patagonia-v04-2023.RData"))

draft_path <- "../Manuscript/TablesPics"

#check if paths exist

dir.exists(c(output_path, figure_path))

### Create table 2 ####
um <- c("Date" = "yyyy-mm-dd",
        "Latitude" = "° (WGS84)",
        "Longitude" = "° (WGS84)",
        "Location_uncertainty_km" = "km",
        "Releve_area_m2" = "m^2^",
        "Elevation_m" = "m a.s.l.",
        "Slope_perc" = "%",
        "Degraded_soil_perc" = "%",
        "Total_vegetation_cover" = "%",
        "Height_tree_layer_m" = "m")

um <- data.frame(Variable = names(um), `Unit of Measurement` = um)


table2 <- header1 |> 
  dplyr::summarize_all(#.vars=vars(!starts_with("PlotID")),
                      .funs = list(xxxNo.records = ~sum(!is.na(.)),
                                   xxxType.of.variable = ~ifelse("logical" %in% class(.), "b",
                                                               ifelse("ordered" %in% class(.), 
                                                                      "o", 
                                                                      ifelse(any(class(.) %in% c("character", "factor")), 
                                                                             "c",
                                                                             ifelse(class(.) == "Date",
                                                                                    "d",
                                                                                    "n")))),
                                   xxxLevels = ~(ifelse(is.numeric(.) | lubridate::is.Date(.), 
                                                      paste(range(., na.rm = TRUE), collapse = " - "),
                                                      ifelse(is.ordered(.), 
                                                             paste(paste(1:nlevels(.), 
                                                                         levels(.), sep = " = "), collapse = ", "),
                                                             ifelse(is.factor(.), 
                                                                    paste(levels(.), collapse = ", "),
                                                                    ifelse(is.logical(.),
                                                                           paste(names(table(.)), "=", table(.),
                                                                                 collapse = "; "),
                                                                           ""))))))) |> 
  gather(key = "Variable") |> 
  separate(Variable, into = c("Variable", "feature"), sep = "_xxx") |>  
  spread(key = feature, value = value) |>  
  rename(`Range/Levels` = Levels) |> 
  mutate(Variable = factor(Variable, levels = colnames(header1))) |>  
  arrange(Variable) |> 
  left_join(um, by = "Variable") |> 
  mutate(Unit.of.Measurement = as.character(Unit.of.Measurement)) |> 
  replace_na(list(Unit.of.Measurement = "")) |> 
  dplyr::select(Variable, 
                `Range/Levels`, 
                `Unit of Measurement` = Unit.of.Measurement, 
                `Nr. of plots with information` = No.records, 
                `Type` = Type.of.variable)


openxlsx::write.xlsx(table2, file.path(draft_path, "Table2-Header-v04-2023.xlsx"), overwrite = TRUE)


# Some exploratory analyses: ####
### Create Map of plots ####
world <- ne_countries(scale = "large", returnclass = "sf", continent = "south america")   
header1.sf <- st_as_sf(header1, 
                       coords = c("Longitude", "Latitude"), 
                       crs =  4326) 

## Double check country match with spatial coordinates
st_intersection(header1.sf %>% 
                  select(PlotID, Country, Location_uncertainty_km), world[, "iso_a3"]) %>% 
  filter(Country != iso_a3)


## Download Hi-res spatial data of South America, if needed
southam.link <- "https://stacks.stanford.edu/file/druid:vc965bq8111/data.zip"

if(!file.exists("../Data/Spatial-data/data.zip")) {
  download.file(southam.link, 
                destfile="../Data/Spatial-data/data.zip")
}
unzip(zipfile = "../Data/Spatial-data/data.zip", exdir = "../Data/Spatial-data/")

southam <- read_sf("../Data/Spatial-data/SouthAmerica.shp")


## Create buffers around each point to show location uncertainties
myBuff <- st_buffer(header1.sf |>  
                      st_transform(crs=9822),  #lambers equal area conic
                    dist = header1.sf |> 
                      mutate(Location_uncertainty_km = 
                               ifelse(Location_uncertainty_km == 200, 
                                      250, 
                                      Location_uncertainty_km)) |>  
                      pull(Location_uncertainty_km) * 2000) |>  
  st_transform(crs = 4326)


## Plot
ylabs <- -51: -53
xlabs <- -68: -75

(left <- ggplot() +
  theme_minimal() +
  geom_sf(data = southam, fill = gray(0.8), col = gray(0.6)) + 
  geom_sf(data = header1.sf, col = "dark red") + #aes(col=Country)) +
  #geom_sf(data=myBuff, col="black", fill=NA) + 
  coord_sf(xlim = c(-75, -68), ylim = c(-53, -50.5), ) + 
  scale_y_continuous(breaks = ylabs) + 
  scale_x_continuous(breaks = xlabs) + 
  #scale_color_brewer(palette="Set1")+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
)

bbox <- sf::st_bbox(header1.sf)
bbox[[1]] <- -77
bbox[[3]] <- -67
bbox[[2]] <- -53
bbox[[4]] <- -49
bbox <- bbox |>  
  sf::st_as_sfc()

right <- ggplot(data = header1.sf) +
  theme_void() +
  geom_sf(data = southam, fill = gray(0.8), col = gray(0.6)) + 
  geom_sf(data = bbox, fill = NA, col = "black", linewidth = 1.2) +
  geom_sf_text(data = bbox, aes(label = "a"), nudge_x = 8, nudge_y = 3) + 
  coord_sf(xlim = c(-82, -35), ylim = c(12, -56)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

both <- left + right + patchwork::plot_annotation(tag_levels = "a", )

ggsave(plot = both, 
       file.path(draft_path, "Figure1-Studyarea.png"), 
       dpi = 300, bg = "white", 
       type = "cairo", width = 8, height = 4, units = "in")



### Show Species richness with longitude per formation ####

header1.sel <- header1 |> 
  filter(Location_uncertainty_km < 200) |> 
  rename(FB = FaberLangendoen_formation)

header1.sf.jitter <- st_as_sf(header1.sel |>  
                                mutate(Latitude = jitter(Latitude, amount = 0.08), 
                                       Longitude = jitter(Longitude, amount = 0.08)), 
                              coords = c("Longitude", "Latitude"), 
                              crs = 4326) 

SR_data <- DT1 |>  
  filter(PlotID %in% (header1.sf.jitter |>  
                        pull(PlotID))) |>  
  count(PlotID) |> 
  rename(SR = n) |>  
  left_join(header1.sel |>  
              dplyr::select(PlotID, Longitude, FB), 
            by = "PlotID") |>  
  arrange(Longitude)

(SR_long <- ggplot(data = SR_data, aes(x = Longitude, y = SR)) + 
    geom_point(aes(col = FB), show.legend = FALSE) + 
    geom_smooth(method = "gam") + 
    theme_minimal() + 
    coord_fixed(ratio = 0.05) + 
    scale_x_continuous(limits = c(-75, -68))+
    scale_y_continuous(name = "No of species") + 
    scale_color_brewer(palette = "Set1", drop = FALSE) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          panel.grid.minor.y = element_blank() )
)

(left2 <- ggplot(data = header1.sf.jitter) +
  theme_minimal() +
  geom_sf(data = southam, fill = gray(0.8), col = gray(0.6)) + 
  geom_sf(aes(col = FB)) + 
  coord_sf(xlim = c(-75, -68), 
           ylim = c(-53, -50.5)) + 
  scale_y_continuous(breaks = ylabs) + 
  scale_x_continuous(breaks = xlabs) + 
  scale_color_brewer(palette = "Set1", 
                     name = "Faber-Langendoen\n Formation", 
                     drop = FALSE,
                     labels = stringr::str_extract(levels(header1.sf.jitter$FB), "^.{5}"))
)
#  theme(legend.position="none")
#  guides(col=guide_legend(ncol=1,byrow=TRUE)) + 
#  theme(legend.position = "bottom", 
#        legend.title = element_blank())
#mylegend <- cowplot::get_legend(left2)

SR_long + left2 + patchwork::plot_layout(nrow = 2) + patchwork::plot_annotation(tag_levels = "a",)


ggsave(last_plot(), 
       filename = file.path(draft_path, "Figure4-Richness-Longitude.png"), 
       width = 11, height = 7, dpi = 300, units = "in", bg = "white", type = "cairo")

#SR_long + plot_spacer() + left + right + patchwork::plot_layout(ncol=2, nrow=2)


### Barplot showing the number of plots per vegetation type ####
(ggplot(data = header1) + 
   geom_bar(aes(x = FaberLangendoen_formation, 
                group = FaberLangendoen_formation, 
                col = FaberLangendoen_formation, 
                fill = FaberLangendoen_formation)) + 
   theme_minimal() + 
   scale_y_continuous(name = "N° of vegetation plots") + 
   scale_x_discrete(labels = stringr::str_extract(levels(header1$FaberLangendoen_formation), "^.{5}")) + 
   scale_color_brewer(palette = "Set1", name = "Faber-Langendoen Formation") + 
   scale_fill_brewer(palette = "Set1", name = "Faber-Langendoen Formation") + 
   theme(axis.text.x = element_text(angle = 45), 
         axis.title.x = element_blank())
)

ggsave(last_plot(), 
       filename= file.path(draft_path, "Figure2-Vegtypes.png"), 
       width = 10, height = 4, dpi = 300, units = "in", bg = "white")


### Boxplot of species richness per formation ####
SR_data_all <- DT1 |>  
  filter(PlotID %in% (header1 |>  
                        pull(PlotID))) |> 
  count(PlotID) |> 
  rename(SR = n) |>  
  left_join(header1 |>  
              dplyr::select(PlotID, 
                            Longitude, 
                            FB = FaberLangendoen_formation), 
            by = "PlotID") |> 
  arrange(Longitude)



ggplot(data = SR_data_all, aes(x = FB, y = SR, fill = FB)) + 
  geom_boxplot(show.legend = FALSE, 
               outlier.shape = NA
  ) + 
  geom_point(
    #    aes(color = FB,
    #        color = after_scale(darken(color, .2, space = "HLS"))),
    color = "black",
    fill = "white",
    shape = 21,
    stroke = 0.4,
    size = 2,
    position = position_jitter(seed = 1, width = 0.12), 
    show.legend = FALSE
  )  +
  geom_point(
    aes(fill = FB),
    color = "transparent",
    shape = 21,
    stroke = 0.4,
    size = 2,
    alpha = 0.7,
    position = position_jitter(seed = 1, width = 0.12), 
    show.legend = FALSE
  ) +  
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1", name = "Faber-Langendoen Formation") + 
  scale_color_brewer(palette = "Set1") + 
  scale_y_continuous(name = "Number of species") + 
  scale_x_discrete(labels = stringr::str_extract(levels(SR_data_all$FB), "^.{5}")) + 
  theme(axis.text.x = element_text(angle = 45), 
        axis.title.x = element_blank())

ggsave(last_plot(), 
       filename = file.path(draft_path, "Figure3-Richness-Vegtypes.png"), 
       width = 7, height = 3.5, dpi = 300, units = "in", bg = "white")




### Species richness with elevation ####
# correspondence between zoom level and resolution - https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution

library(elevatr)
mydem <- get_elev_point(locations = header1.sf, src = "aws", zoom = 7)

SR_data1 <- SR_data |>  
  left_join(mydem |>  
              dplyr::select(PlotID, elevation), 
            by = "PlotID") |> 
  filter(elevation > 0) |> 
  filter(FB != "2.C.5 Salt Marsh") |>   #drop level without enough obs
  mutate(FB = factor(FB))

ggplot(data = SR_data1, aes(x = SR, y = elevation)) + 
  geom_point(aes(col = FB)) + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(legend.position = "bottom")


## TRY REFITTING THE MODELS WITH CONTRASTS
mod0 <- lm(SR ~ elevation, data = SR_data1)
summary(mod0)


mod1 <- lm(SR ~ elevation*FB, data=SR_data1)
summary(mod1)


(newdata <- data.frame(pred = NA, 
                       elevation = rep(seq(0, max(SR_data1$elevation), length=100), 
                                     nlevels(SR_data1$FB)), 
                       FB = factor(rep(levels(SR_data1$FB), each = 100))) |>  
    as_tibble() |> 
    mutate(pred = predict.lm(mod1, newdata={cur_data()})) |>  
    left_join((SR_data1 |>  
                 group_by(FB) |>  
                 summarize(max.elev = max(elevation), 
                           min.elev = min(elevation))), 
              by = "FB") |> 
    filter(elevation < max.elev & elevation > min.elev))

SR_data2 <- SR_data1 |>  
  mutate(FB = factor(FB, levels = levels(header1$FaberLangendoen_formation)))

ggplot(data = SR_data2, 
       aes(x = elevation, y = SR)) + 
  geom_point(aes(col = FB)) + 
  geom_line(data = newdata, aes(x = elevation, y = pred, group = FB, col = FB)) + 
  scale_color_brewer(name = "Faber-Langendoen\nFormations",
                     palette = "Set1", 
                     drop = FALSE, 
                     #labels = stringr::str_extract(levels(SR_data2$FB), "^.{5}")
                     labels = stringr::str_replace(levels(SR_data2$FB), pattern = ",", replacement = "\n") |>
                       stringr::str_replace(pattern = " Herb", replacement = "\nHerb")
                     ) + 
  scale_x_continuous(name = "Elevation (m a.s.l.)") + 
  scale_y_continuous(name = "Species Richness") + 
  theme_bw() #+ 
#  theme(legend.position="none")

ggsave(last_plot(), 
       filename = file.path(draft_path, "Figure5-Richness-Elevation.png"), 
       width = 7, height = 4, dpi = 300, units = "in", bg = "white")
