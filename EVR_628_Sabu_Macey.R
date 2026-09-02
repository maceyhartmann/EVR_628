################################################################################
# EVR_628 Macey and Sabu Practice Push/Pull
################################################################################
#
# Macey Hartmann
# Sabirah Carfagna
# 
# mrh293@miami.edu
# 9/1/2026
# smc623@miami.edu
# 9/1/2026
# Practice script for EVR_628
#
################################################################################


# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
install.packages("EVR628tools")
install.packages("tidyverse")
library(EVR628tools)
library(tidyverse)
library(dplyr)

## Load data -------------------------------------------------------------------

glimpse(data_lionfish)

# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------

# VISUALIZE ####################################################################

## Scatter plot
ggplot(data = data_lionfish, # use data_lionfish
       mapping = aes(x = depth_m, # x axis is depth_m
                     y = total_length_mm)) + # y axis is total_length_mm
  geom_point(shape = 21, fill = "steelblue", size = 2) + # shape, color, size of points
  labs(x = "Depth (m)", # x axis title
       y = "Total length (mm)", # y axis title
       title = "Body length and depth", # graph title
       subtitle = "Larger fish tend to live deeper", # graph subtitle
       caption = "Source EVR628tools::data_lionfish") # data source title

## Basic bar plot with site  - distribution of a categorical variable
ggplot(data = data_lionfish, # use data_lionfish
       aes(x = site)) + # x axis is site 
  geom_bar() + # add bars
  coord_flip() + # flip x axis (site) onto y 
  labs(x = "Site", y = "Number of fish") # x and y titles

## Bar plot with sites ordered by frequency 
ggplot(data = data_lionfish, # data is data_lionfish
       aes(x = fct_infreq(site))) + # facets by frequency
  geom_bar() # adds bars 
  coord_flip() + # flips x axis (site) onto y
  labs(x = "Site", y = "Number of fish") # x and y titles
  
## Distribution of numeric variable 
### Histogram of lengths 
ggplot(data = data_lionfish, # use data_lionfish
       aes(x = total_length_mm)) + # x axis is total_length_mm
  geom_histogram(bins = 15) +  # histogram with 15 bins
  labs(x = "Total length (mm)", # x axis title
       y = "Count") # y axis title

## Visualizing relationships 
### A numerical and a categorical variable 
### Numerical vs categorical: depth by size class 
ggplot(data = data_lionfish, aes(x = size_class, 
                                 y = depth_m)) + 
  geom_boxplot() + 
  labs(x = "Size class", 
       y = "Depth (m)")

### Two categorical variables: N by size class and site 
ggplot(data = data_lionfish, 
       aes(x = site, 
           y = size_class)) + 
  geom_bin2d() + 
  coord_flip() + 
  labs(x = "Site", 
       y = "Number of fish",
       fill = "Size class")

### Three or more 
### Three or more variables: length vs depth with size for weight 
ggplot(data = data_lionfish, 
       aes(x = depth_m, 
           y = total_length_mm, 
           size = total_weight_gr,
           color = temperature_C)) + 
  geom_point() + 
  scale_size_continuous(range = c(1,5)) + 
  labs(x = "Depth (m)", 
       y = "Total length (mm)",
       size = "Weight (gr)", 
       color = "Temperature (°C)")

### Modifying Color 
### example 
p <- ggplot(data = data_lionfish, 
            aes(x = depth_m, 
                y = total_length_mm, 
                size = total_weight_gr,
                color = temperature_C)) + 
  geom_point() + 
  scale_size_continuous(range = c(1,5)) + 
  labs(x = "Depth (m)", 
       y = "Total length (mm)",
       size = "Weight (gr)", 
       color = "Temperature (°C)") + 
  scale_colour_viridis_c(option = "mako")
p

p + 
  scale_color_gradientn(colours = palette_IPCC(var = "temp", type = "seq"))

### Green to orange
p + 
  scale_color_gradient(low = "green", 
                       high = "orange")

### Going through white
p +
  scale_color_gradient2(low = "green",
                        mid = "white",
                        high = "orange")
# Set white at 0
p +
  scale_color_gradient2(low = "green",
                        mid = "white",
                        high = "orange",
                        midpoint = 29)

p <- ggplot(data = data_milton,
            aes(x = iso_time, y = pressure, color = sshs)) +
  geom_point()

p

p +
  scale_color_manual(values = palette_UM(n = 10))

p + 
  scale_color_viridis_d()

# Example with manual color scale for discrete variables
ggplot(data = data_lionfish, aes(x = site, fill = size_class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("small" = "lightblue", 
                               "medium" = "blue", 
                               "large" = "darkblue")) +
  labs(x = "Site", y = "Number of fish", fill = "Size class")

# Save plot to object
p <- ggplot(data = data_lionfish,
            mapping = aes(x = depth_m, y = total_length_mm)) +
  geom_point(shape = 21, fill = "steelblue", size = 2) +
  labs(x = "Depth (m)",
       y = "Total length (mm)",
       title = "Body length and depth")
# Save plot to file
ggsave("lionfish_depth_length.png", p, width = 8, height = 6, dpi = 300)

p <- ggplot(data = data_milton,
            mapping = aes(x = iso_time,
                          y = pressure,
            )) +
  geom_line() +
  geom_point(aes(color = wind_speed),
             size = 2) 

p +
  scale_color_gradientn(colours = palette_IPCC(var = "wind", type = "seq"))

### Facet wrap example 
ggplot(data = data_lionfish, 
       aes( x = depth_m, 
            y = total_length_mm)) + 
  geom_point() + 
  facet_wrap(~size_class, ncol = 3) + 
  labs( x = "Depth (m)", 
        y = "Total length (mm)")

# Facet grid example
ggplot(data = data_lionfish, aes(x = depth_m, y = total_length_mm)) +
  geom_point() +
  facet_grid(size_class ~ site) +
  labs(x = "Depth (m)", y = "Total length (mm)")

# Facet wrap with different scales
ggplot(data = data_lionfish, aes(x = depth_m, y = total_length_mm)) +
  geom_point() +
  facet_wrap(~size_class, ncol = 3, scales = "free_y") +
  labs(x = "Depth (m)", y = "Total length (mm)")

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------