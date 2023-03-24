# Script summary ----------------------------------------------------------
# Script for workshop on data visualization with R
# Updated - March 23, 2023
#
# Install and load packages used throughout script ------------------------
install.packages('pacman')
# Load necessary packages
pacman::p_load(tidyverse, raster, ggthemes, sp, sf, rasterVis, palmerpenguins)

# Static visualization - Old Faithful -------------------------------------
# Create plot object
plot <- ggplot(data=faithful,
               mapping=aes(x=eruptions,
                           y=waiting)) +
  geom_smooth(method="lm") +
  geom_point(size=0.5)

# Clean up overall plot with some theme
plot <- plot +
  theme(plot.background=element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        axis.ticks=element_line(linewidth=0.25),
        axis.text=element_text(size = 8),
        axis.line=element_line(linewidth=0.25),
        axis.title=element_text(size = 10),
        plot.title=element_text(hjust=.5, size=10,
                                face='bold')) +
  labs(title="Old Faithful Waiting Time",
       x="Length of eruption (minutes)",
       y="Waiting time to next eruption (minutes)")

# Static visualization - Palmer Penguins ----------------------------------
slice_head(penguins, n=3)

# Visualize body mass by flipper length
penguins |> 
  drop_na() |> 
  ggplot(aes(x=flipper_length_mm,
             y=body_mass_g)) +
  geom_point(aes(color=species)) +
  geom_smooth(method='lm',
              color='black') +
  scale_y_continuous(limits=c(2500,6500),
                     breaks=seq(2500,6500,1000)) +
  scale_color_discrete(name='Penguin species:') +
  labs(x='Flipper length (mm)',
       y='Body mass (g)') +
  theme_minimal() +
  theme(legend.position='top')

# Plot 1
plot1 <- penguins |> 
  drop_na() |> 
  ggplot(aes(x=flipper_length_mm,y=body_mass_g)) +
  geom_point(aes(color=species)) +
  geom_smooth(method='lm') +
  theme_minimal() +
  theme(legend.position='top',
        legend.title=element_blank())

# Plot 2
plot2 <- penguins |> 
  drop_na() |>
  ggplot(aes(x=species,y=body_mass_g)) +
  geom_boxplot(aes(color=species)) +
  theme_minimal() +
  theme(legend.position='none',
        legend.title=element_blank())

# Plot 3
plot3 <- penguins |> 
  drop_na() |>
  ggplot(aes(x=body_mass_g,fill=species)) +
  geom_histogram(alpha=0.5,position="identity") +
  theme_minimal() +
  theme(legend.position='none',
        legend.title=element_blank())

plot1 + plot2

plot1 / plot2

(plot1 + plot2) / plot3

(plot1 + plot2) / plot3 + plot_layout(heights=c(2,1))

# Visualize body mass by flipper length with facets
penguins |> 
  drop_na() |> 
  ggplot(aes(x=flipper_length_mm,
             y=body_mass_g)) +
  geom_point(aes(color=species), show.legend=FALSE) +
  geom_smooth(method='lm',
              color='black') +
  scale_y_continuous(limits=c(2500,6500),
                     breaks=seq(2500,6500,1000)) +
  scale_color_discrete(name='Penguin species:') +
  facet_wrap(vars(species),ncol=1) +
  labs(x='Flipper length (mm)',
       y='Body mass (g)') +
  theme_minimal()

install.packages(ggthemes); library(ggthemes)
# Excel theme
p1 <- ggplot(data=penguins) +
  geom_point(aes(x=flipper_length_mm,
                 y=body_mass_g)) +
  theme_excel()
# Wall Street Journal theme
p2 <- p1 + theme_wsj()
# Google Docs theme
p3 <- p1 + theme_gdocs()
# Clean ggplot theme
p4 <- p1 + theme_clean()
# Patchwork
(p1 + p2) / (p3 + p4)

# plotly ------------------------------------------------------------------
# Plotly visualization using gapminder data
# Create ggplot object with gapminder data
plot <- ggplot(data=gapminder, 
               aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
  # Add a point geom
  geom_point(alpha=0.7, show.legend=FALSE, aes(frame=year, ids=country)) +
  # Add some manual scaling
  scale_colour_manual(values=continent_colors) +
  scale_size(range=c(2, 12)) +
  scale_x_log10() +
  labs(x='GDP per capita', 
       y='Life expectancy at birth') +
  # Add some theme elements
  theme(panel.background=element_rect(color="black", fill="lightyellow", size=0.25),
        panel.grid=element_blank(),
        plot.title=element_text(size=9, color="black"),
        strip.background=element_rect(color="black", size=0.25),
        axis.ticks=element_line(size=0.25),
        axis.text=element_text(size=7, color="black"),
        axis.title=element_text(size=8, color="black"))

# Pass ggplot object to plotly
plotly <- ggplotly(plot, height=500, width=800) |> 
  animation_opts(1000, easing="linear", redraw=FALSE) |> 
  animation_slider(currentvalue=list(prefix="YEAR ", font=list(color="red"), xanchor='left', yanchor='top')) |> 
  animation_button(x = 1, xanchor = "right", y = -0.2, yanchor = "bottom")

plotly

# rayshader ---------------------------------------------------------------
pacman::p_load(rayshader)
# 3D visualization of South Mountain
# Read raster dataset from url and convert to data frame
url      <- "https://github.com/jeremymack-LU/rviz/blob/master/data/smtn_dem_clip.tif?raw=true"
r.dem    <- raster::raster(url)
r.df     <- raster::as.data.frame(r.dem, xy=TRUE)

# Drop NAs and rename raster grid cell variable
r.df     <- r.df |>
  drop_na() |>
  rename(elevation=3)

# Create ggplot2 object
r.plot <- ggplot() +
  geom_raster(data=r.df,
              aes(x=x, y=y, fill=elevation),
              interpolate=TRUE,
              show.legend=TRUE) +
  scale_fill_gradientn(name = "Elevation", colors=terrain.colors(10)) +
  coord_quickmap() +
  theme_dark(base_size=6) +
  theme(axis.title=element_blank(), legend.position='top', legend.direction='horizontal')

# Pass ggplot2 object to rayshader function
plot_gg(r.plot,
        multicore=TRUE,
        width=5,
        height=5,
        scale=200,
        windowsize=c(1400,866),
        zoom=0.55,
        phi=30)

# Capture snapshot of the rgl window
render_snapshot(filename="southmountain", clear=TRUE)

# 3D visualization of mtcars data
# Load viridis package
library(viridis)
# Plot using mtcars dataset
gg_cars <- ggplot(mtcars) + 
  geom_point(aes(x=disp, y=mpg, color=wt), size=2) +
  scale_fill_viridis("Weight") +
  ggtitle("mtcars: Displacement vs mpg vs # of cylinders") +
  theme_dark() +
  theme(title = element_text(size=8),
        text = element_text(size=12),
        legend.position = 'bottom')

# Pass ggplot2 object to rayshader function
plot_gg(gg_cars,
        multicore=TRUE,
        width=6,
        height=5,
        scale=250,
        windowsize=c(1400,866),
        zoom=0.55,
        phi=30)

# Capture snapshot of the rgl window
render_snapshot(filename="mtcars", clear=TRUE)
