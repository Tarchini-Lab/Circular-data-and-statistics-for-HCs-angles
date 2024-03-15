# ToDo:
# 1. Replace flatten with standard R function
# 2. Merge arrow and arc in one routine
# 3. Use plot object to fetch optimum y-range.


############################# start of hearing_library ########################

#------------------------ flatten ------------------------------

flatten <- function(df) {
  # Returns a data.frame with two columns:
  #     specimen = names(df)004
  #     angle    = values in the column for the specimen
  #
  # Input looks like csv file or excel table.
  # Return all the columns joined vertically.
  
  expt = data.frame(specimen=names(df)[1], angle=df[[1]])
  if (length(df)==1) return(expt)
  rbind(expt, flatten(df[-1]))
}


#------------------------- colstats -----------------------------


colstats <- function(expt) {
  # Find the circular mean and meandeviation for each specimen
  
  # Functions that apply the na.rm=TRUE option
  mc <- function(x) {
    mean.circular(x, na.rm=TRUE)
  } 
  md <- function(x) {
    meandeviation(x, na.rm=TRUE)
  } 
  
  # Convert angles to radians
  r <- expt
  r$angle <- as.circular(rad(r$angle))
  #r$angle <- rad(r$angle)
  
  # Calculate the 2 statistics  
  a <- ddply(r, .(specimen), colwise(mc))
  d <- ddply(r, .(specimen), colwise(md))
  
  # Convert angle to degrees
  a[2] = deg(a[2])
  d[2] = deg(d[2])
  
  # Assemble the data.frame for ggplot facets
  m=merge(a, d, by="specimen")
  names(m) = c("specimen", "meanAngle", "meanDeviation")
  
  # Angles have to be positive
  m$meanAngle = ifelse((y=m$meanAngle)<0, y+360, y)
  
  return(m)
}

#------------------------- calc_arrows -----------------------------

calc_arrows <- function(p, data) {
  # 
  # Construct a data.frame of the arrow angle and length and an arc
  # at the end of the arrow shaft proportional to the angle stdev
  # scaled to the max density in each graph.
  
  # Obtain the ggplot object 
  gdetail <- ggplot_build(p)
  
  # Extract the density for each specimen. The specimen is
  # sequentially identified by PANEL.
  density <- subset(gdetail$data[[1]], TRUE ,c(PANEL,density))
  
  # Restore the specimen names
  density <- merge(density, gdetail$layout$layout, by="PANEL")[c(5,2)]
  
  # Find the max density for each specimen
  md <- ddply(density, .(specimen), colwise(max))
  
  # Merge the stats by specimen
  md <- merge(md, data, by="specimen")
  
  # Calculate arrow length reduced by meandeviation 
  # md$length <- with(md, density*meanAngle/(meanAngle+meanDeviation))
  md$length <- with(md, density)
  print(md)
  
  # Create two columns for the arc angle and length
  md$arc_angle = md$meanAngle + 0.5*md$meanDeviation
  md$arc_length = md$length

  # Calculate the starting point for each arrow.
  md_origin <- data.frame(
    specimen=gdetail$layout$layout[4], 
    density=0, 
    meanAngle=md$meanAngle, 
    meanDeviation=0, 
    length=0,
    arc_angle=md$meanAngle - 0.5*md$meanDeviation,
    arc_length=md$length)
  
  # Bind the starting and ending rows together
  md <- rbind(md_origin, md)

  return(md)
  
}

#------------------------- draw_graphs  -----------------------------

draw_graphs <- function(excelfile, bin_angle) {
  
  # bin_angle : binwidth in degrees for the polar histogram
  # excelfile : Spreadsheet with column angles for each specimen
  
  # ------------------------- Setup ------------------------------
  
  library(ggplot2)
  library(circular)
  library(dplyr)
  library(plyr)
  library(readxl)
  
  # Build a new layer "coord_polar" that supports free scales.
  # Ref: github.com/tidyverse/ggplot2/issues/2815
  cp <- coord_polar(theta = "x",
                    start = -90 * pi / 180,
                    direction = -1)
  cp$is_free <- function()
    TRUE
  
  # ------------------------- Calculation ------------------------------
  
  # Load the data:
  
  Angle_dataset <- read_excel(excelfile)
  
  
  # Alter shape of data into 2 columns: [specimen, angle].
  # This allows all the specimens to be graphed at same time as facets.
  
  expt <- flatten(Angle_dataset)
  
  
  # Calculate the circular mean and meandeviation for each specimen.
  
  stats <- colstats(expt)
  
  print("stats:")
  print(stats)
  
  
  # First, build a graph so its dimensions can be used to scale arrows.
  # Note:
  #   alpha=0.7      : Make the bars transparent to show grid.
  #   scale="free_y" : Allow radial scale of the graphs to vary independently.
  #
  
  p <- ggplot(data = expt) +
    geom_histogram(
      aes(x = angle, y = bin_angle * (..density..)),
      binwidth = bin_angle,
      center = round(bin_angle / 2, 0),
      fill = "darkturquoise",
      col = "black",
      size = 0.2,
      alpha = 0.7
    ) +
    coord_polar(theta = "x",
                start = -90 * pi / 180,
                direction = -1) +
    facet_wrap(vars(specimen))
  
  
  # Use the extents of the graph and stats to define arrows
  # to indicate the meanAngle and error for each specimen.
  # ~length    : max(circularMean of angle) reduced in length
  #              proportionally to its meandeviation.
  # ~meanAngle : circular mean of the angle.
  # -arc_angle : Arc at end of arrow indicating stdev of meanAngle
  # -arc_length: Length of the arrow
  
  arrows <- calc_arrows(p, stats)
  print("Arrows:")
  print(arrows)

  # Multiply by bin width
  arrows$length     <- bin_angle * arrows$length
  arrows$density    <- bin_angle * arrows$density
  arrows$arc_length <- bin_angle * arrows$arc_length
  
  # Use the max density of all panels rounded up a little as the common upper limit
  ubound = round(max(arrows$density), digits = 2)+ 0.005

  # Finish the graphs: add arrows, scale and theme.
  
  p <- p +
    
    # Draw an arrow to the mean density.
    # See https://rdrr.io/r/grid/arrow.html fro parameters of the arrow
      geom_line(
        data = arrows,
        aes(x = meanAngle, y = length),
        # arrow = arrow(angle=20, length=unit(3, "mm")),
        color = "darkblue",
        size = 1.1,
    ) +
    
    # Draw an arc proportional to the mean deviation of the angle
    geom_line(
      data = arrows,
      aes(x = arc_angle, y = arc_length), 
      color = "red",
      size = 1.1
    ) +
    
    geom_hline(yintercept = seq(0, ubound, length.out = 5), size = 0.2, color = "grey70") + 
    
    geom_vline(xintercept = seq(0,360, by = 45), size = 0.2, color = "grey70") +
    
    scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, ubound), 
                       breaks = seq(0, ubound, length.out = 5), 
                       labels = round(seq(0, ubound, length.out = 5), 2),
                       expand = c(0,0)) +
    theme(
      aspect.ratio = 1,
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank() #+
    )
  
}

############################# end of hearing_library ##########################

# "Angle dataset.xlsx" for the original data
# "Explants_Vangl2.xlsx" for simulation for 4 specimens

bin_angle = 10
excelfile = "Explants_Vangl2.xlsx"

(draw_graphs(excelfile, bin_angle))


