# loading fonts (before loading ggplot) -----------------------------------

library(needs)
needs(extrafont, extrafontdb)
extrafont::loadfonts(quiet = TRUE, device = "win")
# extrafont::loadfonts(quiet = TRUE, device = "postscript")

# library(showtext)
# sysfonts::font_add(family = "PT Sans Pro Narrow", 
#                    regular = "C:/Users/cdietrich/AppData/Local/Microsoft/Windows/Fonts/PTSansNarrow-Regular.ttf")
# sysfonts::font_add_google(name = "PT Sans Narrow", 
#                           family = "ptsansnarrow",
#                           db_cache = TRUE)
# showtext::showtext_auto()

# RUN ON NEW SYSTEM
# font_import()
# # or
# font_import(paths = NULL, recursive = TRUE, prompt = TRUE, pattern = NULL)
# # check import
# fonts()
# fonttable()
# # load fonts that aren't available otherwise; but first put them in a location
# font_import(path = "C:/Users/cdietrich/Downloads/PT_Sans_Narrow/PTSansNarrow-Regular.ttf")
# font_import(paths = "C:/Users/cdietrich/AppData/Local/Microsoft/Windows/Fonts",
#             recursive = FALSE,
#             pattern = "PT")

# packages ----------------------------------------------------------------

needs(here, tidyverse, readxl)

# parameters --------------------------------------------------------------

# output dimensions
width_cp_onecol <- 65.767     # single column in CP 
width_cp_twocol <- 140        # two columns in CP 
width_cp_full <- 180          # full page in CP
height_cp <- 221.9            # full page height in CP
width_book_col <- 108         # main column in book
width_book_half <- 60         # half column in book
width_book_full <- 135        # full page in book
height_book <- 181.25         # full page height in book
width_brief_onecol <- 85      # half column in brief
width_brief_twocol <- 180     # two columns in brief
width_brief_full <- 210       # full page in brief
height_brief <- 258.233       # full page height in brief

# text
# windowsFonts(sg = windowsFont("Segoe UI"))
# txt_family <- "ptsansnarrow"
txt_family <- "PT Sans Narrow"
# txt_family <- "PTSans-Narrow"
# txt_family <- "Bahnschrift"
# txt_family <- "mono"
txt_bold <- "bold"
txt_height <- .85
txt_label <- 2.66             # label size
txt_params <- list(lineheight = .85, 
                   family = txt_family,
                   size = txt_label)

# no plot margins
mar0 <- theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "mm"))

# lines
lwd_grid <- 0.176 * 1.32    	# linewidth in mm + adjustment
lwd_line <- 0.353 * 1.325   	# linewidth in mm

# colors
# basic
col_grid <- "#C6C6C6"
col_axis <- "#1D1D1B"
col_axis_text <- "#646363"
grey25 <- "#646363"

# visual ID
# primary
blue <- "#113655"
lightorange <- "#fab85f"
orange <- "#f28c00"

# warm
egg <- "#ffde74"
lightorange <- "#f9b466"
orange <- "#f28d22"
fuchsia1 <- "#ec5f5b"
fuchsia2 <- "#df3144"
fuchsia3 <- "#a41e26"
mauve <- "#33163a"

# cold
teal <- "#00A0C1" # euiss_col1
grey <- "#595959" # euiss_col2
teal0 <- "#86baac"
teal1 <- "#64c2c7"
teal2 <- "#376882" 
teal3 <- "#1d3956"

# green
frog <- "#4cb748"
mint <- "#99cb92"


# custom geoms ------------------------------------------------------------

# points
euiss_point <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                        ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, 
                      size = 2, 
                      # col = teal, 
                      shape = 21, fill = NA,
                      ...))
}

# bars
euiss_col <- function(mapping = NULL, data = NULL, position = "stack", ..., 
                      width = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = "identity", 
        geom = GeomCol, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(width = .25, 
                                                 na.rm = na.rm, ...))
}

euiss_bar <- function(mapping = NULL, data = NULL, stat = "count", position = "stack", 
                      ..., width = NULL, binwidth = NULL, na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE) 
{
  if (!is.null(binwidth)) {
    warning("`geom_bar()` no longer has a `binwidth` parameter. ", 
            "Please use `geom_histogram()` instead.", call. = "FALSE")
    return(geom_histogram(mapping = mapping, data = data, 
                          position = position, width = width, binwidth = binwidth, 
                          ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes))
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomBar, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(width = .25, na.rm = na.rm, ...))
}

# text
euiss_text <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                       ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, 
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", 
           call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(parse = parse, check_overlap = check_overlap, 
                      lineheight = .85, 
                      family = txt_family,
                      size = txt_label, hjust = "left",
                      na.rm = na.rm, ...))
}

# line
euiss_line <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) 
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomLine, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, 
                      col = teal,
                      size = lwd_line, ...))
}

# lollipop
euiss_lollipop_x <-   function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                               ..., na.rm = FALSE, orientation = NA, show.legend = NA, 
                               inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomLinerange, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, 
                      orientation = orientation, 
                      xmin = 0,
                      # ymin = 0,
                      # col = teal,
                      size = lwd_line,
                      ...))
}

euiss_lollipop_y <-   function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                               ..., na.rm = FALSE, orientation = NA, show.legend = NA, 
                               inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomLinerange, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, 
                      orientation = orientation, 
                      ymin = 0,
                      # ymin = 0,
                      # col = teal,
                      size = lwd_line,
                      ...))
}

# area
euiss_area <- function(mapping = NULL, data = NULL, stat = "identity", position = "stack", 
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) 
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomArea, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, 
                      fill = alpha(teal, .1),
                      # color = teal,
                      outline.type = "upper", ...))
}

# annotation
euiss_annotate <- function(geom = "text", x = NULL, y = NULL, xmin = NULL, xmax = NULL, 
                           ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ..., 
                           na.rm = FALSE) 
{
  position <- compact(list(x = x, xmin = xmin, xmax = xmax, 
                           xend = xend, y = y, ymin = ymin, ymax = ymax, yend = yend))
  aesthetics <- c(position, list(...))
  lengths <- vapply(aesthetics, length, integer(1))
  n <- unique(lengths)
  if (length(n) > 1L) {
    n <- setdiff(n, 1L)
  }
  if (length(n) > 1L) {
    bad <- lengths != 1L
    details <- paste(names(aesthetics)[bad], " (", lengths[bad], 
                     ")", sep = "", collapse = ", ")
    stop("Unequal parameter lengths: ", details, call. = FALSE)
  }
  data <- new_data_frame(position, n = n)
  layer(geom = geom, params = list(na.rm = na.rm, 
                                   lineheight = .85, 
                                   family = txt_family,
                                   size = txt_label,
                                   ...), stat = StatIdentity, 
        position = PositionIdentity, data = data, mapping = aes_all(names(data)), 
        inherit.aes = FALSE, show.legend = FALSE)
}

# x-axis
euiss_x_axis <- function(mapping = NULL, data = NULL, ..., yintercept, na.rm = FALSE, 
                         show.legend = NA) 
{
  if (!missing(yintercept)) {
    if (!missing(mapping)) {
      warning(paste0("Using both `yintercept` and `mapping` may not have the", 
                     " desired result as mapping is overwritten if", 
                     " `yintercept` is specified/n"))
    }
    data <- new_data_frame(list(yintercept = yintercept))
    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }
  layer(data = data, mapping = mapping, stat = StatIdentity, 
        geom = GeomHline, position = PositionIdentity, show.legend = show.legend, 
        inherit.aes = FALSE, params = list(na.rm = na.rm, 
                                           yintercept = 0,
                                           size = lwd_grid, 
                                           col = col_grid,
                                           ...))
}

# y-axis
euiss_y_axis <- function(mapping = NULL, data = NULL, ..., xintercept, na.rm = FALSE, 
                         show.legend = NA) 
{
  if (!missing(xintercept)) {
    if (!missing(mapping)) {
      warning(paste0("Using both `xintercept` and `mapping` may not have the", 
                     " desired result as mapping is overwritten if", 
                     " `xintercept` is specified/n"))
    }
    data <- new_data_frame(list(xintercept = xintercept))
    mapping <- aes(xintercept = xintercept)
    show.legend <- FALSE
  }
  layer(data = data, mapping = mapping, stat = StatIdentity, 
        geom = GeomVline, position = PositionIdentity, show.legend = show.legend, 
        inherit.aes = FALSE, params = list(na.rm = na.rm, 
                                           xintercept = 0,
                                           size = lwd_grid, 
                                           col = col_grid,
                                           ...))
}


# custom theme ------------------------------------------------------------

theme_set(theme_minimal() +
            theme(
              
              text = element_text(family = txt_family),
              # title = element_text(face = txt_bold),
              
              legend.position = "top",
              legend.justification = c("left", "top"),
              legend.key.height = unit(5, "points"),
              legend.title = element_text(size = rel(.75)),
              # legend.title.align = 1,
              legend.text = element_text(size = rel(.75), color = col_axis),
              
              axis.title = element_blank(),
              axis.text.y = element_text(size = rel(.75), color = col_axis_text),
              axis.text.x = element_text(size = rel(.75), color = col_axis_text),
              
              strip.text = element_text(hjust = 0),
              strip.text.x = element_text(size = rel(10/9),
                                          lineheight = .85),
              strip.text.y = element_text(size = rel(10/9),
                                          lineheight = .85),
              # axis.line.x.bottom = element_line(color = col_axis, size = lwd_grid),
              
              line = element_line(size = 2, lineend = "round"),
              
              # background
              plot.background = element_rect(fill = "white",
                                             color = NA),
              
              # theoretical grid styling
              panel.grid = element_line(color = col_grid, size = lwd_grid),
              # white grid
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_line(color = "white"),
              panel.grid.major.y = element_line(color = "white"),
              
              # axis ticks instead
              axis.ticks.length = unit(10, "pt"),
              # axis.ticks.y = element_line(color = col_grid, size = lwd_grid),
              # axis.ticks.x = element_line(color = col_grid, size = lwd_grid),
              
              plot.margin = margin(0, 0, 0, 0, "mm"),
              plot.title = element_text(color = "black", face = txt_bold, size = 24),
              plot.title.position = "plot",
              plot.subtitle = element_text(color = grey25, face = "plain", size = 10),
              plot.caption = element_text(color = grey25, size = 6))
          
)


# color palettes ----------------------------------------------------------

# manual
pal_cat <- colorRampPalette(c(fuchsia2, teal, orange, teal3, egg, mauve))
pal_div <- colorRampPalette(c(teal3, teal, col_grid, egg, fuchsia2))
pal_seq_r <- colorRampPalette(c(fuchsia3, fuchsia2, fuchsia1, egg))
pal_seq_g <- colorRampPalette(c(teal3, teal, mint, egg))
pal_seq_b <- colorRampPalette(c(teal3, teal2, teal, teal1))
pal_seq_v <- colorRampPalette(c(col_grid, teal2, mauve, fuchsia2))
pal_seq_vir <- colorRampPalette(c(mauve, teal2, fuchsia1, egg))

# gradients
euiss_color_continuous <- scale_color_gradientn(colors = pal_seq_b(4),
                                                aesthetics =  c("color", "fill"))
euiss_color_discrete <- scale_color_manual(values = pal_cat(6), 
                                           aesthetics =  c("color", "fill"))


# other themes ------------------------------------------------------------

theme_ticks <- theme_get() +
  theme(
    
    line = element_line(size = 2, lineend = "round"),
    
    # axis ticks instead
    axis.ticks.length = unit(5, "pt"),
    axis.ticks.y = element_line(color = col_grid, size = lwd_grid),
    axis.ticks.x = element_line(color = col_grid, size = lwd_grid))

# default colors ----------------------------------------------------------

options(ggplot2.discrete.fill = pal_cat(6),
        # ggplot2.continuous.fill = pal_seq_vir(4),
        ggplot2.ordinal.fill = pal_seq_v(4),
        # ggplot2.manual.fill = pal_cat(6),
        ggplot2.binned.fill = pal_cat(6), 
        ggplot2.discrete.colour = pal_cat(6),
        # ggplot2.continuous.colour = pal_seq_vir(4),
        ggplot2.ordinal.colour = pal_seq_v(4),
        # ggplot2.manual.colour = pal_cat(6),
        ggplot2.binned.colour = pal_cat(6))
