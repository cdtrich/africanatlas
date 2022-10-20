geom_sf_coast <- function(...) {geom_sf(data = coastline_sf, 
                                        size = lwd_grid, col = col_grid,
                                        fill = NA)
}

geom_coast <- function(...) {geom_sf(data = coastline, 
                                     size = lwd_grid, col = col_grid,
                                     fill = NA)
}

downloadunzip <- function(df, url, file, unlink = TRUE) {
  temp <- tempfile()
  download.file(url, temp)
  data <- read.table(unz(temp, file))
  
  if (unlink) {
    unlink(temp)
  } 
}
