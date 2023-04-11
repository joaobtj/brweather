#' Title
#'
#' @param longitude longitude
#' @param latitude latitude
#' @param variables var
#' @param time time
#' @param path path
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom plyr join_all
#' @importFrom stringr word
#' @import dplyr
#' @import ncdf4
#'
#' @return Data.frame
#' @export
#'
#' @examples
#' wd <- brweather_read(
#'   longitude = -50.5995,
#'   latitude = -27.2863,
#'   variables = c("pr", "ETo"),
#'   time = c("1968-01-01", "1983-12-31"),
#'   path = "C:/Users/joaob/OneDrive - UFSC/clima_Xavier"
#' )
brweather_read <- function(longitude, latitude, variables, time = NULL, path = ".") {
  # if time is NULL than use all the time (1961 to 2020)
  if (is.null(time)) time <- c("1961-01-01", "2020-07-31")
  if(time[1]>time[2]) stop("Stop:error")
  time <- as.Date(time)

  a <- list()
  for (i in seq_along(variables)) {
    ## Files *.nc
    files_nc <- list.files(path = path, pattern = c(paste0(variables[i], "+_"), ".nc"))

    ## Loop to run all *.nc files
    var_file_nc <- list()
    for (j in seq_along(files_nc)) {
      ## Ler e abrir o arquivo *.nc
      file_nc <- ncdf4::nc_open(file.path(path, files_nc[j]))

      ## read the variables
      ### longitude, latitude, and time
      lat <- ncdf4::ncvar_get(file_nc, varid = "latitude")
      lon <- ncdf4::ncvar_get(file_nc, varid = "longitude")
      timestamp <- ncdf4::ncvar_get(file_nc, varid = "time") %>%
        magrittr::divide_by(24) %>%
        as.Date(origin = stringr::word(ncdf4::ncatt_get(file_nc, "time")$units, 3))


      ### find the closest point by the nearest Cartesian distance
      prox <- expand.grid(lat = lat, lon = lon) %>%
        dplyr::mutate(distance = sqrt((lon - longitude)^2 + (lat - latitude)^2)) %>%
        dplyr::slice_min(distance)

      start_time <- ifelse(is.na(match(time[1], timestamp)), 1,
        match(time[1], timestamp)
      )
      final_time <- ifelse(is.na(match(time[2], timestamp)), length(timestamp),
        match(time[2], timestamp)
      )

      #### file starting point
      start <- c(match(prox$lon, lon), match(prox$lat, lat), start_time)
      #### endpoint of file
      count <- c(1, 1, final_time - start_time + 1)

      ### Read the variable at the nearest point and time
      var_file_nc[[j]] <- ncdf4::ncvar_get(file_nc,
        varid = names(file_nc$var),
        start = start,
        count = count
      ) %>%
        dplyr::bind_cols(
          date = timestamp[start_time:final_time],
          lat = prox$lat,
          lon = prox$lon,
          var_name = .
        ) %>%
        dplyr::rename(!!dplyr::quo_name(variables[i]) := var_name)


      ## close the *.nc file
      ncdf4::nc_close(file_nc)
    }
    ## all periods together in a data.frame
    a[[i]] <- dplyr::bind_rows(var_file_nc)
  }
  ## return all the variables in a data.frame
  ## correct the dates
  b <- plyr::join_all(a) %>%
    slice(which(date >= time[1])) %>%
    slice(which(date <= time[2]))


  return(b)
}
