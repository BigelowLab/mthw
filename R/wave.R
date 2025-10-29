
#' Generate the MTH wave data
#' 
#' @export
#' @param db tabular database for a region
#' @param dates Date, two element start and stop
#' @param region 
#' @return stars object with sst (thetao), q10, q90 and mwe (marine weather event)
#'    heating events are positive, cooling events are negative
generate_wave = function(db = read_database() |> 
                           dplyr::filter(name == "temp", depth == "sur"),
                         dates = c(Sys.Date() - 5, Sys.Date() + 10),
                         region = dplyr::pull(db, dplyr::all_of("region"))[1],
                         cDB = andreas::read_database(andreas::copernicus_path(region), multi = TRUE)){
  if(FALSE){
    db = read_database() |> 
      dplyr::filter(name == "temp", depth == "sur")
    dates = c(Sys.Date() - 5, Sys.Date() + 10)
    region = dplyr::pull(db, dplyr::all_of("region"))[1]
    cDB = andreas::read_database(andreas::copernicus_path(region), multi = TRUE)
  }
  dates = seq(from = dates[1], to = dates[2], by = "day")
  doys = format(dates, "%j")
  coppath = andreas::copernicus_path(region)
  mthw_path = andreas::copernicus_path("mthw")
  cDB = cDB |>
    dplyr::ungroup() |>
    dplyr::mutate(doy = format(.data$date, "%j")) |>
    dplyr::filter(.data$name %in% db$name, 
                  .data$depth %in% db$depth,
                  .data$date %in% dates)
    
  ss = cDB |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        d = dplyr::filter(db,
                          name == row$name,
                          depth == row$depth,
                          doy == row$doy) |>
            dplyr::arrange(.data$percentile)
        p = dplyr::filter(d,
                          name == row$name,
                          depth == row$depth,
                          doy == row$doy) |>
          dplyr::arrange(.data$percentile) |>
          read_mthw(mthw_path) |>
          rlang::set_names(d$percentile)
        
        s = andreas::compose_filename(row, coppath) |>
          stars::read_stars() |>
          rlang::set_names(row$name)
        stars::st_dimensions(s) <- stars::st_dimensions(p)
        
        mwe = p['q90'] * 0              # marine wave event
        ix = s[[1]] > p['q90'][[1]]     # above hi
        mwe[[1]][ix] <- 1
        ix = s[[1]] < p['q10'][[1]]     # below low
        mwe[[1]][ix] <- -1
        names(mwe) <- "mwe"
        c(s, p, mwe, along = NA_integer_, tolerance = 1e-6)
      }) 
  x = do.call(c, append(ss, list(along = list(time = cDB$date))))
  add_class(x, "mwe")
}


#' Given marine wave event data, compute duration of waves
#' 
#' @export
#' @param x stars object that includes 'mwe' 
#' @param event_window num, the number of days needed to define an event
#' @param gap_width num, when gaps of this or fewer days occur between events, 
#'   try to merge the events
#' @param mask logical, if TRUE, then mask the result so only events of 
#'   event_window days are returned.  Shorter events are set to 0.
#' @return stars object with records of thermal waves duration (hence "mwd" attribue name).
encode_wave = function(x = generate_mtw(), 
                       event_window = 5, 
                       gap_width = 2,
                       mask = TRUE){
  if ('mwe' %in% names(x)) x = x['mwe']
  m = x[[1]]
  R = apply(m, 1:2, 
            function(v = c(0,1,1,1,1,1,0,0,1,1,1,1,1,1,-1)){
              orig_na = is.na(v)
              r = rle(v)
              # right here we need to deal with gaps <= gap_width
              # if a gap < gap_width and before/after are the same sign
              # and greater than event_window than it is 
              ix = r$lengths <= gap_width
              if (any(ix)){
                ix = which(ix)
                
                for (i in ix){
                  if (i == 1){
                    # skip - do nothing
                  } else if (i == length(r$lengths)) {
                    # skip - do nothing
                  } else {
                    # merge the runs... by adjusting v
                    if (sign(r$values[i-1]) == sign(r$values[i+1]) &&
                        r$lengths[i-1] >= event_window[1] && 
                        r$lengths[i+1] >= event_window[1] ){
                      # gap is less than gap_width, 
                      # signs before and after are the same
                      # each is at least event_window long
                      starts = cumsum(c(1, r$lengths))
                      start = starts[i-1]
                      len = sum(r$lengths[(i-1):(i+1)])
                      index = seq(from = start, length = len)
                      v[index] = r$values[i-1]
                    } # signs are the same before and after - otherwise skip
                  }
                }
                r = rle(v)
              } # any ix
              n = rep(r$lengths, times = r$lengths) * sign(v)
              if (!identical(orig_na, is.na(n))) browse()
              n
            }) |>
    aperm(c(2,3,1))
  if (mask) {
    ix = R > (-1*event_window) & R < event_window
    R[ix] <- 0
  }
  x[[1]] <- R
  names(x) <- "mwd"
  
  add_class(x, "mwd")
}

#' A slicer that retains class
#' 
#' @export
#' @param x stars object
#' @param date numeric or Date identifying the slabs of time to slice
#' @param the input sliced and with its original class label
slice_date = function(x, date){
  k = class(x)[1]
  if (inherits(date, "Date")){
    dates = stars::st_get_dimension_values(x, "time")
    ix = which(dates %in% date)
    r = dplyr::slice(x, "time", ix) |>
      add_class(k)
  } else {
    r = dplyr::slice(x, "time", date) |>
      add_class(k)
  }
  r
}

#' Add a class membership to any object
#' 
#' This is handy when slicing/filtering stars objects which may drop 
#' any extra class label you hmay have added earlier
#' 
#' @export
#' @param x stars object
#' @param name chr, the class name(s) to add to the object
#' @return the input, `x`, which class augmentation
add_class = function(x, name){
  class(x) <- c(name, class(x))
  x
}

#' Round numbers to nearest base value
#' @seealso https://stat.ethz.ch/pipermail/r-help/2009-August/401258.html
#' 
#' @export
#' @param x numeric numbers to round
#' @param base number, round to the nearest `base`
mround <- function(x,base){
  base*round(x/base)
}


#' Check if a stars object has a time dimension
#' 
#' @export
#' @param x stars object
#' @param names chr, possible time related dimension names
#' @return logical
has_time = function(x, names = c("time", "date", "doy")){
  any(names %in% names(stars::st_dimensions(x)))
}


count_mwe = function(x = generate_wave()){
  m = x['mwe'][[1]]
  mhi = (m > 0) * 1
  mhics = apply(mhi, 1:2, cumsum) |>
    aperm(c(2,3,1))
  mlo = (m < 0) * 1
  mlocs = apply(mlo, 1:2, cumsum) |>
    aperm(c(2,3,1))
  
  m = m * 0
  ihi = which(mhics > 0)
  m[ihi] = mhics[ihi]
  ilo = which(mlocs > 0)
  m[ilo] = 0 - mlocs[ilo]
  
  r = x['mwe'] |>
    rlang::set_names("mtw")
  r[[1]] = m
  r
}