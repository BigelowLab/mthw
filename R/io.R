#' Read a database of MTHW data into a stars object
#' 
#' @export
#' @param db a tabular database that must be symmetric about name, depth and percentile
#' @param path chr, the path to the database
#' @return stars object of [name_depth_percentile, x, y, doy] or the input table augmented
#'   with a 'data' list column of individual stars objects
read_mthw = function(db = read_database() |>
                       dplyr::filter(doy %in% c("001", "002", "003"),
                                     name %in% c("temp", "sal")), 
                     path = andreas::copernicus_path("mthw"),
                     form = c("stars", "table")[1]){
  
  
  if (tolower(form[1]) == 'stars'){
    r = db |>
      dplyr::group_by(name, depth, percentile) |>
      dplyr::arrange(doy, .by_group = TRUE) |>  # by group
      dplyr::group_map(
        function(grp, key){
          id = paste(key$name, key$depth, key$percentile, sep = "_")
          files = compose_filename(grp, path)
          stars::read_stars(files, along = list(doy = grp$doy), crs = 4326) |>
            rlang::set_names(id)
        }, .keep = TRUE)
    
    r = do.call(c, append(r, list(along = NA_integer_)))
  } else {
    r = dplyr::rowwise(db) |>
      dplyr::group_map(
        function(grp, key){
          s = stars::read_stars(compose_filename(grp, path), crs = 4326) |>
            rlang::set_names(grp$name)
        })
    r = dplyr::mutate(db, data = r)
  }
  r
}