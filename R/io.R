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

#' Read and write mthw raster data
#' 
#' @export
#' @param x stars object
#' @param filename chr then name of the file
#' @return for `write_raster` and `read_raster` a stars object, while 
#'   `get_filename` returns a characater file path
write_raster = function(x, filename){
  if (missing(filename)) filename = "mthw.rds"
  saveRDS(x, filename[1])
  invisible(x)
}

#' @rdname write_raster
#' @export
read_raster = function(filename = mthw_filename(path = system.file(package = "mthw"))){
  readRDS(filename[1])
}

#' Generate a mthw-centric filename 
#' @export
#' @param region chr the region code
#' @param variable chr a single variable to read
#' @param depth chr a single depth to read
#' @param extension chr, the file extension (with dot)
#' @param path chr, the file path
mthw_filename = function(region = "chfc",
                        variable = "temp", 
                        depth = "sur",
                        extension = c(".rds", ".png", ".pdf", ".jpg")[1],
                        path = c("/mnt/ecocast/corecode/R/mthw/inst",
                                 system.file(package = "mthw"))[1]){
  file.path(path,
            sprintf("exdata/%s_%s_%s%s", 
                    region[1],
                    variable[1], 
                    depth[1],
                    extension[1]))
}




