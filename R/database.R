#' Decompose one or more filenames into a simple tabular database
#' 
#' @export
#' @param x chr, one or more filenames (with complete paths)
#' @param ext chr, the file extension to strip including the dot
#' @return tabular database
decompose_filename = function(
    x = c("/mnt/s1/projects/ecocast/coredata/copernicus/mthw/chfc/temp/bot/q10/011.tif", 
          "/mnt/s1/projects/ecocast/coredata/copernicus/mthw/chfc/temp/sur/q10/353.tif", 
          "/mnt/s1/projects/ecocast/coredata/copernicus/mthw/chfc/sal/sur/q90/088.tif", 
          "/mnt/s1/projects/ecocast/coredata/copernicus/mthw/chfc/temp/bot/q10/363.tif", 
          "/mnt/s1/projects/ecocast/coredata/copernicus/mthw/chfc/temp/sur/q90/164.tif"),
    ext = andreas::get_extension(x)[1]){
  
  
  set_colnames = function(x, nm){
    colnames(x) <- nm
    x
  }
  #<root>/region/variable/depth/percentile/doy.tif
  f = sub(ext, "", x, fixed = TRUE) |>
    strsplit(.Platform$file.sep, fixed = TRUE)
  n = lengths(f)
  cnames = c("region", "name", "depth", "percentile", "doy")
  nc = length(cnames)
  sapply(seq_along(n),
    function(i){
      f[[i]][seq(from = n[i]-nc+1, n[i])]
    }) |>
    t() |>
    set_colnames(cnames) |>
    dplyr::as_tibble()
}


#' Compose one or more filenames from a tabular database
#' 
#' @export
#' @param x tabular database
#' @param path chr, the path to where the data resides
#' @param ext chr, the extension to apply (with dot) 
#' @return filenames
compose_filename = function(x = decompose_filename(), 
                            path = andreas::copernicus_path("mthw"), 
                            ext = '.tif'){
  
  file.path(path[1],
            x$region,
            x$name,
            x$depth,
            x$percentile,
            paste0(x$doy, ext))
}


#' Read a database files
#' 
#' @export
#' @param path chr the path to the database
read_database = function(path = andreas::copernicus_path("mthw", "chfc"), 
                         filename = "database"){
  x = readr::read_csv(file.path(path, filename),
                      col_types = 'ccccc')
}

#' Build a database by mining for files with a particualr extension
#' 
#' @export
#' @param path chr, the path description
#' @param save_db logical, if true then save the database under `filename`
#' @param filename chr the database filename to save under
#' @param ext chr, the file extension (with dot) that defines a record
#' @param a tabular database
build_database = function(path = copernicus::copernicus_path("mthw", "chfc"),
                          save_db = FALSE,
                          filename = "database",
                          ext = ".tif"){
  
  ff = list.files(path, 
                  pattern = glob2rx(paste0("*", ext)), 
                  full.names = TRUE, 
                  recursive = TRUE)
  db = decompose_filename(ff, ext = ext)
  if (save_db) readr::write_csv(db, file.path(path, filename))
  db  
}
