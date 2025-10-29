
# usage: ts_forecast.R [--] [--help] [--date DATE] [--region REGION] [--depth DEPTH]
# [--variable VARIABLE]
# 
# Build a MTHW forecast and graphic
# 
# flags:
#   -h, --help      show this help message and exit
# 
# optional arguments:
#   -d, --date      the date of the forecast [default: 2025-10-27]
#   -r, --region    the region code ala 'chfc' [default: chfc]
#   --depth         the depth to use such as 'sur', 'bot', ... [default: sur]
#   -v, --variable  name(s) of the variables [default: temp+sal]

suppressPackageStartupMessages({
  library(andreas)
  library(mthw)
  library(dplyr)
  library(argparser)
  library(stars)
  library(charlier)
})

parse_argument = function(x = "temp+sal,bot+sur"){
  strsplit(x, '[[:punct:]]')[[1]]
}
  

Args = argparser::arg_parser("Build a MTHW forecast and graphic",
                             name = "ts_forecast.R", 
                             hide.opts = TRUE) |>
  add_argument("--date",
               help = "the date of the forecast",
               default = format(Sys.Date(), "%Y-%m-%d"),
               type = "character") |>
  add_argument("--region",
               help = "the region code ala 'chfc'",
               default = "chfc",
               type = "character") |>  
  add_argument("--depth",
               help = "the depth to use such as 'sur', 'bot', sur+bot' etc",
               default = 'sur+bot',
               type = "character") |>
  add_argument("--variable",
               help = "name(s) of the variables such as 'temp', 'sal', 'temp+sal', etc",
               default = "temp+sal",
               type = "character") |>
  add_argument("--path",
               help = "path to the package",
               default = "/mnt/ecocast/corecode/R/mthw",
               type = "charcater") |>
  parse_args()

OUTPATH = file.path(Args$path, "inst")
Args$date = as.Date(Args$date)
Args$depth = parse_argument(Args$depth)
Args$variable = parse_argument(Args$variable)
dates = c(Args$date - 5, Args$date + 10)
andpath = andreas::copernicus_path(Args$region)
mwpath = andreas::copernicus_path("mthw")

charlier::start_logger(file.path(mwpath, "log"))

MWDB = mthw::read_database(file.path(mwpath, Args$region)) |>
  dplyr::filter(depth %in% Args$depth, 
                name %in% Args$variable)
CDB = andreas::read_database(andpath, multi = TRUE) |>
  dplyr::ungroup() |>
  dplyr::filter(depth %in% Args$depth, 
                name %in% Args$variable, 
                period == "day") 

if(FALSE){
  key = tibble(region = Args$region[1],
               name = "temp",
               depth = "sur")
  grp = filter(MWDB,
               region == key$region,
               name == key$name,
               depth == key$depth)
}

main = function(){
 MWDB |>
  dplyr::group_by(region, name, depth) |>
  dplyr::group_map(
    function(grp, key){
      charlier::info("working on %s %s %s", 
                     key$region[1],
                     key$name[1],
                     key$depth[1])
      cdb = dplyr::filter(CDB,
                          name == key$name,
                          depth == key$depth)
      mwe = mthw::generate_wave(
        db = grp,
        dates = dates,
        region = key$region,
        cDB = cdb)
      mwd = mthw::encode_wave(mwe)
      filename = mthw_filename(region = key$region,
                              variable = key$name,
                              depth = key$depth)
      charlier::info("writing data %s", filename)
      write_raster(mwd, filename)
    }, .keep = TRUE)
  return(0)
}


graphics = function() { 
  for (reg in Args$region){
    filename = mthw_filename(region = reg, 
                             variable = "temp",
                             depth = "sur",
                             path = OUTPATH)
    tempd = read_raster(filename) |>
      slice_date(Args$date)
    filename = mthw_filename(region = reg, 
                             variable = "sal",
                             depth = "sur",
                             path = OUTPATH)
    sald = read_raster(filename) |>
      slice_date(Args$date)
    gg = plot_mwd_paired(tempd, sald,
                         title = sprintf("Marine Thermohaline Waves, %s",
                                         format(Args$date, "%Y-%m-%d")))
    filename = mthw_filename(region = reg, 
                             variable = "mwd",
                             depth = "sur",
                             extension = ".png",
                             path = OUTPATH)
    charlier::info("writing graphics %s", filename)
    ggplot2::ggsave(filename, plot = gg)
  }
  return(0)
}

git = function(){
  devtools::document(Args$path)
  devtools::install(Args$path, upgrade = FALSE)
  
  orig = setwd(Args$path)
  
  # add
  ok = system("git add *")
  
  # commit
  date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg = sprintf("git commit -a -m 'auto update %s'", date)
  ok = system(msg)
  
  # now push
  ok = system("git push origin main")
  setwd(orig)
}

if (!interactive()){
  r = main()
  if (r <= 0) r = graphics()
  if (r <= 0) r = git()
  quit(save = "no", status = r)
}
