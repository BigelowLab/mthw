#' usage: ts_forecast.R [--] [--help] [--date DATE] [--region REGION] [--depth DEPTH]
#' 
#' Build a MTHW forecast graphic
#' 
#' flags:
#'   -h, --help    show this help message and exit
#' 
#' optional arguments:
#'   -d, --date    the date of the forecast [default: 2025-10-21]
#'   -r, --region  the region code ala 'chfc' [default: chfc]
#'   --depth       the depth to use such as 'sur', 'bot', ... [default: sur]


suppressPackageStartupMessages({
  library(andreas)
  library(mthw)
  library(dplyr)
  library(ggplot2)
  library(argparser)
})


Args = argparser::arg_parser("Build a MTHW forecast graphic",
                             name = "ts_forecast.R", 
                             hide.opts = TRUE) |>
  add_argument("--date",
               help = "the date of the forecast",
               default = format(Sys.Date() - 1, "%Y-%m-%d"),
               type = "character") |>
  add_argument("--region",
               help = "the region code ala 'chfc'",
               default = "chfc",
               type = "character") |>
  add_argument("--depth",
               help = "the depth to use such as 'sur', 'bot', ...",
               default = 'sur',
               type = "character") |>
  parse_args()

Args$date = as.Date(Args$date)

dates =  c(Args$date - 5, Args$date + 10)
andpath = andreas::copernicus_path(Args$region)
mwpath = andreas::copernicus_path("mthw")

mwdb = mthw::read_database(file.path(mwpath, Args$region)) |>
  dplyr::filter(depth == Args$depth, name %in% c("temp", "sal"))
cDB = andreas::read_database(andpath, multi = TRUE) |>
  dplyr::filter(depth == Args$depth, name %in% c("temp", "sal"), period == "day")


temp = mthw::generate_wave(
  db = mwdb |> dplyr::filter(name == "temp"),
  dates = dates,
  region = Args$region,
  cDB = cDB)

sal = mthw::generate_wave(
  db = mwdb |> dplyr::filter(name == "sal"),
  dates = dates, 
  region = Args$region,
  cDB = cDB)

tempd = mthw::encode_wave(temp) |>
  dplyr::filter(time >= Sys.Date())

sald = mthw::encode_wave(sal) |>
  dplyr::filter(time >= Sys.Date())

tempp = plot_mwd(tempd, nrow = 1, show.legend = FALSE)
salp = plot_mwd(sald, nrow = 1, show.legend = FALSE)

