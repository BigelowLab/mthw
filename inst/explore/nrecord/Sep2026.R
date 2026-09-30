suppressPackageStartupMessages({
  library(mthw)
  library(dplyr)
  library(stars)
})


temp = generate_wave(
  db = read_database() |> 
    dplyr::filter(name == "temp", depth == "sur"),
  dates = as.Date( c("2026-09-01", "2026-09-30")) )
tempd = mthw::encode_wave(temp)

sal = generate_wave(
  db = read_database() |> 
    dplyr::filter(name == "sal", depth == "sur"),
  dates = as.Date( c("2026-09-01", "2026-09-30")) )
sald = mthw::encode_wave(sal)


dates = stars::st_get_dimension_values(tempd, 3)


gg = lapply(seq_along(dates),
  function (i) {
    cat(i, dates[i], "\n")
    g = plot_mwd_paired(dplyr::slice(tempd, "time", i),
                       dplyr::slice(sald, "time", i),
                       title = sprintf("Marine Thermohaline Waves %s",
                                       format(dates[i], "%Y-%m-%d")))
    ofile = file.path("/mnt/ecocast/corecode/R/mthw/inst/explore/nrecord",
                    sprintf("mthw_event_%s.png", format(dates[i], "%Y-%m-%d")))
    cat("  ", ofile, "\n")
    ggplot2::ggsave(ofile, plot = g, width = 11, height = 8.5)
  g
})
