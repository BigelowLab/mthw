#' Plot a paired wave event or duration for a single day
#' 
#' @export
#' @param temp stars object with "mwd" - perhaps temp
#' @param sal stars object with "mwd" - say for sal
plot_mwd_paired = function(temp, sal, 
                           title = "Marine Thermohaline Waves"){
  if (has_time(temp)){
    temp = slice_date(temp, 1)
  }
  if (has_time(sal)){
    sal = slice_date(sal, 1)
  }
  px = plot(temp, show.legend = FALSE, title = "Temperature")
  py = plot(sal, title = "Salinty")
  patchwork::wrap_plots(px, py, ncol = 2) + 
    patchwork::plot_annotation(title = title,
                               caption = "Data sourced from Copernicus")
}


#' Plot a paired wave event or duration for a single day
#' 
#' @export
#' @param x named list of stars stars objects with "mwd/stars" inheritance
#' @param title str, the title for the plot
plot_mwd_list = function(x, 
                         title = "Marine Thermohaline Waves"){
  
  klass = sapply(x, inherits, "stars")
  nms = names(x)[klass]
  n = length(nms)
  last_name = nms[n]
  
  pp = lapply(nms,
              function(nm) {
                tmp = slice_date(x[[nm]],1)
                gg = plot(tmp,
                          show.legend = (nm == last_name),
                          title = toupper(nm))
                return(gg)
              } ) |>
    rlang::set_names(nms)
  
 
  patchwork::wrap_plots(pp, ncol = n) + 
    patchwork::plot_annotation(title = title,
                               caption = "Data sourced from Copernicus")
}


#' Generate breaks for the display of encoded wave data
#' @export
#' @param x stars object, encoded wave event durations
#' @param base number, round the breaks to the nearest `base`
#' @return number, sequence of break points
mwd_breaks = function(x = encode_wave(),
                      base = 5){
  r = range(x[1][[1]], na.rm = TRUE) |>
    mround(base)
  seq(from = r[1], to = r[2], by = 5)
}


#' Plot a set of 'mwd' rasters
#' @export
#' @param x stars 'mwd' raster ala `encode_wave`
#' @param y ignored
#' @param show.legend logical, shown if TRUE
#' @param ... arguments for facet_wrap
#' @return ggplot2 object 
plot.mwd = function(x = encode_wave(), y = NULL, 
                    show.legend = TRUE, 
                    title = "", 
                    ...){
  b = sf::st_bbox(x) |>
    sf::st_as_sfc(crs = sf::st_crs(x)) |>
    sf::st_as_sf()
  gg = ggplot2::ggplot() + 
    #ggplot2::coord_sf() + 
    stars::geom_stars(data = x['mwd'] |>
                        rlang::set_names("days"),
                      show.legend = show.legend, 
                      mapping = ggplot2::aes(fill = days)) + 
    ggplot2::geom_sf(data = sf::st_geometry(b), 
                     color = "grey50", 
                     linewidth = 0.5,
                     fill = NA) +
    colors_mwd()  + 
    ggplot2::scale_x_continuous(expand=c(0,0)) +
    ggplot2::scale_y_continuous(expand=c(0,0)) +
    ggplot2::theme_void()
  
  if ("time" %in% names(stars::st_dimensions(x))){
    gg = gg + 
      ggplot2::facet_wrap(~time, ...) 
  } else {
    gg = gg + 
      ggplot2::labs(x = "", y = "", title = title)
  }
  gg
}

#' Retrieve a standard mwd (duration) color scheme
#' 
#' @param colors chr, color palette
#' @param labels chr, labels (length must be one more than colors)
#' @return ggplot2::scale_fill_binned object
colors_mwd = function(colors = rev(c('#b2182b',
                                     '#ef8a62',
                                     '#ffffff',
                                     '#ffffff',
                                     '#67a9cf',
                                     '#2166ac')),
                      breaks = NULL, #c(-15, -10, -5,  5, 10, 15),
                      labels = NULL){ # c("15", "10", "5", "5", "10", "15")){
  ggplot2::scale_fill_binned(
    palette = colors)
}


#' Plot a set of 'mwe' rasters
#' @export
#' @param x stars 'mwe' raster ala `generate_wave`
#' @param y ignored
#' @param show.legend logical, shown if TRUE
#' @param ncol NULL or numeric number of columns
#' @return ggplot2 object 
plot.mwe = function(x = generate_wave(), y = NULL, 
                    show.legend = has_time(x),
                    title = "",
                    ...){
  b = sf::st_bbox(x) |>
    sf::st_as_sfc(crs = sf::st_crs(x)) |>
    sf::st_as_sf()
  gg = ggplot2::ggplot() + 
    #ggplot2::coord_sf() + 
    stars::geom_stars(data = x['mwe'] |>
                        rlang::set_names("event"), 
                      show.legend = show.legend) +
    ggplot2::geom_sf(data = sf::st_geometry(b), 
                     color = "grey50", 
                     linewidth = 0.5,
                     fill = NA) +
    ggplot2::scale_fill_gradient2(high = scales::muted("red"), 
                                  low = scales::muted("blue"),
                                  mid = "white",
                                  breaks = c(-1.5, -1, 0, 1, 1.5),
                                  labels = c("","<q10", "", ">q90", "")) + 
    #colors_mwe() + 
    ggplot2::theme_void() +
    ggplot2::scale_x_continuous(expand=c(0,0)) +
    ggplot2::scale_y_continuous(expand=c(0,0))
  
  
  if ("time" %in% names(stars::st_dimensions(x))){
    gg = gg + 
      ggplot2::facet_wrap(~time, ...) 
  } else {
    gg = gg + 
      ggplot2::labs(x = "", y = "", title = title)
  }
  gg
}

#' Retrieve a standard mwe (event) color scheme
#' 
#' @return ggplot2::scale_fill_stepsn object
colors_mwe = function(breaks = c(-1, -0.9, 0, 0.9, 1),
                      values = scales::rescale(breaks),
                      labels = c("", "< q10", "q10 to q90", "> q90", ""),
                      colors = c("#ffffff",
                                 scales::muted("blue"), 
                                 "#ffffff",
                                 scales::muted("red"),
                                 "#ffffff")){
  ggplot2::scale_fill_stepsn(
    breaks = breaks,
    labels = labels,
    values =  values,
    colours = colors)
}


plot_count = function(x = count_mwe()){
  ggplot2::ggplot() + 
    stars::geom_stars(data = s['mwe']) +
    #scale_fill_gradient2(high = scales::muted("red"), 
    #                     low = scales::muted("blue")) + 
    ggplot2::coord_equal() +
    ggplot2::facet_wrap(~time) +
    ggplot2::theme_void() +
    ggplot2::scale_x_discrete(expand=c(0,0))+
    ggplot2::scale_y_discrete(expand=c(0,0))
}