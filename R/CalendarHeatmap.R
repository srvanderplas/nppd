library(lubridate)
library(magrittr)
library(dplyr)

#' Function to calculate the number of weeks or partial weeks (so if the year starts on a thursday, the first 3 days are week 1)
#' @param date Date vector
sundays <- function(date){
  floor((lubridate::yday(date) +
           lubridate::wday(ymd(
             paste0(lubridate::year(date), "-01-01"))) - 2) / 7) + 1
}

#' Convenience Null-replacement function from ggplot2
#' @param a option #1
#' @param b option #2, used if a is null
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#' Function to create segments on calendar heatmap tile boundary
#' @param date Date vector
#' @param type side of tile to create segment (l, r, t, b, or m). "m" creates a point centered on the tile (useful for labels)
#' @param label optional vector of labels for "m" options
date.segment <- function(date, type = "", label = NULL){
  # Require type to be one of the specified types
  stopifnot(type %in% c("l", "r", "t", "b", "m"))
  # Require label to have length 1 or the same length as date, if it is defined.
  stopifnot(length(label) == length(date) | length(label) == 1 | is.null(label))
  # Require date to have type Date or POSIXt
  stopifnot(is.Date(date) | is.POSIXt(date))

  if (type == "l") {
    data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) - .5,
               y = lubridate::wday(date) - .5, yend = lubridate::wday(date) + .5)
  } else if (type == "r") {
    data.frame(date = date, x = sundays(date) + .5, xend = sundays(date) + .5,
               y = lubridate::wday(date) - .5, yend = lubridate::wday(date) + .5)
  } else if (type == "t") {
    data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) + .5,
               y = lubridate::wday(date) + .5, yend = lubridate::wday(date) + .5)
  } else if (type == "b") {
    data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) + .5,
               y = lubridate::wday(date) - .5, yend = lubridate::wday(date) - .5)
  } else if (type == "m") {
    data.frame(date = date, x = sundays(date), xend = NA,
               y = lubridate::wday(date), yend = NA,
               label = lubridate::month(date, label = T, abbr = T))
  } else {
    data.frame()
  }
}

StatCalHeatmap <- ggproto(
  "StatCalHeatmap", Stat,
  required_aes = c("date"),
  setup_params = function(data, params) {
    if (!is.null(data$x) || !is.null(data$y) ||
        !is.null(params$x) || !is.null(params$y)) {
      stop("stat_calheatmap() must not be used with x or y aesthetics.", call. = FALSE)
    }
    params
  },
  compute_group = function(data, scales){




    all.dates <- data.frame(
      date = start + days(0:as.numeric(difftime(end, start, units = 'days')))) %>%
      dplyr::mutate(
        wday = lubridate::wday(date),
        week = sundays(date),
        day = lubridate::day(date),
        month = lubridate::month(date),
        year = lubridate::year(date),
        yday = lubridate::yday(date)
      ) %>%
      dplyr::group_by(year) %>%
      dplyr::arrange(yday) %>%
      dplyr::mutate(nday = max(day)) %>%
      dplyr::mutate(
        last.week = (day >= (nday - 6)),
        last.day = (day == nday)
      )

    data <- merge(data, all.dates, by = "date")

  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
GeomCalHeatmap <- ggproto(
  "GeomCalHeatmap", Geom,
  # Format data - ensure data is a date
  setup_data = function(data, params){

    if (!(is.POSIXt(data$date) | is.Date(data$date))) {
      if (is.numeric(data$date)) {
        data$date.old <- data$date
        data$date <-  lubridate::date_decimal(data$date)
        warning("Proceeding by converting the numeric variable `date` to a date")
      } else if (is.character(data$date)) {
        data$date.old <- data$date
        warning("Attempting to parse the date using known formats")
        data$date <-  lubridate::parse_date_time(data$date, c("%d%m%y", "%y%m%d", "%y%m%d %H%M", "%y %m %d", "%y%m%d", "ymd"))
      }
    }

    stopifnot(with(data, (is.Date(date) | is.POSIXt(date))))

    data
  },
  draw_panel = function(
    data, panel_scales, coord,
    text.color = "black", text.size = 3,
    line.color = "black", line.size = 1, line.linetype = 1){


    calendar.data <-
      dplyr::bind_rows(
        # First day of month
        subset(data, day == 1) %>% dplyr::do(date.segment(.$date, "b")),
        # First week of month
        subset(data, day <= 7) %>% dplyr::do(date.segment(.$date, "l")),
        # Sundays
        subset(data, wday == 1) %>% dplyr::do(date.segment(.$date, "b")),
        # Saturdays
        subset(data, wday == 7) %>% dplyr::do(date.segment(.$date, "t")),
        # Last week of year
        subset(data, last.week) %>% dplyr::do(date.segment(.$date, "r")),
        # Last day of year
        subset(data, last.day) %>% dplyr::do(date.segment(.$date, "t")),
        # Month labels
        subset(data, day == 1) %>% dplyr::do(date.segment(.$date, "m"))
      )


    segment.data <- subset(calendar.data, is.na(label))
    segment.data$colour = line.color %||% data$colour[1]
    segment.data$size = line.size %||% data$size[1]
    segment.data$linetype = line.linetype %||% data$linetype[1]

    label.data <- subset(calendar.data, !is.na(label))
    label.data$colour <- text.color %||% data$colour[1]
    label.data$size <- text.size %||% data$size[1]

    tile.data <- subset(data, !is.na(x))

    ggname(
      prefix = "geom_calheatmap",
      grob = grid::grobTree(
        ggplot2::GeomTile$draw_panel(tile.data, panel_scales, coord),
        ggplot2::GeomSegment$draw_panel(segment.data, panel_scales, coord),
        ggplot2::GeomText$draw_panel(label.data, panel_scales, coord)
      )
    )
  },
  draw_key = draw_key_rect,
  required_aes = c("date"),
  default_aes =
    aes(colour = NA, fill = "grey50", size = 0.1, alpha = NA, linetype = 1,
        line.color = "black", line.size = 1, line.linetype = "solid",
        label.color = "black", label.size = 3)
)

#' Calendar-style heatmap
#'
#' This plot shows weekdays on the y-axis (Sunday - Saturday) and weeks on the x-axis. As with standard heatmaps, the variable of interest is encoded in the fill aesthetic.
#' This particular geom combination provides (in addition to the standard tile heatmap) boundaries and labels for months within the specified x/y framework.
#' @seealso \url{https://stackoverflow.com/questions/27000131/calendar-heat-map-tetris-chart#}
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "calheatmap")}
#'
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_calheatmap} and \code{stat_identity}.
#' @param line.color Override aesthetics used for line segments. Defaults from \code{geom_segment()}.
#' @param line.size  Override aesthetics used for line segments. Defaults from \code{geom_segment()}.
#' @param line.linetype Override aesthetics used for line segments. Defaults from \code{geom_segment()}.
#' @param label.color Override aesthetics used for labels. Defaults from \code{geom_text()}.
#' @param label.size Override aesthetics used for labels.
#' @export
#' @examples
#' ## Example - Microsoft stock prices
#' stock <- "MSFT"
#' quote <- sprintf("http://ichart.finance.yahoo.com/table.csv?s=%s&g=d&ignore=.csv", #' stock)
#'
#' stock.data <- read.csv(quote, as.is = TRUE) %>% dplyr::tbl_df %>%
#'   dplyr::mutate(date = ymd(Date)) %>%
#'  # dplyr::filter(date >= ymd('2006-02-13') & date <= ymd('2009-10-30')) %>%
#'   dplyr::mutate(year = year(Date))
#'
#' ggplot(data = stock.data) +
#'   geom_calendar(aes(date = date, fill = Adj.Close))
geom_calheatmap <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  line.color = "black", line.size = 1, line.linetype = "solid",
  label.color = "black", label.size = 3, show.legend = NA, inherit.aes = TRUE,
  ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCalHeatmap,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

# stock <- "MSFT"
# quote <- sprintf("http://ichart.finance.yahoo.com/table.csv?s=%s&g=d&ignore=.csv", stock)
#
# stock.data <- read.csv(quote, as.is = TRUE) %>% dplyr::tbl_df %>%
#   dplyr::mutate(date = ymd(Date)) %>%
#   dplyr::filter(date >= ymd('2012-01-15')) %>%
#   dplyr::mutate(year = year(Date))

ggplot(data = stock.data) +
  geom_calheatmap(aes(date = date, fill = Adj.Close))

