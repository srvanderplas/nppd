library(lubridate)
library(magrittr)
library(dplyr)

# Number of weeks or partial weeks (so if the year starts on a thursday, the first 3 days are week 1)
sundays <- function(date){
  floor((yday(date) +
           wday(ymd(
             paste0(year(date), "-01-01"))) - 2) / 7) + 1
}

# Function to create segment on tile boundary
date.segment <- function(date, type = ""){
  stopifnot(type %in% c("l", "r", "t", "b", "m"))
  if (type == "l") {
    data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) - .5,
               y = wday(date) - .5, yend = wday(date) + .5)
  } else if (type == "r") {
    data.frame(date = date, x = sundays(date) + .5, xend = sundays(date) + .5,
               y = wday(date) - .5, yend = wday(date) + .5)
  } else if (type == "t") {
    data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) + .5,
               y = wday(date) + .5, yend = wday(date) + .5)
  } else if (type == "b") {
    data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) + .5,
               y = wday(date) - .5, yend = wday(date) - .5)
  } else if (type == "m") {
    data.frame(date = date, x = sundays(date), xend = NA,
               y = wday(date), yend = NA,
               label = month(date, label = T, abbr = T))
  } else {
    data.frame()
  }
}

GeomCalendar <- ggproto(
  "GeomCalendar", Geom,
  # Format data - ensure data is a date
  setup_data = function(data, params){

    if (!(is.POSIXt(data$date) | is.Date(data$date))) {
      if (is.numeric(data$date)) {
        data$date.old <- data$date
        data$date <-  date_decimal(data$date)
        warning("Proceeding by converting the numeric variable `date` to a date")
      } else if (is.character(data$date)) {
        data$date.old <- data$date
        warning("Attempting to parse the date using known formats")
        data$date <-  parse_date_time(data$date, c("%d%m%y", "%y%m%d", "%y%m%d %H%M", "%y %m %d", "%y%m%d", "ymd"))
      }
    }

    stopifnot(with(data, (is.Date(date) | is.POSIXt(date))))

    data
  },
  draw_group = function(
    data, panel_scales, coord,
    text.color = "black", text.size = 3,
    line.color = "black", line.size = 1, line.linetype = 1){

    date_min <- min(data$date)
    date_max <- max(data$date)

    # Range of years between dates
    start <- floor_date(ymd_hms(ISOdate(year(min(date_min)),1,1)), "day")
    end <- floor_date(ymd_hms(ISOdate(year(max(date_max)), 12, 31)), "day")

    all.dates <- data.frame(
      date = start + days(0:as.numeric(difftime(end, start, units = 'days')))) %>%
      dplyr::mutate(
        wday = wday(date),
        week = sundays(date),
        day = day(date),
        month = month(date),
        year = year(date),
        yday = yday(date)
      ) %>%
      dplyr::group_by(year) %>%
      dplyr::arrange(yday) %>%
      dplyr::mutate(nday = max(day)) %>%
      dplyr::mutate(
        last.week = (day >= (nday - 6)),
        last.day = (day == nday)
      )

    tile.data <- data.frame(
      x = sundays(data$date),
      y = wday(data$date),
      year = year(data$date),
      colour = data$colour,
      size = data$size,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )

    calendar.data <-
      bind_rows(
        # First day of month
        subset(all.dates, day == 1) %>% do(date.segment(.$date, "b")),
        # First week of month
        subset(all.dates, day <= 7) %>% do(date.segment(.$date, "l")),
        # Sundays
        subset(all.dates, wday == 1) %>% do(date.segment(.$date, "b")),
        # Saturdays
        subset(all.dates, wday == 7) %>% do(date.segment(.$date, "t")),
        # Last week of year
        subset(all.dates, last.week) %>% do(date.segment(.$date, "r")),
        # Last day of year
        subset(all.dates, last.day) %>% do(date.segment(.$date, "t")),
        # Month labels
        subset(all.dates, day == 1) %>% do(date.segment(.$date, "m"))
      )


    segment.data <- subset(calendar.data, is.na(label))
    segment.data$colour = line.color %||% data$colour[1]
    segment.data$size = line.size %||% data$size[1]
    segment.data$linetype = line.linetype %||% data$linetype[1]

    label.data <- subset(calendar.data, !is.na(label))
    label.data$colour <- text.color %||% data$colour[1]
    label.data$size <- text.size %||% data$size[1]

    ggname(
      "geom_calendar",
      grobTree(
        GeomTile$draw_panel(tile.data, panel_scales, coord),
        GeomSegment$draw_panel(segment.data, panel_scales, coord),
        GeomText$draw_panel(label.data, panel_scales, coord)
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

geom_calendar <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  line.color = "black", line.size = 1, line.linetype = "solid",
  label.color = "black", label.size = 3, show.legend = NA, inherit.aes = TRUE,
  ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCalendar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      line.color = line.color,
      line.size = line.size,
      line.linetype = line.linetype,
      label.color = label.color,
      label.size = label.size,
      ...
    )
  )
}

## Example
stock <- "MSFT"
quote <- sprintf("http://ichart.finance.yahoo.com/table.csv?s=%s&g=d&ignore=.csv", stock)

stock.data <- read.csv(quote, as.is = TRUE) %>% dplyr::tbl_df %>%
  dplyr::mutate(date = ymd(Date)) %>%
  dplyr::filter(date >= ymd('2006-02-13') & date <= ymd('2009-10-30')) %>%
  dplyr::mutate(year = year(Date))


ggplot(data = stock.data) +
  geom_calendar(aes(date = date, fill = Adj.Close))



###
#
# sundays <- function(date){
#   floor((yday(date) + wday(ymd(paste0(year(date), "-01-01"))) - 2) / 7) + 1
# }
#
# date.segment <- function(date, type = ""){
#   stopifnot(type %in% c("l", "r", "t", "b", "m"))
#   if (type == "l") {
#     data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) - .5,
#                y = wday(date) - .5, yend = wday(date) + .5)
#   } else if (type == "r") {
#     data.frame(date = date, x = sundays(date) + .5, xend = sundays(date) + .5,
#                y = wday(date) - .5, yend = wday(date) + .5)
#   } else if (type == "t") {
#     data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) + .5,
#                y = wday(date) + .5, yend = wday(date) + .5)
#   } else if (type == "b") {
#     data.frame(date = date, x = sundays(date) - .5, xend = sundays(date) + .5,
#                y = wday(date) - .5, yend = wday(date) - .5)
#   } else if (type == "m") {
#     data.frame(date = date, x = sundays(date), xend = NA,
#                y = wday(date), yend = NA,
#                label = month(date, label = T, abbr = T))
#   } else {
#     data.frame()
#   }
# }
#
# calendar_tetris_geoms <- function(date, date_min = NULL, date_max = NULL, text.size = 4) {
#
#   # Dates should be dates
#   if (!is.null(date)) {
#     stopifnot(is.Date(date) | is.POSIXt(date))
#   } else if ((is.null(date_min) | is.null(date_max)) & !is.null(date)) {
#     date_min <- min(c(date, date_min))
#     date_max <- max(c(date, date_max))
#   }
#
#   # date min/max should exist at this point
#   stopifnot(!is.null(date_min) & !is.na(date_min) &
#               !is.null(date_max) & !is.na(date_max))
#
#   tdata <- calendar_tetris_data(date_min, date_max)
#   list(
#     # segments
#     ggplot2::geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
#                           data = subset(tdata, is.na(label))),
#     # axis labels
#     ggplot2::scale_y_continuous("", breaks = 1:7, labels = wday(1:7, label = T)),
#     ggplot2::scale_x_continuous("", breaks = NULL),
#     # month labels
#     ggplot2::geom_text(aes(x = x, y = y, label = label),
#                        hjust = 0.25, data = subset(tdata, !is.na(label)), size = text.size)
#   )
# }
#
#
# ggplot(data = stock.data) +
#   geom_tile(aes(x = sundays(date), y = wday(date), fill = Adj.Close), colour = "white") +
#   calendar_tetris_geoms(min(stock.data$date), max(stock.data$date)) +
#   facet_wrap(~year, ncol = 1)
