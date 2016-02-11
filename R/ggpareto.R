
ggpareto <- function(data, x, count, percentage = NULL, group = NULL, cum.group = NULL, title = NULL) {
  n <- names(data)
  n <- str_replace(n, paste0("^", x, "$"), "x") %>%
    str_replace(paste0("^", count, "$"), "count")  %>%
    str_replace(paste0("^", group, "$"), "group")

  df <- data %>% set_names(n)

  if (is.null(percentage)) {
    df <- df %>%
      group_by(group) %>%
      mutate(percentage = cumsum(count/sum(count)))
  } else {
    df <- df %>% set_names(
      str_replace(paste0("^", percentage, "$"), "percentage")
    )
  }

  if (!is.null(group) & is.null(cum.group)) {
    cum.group <- df$group[1]
  }

  # Subset data by group, including only values matching cum.group
  if (!is.null(cum.group) & !is.null(group)) {
    d1 <- df[df$group == cum.group,] %>% arrange(desc(count))

    df$x <- factor(df$x, levels = d1$x, ordered = T)
    d1$x <- factor(d1$x, levels = d1$x, ordered = T)
  } else {
    d1 <- df
  }

  if (!is.null(group)) {
    p1 <- ggplot(df, aes(x = x, y = count, fill = group))
  } else {
    p1 <- ggplot(df, aes(x = x, y = count))
  }

  p2 <- ggplot(d1, aes(x = as.numeric(x), y = percentage)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = unique(as.numeric(d1$x)), labels = levels(d1$x)) +
    scale_y_continuous(limits = c(0, 1), name = "Percent of Variation") +
    ggtitle(title) +
    theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(), legend.position = "none")

  p1 <- p1 +
    geom_bar(stat = "identity", position = "dodge") + theme(legend.position = "bottom")

  ## extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))

  # overlap the panel of the 2nd plot on that of the 1st plot
  pp <- c(subset(g1$layout, name=="panel", se=t:r))
  g <- gtable_add_grob(
    g1,
    g2$grobs[[which(g2$layout$name=="panel")]],
    pp$t, pp$l, pp$b, pp$l)

  # steal axis from second plot and modify
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]

  # switch position of ticks and labels
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")

  # modify existing row to be wide enough for axis
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

  # draw it
  grid.draw(g)
}

