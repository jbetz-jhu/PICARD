# color_palette <- ggsci::scale_color_jama()
# color_palette <- ggsci::scale_color_nejm()
# color_palette <- ggsci::scale_color_npg()
color_palette <- ggsci::scale_color_uchicago()

## Figure Options ##
axis_text_size <- 16
axis_title_size <- 16
facet_text_size <- 20
legend_text_size <- 15
legend_title_size <- 16
plot_title_size <- 20

fig_w <- 8
fig_h <- 8


my_theme <-
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = 
      ggplot2::element_text(size = axis_text_size),
    axis.text.y = 
      ggplot2::element_text(size = axis_text_size),
    axis.title = ggplot2::element_text(
      size = axis_title_size,
      face = "bold"
    ),
    strip.text =
      ggplot2::element_text(
        size = facet_text_size,
        face = "bold"
      ),
    plot.title = 
      ggplot2::element_text(
        size = plot_title_size,
        face = "bold"
      ),
    legend.text = 
      ggplot2::element_text(size = legend_text_size),
    legend.title = 
      ggplot2::element_text(size = legend_title_size),
    legend.position = "bottom"
  )