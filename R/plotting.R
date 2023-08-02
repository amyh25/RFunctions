#' remove_x_spine
#'
#' @return a ggplot theme to remove the x spine
#' @export

remove_x_spine <- function(...) {
  remove.x.spine.theme <- theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    validate = TRUE,
    ...
  )
  return(remove.x.spine.theme)
}

#' remove_spines
#'
#' @return a ggplot theme to remove the spines
#' @export

remove_spines <- function(...) {
  remove.spine.theme <- theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    validate = TRUE,
    ...
  )
  return(remove.spine.theme)
}

#' plot_umap
#'
#' Plots a 2D UMAP with the aesthetics that I like
#'
#' @param .tbl a data table with umap cols
#' @param x UMAP 1
#' @param y UMAP 2
#' @param color the color to show
#' @param pixels the pixels input to `geom_scattermore`
#' @param pointsize the pointsize input to `geom_scattermore`
#' @param fix_coords whether or not to use `coord_fixed`, TRUE by default
#' @param label bool specifying whether to label the color category
#' @param textsize int specifying textsize; only applies if label=TRUE
#' @param label_groups character vector specifying additional groups to label
#' @param repel_min_segment_length min segment length to pass to `geom_text_repel`
#'
#' @return ggplot2 object
#' @export

plot_umap <- function(.tbl, x = UMAP_1, y = UMAP_2,
                      color_str = "value",
                      pixels = c(512, 512),
                      pointsize = 0,
                      fix_coords = TRUE,
                      label = FALSE,
                      textsize = 3,
                      label_groups = NULL,
                      repel_min_segment_length = 0.1) {
  x <- enquo(x)
  y <- enquo(y)
  p <- ggplot(.tbl) +
    aes(!!x,!!y) +
    geom_scattermore(aes(color = get(color_str)),
                     pointsize = pointsize,
                     pixels = pixels) +
    labs(color = color_str, x = "UMAP dim. 1", y = "UMAP dim. 2") +
    scale_x_continuous(expand = expansion(mult = 0.05)) +
    scale_y_continuous(expand = expansion(mult = 0.05)) +
    theme(
      axis.line = element_line(size = 0.2),
      axis.title.x = element_text(hjust = 0.5, vjust = 1),
      axis.title.y = element_text(hjust = 0.5, vjust = 0),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.key.size = unit(3, "mm")
    )
  if (fix_coords) p <- p + coord_fixed()
  if (is.numeric(.tbl[[color_str]]))
    p <- p + scale_color_viridis()
  if (label) {
    if (!(is.factor(.tbl[[color_str]]) | is.character(.tbl[[color_str]])))
      stop("Can only label if color_str is a column of factors or characters")
    if (is.null(label_groups)) {
      label_tbl <- .tbl %>%
        group_by(!!sym(color_str)) %>%
        summarise(mean_x = mean(!!x), mean_y = mean(!!y))
    } else {
      if (!is.character(label_groups))
        stop("label_groups must be a character vector")
      label_tbl <- .tbl %>%
        group_by(!!!syms(c(color_str, label_groups))) %>%
        summarise(mean_x = mean(!!x), mean_y = mean(!!y))
    }
    p <- p +
      geom_point(aes(mean_x, mean_y),
                 data = label_tbl,
                 size = 1) +
      geom_text_repel(aes(mean_x, mean_y, label = get(color_str)),
                      data = label_tbl,
                      min.segment.length = repel_min_segment_length,
                      force = 5,
                      size = textsize)
  }
  return(p)
}

#' plot_proportion
#' @param .tbl a tibble
#' @param x symbol designating column of tibble with group information
#' @param y symbol designating column of tibble with proportion values
#' @param fill symbol designating fill color
#' 
#' @return ggplot object
#' @export

plot_proportion <- function(.tbl, x = group, y = prop, fill = clusters) {
  x <- enquo(x)
  y <- enquo(y)
  fill <- enquo(fill)
  ggplot(.tbl) +
    aes(!!x, !!y, fill = !!fill) +
    geom_col() +
    remove_x_spine() +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none")
}


#' make_galaxy_plot
#'
#' Pretty sure Kevin Bi deserve the credit for this code but I've messed
#' around with it some too
#'
#' @param .tbl a data frame or tibble with x and y coordinates
#' @param sample_n the number of points to display on top of the density map.
#' Set to 0 to remove all points
#' @param x string designating variable to plot on x-axis
#' @param y string designating variable to plot on y-axis
#'
#' @return ggplot object
#' @export

make_galaxy_plot <- function(.tbl,
                             sample_n = 800,
                             x = "UMAP_1",
                             y = "UMAP_2",
                             pointsize = 0,
                             pixels = c(512, 512)) {
  if (sample_n != 0)
    data_sampled <- slice_sample(.tbl, n = sample_n)
  limits <-
    c(min(.tbl[[x]], .tbl[[y]]), max(.tbl[[x]], .tbl[[y]]))
  retplot <- ggplot(.tbl) +
    aes_string(x, y) +
    stat_density_2d(aes(fill = ..density..),
                    geom = 'raster',
                    contour = FALSE) +
    scale_fill_viridis(option = "magma") +
    coord_fixed(expand = FALSE,
                xlim = limits,
                ylim = limits) +
    theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(hjust = 0, vjust = 1),
      axis.title.y = element_text(hjust = 0, vjust = 0),
      panel.background = element_rect(fill = "black")
    )
  if (x == "UMAP_1" & y == "UMAP_2") {
    retplot <- retplot +
      labs(x = "UMAP dim. 1", y = "UMAP dim. 2")
  }
  if (sample_n != 0) {
    retplot <- retplot +
      scattermore::geom_scattermore(col = 'white',
                                    data = data_sampled,
                                    pointsize = pointsize,
                                    pixels = c(512, 512))
  }
  return(retplot)
}

#' plot_violin
#'
#' @param .tbl a data frame or tibble with x and y coordinates
#' @param var variable to plot as symbol
#' @param group group to plot as symbol
#'
#' @return ggplot object
#' @export

plot_violin <- function(.tbl, group, var, pt.size = 0, pt.stroke = 1) {
  var <- enquo(var)
  group <- enquo(group)
  p <- .tbl %>%
    ggplot() +
    aes(!!group, !!var, fill = !!group) +
    geom_violin(scale = "width") +
    scale_y_continuous(expand = c(0, 0)) +
    remove_x_spine() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  if (pt.size != 0) {
    p <- p +
      geom_jitter(width = 0.25, size = pt.size, stroke = pt.stroke)
  }
  return(p)
}

#' plot_two_groups
#'
#' @param .tbl a data frame or tibble
#' @param var variable to plot as symbol
#' @param group group to plot as symbol
#'
#' Use with fig.asp = 2, fig.width = 2.
#'
#' @return ggplot object
#' @export

plot_two_groups <- function(.tbl,
                            group,
                            var = prop * 100,
                            stat.method = "wilcox.test",
                            color_palette = c("black", "lightblue")) {
  group <- enquo(group)
  var <- enquo(var)
  
  mean_df <- .tbl %>%
    group_by(!!group) %>%
    summarise(mean = mean(!!var),
              se = sd(!!var) / sqrt(n()))
  groups_vec <- unique(as.character(.tbl[[rlang::as_name(group)]]))
  
  .tbl %>%
    ggplot() +
    aes(!!group, y = !!var) +
    geom_errorbar(
      aes(x = !!group,
          y = mean,
          ymin = ..y..,
          ymax = ..y..),
      data = mean_df,
      inherit.aes = FALSE,
      width = 0.5,
      size = 1
    ) +
    geom_errorbar(
      aes(x = !!group,
          ymin = mean - se,
          ymax = mean + se),
      data = mean_df,
      inherit.aes = FALSE,
      width = 0.25,
      size = 0.5
    ) +
    geom_jitter(
      aes(shape = !!group, fill = !!group),
      color = "black",
      size = 3,
      width = 0.3
    ) +
    ggpubr::stat_compare_means(comparison = list(groups_vec),
                               label = "p.signif",
                               method = stat.method,
                               tip.length = 0) +
    scale_y_continuous(expand = c(0, 0), limits = ~ c(0, max(..1) * 1.1)) +
    scale_shape_manual(values = c(21, 22)) +
    scale_fill_manual(values = color_palette) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())
}

#' plot_volcano
#' Generates a pretty volcano plot, by default takes in dataframes
#' formatted like the output of Seurat::FindMarkers
#' Pretty by my standards
#' @param volcano_df data frame with volcano data
#' @param var_fc fold change variable
#' @param var_p pvalue variable
#' @param thresh pval-threshold of where to display gene names up to. Can be
#' a single number, which will set a single cutoff, or a numerical vector of
#' length 2, which will set two cutoffs on the left and the right side of the
#' volcano
#' @return a ggplot2 object displaying a pretty volcano plot
#' @export

plot_volcano <- function(volcano_df,
                         var_fc = avg_log2FC,
                         var_pval = p_val_adj,
                         var_gene = gene,
                         thresh = Inf) {
  var_fc <- enquo(var_fc)
  var_pval <- enquo(var_pval)
  var_gene <- enquo(var_gene)
  
  
  p <- volcano_df %>%
    ggplot() +
    aes(!!var_fc,-log10(!!var_pval)) +
    geom_scattermore(pointsize = 2.1, pixels = c(1024, 1024)) +
    geom_vline(xintercept = c(-0.25, 0.25), linetype = "dashed")
  
  # label points based on threshold
  if (!is.numeric(thresh))
    stop("Error: thresh must be numeric")
  if (length(thresh) == 1) {
    if (!(thresh == Inf)) {
      p <- p +
        geom_text_repel(
          aes(label = !!var_gene),
          data = ~ ..1 %>% filter(p_val_adj < thresh),
          max.overlaps = Inf,
          size = 3
        ) +
        annotate(
          "segment",
          x = -Inf,
          xend = Inf,
          y = -log10(thresh),
          yend = -log10(thresh),
          linetype = "dashed"
        )
    }
  } else {
    if (length(thresh) != 2)
      stop("Error: thresh must be have either length 1 or 2")
    p <- p +
      geom_text_repel(
        aes(label = !!var_gene),
        data = ~ ..1 %>%
          filter((p_val_adj < thresh[2] & avg_log2FC > 0) |
                   (p_val_adj < thresh[1] & avg_log2FC < 0)
          ),
        max.overlaps = Inf,
        size = 3
      ) +
      annotate(
        "segment",
        x = 0,
        xend = Inf,
        y = -log10(thresh[2]),
        yend = -log10(thresh[2]),
        linetype = "dashed"
      )  +
      annotate(
        "segment",
        x = -Inf,
        xend = 0,
        y = -log10(thresh[1]),
        yend = -log10(thresh[1]),
        linetype = "dashed"
      )
  }
  
  p <- p +
    theme(aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(caption = "genes above dashed horizontal lines are labelled")
  
  return(p)
}

