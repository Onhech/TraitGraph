# TraitGraph R Package

TraitGraph generates personality and preference visuals for group reports (traits, Jungian preferences, voting results, similarity heatmaps/networks).

## Installation

```r
remotes::install_github("Onhech/TraitGraph", upgrade = "always")
library(TraitGraph)
```

## Core Functions

- `TG_trait()`: circular percentile bars with group average, name coloring/ordering, and export helpers.
- `TG_jung()`: dual-ended bar for Jungian pairs (E/I, S/N, T/F, J/P).
- `TG_doughnut_chart()`: voting/outcome doughnut.
- `TG_similarity_heatmap()` and `TG_similarity_network()`: similarity matrix and network.

See `ExamplePlots` for sample outputs.

## Quick Start (Trait Plot)

```r
TG_trait(
  dataset = df,
  column_name = "HEX_H",
  name = "name",
  color = "favourite_color",
  show_title = FALSE,
  group_average_color = "#0F2240",
  output_path = "hex_h_plot.svg",
  save_plot = TRUE,
  show_plot = FALSE
)
```

Key arguments:
- `group_average_color`: sets both slice and label for the group average (defaults to `#0F2240`).
- `output_width` / `output_height`: inches for `ggsave`.
- `margin_x_mod` / `margin_y_mod`, `plot_zoom_mod`, `inner_hole_size_mod`: spacing and hole size tweaks.
- `name_size_mod`: adjust name label size.
- `color_mode = "favorite"` or `"midpoint"` (with `midpoint_colors`, `midpoint_label_color`, and lightening controls).

## Handling Long Names and Ordering

- Labels keep their radial distance; only order changes.
- Default ordering randomizes participants, then places longest names at safer angles (top/bottom) using an angle “risk” heuristic; shortest names trend toward left/right. Group average always stays first.
- Labels darken only when their fill is considered light (`is_color_light()`); otherwise they use the base color (including `group_average_color`).

## Similarity Heatmap

```r
TG_similarity_heatmap(
  dataset = df,
  columns = c("HEX_H", "HEX_E", "HEX_X"),
  name_col = "name",
  show_title = FALSE,
  show_legend = FALSE,
  show_values = TRUE,
  output_path = "similarity_heatmap.svg",
  save_plot = TRUE,
  show_plot = FALSE
)
```

Set `show_upper = TRUE` to include the upper triangle; diagonal stays hidden by default.

## Tips for Exports

- Use a square or slightly wide canvas to minimize side whitespace for circular plots.
- If labels clip in HTML, consider post-processing (e.g., `magick::image_trim()`) after `ggsave`.
- Keep `clip = "off"` and modest positive margins if you customize the theme to let labels extend beyond the panel.

## Contributing

Issues and PRs are welcome. Please include a minimal reproducible example and note your R version/OS.
