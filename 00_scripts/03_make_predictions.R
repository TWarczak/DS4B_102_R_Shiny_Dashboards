generate_new_bike <-
  function(bike_model,
           category_1,
           category_2,
           frame_material,
           .ml_model) {
    new_bike_tbl <- tibble(
      model          = bike_model,
      category_1     = category_1,
      category_2     = category_2,
      frame_material = frame_material
    ) %>%
      separate_bike_model()

    predict(.ml_model, new_data = new_bike_tbl) %>%
      bind_cols(new_bike_tbl) %>%
      rename(price = .pred)

  }
format_table <-
  function(new_bike_tbl) {
    new_bike_tbl %>%
      mutate(price = scales::dollar(price, accuracy = 1)) %>%
      mutate_if(is.numeric, as.character) %>%
      pivot_longer(
        cols = -model,
        names_to = "New Model Attribute",
        names_transform  = list("New Model Attribute" = as.factor)
      ) %>%
      pivot_wider(names_from = model, values_from = value)

  }
bind_bike_prediction <-
  function(bikes_tbl, new_bike_tbl) {
    bikes_tbl %>%
      separate_bike_description() %>%
      mutate(estimate = "Actual") %>%
      bind_rows(new_bike_tbl %>% mutate(estimate = "Prediction")) %>%
      select(estimate,
             model,
             category_1,
             category_2,
             frame_material,
             price)

  }
plot_bike_prediction <-
  function(data, interactive = TRUE) {
    g <- data %>%
      mutate(category_2 = fct_reorder(category_2, price)) %>%
      mutate(
        label_text = str_glue(
          "Unit Price: {scales::dollar(price, accuracy = 1)}
                               Model: {model}
                               Bike Type: {category_1}
                               Bike Family: {category_2}
                               Frame Material: {frame_material}"
        )
      ) %>%
      ggplot(aes(category_2, price, color = estimate)) +
      #geom_violin() +
      geom_jitter(
        aes(text = label_text),
        size = 1.5,
        width = 0.1,
        alpha = 0.75
      ) +
      facet_wrap( ~ frame_material) +
      coord_flip() +
      scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
      #scale_color_tq() +
      #theme_tq() +
      scale_color_todd_dark() +
      theme_dark_grey() +
      theme(strip.text.x = element_text(margin = margin(5, 5, 5, 5))) +
      labs(title = "", x = "", y = "Log Scale")

    if (interactive) {
      return(ggplotly(g, tooltip = "text"))
    } else {
      return(g)
    }
  }
scale_color_todd_dark <-
  function(...) {
    library(scales)
    discrete_scale("color", "todd", manual_pal(
      values = c(
        "#fbb4ae",
        "#e31a1c",
        "#75e6da",
        "#94c973",
        "#d4f1f4",
        "#d3ae7c",
        "#307ca1",
        "#ffdf00"
        )
    ), ...)
  }
theme_dark_grey <-
  function(base_size = 14,
           base_family = "sans-serif") {
    library(grid)
    library(ggthemes)
    (
      theme_foundation(base_size = base_size, base_family = base_family)
      + theme(
        plot.title = element_text(
          face = "bold",
          colour = '#ffffb3',
          size = rel(1.1),
          hjust = 0.0,
          margin = margin(0, 0, 5, 0)
        ),
        text = element_text(),
        panel.background = element_rect(colour = NA, fill = 'grey20'),
        plot.background = element_rect(colour = NA, fill = '#262626'),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(
          face = "bold",
          size = rel(1),
          colour = 'white'
        ),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(colour = 'grey70'),
        axis.line.x = element_line(colour = "grey70"),
        axis.line.y = element_line(colour = "grey70"),
        axis.ticks = element_line(colour = "grey70"),
        panel.grid.major = element_line(colour = "#262626"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = '#262626'),
        legend.text = element_text(color = 'white'),
        legend.key = element_rect(colour = NA, fill = '#262626'),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vetical",
        legend.key.size = unit(0.5, "cm"),
        #legend.margin = unit(0, "cm"),
        legend.title = element_text(face = "italic", colour = 'white'),
        plot.margin = unit(c(5, 5, 5, 5), "mm"),
        strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
        strip.text = element_text(face = "bold", colour = 'white')
      )
    )
  }
