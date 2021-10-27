plot_bike_prediction_cannondale <-
    function(data, interactive = TRUE) {

        g <- data %>%
            mutate(category_2 = fct_reorder(category_2, price)) %>%
            mutate(label_text = str_glue("Unit Price: {scales::dollar(price, accuracy = 1)}
                                 Model: {model}
                                 Bike Type: {category_1}
                                 Bike Family: {category_2}
                                 Frame Material: {frame_material}")) %>%

            ggplot(aes(category_2, price, color = estimate)) +

            geom_violin() +
            geom_jitter(aes(text = label_text),
                        width = 0.12,
                        height = 0.02,
                        alpha = 0.9,
                        size  = 1.8) +
            facet_wrap(~ frame_material) +

            coord_flip() +

            scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
            #scale_color_tq() +
            scale_color_manual(values = c("#ffffff", "#77bb00")) +
            theme_tq() +
            ggdark::dark_theme_bw(
                # strip.text.x = element_text(margin = margin(5, 5, 5, 5)),
                # strip.background = element_rect(fill = "#000000"),
                # axis.text = element_text(colour = "black"),
                # axis.title = element_text(color = "black"),
                # legend.text = element_text(color = "black")
                ) +
            labs(title = "", x = "", y = "Log Scale")


        if (interactive) {
            return(ggplotly(g, tooltip = "text"))
        } else {
            return(g)
        }

    }
