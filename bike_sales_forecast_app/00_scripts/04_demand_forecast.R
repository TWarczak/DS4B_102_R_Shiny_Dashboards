aggregate_time_series <-
function(data, time_unit = "month") {

  output_tbl <- data %>%

    mutate(date = floor_date(order.date, unit = time_unit)) %>%

    group_by(date) %>%
    summarize(total_sales = sum(extended_price)) %>%
    ungroup() %>%

    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

  return(output_tbl)

}
plot_time_series <-
function(data) {

  g <- data %>%

    ggplot(aes(date, total_sales)) +
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.3,
                alpha = 0.2, size = 0.8, color = "#00b4d8" ) +
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")


  ggplotly(g, tooltip = "text")

}

plot_time_series_dark <-
  function(data) {

    g <- data %>%

      ggplot(aes(date, total_sales)) +
      geom_line(color = "#fbb4ae") +
      geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
      geom_smooth(method = "loess", span = 0.3,
                  alpha = 0.2, size = 0.8, color = "#00b4d8" ) +
      theme_dark_grey() +
      scale_color_todd_dark() +
      expand_limits(y = 0) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(x = "", y = "")


    ggplotly(g, tooltip = "text")

  }

generate_forecast <-
function(data, length_out = 12, seed = NULL) {

  train_tbl <- data %>% tk_augment_timeseries_signature()

  future_data_tbl <- data %>% tk_index() %>%
    tk_make_future_timeseries(length_out       = length_out,
                              inspect_weekdays = TRUE,
                              inspect_months   = TRUE) %>%
    tk_get_timeseries_signature()

  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)

  if (time_scale == "year") {

    model <- linear_reg(mode = "regression") %>%
      set_engine(engine = "lm") %>%
      fit.model_spec(total_sales ~ ., data = train_tbl %>% select(total_sales,
                                                                  index.num))

  } else {

  seed <- seed
  set.seed(seed)
  model <- boost_tree(mode = "regression",
                              mtry = 20,
                              trees = 500,
                              min_n = 3,
                              tree_depth = 8,
                              learn_rate = 0.01,
                              loss_reduction = 0.01) %>%
    set_engine(engine = "xgboost") %>%
    fit.model_spec(total_sales ~ ., data = train_tbl %>%
                     select(-date, -label_text, -diff))

  }



  prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
    bind_cols(future_data_tbl) %>%
    select(.pred, index) %>%
    rename(total_sales = .pred,
           date = index) %>%
    mutate(label_text = str_glue("Date: {date}
                               Revenue: {scales::dollar(total_sales)}")) %>%
    mutate(key = "Prediction")

  output_tbl <- data %>%
    mutate(key = "Actual") %>%
    bind_rows(prediction_tbl)

  return(output_tbl)

}
plot_forecast <-
function(data) {

  # Yearly - LM Smoother
  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)

  # Only 1 Prediction - points
  n_predictions <- data %>%
    filter(key == "Prediction") %>%
    nrow()

  g <- data %>%
    ggplot(aes(date, total_sales, color = key)) +
    geom_line() +
    #geom_point(aes(text = label_text), size = 0.1) +
    #geom_smooth(method = "loess", span = 0.2) +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    expand_limits(y = 0) +
    labs(x = "", y = "")

  # Yearly - LM Smoother
  if (time_scale == "year") {
    g <- g +
      geom_smooth(method = "lm")
  } else {
    g <- g +
      geom_smooth(method = "loess", span = 0.3,
                  alpha = 0.2, size = 0.8, color = "#00b4d8")
  }

  # Only 1 Prediction
  if (n_predictions == 1) {
    g <- g +
      geom_point(aes(text = label_text), size = 1)
  } else {
    g <- g +
      geom_point(aes(text = label_text), size = 0.01)
  }

  ggplotly(g, tooltip = "text")

}

plot_forecast_dark <-
  function(data) {

    # Yearly - LM Smoother
    time_scale <- data %>%
      tk_index() %>%
      tk_get_timeseries_summary() %>%
      pull(scale)

    # Only 1 Prediction - points
    n_predictions <- data %>%
      filter(key == "Prediction") %>%
      nrow()

    g <- data %>%
      ggplot(aes(date, total_sales, color = key)) +
      geom_line() +
      theme_dark_grey() +
      scale_color_todd_dark() +
      scale_y_continuous(labels = scales::dollar_format()) +
      expand_limits(y = 0) +
      labs(x = "", y = "")

    # Yearly - LM Smoother
    if (time_scale == "year") {
      g <- g +
        geom_smooth(method = "lm")
    } else {
      g <- g +
        geom_smooth(method = "loess", span = 0.3,
                    alpha = 0.2, size = 0.8, color = "#00b4d8")
    }

    # Only 1 Prediction
    if (n_predictions == 1) {
      g <- g +
        geom_point(aes(text = label_text), size = 1)
    } else {
      g <- g +
        geom_point(aes(text = label_text), size = 0.01)
    }

    ggplotly(g, tooltip = "text")

  }
