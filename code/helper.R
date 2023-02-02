# Title: Helper functions

# Notes:
    #* Description: helper functions for ANES analyses
    #* Updated: 2023-02-02
        #** by: dcr

# Load dependencies
box::use(
    #* for pipe operator
    magrittr = magrittr[`%>%`]
)

# Tidy the models
custom_tidy <- function( # nolint
    model = NULL,
    include = NULL,
    confidence_interval = 0.95
    ) {
        #' graphatize function
        #' tidies the models...
        #' ... calculates their confidence intervals...
        #' ... and removes nuisance estimates
        #' Parameters:
        #' ----
        #' model(lm): a model object to be tidied
        #' include(chr): a variable name or list of variable names to include
        #' confidence_interval(float):
        #'   - a float of what confidence interval you want
        #' Returns:
        #' ----
        #' tidied_model(tibble): a tibble object
            broom::tidy(
                model,
                conf.int = confidence_interval
            ) %>%
                dplyr::filter(
                    term %in% include # nolint
                )
}

# Graph tidied models
graphatize <- function( # nolint
    non_white_model = NULL,
    white_model = NULL,
    old_names = NULL,
    new_names = NULL
) {
    #' graphatize
    #' Make coefficient plots for models
    #'
    #' Parameters:
    #' ----
    #' models(list of data.frames): list of tidied model data.frames
    #' old_names(list): list of variable names
    #' new_names(list): list of new variable names
    #'
    #' Returns:
    #' ----
    #' ggplot obj
    rbind(
        non_white_model %>%
            dplyr::mutate(model = "Non-White"),
        white_model %>%
            dplyr::mutate(model = "White")
    ) %>%
    dplyr::mutate(
        term = dplyr::case_when(
            term == old_names[1] ~ new_names[1], # nolint
            term == old_names[2] ~ new_names[2]
        ),
        term = factor(
            term,
            levels = c(
                new_names[1],
                new_names[2]
            )
        ),
        model = factor(
            model # nolint
        )
    ) %>%
    ggplot2::ggplot(
        data = ., # nolint
        ggplot2::aes(
        #    y = term,
        #    x = estimate,
        #    xmin = conf.low,
        #    xmax = conf.high,
            linetype = factor(model),
            shape = factor(model)
        )
    ) +
        ggplot2::geom_point(
            ggplot2::aes(
                x = estimate, # nolint
                y = term
            ),
            position = ggplot2::position_dodge(width = 0.4),
            size = 3
        ) +
        ggplot2::geom_linerange(
            ggplot2::aes(
                xmin = conf.low, # nolint
                xmax = conf.high, # nolint
                y = term
            ),
            position = ggplot2::position_dodge(width = 0.4),
            size = 1
        ) +
        ggplot2::geom_vline(
            xintercept = 0,
            linetype = 2
        ) +
        ggplot2::scale_y_discrete(
            limits = rev
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            text = ggplot2::element_text(size = 20, family = "sans")
        ) +
        ggplot2::labs(
            x = "Estimate",
            y = "",
            shape = "Models",
            linetype = "Models"
        )
}

# Graphs of predicted probabilities
predicted_graph <- function( # nolint # nolint
    model = NULL,
    dataset_name = NULL,
    dependent_variable = NULL,
    confidence_interval = 0.84
) {
    #' predicted_graph
    #' To create graphs of predicted probabilities
    #'
    #' Parameters
    #' ----
    #' model(lm): an lm model object
    #' dataset_name(chr): a string for what to call the dataset
    #' dependent_variable(chr): a string for what to label the DV
    #' confidence_interval(float): a float of the confidence interval
    #'
    #' Returns
    #' ----
    #' ggplot2 object
    ggeffects::ggpredict(
        model,
        terms = c("racial_identity", "pid_three")
    ) %>%
        dplyr::rename(PID = group) %>% # nolint
        ggplot2::ggplot(
            data = ., # nolint
            ggplot2::aes(x, predicted) # nolint # nolint
        ) +
        ggplot2::geom_line(
            ggplot2::aes(linetype = PID, color = PID), # nolint
            size = 0.9
        ) +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = PID), # nolint # nolint
            alpha = 0.10
        ) +
        ggplot2::theme_minimal(
            base_family = "sans",
            base_size = 12
        ) +
        ggplot2::scale_linetype_manual(
            values = 1:3,
            labels = c("Republican", "Independent", "Democrat")
        ) +
        ggplot2::scale_color_manual(
            values = c("#FF0000", "#7F007F", "#0000FF"),
            labels = c("Republican", "Independent", "Democrat")
        ) +
        ggplot2::scale_fill_manual(
            values = c("#FF0000", "#7F007F", "#0000FF"),
            labels = c("Republican", "Independent", "Democrat")
        ) +
        ggplot2::labs(
            y = paste(
                "Predicted probability of support for ", dependent_variable),
            x = "Racial ID",
            caption = c(
                paste("Data source: ", dataset_name),
                "Predicted values of support",
                paste("Confidence intervals calculated at the ", confidence_interval*100, "% level") # nolint
            )
        )
}