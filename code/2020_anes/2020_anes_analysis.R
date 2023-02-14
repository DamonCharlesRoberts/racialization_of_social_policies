# Title: 2020 ANES Analysis

# Notes:
    #* Description: R file to execute 2020 ANES Analyses
    #* Updated: 2023-02-02
        #** by: dcr

# Load functions
box::use(
    #* Pipe operator
    magrittr = magrittr[
        `%>%`
    ],
    #* Helpful functions for dataframe management
    dplyr = dplyr[
        select,
        rename,
        filter
    ],
    #* Helpful functions for table creation
    modelsummary = modelsummary[
        datasummary_skim,
        datasummary_correlation,
        modelsummary
    ],
    #* Helpful function to convert model results to data.frame
    broom = broom[
        tidy
    ],
    #* Helpful functions for combining plots to figure
    patchwork = patchwork[...],
    #* Helpful function to save figures
    ggplot2 = ggplot2[
        ggsave
    ],
)

# Source cleaning script
source("code/2020_anes/2020_anes_cleaning.R")

# Source helper functions script
source("code/helper.R")

# Load data
anes_2020 <- read.csv(
    file = "data/2020-anes-pilot/anes_2020_pilot_cleaned_2023-01-31.csv"
)

# Descriptive statistics
    #* Dependent variables
anes_2020 %>%
        #** Select dependent variables
    select(
        free_college,
        loan_forgiveness,
        universal_healthcare
    ) %>%
        #** Rename variables for nice table output
    rename(
        `Free college` = free_college,
        `Loan forgiveness` = loan_forgiveness,
        `Universal healthcare` = universal_healthcare
    ) %>%
        #** Create a table of descriptive statistics
    datasummary_skim(
        .,
        output = "tables_and_figures/2020_dv.docx",
        title = "Descriptive statistics of policy support",
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Descriptive statistics of dependent variables."
        )
    )
    #* Explanatory variables
anes_2020 %>%
        #** Select explanatory variables
    select(
        racial_identity,
        racial_resentment,
        white
    ) %>%
        #** Rename variables for nice table output
    rename(
        `Racial identity` = racial_identity,
        `Racial resentment` = racial_resentment,
        `White` = white
    ) %>%
        #** Create a table of descriptive statistics
    datasummary_skim(
        .,
        output = "tables_and_figures/2020_explanatory.docx",
        title = "Descriptive statistics of explanatory variables",
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Descriptive statistics of explanatory variables."
        )
    )

    #* Control variables
anes_2020 %>%
        #** Select control variables
    select(
        age,
        female,
        family_income,
        education
    ) %>%
        #** Rename variables for nice table output
    rename(
        `Age` = age,
        `Female` = female,
        `Income` = family_income,
        `Education` = education
    ) %>%
        #** Create a table of descriptive statistics
    datasummary_skim(
        .,
        output = "tables_and_figures/2020_controls.docx",
        title = "Descriptive statistics of control variables",
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Descriptive statistics of control variables."
        )
    )

# Correlations
    #* Create pairwise correlation matrix of variables
anes_2020 %>%
        #** Select variables used in analyses
    select(
        free_college,
        loan_forgiveness,
        universal_healthcare,
        racial_identity,
        racial_resentment,
        white,
        age,
        female,
        family_income,
        education
    ) %>%
        #** Rename variables for nice table output
    rename(
        `Free college` = free_college,
        `Loan forgiveness` = loan_forgiveness,
        `Universal healthcare` = universal_healthcare,
        `Racial identity` = racial_identity,
        `Racial resentment` = racial_resentment,
        `White` = white,
        `Age` = age,
        `Female` = female,
        `Income` = family_income,
        `Education` = education
    ) %>%
        #** Create table of correlation matrix
    datasummary_correlation(
        .,
        output = "tables_and_figures/2020_correlation_matrix.docx",
        title = "Descriptive statistics of policy support",
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Descriptive statistics of dependent variables."
        )
    )

# Fitting models
    #* Create empty model list object to store model results
models <- list()

    #* Race x Racial Identity and Race X Racial Resentment
        #** Among Democrats only
anes_2020_non_white_dems <- anes_2020 %>%
            #*** Filter to only include non-white democrats
    filter(
        white == 0,
        pid_three == 1
    )

anes_2020_white_dems <- anes_2020 %>%
            #*** Filter to only include white democrats
    filter(
        white == 1,
        pid_three == 1
    )
            #*** healthcare ~ non-white democrats
models[["non_white_healthcare"]] <- lm(
    data = anes_2020_non_white_dems,
    weights = anes_2020_non_white_dems$estimated_weight,
    formula = scale(universal_healthcare) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** loans ~ non-white democrats
models[["non_white_loans"]] <- lm(
    data = anes_2020_non_white_dems,
    weights = anes_2020_non_white_dems$estimated_weight,
    formula = scale(loan_forgiveness) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** free tuition ~ non-white democrats
models[["non_white_tuition"]] <- lm(
    data = anes_2020_non_white_dems,
    weights = anes_2020_non_white_dems$estimated_weight,
    formula = scale(free_college) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** healthcare ~ white democrats
models[["white_healthcare"]] <- lm(
    data = anes_2020_white_dems,
    weights = anes_2020_white_dems$estimated_weight,
    formula = scale(universal_healthcare) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** loans ~ white democrats
models[["white_loans"]] <- lm(
    data = anes_2020_white_dems,
    weights = anes_2020_white_dems$estimated_weight,
    formula = scale(loan_forgiveness) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** free tuition ~ white democrats
models[["white_tuition"]] <- lm(
    data = anes_2020_white_dems,
    weights = anes_2020_white_dems$estimated_weight,
    formula = scale(free_college) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
    #* Filter data.frame to only white respondents
anes_2020_white_ind <- anes_2020 %>%
    mutate(
        republican = case_when(
            pid_three == -1 ~ 1,
            pid_three == 0 | pid_three == 1 ~ 0
        ),
        ind = case_when(
            pid_three == 0 ~ 1,
            pid_three == -1 | pid_three == 1 ~ 0),
        democrat = case_when(
            pid_three == 1 ~ 1,
            pid_three == -1 | pid_three == 0 ~ 0
        )
    ) %>%
    filter(
        white == 1,
        ind == 1
    )
anes_2020_white_rep <- anes_2020 %>%
    mutate(
        republican = case_when(
            pid_three == -1 ~ 1,
            pid_three == 0 | pid_three == 1 ~ 0
        ),
        ind = case_when(
            pid_three == 0 ~ 1,
            pid_three == -1 | pid_three == 1 ~ 0),
        democrat = case_when(
            pid_three == 1 ~ 1,
            pid_three == -1 | pid_three == 0 ~ 0
        )
    ) %>%
    filter(
        white == 1,
        republican == 1
    )
anes_2020_white_dem <- anes_2020 %>%
    mutate(
        republican = case_when(
            pid_three == -1 ~ 1,
            pid_three == 0 | pid_three == 1 ~ 0
        ),
        ind = case_when(
            pid_three == 0 ~ 1,
            pid_three == -1 | pid_three == 1 ~ 0),
        democrat = case_when(
            pid_three == 1 ~ 1,
            pid_three == -1 | pid_three == 0 ~ 0
        )
    ) %>%
    filter(
        white == 1,
        democrat == 1
    )
    #* Racial identity X PID
        #** universal healthcare ~ pid x racial_identity
models[["healthcare_ind"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(universal_healthcare) ~ scale(racial_identity) + scale(ind) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
models[["healthcare_rep"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(universal_healthcare) ~ scale(racial_identity) + scale(republican) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
models[["healthcare_dem"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(universal_healthcare) ~ scale(racial_identity) + scale(democrat) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
        #** loans ~ pid x racial_identity
models[["loans_ind"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(loan_forgiveness) ~ scale(racial_identity) + scale(ind) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
models[["loans_rep"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(loan_forgiveness) ~ scale(racial_identity) + scale(republican) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
models[["loans_dem"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(loan_forgiveness) ~ scale(racial_identity) + scale(democrat) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
        #** free tuition ~ pid x racial identity
models[["tuition_ind"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(free_college) ~ scale(racial_identity) + scale(ind) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
models[["tuition_rep"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(free_college) ~ scale(racial_identity) + scale(republican) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
models[["tuition_dem"]] <- lm(
    data = anes_2020_white,
    weights = anes_2020_white$estimated_weight,
    formula = scale(free_college) ~ scale(racial_identity) + scale(democrat) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(racial_resentment) #nolint
)
# Tables
    #* Define labels for coefficients and order they appear in tables
coefficient_labels_one <- c(
    "scale(racial_identity)" = "Racial ID",
    "scale(racial_resentment)" = "Racial resentment",
    "scale(age)" = "Age",
    "scale(education)" = "Education",
    "scale(family_income)" = "Income",
    "scale(female)" = "Female",
    "scale(pid_seven)" = "Party ID (7-item)",
    "(Intercept)" = "Constant"
)

coefficient_labels_two <- c(
    "scale(racial_identity)" = "Racial ID",
    "scale(ind)" = "Independents",
    "scale(republican)" = "Republicans",
    "scale(Democrats)" = "Democrats",
    "scale(age)" = "Age",
    "scale(education)" = "Education",
    "scale(family_income)" = "Income",
    "scale(female)" = "Female",
    "scale(racial_resentment)" = "Racial resentment",
    "(Intercept)" = "Constant"
)
    #* Define labels for goodness-of-fit statistics and order
goodness_of_fit_metrics <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "Adj. R^2", "fmt" = 2),
    list("raw" = "rmse", "clean" = "RMSE", "fmt" = 2)

)
    #* Race x Racial Resentment and Race x Racial identity
list(
        #** give a title to each of the models
    "Non-white sample" = models[["non_white_healthcare"]],
    "White sample" = models[["white_healthcare"]]
) %>%
    #** put the two models in two columns in a table
    modelsummary(
        .,
        statistic = "conf.int",
        output = "tables_and_figures/2020_healthcare_identity_models.docx",
        title = "Effects of racial identity and resentment on attitudes toward healthcare policy proposals", #nolint
        coef_map = coefficient_labels_one,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Point estimates reflect standardized coefficients from a weighted least squares regression.", #nolint
            "95-percent confidence intervals in brackets.",
            "Self-identified Democrats only."
        )

    )

list(
        #** give a title to each of the models
    "Non-white sample" = models[["non_white_loans"]],
    "White sample" = models[["white_loans"]]
) %>%
        #** put the two models in two columns in a table
    modelsummary(
        .,
        statistic = "conf.int",
        output = "tables_and_figures/2020_loans_identity_models.docx",
        title = "Effects of racial identity and resentment on attitudes toward student loan forgiveness proposals", #nolint
        coef_map = coefficient_labels_one,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Point estimates reflect standardized coefficients from a weighted least squares regression.", #nolint
            "95-percent confidence intervals in brackets.",
            "Self-identified Democrats only."
        )
    )

list(
        #** give a title to each of the models
    "Non-white sample" = models[["non_white_tuition"]],
    "White sample" = models[["white_loans"]]
) %>%
        #** put the two models in two columns in a table
    modelsummary(
        .,
        statistic = "conf.int",
        output = "tables_and_figures/2020_tuition_identity_models.docx",
        title = "Effects of racial identity and resentment on attitudes toward free college tuition proposals",#nolint
        coef_map = coefficient_labels_one,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Point estimates reflect standardized coefficients from a weighted least squares regression.",#nolint
            "95-percent confidence intervals in brackets.",
            "Self-identified Democrats only."
        )
    )

list(
        #** give a title to each of the models
    "Independent subsample" = models[["healthcare_ind"]],
    "Republican subsample" = models[["healthcare_rep"]],
    "Democrat subsample" = models[["healthcare_dem"]]
) %>%
        #** put the three models in three columns in a table
    modelsummary(
        .,
        statistic = "conf.int",
        output = "tables_and_figures/2020_healthcare_pid_models.docx",
        title = "Effects of partisan identification and racial identity on attitudes toward universal healthcare",#nolint
        coef_map = coefficient_labels_two,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Point estimates reflect coefficients from a weighted least squares regression.",#nolint
            "95-percent confidence intervals in brackets.",
            "Self-identified White respondents only."
        )

    )

list(
        #** give a title to each of the models
    "Independent subsample" = models[["loans_ind"]],
    "Republican subsample" = models[["loans_rep"]],
    "Democrat subsample" = models[["loans_dem"]]
) %>%
        #** put the three models in three columns in a table
    modelsummary(
        .,
        statistic = "conf.int",
        output = "tables_and_figures/2020_loans_pid_models.docx",
        title = "Effects of partisan identification and racial identity on attitudes toward student loan forgiveness",#nolint
        coef_map = coefficient_labels_two,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Point estimates reflect coefficients from a weighted least squares regression.",#nolint
            "95-percent confidence intervals in brackets.",
            "Self-identified White respondents only."
        )

    )

list(
        #** give a title to each of the models
    "Independent subsample" = models[["tuition_ind"]],
    "Republican subsample" = models[["tuition_rep"]],
    "Democrat subsample" = models[["tuition_dem"]]
) %>%
        #** put the three models in three columns in a table
    modelsummary(
        .,
        statistic = "conf.int",
        output = "tables_and_figures/2020_tuition_pid_models.docx",
        title = "Effects of partisan identification and racial identity on attitudes toward free tuition",#nolint
        coef_map = coefficient_labels_two,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2020 American National Election Pilot Study.",
            "Point estimates reflect coefficients from a weighted least squares regression.",#nolint
            "95-percent confidence intervals in brackets.",
            "Self-identified White respondents only."
        )

    )
# Figures
    #* "Tidy" the models
        #** take the first group of models
models_group_1 <- list(
    "Non-White healthcare" = models[["non_white_healthcare"]],
    "White healthcare" = models[["white_healthcare"]],
    "Non-White loan forgiveness" = models[["non_white_loans"]],
    "White loan forgiveness" = models[["white_loans"]],
    "Non-White tuition" = models[["non_white_tuition"]],
    "White tuition" = models[["white_tuition"]]
)
        #** and clean the first group of models
models_group_1_clean <- lapply(
    models_group_1,
    custom_tidy,
    include = c("scale(racial_identity)", "scale(racial_resentment)"),
    confidence_interval = c(0.84, 0.95)
)
        #** Create a figure for the healthcare models in the first group
healthcare <- graphatize(
    non_white_model = models_group_1_clean[["Non-White healthcare"]],
    white_model = models_group_1_clean[["White healthcare"]],
    old_names = c(
        "scale(racial_identity)",
        "scale(racial_resentment)"
    ),
    new_names = c(
        "Racial ID",
        "Racial resentment"
    ),
    dv_name = "Healthcare"
)
        #** create a figure for the loans models from the first group
loans <- graphatize(
    non_white_model = models_group_1_clean[["Non-White loan forgiveness"]],
    white_model = models_group_1_clean[["White loan forgiveness"]],
    old_names = c(
        "scale(racial_identity)",
        "scale(racial_resentment)"
    ),
    new_names = c(
        "Racial ID",
        "Racial resentment"
    ),
    dv_name = "Loan forgiveness"
)
        #** create a figure for free tuition for first group of models
tuition <- graphatize(
    non_white_model = models_group_1_clean[["Non-White tuition"]],
    white_model = models_group_1_clean[["White tuition"]],
    old_names = c(
        "scale(racial_identity)",
        "scale(racial_resentment)"
    ),
    new_names = c(
        "Racial ID",
        "Racial resentment"
    ),
    dv_name = "Free tuition"
)
        #** combine figures
combined_figure_group_1 <- healthcare / loans / tuition + plot_annotation(
    caption = "Data source: 2020 American National Election Pilot Study.\nDots represent point estimates from weighted least squares regression.\nBars represent 84% and 95% confidence intervals.\na. Universal healthcare, b. Student loan forgiveness, c. Free tuition"
) &
ggplot2::theme(
            text = ggplot2::element_text(size = 19, family = "sans")
)
        #** save the combined plot
ggplot2::ggsave(
    combined_figure_group_1,
    filename = "tables_and_figures/2020_racial_resentment_identity_models.png",
    height = 10,
    width = 10,
    units = "in"
)

    #* Racial identity moderated by pid
healthcare_pid_figure <- predicted_graph(
    model = models[["healthcare_pid"]],
    dataset_name = "2020 American National Election Pilot Study",
    dependent_variable = "universal healthcare",
    confidence_interval = 0.84
)

ggsave(
    healthcare_pid_figure,
    file = "tables_and_figures/2020_healthcare_pid.png"
)

loans_pid_figure <- predicted_graph(
    model = models[["loans_pid"]],
    dataset_name = "2020 American National Election Pilot Study",
    dependent_variable = "student loan forgiveness",
    confidence_interval = 0.84
)

ggsave(
    loans_pid_figure,
    file = "tables_and_figures/2020_loans_pid_figure.png"
)

tuition_pid_figure <- predicted_graph(
    model = models[["tuition_pid"]],
    dataset_name = "2020 American National Election Pilot Study",
    dependent_variable = "free tuition",
    confidence_interval = 0.84
)

ggsave(
    tuition_pid_figure,
    file = "tables_and_figures/2020_tuition_pid_figure.png"
)