# Title: 2019 ANES Analysis

# Notes:
    #* Description: R file to execute 2019 ANES Analyses
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
source("code/2019_anes/2019_anes_cleaning.R")

# Source helper functions script
source("code/helper.R")

# Load data
anes_2019 <- read.csv(
    file = "data/2019-anes-pilot/anes_2019_pilot_cleaned_2023-01-30.csv"
)

# Descriptive statistics
    #* Dependent variables
anes_2019 %>%
        #** Select dependent variables
    select(
        free_college,
        loans,
        universal_healthcare
    ) %>%
        #** Rename variables for nice table output
    rename(
        `Free college` = free_college,
        `Loan forgiveness` = loans,
        `Universal healthcare` = universal_healthcare
    ) %>%
        #** Create a table of descriptive statistics
    datasummary_skim(
        .,
        output = "tables_and_figures/2019_dv.docx",
        title = "Descriptive statistics of policy support",
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
            "Descriptive statistics of dependent variables."
        )
    )
    #* Explanatory variables
anes_2019 %>%
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
        output = "tables_and_figures/2019_explanatory.docx",
        title = "Descriptive statistics of explanatory variables",
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
            "Descriptive statistics of explanatory variables."
        )
    )

    #* Control variables
anes_2019 %>%
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
        output = "tables_and_figures/2019_controls.docx",
        title = "Descriptive statistics of control variables",
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
            "Descriptive statistics of control variables."
        )
    )

# Correlations
    #* Create pairwise correlation matrix of variables
anes_2019 %>%
        #** Select variables used in analyses
    select(
        free_college,
        loans,
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
        `Loan forgiveness` = loans,
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
        output = "tables_and_figures/2019_correlation_matrix.docx",
        title = "Descriptive statistics of policy support",
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
            "Descriptive statistics of dependent variables."
        )
    )

# Fitting models
    #* Create empty model list object to store model results
models <- list()

    #* Race x Racial Identity and Race X Racial Resentment
        #** Among Democrats only
anes_2019_non_white_dems <- anes_2019 %>%
            #*** Filter to only include non-white democrats
    filter(
        white == 0,
        pid_three == 1
    )

anes_2019_white_dems <- anes_2019 %>%
            #*** Filter to only include white democrats
    filter(
        white == 1,
        pid_three == 1
    )
            #*** healthcare ~ non-white democrats
models[["non_white_healthcare"]] <- lm(
    data = anes_2019_non_white_dems,
    weights = anes_2019_non_white_dems$weight,
    formula = scale(universal_healthcare) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** loans ~ non-white democrats
models[["non_white_loans"]] <- lm(
    data = anes_2019_non_white_dems,
    weights = anes_2019_non_white_dems$weight,
    formula = scale(loans) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** free tuition ~ non-white democrats
models[["non_white_tuition"]] <- lm(
    data = anes_2019_non_white_dems,
    weights = anes_2019_non_white_dems$weight,
    formula = scale(free_college) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** healthcare ~ white democrats
models[["white_healthcare"]] <- lm(
    data = anes_2019_white_dems,
    weights = anes_2019_white_dems$weight,
    formula = scale(universal_healthcare) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** loans ~ white democrats
models[["white_loans"]] <- lm(
    data = anes_2019_white_dems,
    weights = anes_2019_white_dems$weight,
    formula = scale(loans) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
            #*** free tuition ~ white democrats
models[["white_tuition"]] <- lm(
    data = anes_2019_white_dems,
    weights = anes_2019_white_dems$weight,
    formula = scale(free_college) ~ scale(racial_identity) + scale(racial_resentment) + scale(age) + scale(education) + scale(family_income) + scale(female) + scale(pid_seven) #nolint
)
    #* Filter data.frame to only white respondents
anes_2019_white <- anes_2019 %>%
    filter(
        white == 1
    )
    #* Racial identity X PID
        #** universal healthcare ~ pid x racial_identity
models[["healthcare_pid"]] <- lm(
    data = anes_2019_white,
    weights = anes_2019_white$weight,
    formula = universal_healthcare ~ racial_identity + pid_three + racial_identity * pid_three + age + education + family_income + female + racial_resentment #nolint
)
        #** loans ~ pid x racial_identity
models[["loans_pid"]] <- lm(
    data = anes_2019_white,
    weights = anes_2019_white$weight,
    formula = loans ~ racial_identity + pid_three + racial_identity * pid_three + age + education + family_income + female + racial_resentment #nolint
)
        #** free tuition ~ pid x racial identity
models[["tuition_pid"]] <- lm(
    data = anes_2019_white,
    weights = anes_2019_white$weight,
    formula = free_college ~ racial_identity + pid_three + racial_identity * pid_three + age + education + family_income + female + racial_resentment #nolint
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
    "racial_identity" = "Racial ID",
    "pid_three" = "Party ID (3-item)",
    "racial_identity * pid_three" = "Racial ID x Party ID (3-item)",
    "age" = "Age",
    "education" = "Education",
    "family_income" = "Income",
    "female" = "Female",
    "racial_resentment" = "Racial resentment",
    "(Intercept)" = "Constant"
)
    #* Define labels for goodness-of-fit statistics and order
goodness_of_fit_metrics <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "Adj. R<sup>2</sup>", "fmt" = 2),
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
        output = "tables_and_figures/2019_healthcare_identity_models.docx",
        title = "Effects of racial identity and resentment on attitudes toward healthcare policy proposals", #nolint
        coef_map = coefficient_labels_one,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
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
        output = "tables_and_figures/2019_loans_identity_models.docx",
        title = "Effects of racial identity and resentment on attitudes toward student loan forgiveness proposals", #nolint
        coef_map = coefficient_labels_one,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
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
        output = "tables_and_figures/2019_tuition_identity_models.docx",
        title = "Effects of racial identity and resentment on attitudes toward free college tuition proposals",#nolint
        coef_map = coefficient_labels_one,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
            "Point estimates reflect standardized coefficients from a weighted least squares regression.",#nolint
            "95-percent confidence intervals in brackets.",
            "Self-identified Democrats only."
        )
    )

list(
        #** give a title to each of the models
    "Universal healthcare" = models[["healthcare_pid"]],
    "Student loans" = models[["loans_pid"]],
    "Free tuition" = models[["tuition_pid"]]
) %>%
        #** put the three models in three columns in a table
    modelsummary(
        .,
        statistic = "conf.int",
        output = "tables_and_figures/2019_proposals_and_pid_models.docx",
        title = "Effects of partisan identification and racial identity on attitudes toward welfare policies",#nolint
        coef_map = coefficient_labels_two,
        gof_map = goodness_of_fit_metrics,
        notes = c(
            "Data source: 2019 American National Election Pilot Study.",
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
    confidence_interval = 0.95
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
    )
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
    )
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
    )
)
        #** combine figures
combined_figure_group_1 <- healthcare / loans / tuition + plot_annotation(
    caption = "Data source: 2019 American National Election Pilot Study.\nDots represent point estimates from weighted least squares regression.\nBars represent 95% confidence intervals.\na. Universal healthcare, b. Student loan forgiveness, c. Free tuition",#nolint
    tag_levels = ("a")
) &
ggplot2::theme(
            text = ggplot2::element_text(size = 20, family = "sans")
)
        #** save the combined plot
ggplot2::ggsave(
    combined_figure_group_1,
    filename = "tables_and_figures/2019-racial_resentment_identity_models.pdf")

    #* Racial identity moderated by pid
healthcare_pid_figure <- predicted_graph(
    model = models[["healthcare_pid"]],
    dataset_name = "2019 American National Election Pilot Study",
    dependent_variable = "universal healthcare",
    confidence_interval = 0.84
)

ggsave(
    healthcare_pid_figure,
    file = "tables_and_figures/2019_healthcare_pid.png"
)

loans_pid_figure <- predicted_graph(
    model = models[["loans_pid"]],
    dataset_name = "2019 American National Election Pilot Study",
    dependent_variable = "student loan forgiveness",
    confidence_interval = 0.84
)

ggsave(
    loans_pid_figure,
    file = "tables_and_figures/2019_loans_pid_figure.png"
)

tuition_pid_figure <- predicted_graph(
    model = models[["tuition_pid"]],
    dataset_name = "2019 American National Election Pilot Study",
    dependent_variable = "free tuition",
    confidence_interval = 0.84
)

ggsave(
    tuition_pid_figure,
    file = "tables_and_figures/2019_tuition_pid_figure.png"
)
