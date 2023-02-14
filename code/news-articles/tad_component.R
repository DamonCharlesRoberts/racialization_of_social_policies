# Title: Text analysis of Scraped articles ####

# Notes:
    #* Description: examining the racialization of particular policy proposals in the media # nolint
    #* Updated: 2023-02-06
        #** by: dcr

# Setup
    #* Modularly load functions
box::use(
        #** Load the pipe operator
    magrittr = magrittr[
        `%>%`
    ],
        #** For reading xlsx files
    readxl = readxl[
        read_excel
    ],
        #** For useful content analysis functions
    quanteda = quanteda[
        convert
    ],
        #** For useful figure creation functions
    ggplot2 = ggplot2[
        ggplot,
        aes,
        geom_bar,
        labs,
        theme_minimal,
        theme,
        element_text,
        scale_fill_manual,
        scale_x_continuous,
        ggsave
    ],
        #** For helpful data.frame manipulation
    dplyr = dplyr[
        mutate,
        select,
        rename
    ],
)
    #* Load data
hlthcare <- read_excel("articleshealthcare.xlsx")
students <- read_excel("articlesstudents.xlsx")

    #* Define dictionary
race <- "black|african american|urban|urban poor|white|race|ethnicity|ethnic"

# Healthcare analysis matches

healthcare_matches <- as.data.frame(hlthcare)
healthcare_matches$racialization <- grepl(race, healthcare_matches$Article)
table(healthcare_matches$racialization)

# Loans analysis matches
students_matches <- as.data.frame(students)
students_matches$racialization <- grepl(race, students_matches$Article)
table(students_matches$racialization)


# Merge matches to data.frame
    #* Clean healthcare
healthcare_matches_clean <- healthcare_matches %>%
    mutate(topic = "HealthCare") %>%
    select(topic, racialization)

students_matches_clean <- students_matches %>%
    mutate(topic = "Students") %>%
    select(topic, racialization)

full_matches <- rbind(healthcare_matches_clean, students_matches_clean) %>%
    rename(
        Topic = topic
    ) %>%
    mutate(
        count = ifelse(
            racialization == TRUE, 1, 0
        )
    )
hist <- full_matches %>%
    ggplot(
        aes(x = count, fill = Topic)
    ) +
    geom_bar(position = "dodge") +
    labs(
        y = "# of Articles Racial References",
        x = "Topic",
        caption = "Count of articles using racialized words. \n Source: News articles crawled from the GoogleNews API." # nolint
    ) +
    theme_minimal() +
    theme(
        text = element_text(size = 15)
    ) +
    scale_fill_manual(
        values = c(
            "#999999",
            "#E69F00"
        )
    ) +
    scale_x_continuous(
        breaks = c(0, 1),
        labels = c("No", "Yes")
    )

ggsave(
    hist,
    file = "tables_and_figures/content_analysis.png",
    width = 10.1,
    height = 10.1,
    units = "in"
)