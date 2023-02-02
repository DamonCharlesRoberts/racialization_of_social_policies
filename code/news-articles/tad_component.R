#### Text analysis of Scraped articles ####

#### Notes: Looking at the racialization of particular policies of interest ####
    ### Files: ###
        ## In: 
#            1. 
#            2. 
#            3. ##

#### Setup ####
box::use(
    quanteda = quanteda[convert],
    readxl = readxl[read_excel],
    ggplot2 = ggplot2[...],
    ggcorrplot = ggcorrplot[...],
    dplyr = dplyr[mutate,select, rename],
    magrittr = magrittr[...]
)
here::here()

#### Read in data ####
hlthcare <- read_excel("articleshealthcare.xlsx")
students <- read_excel("articlesstudents.xlsx")
gnd <- read_excel("articlesgnd.xlsx")

#### Read a sample of the articles ####
nrow(hlthcare)
nrow(students)
nrow(gnd)

set.seed(0906)
sample <- sample(1:59, 10, replace = FALSE)
viewSample1 <- hlthcare$Article[c(12,6,13,34,33,18,40,25,58,37)]
viewSample2 <- students$Article[c(29,45,46,44,25,33,23,36,28,2)]
viewSample3 <- gnd$Article[c(43,47,6,59,9,28,34,51,12)]
print(viewSample1)
print(viewSample2)
print(viewSample3)

#### GND dataset has a bunch about the EU Green New Deal ####
#gnd[-c(1, 3,4,6,7,8,11,14,15,16,19,21,),]

## NOTE: Articles seem to not cross-pollinate with other social policies: Don't need to search for a latent dimension then. Can use the simpler dictionary approach ##

#### Dictionary - Healthcare Reform####
race <- "black|african american|urban|urban poor|white|race|ethnicity|ethnic"
hlthcaredf <- as.data.frame(hlthcare)
hlthcaredf$racialization <- grepl(race, hlthcaredf$Article)
table(hlthcaredf$racialization)

#### Dictionary - Student Loans and debt forgiveness ####
studentsdf <- as.data.frame(students)
studentsdf$racialization <- grepl(race, studentsdf$Article)
table(studentsdf$racialization)

#### Dictionary - Green New Deal ####
#gnddf <- as.data.frame(gnd)
#gnddf$racialization <- grepl(race, gnd$Article)
#table(gnddf$racialization)

#### Commbine the different article types ####
hlthcaredf <- hlthcaredf %>%
    mutate(topic = "HealthCare") %>%
    select(topic, racialization)
studentsdf <- studentsdf %>%
    mutate(topic = "Students") %>%
    select(topic, racialization)
#gnddf <- gnddf %>%
#    mutate(topic = "GreenNewDeal") %>%
#    select(topic, racialization)
fulldf <- rbind(hlthcaredf, studentsdf) %>%
    rename(Topic = topic) %>%
    mutate(count = ifelse(racialization == TRUE, 1, 0))
hist = ggplot(data = fulldf) +
    aes(x = count, fill = Topic) +
    geom_bar(position = "dodge") +
    labs(y = "# of Articles Racial References", x = "Topic", caption = "Count of articles using racialized words. \n Source: News articles crawled from the GoogleNews API.") +
    theme_minimal() +
    theme(text = element_text(size = 15)) +
    scale_fill_manual(values = c("#999999", "#E69F00"))+
    scale_x_continuous(breaks = c(0, 1), 
                        labels = c("No", "Yes"))

    ggsave("Tables and Figures/study-1/hlthcaretextanalysis.png", hist,  width = 10.1, height = 10.1, units = "in")

