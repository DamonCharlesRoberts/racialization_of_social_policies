#### Study 1 - News Article Scraping: Scraping Script ####

#### Notes: Scraping News Articles discussiong social welfare policies popularized during 2020 DNC presidential primary ####

#### Files: ####
    #### In: NA ####
    #### Out: see directory "~Data/news_articles" ####

#### Modules to export ####
#' @export 

##### Setup ####
 box::use(
    here = here[here],
    revest = rvest
 )

#### HealthCare #####
    ### Set General URL ###
hcnyturl <- https://www.nytimes.com/search?dropmab=true&endDate=20200811&query=medicaid%20expansion&sort=best&startDate=20200203&types=article