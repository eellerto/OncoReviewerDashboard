# Oncology Reviewer Dashboard

library(shiny)
library(tidyverse)
library(DT)
library(reader)
library(stringr)


# PART 1: Import the data into R using the process below.


# Change this to your own working directory. 
# setwd("C:/Users/bbaumann/OneDrive - SAGE Publishing/Desktop/Reporting Projects/R/forjen/oncologydashboard")
setwd("K:/Our Documents/Elaine Ellerton/Reviewer Dashboards/Oncology dashboard/Onco Dashboard") 
if (file.exists("master.csv") & file.exists("articlesreviewed.csv") & file.exists("articlesauthored.csv")) {
  master <- readRDS("master.csv")
  articlesauthored <- readRDS("articlesauthored.csv")
  articlesreviewed <- readRDS("articlesreviewed.csv")
} else {
  
  # Download 3 Custom Reports from each journal.
  # Save all as CSV UTF-8.
  # Keep the csv files in the same folder as the Shiny app file. 
  # Here is an example of the files for one journal:
  # 1. TCTmaster.csv, 
  # 3. TCTarticelsreviewed.csv, 
  # 4. TCTarticlesauthored.csv
  
  # Extract vector of CSV file names.
  # Make sure you don't have extra CSV files in this folder.
  filenames <- list.files(pattern = ".csv")
  
  # Read files into R. 
  files <- lapply(filenames, read_csv)
  
  # Append TLA to Each Data Frame
  for (i in 1:length(files)){
    files[[i]] <- cbind(files[[i]], str_sub(filenames[i], 1, 3))
  }
  
  articlesauthored <- files[str_detect(filenames, "articlesauthored")]
  articlesreviewed <- files[str_detect(filenames, "articlesreviewed")]
  master <- files[str_detect(filenames, "master")]
  
  articlesauthored <- do.call("rbind", articlesauthored)
  articlesreviewed <- do.call("rbind", articlesreviewed)
  master <- do.call("rbind", master)
  
  
  # Part 2: Clean the Data.
  
  
  # Rename columns. 
  colnames(master) <-c("Salutation", 
                       "Name",
                       "ReviewsCompleted",
                       "Degree",
                       "Department",
                       "Institution",
                       "Country",
                       "Email",
                       "Keywords",
                       "ORCID",
                       "R.Score",
                       "TLA")
  
  colnames(articlesreviewed) <- c("ArticleTitles",
                                  "Email",
                                  "Recommendation",
                                  "ORCID",
                                  "ArticleAbstracts", 
                                  "DatesInvited",
                                  "TLA")
  
  colnames(articlesauthored) <- c("Name",
                                  "ArticleTitles",
                                  "Email",
                                  "ORCID",
                                  "ArticleAbstracts",
                                  "TLA")
  
  
  # Remove White Space from Persistent IDs
  master$Email <- trimws(master$Email, which = "both")
  articlesreviewed$Email <- trimws(articlesreviewed$Email, which = "both")
  articlesauthored$Email <- trimws(articlesauthored$Email, which = "both")
  
  # Exclude extra fields
  # To Do: Jen, remove extra fields from STC reports.
  master <- subset(master, select = -Degree)
  articlesauthored <- subset(articlesauthored, select = -Name)
  master <- subset(master, select = -ReviewsCompleted)
  
  # Simplify salutation field. 
  master$Salutation <- str_remove_all(master$Salutation, "[.]")
  master$Salutation <- str_replace_all(master$Salutation, "Miss", "Ms")
  master$Salutation <- str_replace_all(master$Salutation, "None", NA_character_)
  #remove Ms and Mr 
  master <- master[!grepl("Ms", master$Salutation),]
  master <- master[!grepl("Mr", master$Salutation),]
  
  
  # Simplify keywords. 
  master$Keywords <- str_replace_all(master$Keywords, "Opt In", NA_character_)
  master$Keywords <- str_replace_all(master$Keywords, "Opt Out", NA_character_)
  master$Keywords <- trimws(master$Keywords, which = "both")
  
  # Remove NAs from articlesreviewed & articlesauthored. 
  articlesreviewed <- articlesreviewed[!is.na(articlesreviewed$ArticleTitles),]
  articlesauthored <- articlesauthored[!is.na(articlesauthored$ArticleTitles),]
  #articlesreviewed <- articlesreviewed[!is.na(articlesreviewed$Recommendation),]
  articlesreviewed <- subset(articlesreviewed, select = -Recommendation)
  
  # Add Reviews Completed Col to Master
  ReviewsCompleted <- articlesreviewed %>%
    group_by(Email) %>%
    summarize(ReviewsCompleted = n_distinct(ArticleTitles))
  master <- inner_join(master, ReviewsCompleted, by = "Email")
  
  
  # Create 2 functions to merge similar fields across different data from different journals.
  
  # Function 1: For text fields.
  merge.similar.concat.dedup.na.omit <- function(df, id, col){
    # Some extra steps (quotation, extracting df name) are needed as dplyr uses NSE.
    id <- enexpr(id) 
    col <- enexpr(col) 
    df.name <- deparse(substitute(df))
    col.name <- deparse(substitute(col))
    
    # Concatenate & dedup "col".
    new.df.concat <- df %>%
      group_by(!!id) %>%
      mutate(col.concat = paste0(unique(na.omit((!!col))), collapse = " | "))
    
    # Drop old column & name modified column. Had to be separate step because NSE.
    drops <- names(new.df.concat) == col.name
    new.df.concat <- new.df.concat[!drops]
    names(new.df.concat)[names(new.df.concat) == "col.concat"] <- col.name
    
    # Create new df. 
    assign(df.name, new.df.concat, envir = .GlobalEnv)
  }
  
  # Function 2: For numeric fields that are averages, like R-scores.
  merge.similar.avg <- function(df, id, col){
    # Some extra steps (quotation, extracting df name) are needed as dplyr uses NSE.
    id <- enexpr(id) 
    col <- enexpr(col) 
    df.name <- deparse(substitute(df))
    col.name <- deparse(substitute(col))
    
    # Avg
    new.df.avg <- df %>%
      group_by(!!id) %>%
      mutate(col.avg = mean(!!col, na.rm = TRUE)) 
    
    # Drop old column & name modified column. Had to be separate step because NSE.
    drops <- names(new.df.avg) == col.name
    new.df.avg <- new.df.avg[!drops]
    names(new.df.avg)[names(new.df.avg) == "col.avg"] <- col.name
    
    # Create new df. 
    assign(df.name, new.df.avg, envir = .GlobalEnv)
  }
  
  
  # Run these two functions over the relevant columns in each data frame.
  # To Do: Figure out how to vectorize col argument in merge.similar.concat.dedup.na.omit.
  
  master <- merge.similar.avg(master, Email, R.Score)
  
  master <- merge.similar.concat.dedup.na.omit(master, Email, Salutation)
  master <- merge.similar.concat.dedup.na.omit(master, Email, Name)
  master <- merge.similar.concat.dedup.na.omit(master, Email, Department)
  master <- merge.similar.concat.dedup.na.omit(master, Email, Institution)
  master <- merge.similar.concat.dedup.na.omit(master, Email, Country)
  master <- merge.similar.concat.dedup.na.omit(master, Email, Keywords)
  master <- merge.similar.concat.dedup.na.omit(master, Email, ORCID)
  master <- merge.similar.concat.dedup.na.omit(master, Email, TLA)
  
  articlesauthored <- merge.similar.concat.dedup.na.omit(articlesauthored, Email, ArticleTitles)
  articlesauthored <- merge.similar.concat.dedup.na.omit(articlesauthored, Email, ORCID)
  articlesauthored <- merge.similar.concat.dedup.na.omit(articlesauthored, Email, TLA)
  articlesauthored <- merge.similar.concat.dedup.na.omit(articlesauthored, Email, ArticleAbstracts)
  
  
  articlesreviewed <- merge.similar.concat.dedup.na.omit(articlesreviewed, Email, ArticleTitles)
  articlesreviewed <- merge.similar.concat.dedup.na.omit(articlesreviewed, Email, ORCID)
  articlesreviewed <- merge.similar.concat.dedup.na.omit(articlesreviewed, Email, TLA)
  articlesreviewed <- merge.similar.concat.dedup.na.omit(articlesreviewed, Email, ArticleAbstracts)
  articlesreviewed <- merge.similar.concat.dedup.na.omit(articlesreviewed, Email, DatesInvited)
  
  
  
  
  
  # Remove duplicate rows.
  master <- unique(master) 
  articlesauthored <- unique(articlesauthored)
  articlesreviewed <- unique(articlesreviewed)
  
  # Add article titles (reviewed & authored) to master
  master <- inner_join(master, subset(articlesauthored, select = c(ArticleTitles, Email)), by = "Email")
  master <- inner_join(master, subset(articlesreviewed, select = c(ArticleTitles, Email)), by = "Email")
  master <- inner_join(master, subset(articlesauthored, select = c(ArticleAbstracts, Email)), by = "Email")
  master <- inner_join(master, subset(articlesreviewed, select = c(ArticleAbstracts, Email)), by = "Email")
  master <- inner_join(master, subset(articlesreviewed, select = c(DatesInvited, Email)), by = "Email")
  
  # Add reviewer names to articlesreviewed & articlesauthored
  articlesreviewed <- inner_join(articlesreviewed, subset(master, select = c(Name, Email)), by = "Email")
  articlesauthored <- inner_join(articlesauthored, subset(master, select = c(Name, Email)), by = "Email")
  
  # Reorder columns
  reviewed.col.order <- c("Name", "Email", "ArticleTitles", "TLA", "ORCID", "DatesInvited")
  authored.col.order <- c("Name", "Email", "ArticleTitles", "TLA", "ORCID")
  
  master.col.order <- c("Salutation", "Name", "Email", "ReviewsCompleted", "Department", 
                        "Institution", "Country", "Keywords", "ORCID", "R.Score",
                        "TLA",  "ArticleTitles.x", "ArticleTitles.y", "ArticleAbstracts.x", "ArticleAbstracts.y", "DatesInvited")
  
  master <- master[master.col.order]
  articlesauthored <- articlesauthored[authored.col.order]
  articlesreviewed <- articlesreviewed[reviewed.col.order]
  
  
  # Replace NANs in R-Score column with NA's so it's consistent with the rest of the dashboard. 
  master$R.Score <- str_replace_all(master$R.Score, "NaN", NA_character_)
  
  # OPTIONAL: Filter out people who don't appear on all three sheets.
  #articlesreviewedfilter <- as.data.frame(articlesreviewed$Email)
  #articlesauthoredfilter <- as.data.frame(articlesauthored$Email)
  #master <- semi_join(master, articlesreviewedfilter, by = c("Email" = "articlesreviewed$Email"))
  #master <- semi_join(master, articlesauthoredfilter, by = c("Email"= "articlesauthored$Email"))
  
  saveRDS(master, file="master.csv")
  saveRDS(articlesauthored,file="articlesauthored.csv")
  saveRDS(articlesreviewed, file= "articlesreviewed.csv")
  
}
# Part 3: Create the Shiny App


# Create these variables for hiding columns in the app. Minus 1 because shiny counts cols from 0. 
mastercolumns2hide <- c((match("ORCID", names(master)) - 1),
                        (match("ArticleTitles.x", names(master)) - 1),
                        (match("ArticleTitles.y", names(master)) -1),
                        (match("ArticleAbstracts.x", names(master)) -1),
                        (match("ArticleAbstracts.y", names(master))) - 1) 

reviewedcolumns2hide <- c((match("ORCID", names(articlesreviewed)) - 1), 
                          (match("ArticleAbstracts", names(articlesreviewed))) -1)

authoredcolumns2hide <- c((match("ORCID", names(articlesauthored)) - 1),
                          (match("ArticleAbstracts", names(articlesauthored))) -1)



# Create UI for Shiny App
ui <- 
  fluidPage(tags$style(HTML(type='text/css', 
                            ".nav-tabs {font-size: 20px} ",
                            "

   .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
   
 ")),
            titlePanel("Oncology Reviewer Pool"),
            actionButton("do", "Refresh",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            mainPanel(
              p("Use the search box to filter this table based on words or phrases in any of the fields below (keywords, countries, etc.). You can also search for words or phrases that you might expect to be in the titles of articles authored or reviewed by the potential reviewers you have in mind. Click on rows below to filter the subsequent tabs in this dashboard (Articles Reviewed and Articles Authored). Note that the subsequent tabs will be blank until users are selected on the first tab."),
              tabsetPanel(
                tabPanel("Reviewer List",
                         p("Select reviewers in this tab to filter results in the next two tabs."),
                         dataTableOutput("master")),
                tabPanel("Articles Reviewed",
                         dataTableOutput("articlesreviewed")),
                tabPanel("Articles Authored",
                         dataTableOutput("articlesauthored"))
              )
            )
  )



# Create Server For Shiny App
server <- function(input, output, session) {
  
  output$master <- renderDataTable({
    input$do 
    datatable(master,
              extensions = "FixedHeader",
              options = list(pageLength = nrow(master),
                             fixedHeader = TRUE,
                             lengthChange = FALSE,
                             searching = TRUE,
                             paging = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = mastercolumns2hide))
              ),
              filter = "top",
              rownames = FALSE,
              selection = list(mode = "multiple", target = "row")
    )
  })
  
  
  output$articlesreviewed <- renderDataTable({
    selectedrows <- master[input$master_rows_selected,]
    datatable(articlesreviewed[articlesreviewed$Email %in% selectedrows$Email,],
              extensions = c("FixedHeader", "Buttons"),
              options = list(lengthChange = FALSE,
                             fixedHeader = TRUE,
                             searching = FALSE,
                             paging = FALSE,
                             dom = "Bfrtip",
                             buttons = "copy",
                             columnDefs = list(list(visible = FALSE, targets = reviewedcolumns2hide))
              ),
              rownames = FALSE,
              selection = "none"
    )
  })
  
  output$articlesauthored <- renderDataTable({
    selectedrows <- master[input$master_rows_selected,]
    datatable(articlesauthored[articlesauthored$Email %in% selectedrows$Email,],
              extensions = c("FixedHeader", "Buttons"),
              options = list(lengthChange = FALSE,
                             fixedHeader = TRUE,
                             searching = FALSE,
                             dom = "Bfrtip",
                             paging = FALSE,
                             buttons = "copy",
                             columnDefs = list(list(visible = FALSE, targets = authoredcolumns2hide))
              ),
              rownames = FALSE,
              selection = "none"
    )
  })
  
}

shinyApp(ui, server)
