#loading necessary packages
library(highcharter)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(data.table)
library(haven)
library(ggplot2)
library(plotly)
library(DT)
#Importing the data
CountyData <- read_sav("EmployeeDataDF.sav")
#attach(CountyData)
#table(CountyData$ComputerLiteracy)
#Rectifying the labels for gender
CountyData <- CountyData %>% 
    mutate(Gender = as.character(Gender)) %>%
    mutate(Gender = ifelse(Gender == "female", "Female", ifelse(Gender == "Female", "Female", "Male"))) %>% 
    mutate(Age = as.character(Age)) %>%
    mutate(Age = ifelse(Age == "36-45", "36-45", ifelse(Age == "36 - 45", "36-45",ifelse(Age == "46-55", "46-55", ifelse(Age == "46 -55", "46-55", ifelse(Age == "above 55", "Above 55", "Below 35"))) )))
#Department data in descending order
DepartmentAppData <- as.data.frame(table(CountyData$Department)) %>% 
    arrange(desc(Freq))
colnames(DepartmentAppData) <- c("Department", "Frequency")
DepartmentAppData[11,] <- NA
DepartmentAppData <- na.omit(DepartmentAppData)
#Computer literacy
ComputerLiteracyData <- as.data.frame(table(CountyData$ComputerLiteracy)) %>% 
    arrange(desc(Freq))
colnames(ComputerLiteracyData) <- c("LiteracyLevel", "Frequency")
ComputerLiteracyData[5,] <- NA
ComputerLiteracyData <- na.omit(ComputerLiteracyData)
options(digits = 3)








server <- function(input, output, dataset)
{
    output$Department <- renderHighchart({
        hchart(DepartmentAppData,hcaes(x=Department,y=Frequency,color="Department"),type="column",name="Count") %>% 
            hc_exporting(enabled = TRUE) %>%
            #hc_title(text="Barplot of Departments in Makueni County",align="center") %>%
            hc_add_theme(hc_theme_elementary()) 
    })
    #Gender of participants
    output$Gender <- renderHighchart({
        dfGender <- CountyData %>% filter(Department == input$Department) %>% 
            group_by(Gender) %>% 
            select(Gender) %>% 
            summarise(Count=n()) %>% 
            arrange(desc(Count))
        hchart(dfGender,hcaes(x=Gender,y=Count,color="Gender"),name="Count",type="column") %>%
            # hc_title(text="Barplot of Gender from Department",align="center") %>%
            hc_exporting(enabled=TRUE) %>%
            hc_add_theme(hc_theme_elementary())
    })
    #Age of participants
    output$Age <- renderHighchart({
        dfAge <- CountyData %>% filter(Department == input$Department) %>% 
            group_by(Age) %>% 
            select(Age) %>% 
            summarise(Count=n()) %>% 
            arrange(desc(Count))
        hchart(dfAge, hcaes(x = Age, y = Count, color = "Age"), name = "Count", type ="column") %>% 
            #hc_title(align="center") %>%
            hc_exporting(enabled=TRUE) %>%
            hc_add_theme(hc_theme_elementary())
    })
    #Computer Literacy of participants
    output$Literacy <- renderHighchart(
        {
            dfLiteracy <- CountyData %>% filter(Department == input$Department) %>% 
                group_by(ComputerLiteracy) %>% 
                select(ComputerLiteracy) %>% 
                summarise(Count = n()) %>% 
                arrange(desc(Count))
            dfLiteracy[5,] <- NA
            hchart(na.omit(dfLiteracy), hcaes(x = ComputerLiteracy, y = Count), name ="Count", color = "blue", type = "column") %>% 
                #hc_title(text = "Barplot of Computer Literacy", align = "center") %>% 
                hc_exporting(enabled = TRUE) %>% 
                hc_add_theme(hc_theme_elementary())
        }
    )
    #output chart for computer literacy per age and gender
    output$computerliteracy <- renderHighchart(
        {
            Literacydf <- CountyData %>% select(Age, Gender,Department, ComputerLiteracy) %>% 
                arrange(desc(ComputerLiteracy)) 
            Literacydf[357:366,] <- NA  
            Literacydfff <- Literacydf %>% 
                filter(Gender == input$gender, Age == input$age, Department == input$department) %>% 
                group_by(ComputerLiteracy) %>% 
                summarise(Count = n()) %>%
                arrange(desc(Count))
            
            hchart(na.omit(Literacydfff), hcaes(x = ComputerLiteracy, y = Count, color = "ComputerLiteracy"), name= "Count",type = "column") %>% 
                # hc_title(text = "Computer Literacy distribution", align = "center") %>% 
                hc_exporting(enabled = TRUE) %>% 
                hc_add_theme(hc_theme_elementary())
        }
    )
    #Output for satisfaction measures
    output$satisfactionmeas <- renderPlotly({
        CombDataMergedd <- read.csv("CombDataMerged.csv", stringsAsFactors = FALSE)[,-1]
        CombDataMerged2 <- CombDataMergedd %>% 
            filter(Gender == input$Gender, Age == input$age1, Satisfaction == input$satisfaction, Department == input$departmentt) 
        ggplot(data = CombDataMerged2, aes(x = Statement, fill = Response)) + geom_bar(position="dodge", stat = "count") + coord_flip() + theme_minimal()
        
    })
    #Table output summary
    output$table1 <- renderDataTable({
        Tabledataa <- read.csv("Tabledata.csv", stringsAsFactors = FALSE)
        #Table2Sats <- na.omit(Tabledataa) %>% 
        Table1Sats <- na.omit(Tabledataa) %>% 
            group_by(Gender) %>% 
            summarise('Opportunities Score' =mean(Opportunities),'Workplace Score' =mean(Workplace),
                      'Feedback Score' =mean(Feedback),
                      'Worklife Score' =mean(Worklife),
                      'Compensation Score' =mean(Compensation)) 
       # TableStats <- SharedData$new(Table1Sats)
        datatable(Table1Sats,
                  #class = "display nowrap compact",
                  #filter = "top",
                  options = list(
                      scrollx = TRUE,
                      stateSave = FALSE
                  )) %>% formatRound(c(2:6),2) %>% 
            formatStyle(columns = c(2:6),'text-align' = 'center') %>% 
            formatStyle('Gender', fontWeight = 'bold')
    }
        
        
    )
    output$table2 <- renderDataTable({
        Tabledataa <- read.csv("Tabledata.csv", stringsAsFactors = FALSE)
        Table2Sats <- na.omit(Tabledataa) %>% 
            group_by(Age) %>% 
            summarise('Opportunities Score' =mean(Opportunities),'Workplace Score' =mean(Workplace),
                      'Feedback Score' =mean(Feedback),
                      'Worklife Score' =mean(Worklife),
                      'Compensation Score' =mean(Compensation))  
        #SharedData$new()
        #Table2Stats <- SharedData$new(Table2Sats)
        datatable(Table2Sats,
                  #class = "display nowrap compact",
                  #filter = "top",
                  options = list(
                      scrollx = TRUE,
                      stateSave = FALSE
                  )) %>% formatRound(c(2:6),2) %>% 
            formatStyle(columns = c(2:6),'text-align' = 'center') %>% 
            formatStyle('Age', fontWeight = 'bold')}
        
        
    )
    
}







