#loading necessary packages
library(highcharter)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(data.table)
library(haven)
library(plotly)
library(crosstalk)
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
#Department data in descending order
DepartmentAppData <- as.data.frame(table(CountyData$Department)) %>% 
    arrange(desc(Freq))
colnames(DepartmentAppData) <- c("Department", "Frequency")
DepartmentAppData[11,] <- NA
DepartmentAppData <- na.omit(DepartmentAppData)
#Computer literacy
ComputerLiteracyData <- as.data.frame(table(CountyData$ComputerLiteracy)) %>% 
    arrange(desc(Freq))
colnames(ComputerLiteracyData) <- c("Literacy Level", "Frequency")
ComputerLiteracyData[5,] <- NA
ComputerLiteracyData <- na.omit(ComputerLiteracyData)
GenderAppData <- as.data.frame(table(CountyData$Gender)) %>% 
    arrange(desc(Freq))
colnames(GenderAppData) <- c("Gender", "Frequency")
AgeAppData <- as.data.frame(table(CountyData$Age)) %>% 
    arrange(desc(Freq))
colnames(AgeAppData) <- c("Age", "Frequency")
############################
DataClean <- select(CountyData, -1,-2,-(5:12),-14,-(42:45))
TT <- gather(DataClean, key ="Question", value = "Responses", -Gender,-Age,-Department)
TT <- TT %>% mutate_all(na_if,"")
LabelData <- data.frame(Responses = c(1,2,3,4,5),Label = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
Mergeddata <- merge(TT, LabelData, id = "Responses")[,-1]
MergedData <- rename(Mergeddata, Response = Label)
###
#CombData <- read.csv("Responsess.csv",stringsAsFactors = FALSE)
#Merging the data
#CombDataMerged <- merge(MergedData, (read.csv("Responsess.csv",stringsAsFactors = FALSE)), id = "Question")[,-1]
CombDataMergedd <- read.csv("CombDataMerged.csv", stringsAsFactors = FALSE)[,-1]
#View(CombDataMergedd)
#Table data
options(digits = 3)
Tabledata <- select(CountyData, (3:4),(42:46))
#View(Tabledata)


#
#Tabledataa1 <- read.csv("Tabledata1.csv", stringsAsFactors = FALSE)
#View(Tabledataa1)

#Tabledataa2 <- read.csv("Tabledata2.csv")
#Satifaction groups
SatisfactionData <- as.data.frame(table(CombDataMergedd$Satisfaction)) %>% 
    arrange(desc(Freq))
colnames(SatisfactionData) <- c("Satisfaction", "Frequency")
dashboardPage(
    skin = "green",
    dashboardHeader(title = "Makueni County Employee Satisfaction Survey"),
    #dashboard sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Main Menu", tabName = "tab1", icon = icon("dashboard")),
            menuItem("Department Analysis", tabName = "tab2",icon = icon("bar-chart")),
            menuItem("Computer Literacy", tabName = "tab3", icon = icon("book")),
            menuItem("Satisfaction Scores", tabName = "tab4", icon = icon("align-left")),
            menuItem("Summary Tables", tabName = "tab5", icon = icon("table"))
        )
    ),
    #dashboard body
    dashboardBody(
        #adding custom-css
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            #tab1 main menu
            tabItem(tabName = "tab1",
                    h3("EMPLOYEE SATISFACTION SURVEY REPORT 2018/19:Data Source, Government of Makueni County"),
                    h3("Introduction"),
                    p("Employee satisfaction survey is a  tool of performance  management, which  strengthens 
governance and is also crucial for achieving the objectives of the Kenya Vision  2030 and 
Makueni Vision 2025. These long term blue prints will be implemented through the Third 
Medium Term Plan (MTP 2018-22) and County Integrated Development Plan (CIDP 2018  -22) 
respectively all which are under preparation. "),
                    h3("Literature Review"),
                    p("Employee satisfaction reflects on how much an individual is satisfied with his/her job. It can be 
reviewed from a perspective of affective job satisfaction and cognitive job satisfaction. Affective 
job satisfaction is related to the positive feelings  and emotions attached to a job by an officer 
while cognitive employee satisfaction is related to the particular facets of a job that determines 
how an individual is satisfied with the pay, allowances, working hours, supervision tactics, and 
work environment among other variable of his job. It is of immense significance for the County 
government to have complete awareness of the level of job satisfaction of its employees. It helps 
to improve the standard and efficiency of service delivery. "),
                    h3("Methodology"),
                    p("The sample design facilitated representative estimate of the head quarter and devolved staff in 
various levels. The target population was all staff of Makueni county Government estimated at 
3,000.  The survey instrument was a questionnaire. It  was developed by statistics section of 
Department of Finance and social economic planning and approved by the County Performance 
Management committee. It consists  of 3 parts namely Employees profile, Core thematic sections 
with questions on topical issues  which were considered relevant for the survey and 
challenges/recommendations from the respondents on the core thematic areas."),
                    h3("Findings"),
                    p("The findings of the survey were analyzed around 5 main themes which were used to gauge the 
employee satisfaction levels. They include opportunities for growth; workplace and resources; 
feedback; work/Life Balance; stress and workplace and compensation"),
                    h3("Conclusion"),
                    p("To improve the satisfaction index, we recommend the following based on the findings of the 
study;
1.  The need to develop career growth and development mechanism for the staff.
2.  Provision of  requisite tools and equipment for staff working in sub county and at the 
ward level
3.  The need to encourage continuous staff feedback on their performance to enhance 
efficiency in service delivery.
4.  The need to undertake staff rationalization and harmonization in the establishment."),
                    
                    
                    br()
                    
            ),
            #tab2 analysis per department
            tabItem(tabName = "tab2",
                    h3("Departmental Analysis", align = "center"),
                    #chart for department histogram
                    box(
                        solidHeader = TRUE,
                        title = "Departments",
                        status = "primary",
                        collapsible = TRUE,
                        highchartOutput("Department"),
                        width =12),
                    fluidRow(
                        column(12,
                               box(
                                   selectInput("Department", "Select Department",
                                               choices = DepartmentAppData[,1]),
                                   width =12
                               ),
                               #box for plots
                               box(
                                   solidHeader = TRUE,
                                   title = "Gender Distribution",
                                   status = "primary",
                                   collapsible = TRUE,
                                   highchartOutput("Gender",height = "300px"), 
                                   width = 4
                               ),
                               box(
                                   solidHeader = TRUE,
                                   title = "Age Distribution",
                                   status = "primary",
                                   collapsible = TRUE,
                                   highchartOutput("Age", height = "300px"),
                                   width =4
                               ),
                               box(
                                   solidHeader = TRUE,
                                   title = "Computer Literacy",
                                   status = "primary",
                                   collapsible = TRUE,
                                   highchartOutput("Literacy", height = "300px"),
                                   width = 4
                               )
                               
                               
                        )#end column 1
                    )#end fluid one
                    
                    
            ),#end tab2
            #tab3
            tabItem(tabName = "tab3",
                    h3("Computer Literacy Per gender,department and age", align = "center"),
                    fluidRow(
                        column(12,
                               #Department and computer literacy
                               #h3("Computer Literacy levels distribution"),
                               #br(),
                               box(
                                   selectInput("gender", label = "Select Gender",
                                               choices = GenderAppData[,1]),
                                   width= 4
                               ),
                               #Age select
                               box(
                                   selectInput("age", label = "Select Age",
                                               choices = AgeAppData[,1]),
                                   width = 4
                               ),
                               #Department
                               box(
                                   selectInput("department", label = "Select Department",
                                               choices = DepartmentAppData[,1]
                                   ),
                                   width = 4
                               ),
                               #box for plots
                               box(
                                   solidHeader = TRUE,
                                   title = "Literacy Levels",
                                   status = "primary",
                                   collapsible = TRUE,
                                   highchartOutput("computerliteracy", height= "300px"),
                                   width = 12 
                               )
                               
                        )
                    )
            ),
            #tab 4
            tabItem(
                tabName = "tab4",
                h3("Satisfaction Score", align = "center"),
                br(),
                #select gender
                box(
                    selectInput("Gender",label = "Select Gender",
                                choices = GenderAppData[,1]),
                    width = 3),
                #select age
                box(
                    selectInput("age1", label = "Select Age Group",
                                choices = AgeAppData[,1]),
                    width = 3
                    
                ),
                #select satisfaction
                box(
                    selectInput("satisfaction", label = "Select Satisfaction Parameter",
                                choices = SatisfactionData[,1]),
                    width = 3
                ),
                box(
                    selectInput("departmentt", "Select Department",
                                choices = DepartmentAppData[,1]),
                    width = 3
                )
                ,
                #box for plots
                box(
                    plotlyOutput("satisfactionmeas",height = "380px"),
                    width =12
                )
            ),
            #tab5
            tabItem(
                tabName = "tab5",
                h3("Overall Satisfaction Scores", align = "center"),
                br(),
                #box(
                dataTableOutput("table1"),
                dataTableOutput("table2")
                #)
            )
            
        )
        
        
    )
)







