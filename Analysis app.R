library(shiny)
library('tidyverse')
library('NLP')
library('dplyr')
library('quanteda')
library('tm')
library('tidytext')
library('DT')

# Define UI ----
ui <- 
 navbarPage("DEE test analysis app V2.0",
            navbarMenu('Unique IDs',
                       titlePanel(strong('Unique IDs')),
  sidebarLayout(
    sidebarPanel(
      radioButtons('dataset', 'select data',
                   c('H_emac' = 'H_emac',
                    'H_stom' = 'H_stom',
                    'K_trauma'='K_trauma',
                    'K_oiled'='K_oiled'))
         ),
    mainPanel(
  
  fluidRow(
    column(6, 
           h3('Word-Count Table'),
           dataTableOutput("freqtable")
           ),
    column(6,
           h3('Selected-Words Table'),
           dataTableOutput("seltable")
           )
    ),
  fluidRow(
    column(6,
           h3('Unique Document ID list'),
           textOutput("uniqIDs")
           ),
    column(6,
           h3('Unique Document Count'),
           textOutput("total_uniq"),
           downloadButton("uniqDocs","Download unique ID list")
           )
        )
      )
    )
            ),
  navbarMenu('Sens/Spec Stats',
             titlePanel(strong('Sens/Spec Stats')),
             sidebarLayout(
               sidebarPanel(fileInput("file","Upload app-user csv")
             
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                h2('Records to check manually'),          
                 dataTableOutput("to_check_table"),
                downloadButton("docs2check","False Positives"),
                downloadButton("docs2checkmenotapp","False negatives"),
                 h4("Number found by me but not by the app-tester"),
                 textOutput('me_not_app'),
                 h4("Number found by app-tester but not by me"),
                 textOutput("app_not_me")
               ),
               column(6,
                      h2('Comparison Table'),         
               dataTableOutput("full_join_table"),
               h4("Total found by me"),
               textOutput("total_mine"),
               h4("Total found by app-user"),
               textOutput("total_app")
               )
             )
               )
             )
)
            )
# Define server logic ---- a
server <- function(input, output) {
  
 #load data (hoiho corpus and korora corpus) ####
  #use radio buttons to choose
  
 hcorpus<-readRDS('hcorpus.rds')
  kcorpus<-readRDS('kcorpus.rds')
  H_emac<-read.csv("C:/Users/ssav2/OneDrive/Desktop/DVMS/Project/DEE tester data/analysis app/data/H_emac.csv",
                       stringsAsFactors=TRUE, header = TRUE)
  H_stom<-read.csv("C:/Users/ssav2/OneDrive/Desktop/DVMS/Project/DEE tester data/analysis app/data/H_stom.csv",
                        stringsAsFactors=TRUE, header = TRUE)
  K_trauma<-read.csv("C:/Users/ssav2/OneDrive/Desktop/DVMS/Project/DEE tester data/analysis app/data/K_trauma.csv",
                   stringsAsFactors=TRUE, header = TRUE)
  K_oiled<-read.csv("C:/Users/ssav2/OneDrive/Desktop/DVMS/Project/DEE tester data/analysis app/data/K_oiled.csv",
                   stringsAsFactors=TRUE, header = TRUE)
  
  full_set<-reactive({
   full_set<- switch(input$dataset,
           H_emac = H_emac,
           H_stom = H_stom,
           K_trauma = K_trauma,
           K_oiled = K_oiled)
    return(full_set)
  })

  #file upload for app-tester data
  
  file_upload<-reactive({ 
    req(input$file)
    file<-read.csv(input$file$datapath, header = TRUE, stringsAsFactors = TRUE)
     })

#App misses (False negatives)
  
 false_negatives<- reactive({
   x<-filter(full_set(),!(full_set()$Accession_Number %in% file_upload()$Accession_Number))
   })
 
 false_positives<-reactive({
   filter(file_upload(),!(file_upload()$Accession_Number %in% full_set()$Accession_Number))
   })
    
 output$docs2checkmenotapp<-downloadHandler( filename = function(){"docs2checkmenotapp.csv"}, 
                                             content = function(fname){
                                               write.csv(false_negatives(), fname)
                                             } )  
 
 output$docs2check<-downloadHandler( filename = function(){"docs2check.csv"}, 
                                             content = function(fname){
                                               write.csv(false_positives(), fname)
                                             } )  
  
  
  
  
  
 
  #stats to log
     
  output$me_not_app<-renderText(nrow(false_negatives()))
  output$app_not_me<-renderText(nrow(false_positives()))
  output$total_app<-renderText(nrow(file_upload()))
  output$total_mine<-renderText(nrow(full_set()))

}

# Run the app ----
shinyApp(ui = ui, server = server)

