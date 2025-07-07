library(shiny)
library('tidyverse')
library('NLP')
library('dplyr')
library('quanteda')
library('tm')
library('tidytext')
library('DT')

# Define UI ----
ui <- fluidPage(
  titlePanel("Merge app"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      fileInput("master_file","Upload MASTER csv"),
      fileInput("new_file","Upload NEW csv"),
      fileInput("updated_file","Upload UPDATED csv")
        ),
    mainPanel(
      downloadButton("finish_annot","finish annotating"),
      downloadButton("new_master","Updated Master")
    )
    )
)
  # Define server logic ---- a
  server <- function(input, output) {
    
#Upload annotated file - To become MASTER file
    master_file<-reactive({ 
      req(input$master_file)
      file<-read.csv(input$master_file$datapath, header = TRUE, stringsAsFactors = TRUE)
    })
      
    # Upload new file     
    new_file<-reactive({ 
      req(input$new_file)
      file<-read.csv(input$new_file$datapath, header = TRUE, stringsAsFactors = TRUE)
    })    
 
# Create file to finish annotating
  #Join Master with new file only keeping records in the New file (left/right_join)
  
 join_files<- reactive ({
   filteredmaster<- master_file()[c('Accession_Number','code')]
   joined<- left_join(new_file(),filteredmaster)
    })    

    #download this file to finish annotating     
    output$finish_annot<-downloadHandler( filename = function(){"finish_annot.csv"}, 
                                        content = function(fname){
                                          write.csv(join_files(), fname)
                                        } )       
    #update Master file 
    #upload newly annotated file 
  updated_file<-reactive({ 
      req(input$updated_file)
      file<-read.csv(input$updated_file$datapath, header = TRUE, stringsAsFactors = TRUE)
    })  

  #Join with Master file (full_join)
  cols<-c("Accession_Number", "Type", "Status", "Case_Type", "Reviewed", "Has_Attachments",
          "Gross_Images_Stored", "Fixed_Specimens_Stored", "Submitter", "Submitter_Reference", 
          "Organisation", "City", "Date_Sent", "Report_Date", "Animal_Identification", "Animal_Colour", 
          "Animal_Supplementary_Details", "Animal_Number_Submitted", "Animal_Number_Risk", "Animal_Number_Affected",
          "Animal_Number_Dead", "Animal_Location", "Animal_Location_Type", "Animal_Location_Details", 
          "Animal_Breed", "Animal_Species", "Animal_Species_Common_Name", "Animal_Sex_Classification", 
          "Animal_Sex_Details", "Animal_Age_Classification", "Animal_Age_Details", "History_Details", 
          "History_Previous_Accession", "Pathologists", "Findings_Gross", "Findings_Post_Mortem_Date",
          "Histopathology_Results", "Histopathology_Notes", "Microbiology_Results", "Microbiology_Notes",
          "Parasitology_Results", "Parasitology_Notes", "Clinical_Pathology_Results", "Clinical_Pathology_Notes", 
          "Diagnosis", "Diagnosis_Provisional", "Diagnosis_Morphological", "Diagnosis_Comments", "code")
  
  updated_master<- reactive({ 
    new_updated<-updated_file()[cols]
    new_master<-master_file()[cols]
    anti<-anti_join(new_updated, new_master, by='Accession_Number')
    bind<-rbind(new_master,anti)
    })
 
   #download new Master file   
  
  output$new_master<-downloadHandler( filename = function(){"new_master.csv"}, 
                                        content = function(fname){
                                          write.csv(updated_master(), fname)
                                        } )         
    
  }

# Run the app ----
shinyApp(ui = ui, server = server)



# Upload the annotated file (this will be the 'MASTER' file)
# Upload new file 
# Create file to finish annotating
  #Join Master with new file only keeping records in the New file (left/right_join)
  #download this file to finish annotating
#update Master file 
  #upload newly annotated file 
  #Join with Master file (full_join)
  #download new Master file 