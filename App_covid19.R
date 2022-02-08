#Importation des bibliothèques 
library(shiny)
library(shinyjs)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel('Prédiction du test covid19'),
  sidebarLayout(
    sidebarPanel(
      div(
        fluidRow(
          column(5,align="center",style=list("padding-right: 3px;"),
                 selectInput(inputId = "Sexe", label = " Votre sexe :", choices = unique(data$sexe),selected ='Femme')),
          
          column(5,align="center",style=list("padding-right: 3px;"),          
                 selectInput(inputId = "Age", label = "Votre classe d'age :", choices= unique(data$age), selected='[21 - 39]'))),
        fluidRow(
          
          
          column(4,align="center",style=list("padding-right: 3px;"),   
                 radioButtons(inputId = "maux_de_gorge", label = "Est ce que vous avez des maux de gorge ?", choices = unique(data$maux_de_gorge),selected='Non')),
          
          column(4,align="center",style=list("padding-right: 3px;"),   
                 radioButtons(inputId = "maux_de_tete", label = "Est ce que vous avez des maux de tete ?", choices = unique(data$maux_de_tete), selected='Non')),
          
          column(4,align="center",style=list("padding-right: 3px;"),   
                 radioButtons(inputId = "gout", label = "Est ce que vous avez perdu le gout ?", choices = unique(data$gout), selected='Non'))),
        
        fluidRow(
          column(4,align="center",style=list("padding-right: 3px;"),   
                 radioButtons(inputId = "Toux", label = "Est ce que vous toussez ? ", choices = unique(data$toux), selected='Non')),
          
          column(4,align="center",style=list("padding-right: 3px;"),   
                 radioButtons(inputId = "essoufflement", label = "Est ce que vous essouflez ? ", choices = unique(data$essoufflement),selected='Non'))),
        fluidRow(
          
          column(5,align="center",style=list("padding-right: 3px;"),
                 selectInput(inputId = "fievre", label = " Est ce que vous avez la fièvre ?", choices = unique(data$fievre), selected='Non')),
          
          column(5,align="center",style=list("padding-right: 3px;"),          
                 selectInput(inputId = "Test_indication", label = "Vous êtes :", choices = unique(data$test_indication), selected='Autre'))),
        
        fluidRow(
          column(10,align="center",style=list("padding-right: 3px;"),
                 actionButton("submitForm", label = "Soumettre", class = "btn-success")))
      )),
    mainPanel(
      tableOutput("filledForm"),
      tableOutput("prediction")
    )
  )
)


srv <- function(input, output) {
  
  output$questionnaire <- renderUI({
    fieldsToFill
  })
  
  observeEvent(input$submitForm, {
    fieldsIDs <- c("Toux","maux_de_gorge","essoufflement","maux_de_tete","Sexe","Test_indication","Age","fievre","gout")
    data <- reactive(sapply(fieldsIDs, function(x) input[[x]]))
    df <- as.data.frame(t(data())
    )
    colnames(df) <- c("toux","maux_de_gorge","essoufflement","maux_de_tete","sexe","test_indication","age","fievre","gout")
    if ("responses" %in% ls()) {
      responses <<- rbind(responses, df)
      output$filledForm <- renderTable(responses)
      output$prediction <- renderTable({
        data.frame(
          Statistique = c("Résultat", "Probabilité d'être positif", "Probabilité d'être négatif"),
          Valeur = c(ifelse((predict(LR,responses,type="response"))> 0.5 , "Positif", "Négatif"),
                     (predict(LR,responses,type="response")), 
                     (1-(predict(LR,responses,type="response")))
          ))
      })
    } else {
      responses <<- df
      output$filledForm <- renderTable(responses)
      output$prediction <- renderTable({
        data.frame(
          Statistique = c("Résultat", "Probabilité d'être positif", "Probabilité d'être négatif" ),
          Valeur = c(ifelse((predict(LR,responses,type="response"))> 0.5 , "Positif", "Négatif"),
                     (predict(LR,responses,type="response")), 
                     (1-(predict(LR,responses,type="response")))
          ))
      })
    }
    reset(id = "userForm")
  })
}

shinyApp(ui, srv)

