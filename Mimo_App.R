# This is an App done to help you find the better food to eat according to you fitness requirements in order to achive the goal.
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)


df <- read_excel("C:/Users/Usuario/Desktop/mimo_proyecto/DF_alimentos_no_tildes.xlsx")

df <- df %>% select(-colnames(df)[1]) #Storage in de df a filter by selection all except that path

df <- df %>% distinct()

#rownames(df)  <- df$Nombre

# Via filter select data from what user type
find_possibles <- function(food){ 
    
    possibles <<-filter(df, grepl(food, Nombre)) # Filter all rows that contain a certain string
    
    return(possibles$Nombre)
}

# Calculo para hallar Total calorias perdida de grasa
TCPG <- function(Perdida_peso_semanal){ 
    
    kg = c(0.23,0.45,0.68,0.91)
    kcals = c(250,500,700,1000)
    
    df_tcpg <- data.frame(Kg = kg, Calorias = kcals) 
    rownames(df_tcpg) <- df_tcpg$Kg
    df_tcpg$Kg <- NULL
    n_df_tcpg <<- df_tcpg[which(rownames(df_tcpg) == Perdida_peso_semanal), 1]
    return(n_df_tcpg)
} 





# formula to MM 
MM <- function( Peso, Grasa_corporal){
    
    masa_magra <<- (Peso * (100 - Grasa_corporal))/100
    
    return(masa_magra)
}

# Metabolismo basal
MB <- function( Peso, Grasa_corporal){
    
    met_basal <<- (370 + (21.6 * MM(Peso, Grasa_corporal)))
    
    return(met_basal)
}

# Encontrar el nivel de actividad
nivel_act <- function( n_a){ 
    hrs <-  c("0 ", "1 a 2", "3 a 4", "5 a 6", "7 a 9", "10 a 11",
              "12 a 13", "14 a 16", "17 a 20", "21 a 22", "+ 23")
    
    n_act <- c(1.20, 1.30,1.40,1.50,1.60,1.70,1.80,1.90,2 ,2.1 ,2.2)
    
    act <- data.frame(Nivel_act = n_act, Hrs = hrs)
    rownames(act) <- act$Hrs
    act$Hrs <- NULL
    n_act_f <<- act[which(rownames(act) == n_a), 1]
    
    return(n_act_f)
    
}


########### Proteina a consumir 

c_prot <- function(gm, somatipo){ 
    g_musc = c(1.15,1.15,1.25)
    p_grasa = c(1.35,1.35,1.5)
    n_soma = c("Ectomorfo", "Mesomorfo", "Endomorfo")
    
    df_gananmusc <- data.frame(Somatipo = n_soma, Ganar_musculo = g_musc, Perder_grasa = p_grasa) 
    rownames(df_gananmusc) <- df_gananmusc$Somatipo
    df_gananmusc$Somatipo <- NULL
    if (gm == "Ganar musculo"){ 
        
        gananmusc <<- df_gananmusc[which(rownames(df_gananmusc) == somatipo ), 1]
        
    }else{
        
        gananmusc <<- df_gananmusc[which(rownames(df_gananmusc) == somatipo), 2]
        
    }
    return(gananmusc)
}



############# Grasa a consumir

c_gra <- function(gm, somatipo){ 
    g_musc = c(0.45,0.4,0.6)
    p_grasa = c(0.4,0.35,0.4)
    n_soma = c("Ectomorfo", "Mesomorfo", "Endomorfo")
    
    df_gananmusc <- data.frame(Somatipo = n_soma, Ganar_musculo = g_musc, Perder_grasa = p_grasa) 
    rownames(df_gananmusc) <- df_gananmusc$Somatipo
    df_gananmusc$Somatipo <- NULL
    if (gm == "Ganar musculo"){ 
        
        gananmusc_f <<- df_gananmusc[which(rownames(df_gananmusc) == somatipo ), 1]
        
    }else{
        
        gananmusc_f <<- df_gananmusc[which(rownames(df_gananmusc) == somatipo), 2]
        
    }
    return(gananmusc_f)
}

###### carbos en buscar alimentos

carbos <- function(food){
    
    d_car <<- df %>% filter( Nombre == food )
    #d_car <<- as.numeric(d_car['Carbohidratos'])
    return(d_car)
    
    
}



######     UX     #####
 
header <- dashboardHeader(title = "Yesi App <3")




sidebar <- dashboardSidebar(
    
    sidebarMenu(
        menuItem("Calculadora",
                 tabName = "calculadora"),
        menuItem("Buscar alimento",
                 tabName = "ba1")
        
    )
)



body <- dashboardBody(
    fluidRow( 
tabItems(     
    tabItem(tabName = "calculadora", 
           
            box( selectInput("txt_1", "Objetivo", c("Perder grasa", "Ganar musculo")),
                 textInput("txt_2", "Peso en Kg:"),
                 textInput("txt_3", "Grasa corporal en %:"),
                 selectInput("txt_4", "Actividad Fisica por semana", c("1 a 2", "3 a 4", "5 a 6", "7 a 9", "10 a 11",
                                                "12 a 13", "14 a 16", "17 a 20", "21 a 22", "23 +")),
                 selectInput("txt_5", "Perdida peso semanal", c(0.23,0.45,0.68,0.91)),
                 selectInput("txt_6", "Somatipo", c("Ectomorfo", "Mesomorfo", "Endomorfo")),
                 submitButton("Calcular"),
                
              ),
            box(valueBoxOutput("valor"),
                valueBoxOutput("valor_2"),
                valueBoxOutput("act_fisi"),
                valueBoxOutput("k_obj"),
                valueBoxOutput("c_prote"),
                valueBoxOutput("gras"),
                valueBoxOutput("carbs")
            
            
            )
            ),
    
    tabItem(
        tabName = "ba1", 
                        
           box( textInput("txt", "Digita un alimento:"),
                ),
           box( 
                selectInput("inSelect", "Select input",c("Item A", "Item B", "Item C")),
                ),
           
           box( 
               submitButton("Actualizar")),
        
           box(
                valueBoxOutput("result"),
                valueBoxOutput("result_1"),
                valueBoxOutput("result_2"),
                valueBoxOutput("result_3"),
                )
            )
            
        )
    )  
 )








ui <- fluidPage(dashboardPage(skin = "green", header, sidebar, body))


# SERVER  
server <- function(input, output, session) {
    

    observe({
       x <- find_possibles( input$txt )
       updateSelectInput(session, "inSelect",
                         label = paste("Selecione una de las ", length(x), " opciones:"),
                         choices = x,
                         selected = tail(x, 1)
       )

    })
    
    
    output$text <- renderTable({
        
        
        
        find_possibles( input$txt )
        
        })
   
    
    output$valor <- renderValueBox({
        
            valueBox( 
            round(MM(as.numeric(input$txt_2), as.numeric(input$txt_3)),0),
            subtitle = "Masa magra (kg)",
            icon = icon("fire",  lib= "font-awesome"),
            color= "maroon"    
            )
        
    })

    output$valor_2 <- renderValueBox({
            valueBox( 
            round(MB(as.numeric(input$txt_2), as.numeric(input$txt_3)),0),
            subtitle = "Metabolismo basal (kcal)",
            icon = icon("bolt",  lib= "font-awesome"),
            color= "purple"
        )
    
  
       
    })

    output$act_fisi <- renderValueBox({
       
        valueBox( 
        round(nivel_act(input$txt_4) * MB(as.numeric(input$txt_2), as.numeric(input$txt_3)),0),
        subtitle = "Calorias mantenimiento (Kcal)",
        icon = icon("check",  lib= "font-awesome"),
        color= "olive"
        )
       
         })
    
    
    output$k_obj <- renderValueBox({
        
        valueBox( 
            if(input$txt_1 == "Ganar musculo"){
            
            round((nivel_act(input$txt_4) * MB(as.numeric(input$txt_2), as.numeric(input$txt_3)))) + 200
            
            } else{
            
            round((nivel_act(input$txt_4) * MB(as.numeric(input$txt_2), as.numeric(input$txt_3)))- TCPG(input$txt_5)) 
            },
        
            subtitle = "Calorias objetivo (kcal)",
            icon = icon("bullseye",  lib= "font-awesome"),
            color= if (input$txt_1 == "Ganar musculo") "green"else "aqua"
         )
    })
    
    
    output$c_prote<- renderValueBox({
        valueBox( 
        round((MM(as.numeric(input$txt_2), as.numeric(input$txt_3)) * 2.20462) * c_prot(input$txt_1,input$txt_6 ),0),
        subtitle = "Cantidad Proteina (g)",
        icon = icon("drumstick-bite",  lib= "font-awesome"),
        color= "yellow"
        )
        
    })
    
    
    output$gras<- renderValueBox({
        valueBox( 
            round((MM(as.numeric(input$txt_2), as.numeric(input$txt_3)) * 2.20462) * c_gra(input$txt_1,input$txt_6 ),0),
            subtitle = "Cantidad grasa (g)",
            icon = icon("bacon",  lib= "font-awesome"),
            color= "teal"
        )
        
    })
    
    output$carbs<- renderValueBox({
      
        valueBox( 
        if (input$txt_1 == "Ganar musculo"){
            (round(nivel_act(input$txt_4) * MB(as.numeric(input$txt_2), as.numeric(input$txt_3)),0)+200 -
            (round((MM(as.numeric(input$txt_2), as.numeric(input$txt_3)) * 2.20462) * c_gra(input$txt_1,input$txt_6 ),0)*9)-
            (round((MM(as.numeric(input$txt_2), as.numeric(input$txt_3)) * 2.20462) * c_prot(input$txt_1,input$txt_6 ),0) *4))/4
         
        }else{
            (round((nivel_act(input$txt_4) * MB(as.numeric(input$txt_2), as.numeric(input$txt_3)))- TCPG(input$txt_5)) -
            (round((MM(as.numeric(input$txt_2), as.numeric(input$txt_3)) * 2.20462) * c_gra(input$txt_1,input$txt_6 ),0)*9)-
            (round((MM(as.numeric(input$txt_2), as.numeric(input$txt_3)) * 2.20462) * c_prot(input$txt_1,input$txt_6 ),0) *4))/4

        },
        
            subtitle = "Cantidad carbohidratos (g)",
            icon = icon("carrot",  lib= "font-awesome"),
            color= "light-blue"
        )
          
    })
    
    
    output$carbs_bd<- renderValueBox({
        valueBox( 
            carbos(input$inSelect),
            subtitle = "Cantidad grasa (g)",
            icon = icon("bacon",  lib= "font-awesome"),
            color= "teal"
        )
        
    })
    
    
    
    
    text <- reactiveValues()
    
    observeEvent(input$inSelect,{
      for(h in 1:1280){       
        if (df[h,1] == input$inSelect){
          
          text$result <- df[h,10] # carbos
          text$result_1 <- df[h,4] # Grsas
          text$result_2 <- df[h,3] # Prote
          text$result_3 <- df[h,2] # Calorias
          
        }
      } 
    })
    
    
    
    
    
    output$result<- renderValueBox({
        
      valueBox( 
        text$result,
        subtitle = "Carbohidratos (g)",
        icon = icon("carrot",  lib= "font-awesome"),
        color= "light-blue"
      )
     })
    
    output$result_1<- renderValueBox({
      
      valueBox( 
        text$result_1,
        subtitle = "Grasas (g)",
        icon = icon("bacon",  lib= "font-awesome"),
        color= "teal"
      )
    })
    
    output$result_2<- renderValueBox({
      
      valueBox( 
        text$result_2,
        subtitle = "Proteina (g)",
        icon = icon("drumstick-bite",  lib= "font-awesome"),
        color= "yellow"
      )
    })
    
    output$result_3 <- renderValueBox({
      
      valueBox( 
        text$result_3,
        subtitle = "Calorias (Kcal)",
        icon = icon("fire",  lib= "font-awesome"),
        color= "maroon"
      )
    })
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)

