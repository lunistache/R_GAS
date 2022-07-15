pacman::p_load(lubridate,htmltools,shiny,shinydashboard,shinythemes,httr,tidyverse,jsonlite,janitor,leaflet,leaflet.extras, purrr)

url <- 'https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'
GET(url)

#save info of url
df <- url %>%
  fromJSON() %>%
  .$ListaEESSPrecio %>%
  tibble()



# data cleaning
df <- df %>%
  clean_names()#standardize name;
df$precio_hidrogeno <- as.integer(df$precio_hidrogeno)

df <- df %>%
  type_convert(locale = locale(decimal_mark = ',')) %>%
  rename(longitud=longitud_wgs84)


#DATASET PRICE
fuel_price <- df[9:22] %>% #look only from row 9 to 22
  mutate(df$idccaa) %>%
  rename(idccaa=`df$idccaa`) #comunidad autonama ID


# MEAN AND GROUP BY IDCCAA
fuel_price <- fuel_price %>%
  group_by(idccaa) %>%
  summarise_all(mean,na.rm=TRUE) ##add al values than do mean with mean


#rENAMING NAMES
fuel_price <- fuel_price %>%
  rename( mean_price_biodiesel=precio_biodiesel,
          mean_price_bioetanol= precio_bioetanol,
          mean_price_gas_natural_comprimido = precio_gas_natural_comprimido,
          mean_price_gas_natural_licuado = precio_gas_natural_licuado,
          mean_price_gases_licuados_del_petroleo = precio_gases_licuados_del_petroleo,
          mean_price_gasoleo_a = precio_gasoleo_a,
          mean_price_gasoleo_b = precio_gasoleo_b, 
          mean_price_gasoleo_premium = precio_gasoleo_premium,
          mean_price_gasolina_95_e10 = precio_gasolina_95_e10,
          mean_price_gasolina_95_e5 = precio_gasolina_95_e5,
          mean_price_gasolina_95_e5_premium = precio_gasolina_95_e5_premium,
          mean_price_gasolina_98_e10 = precio_gasolina_98_e10,
          mean_price_gasolina_98_e5 = precio_gasolina_98_e5,
          mean_price_hidrogeno = precio_hidrogeno)



#MERGE DATASET WITH PRICE DS

df <- merge(x = df, y = fuel_price, by = c("idccaa", "idccaa")) %>% #merge datasets location with price 
  as_tibble()


#CREATE COLUMN FOR FUEL PRICES
df<-df %>% mutate(assessment_price_biodiesel= (df$precio_biodiesel>df$mean_price_biodiesel))
df<-df %>% mutate(assessment_price_bioetanol= (df$precio_bioetanol>df$mean_price_bioetanol))
df<-df %>% mutate(assessment_price_gas_natural_comprimido= (df$precio_gas_natural_comprimido>=df$mean_price_gas_natural_comprimido))
df<-df %>% mutate(assessment_price_gas_natural_licuado= (df$precio_gasoleo_b>df$mean_price_gasoleo_b))
df<-df %>% mutate(assessment_price_gases_licuados_del_petroleo= (df$precio_gases_licuados_del_petroleo>=df$mean_price_gases_licuados_del_petroleo))
df<-df %>% mutate(assessment_price_gasoleo_a= (df$precio_gasoleo_a>df$mean_price_gasoleo_a))
df<-df %>% mutate(assessment_price_gasoleo_b= (df$precio_gasoleo_b>df$mean_price_gasoleo_b))

df<-df %>% mutate(assessment_price_gasoleo_premium= (df$precio_gasoleo_premium>df$mean_price_gasoleo_premium))
df<-df %>% mutate(assessment_price_gasolina_95_e10= (df$precio_gasolina_95_e10>df$mean_price_gasolina_95_e10))
df<-df %>% mutate(assessment_price_gasolina_95_e5= (df$precio_gasolina_95_e5>df$mean_price_gasolina_95_e5))
df<-df %>% mutate(assessment_price_gasolina_95_e5_premium= (df$precio_gasolina_95_e5_premium>=df$mean_price_gasolina_95_e5_premium))
df<-df %>% mutate(assessment_price_gasolina_98_e10= (df$precio_gasolina_98_e10>df$mean_price_gasolina_98_e10))
df<-df %>% mutate(assessment_price_gasolina_98_e5= (df$precio_gasolina_98_e5>df$mean_price_gasolina_98_e5))
df<-df %>% mutate(assessment_price_hidrogeno= (df$precio_hidrogeno>df$mean_price_hidrogeno))


for(i in 0:13){
  df[47+i] = ifelse(df[47+i] == 'TRUE','no_low_cost','low_cost')
}



#CREATING COLUMN FOR CCAA

name_ccaa=c('ANDALUCIA','ARAGON','PRINCIPADO DE ASTURIAS', 'ILLES BALEARS', 'CANARIAS', 'CANTABRIA', 'CASTILLA Y LEON',
            'CASTILLA-LA MANCHA', 'CATALUNA', 'COMUNITAT VALENCIANA', 'EXTREMADURA','GALICIA', 'COMUNIDAD DE MADRID',
            'REGION DE MURCIA', 'COMUNIDAD FORAL DE NAVARRA', 'PAIS VASCO', 'LA RIOJA', 'CEUTA', 'MELILLA')


df <-df%>%
  mutate(ccaa=case_when(idccaa=='01'~name_ccaa[1], idccaa=='02'~name_ccaa[2],
                        idccaa=='03'~name_ccaa[3], idccaa=='04'~name_ccaa[4],
                        idccaa=='05'~name_ccaa[5], idccaa=='06'~name_ccaa[6], 
                        idccaa=='07'~name_ccaa[7], idccaa=='08'~name_ccaa[8],
                        idccaa=='09'~name_ccaa[9], idccaa=='10'~name_ccaa[10],
                        idccaa=='11'~name_ccaa[11], idccaa=='12'~name_ccaa[12],
                        idccaa=='13'~name_ccaa[13], idccaa=='14'~name_ccaa[14],
                        idccaa=='15'~name_ccaa[15], idccaa=='16'~name_ccaa[16],
                        idccaa=='17'~name_ccaa[17], idccaa=='18'~name_ccaa[18], 
                        idccaa=='19'~name_ccaa[19]))



# MODIFICACION DE HORARIO -------------------------------------------------

horario <- df[4]

horario <- str_split_fixed(horario$horario, "; ", 3)

horario <- as_tibble(horario)

horario_dia_1 <- str_split_fixed(horario$V1, ": ", 2)

horario_dia_1 <- as_tibble(horario_dia_1)%>%
  rename(dia_1=V1, hora_1=V2)

horas_dia_1 <- str_split_fixed(horario_dia_1$hora_1, "-", 2)

horas_dia_1 <- as_tibble(horas_dia_1)%>%
  rename(hora_1_comienzo=V1, hora_1_fin=V2)

horario_dia_1<-data.frame(horario_dia_1, horas_dia_1) %>%
  as_tibble() %>%
  select (-hora_1)

horario_dia_2 <- str_split_fixed(horario$V2, ": ", 2)

horario_dia_2 <- as_tibble(horario_dia_2)%>%
  rename(dia_2=V1, hora_2=V2)

horario_dia_3 <- str_split_fixed(horario$V3, ": ", 2)

horario_dia_3 <- as_tibble(horario_dia_3)%>%
  rename(dia_3=V1, hora_3=V2)

horario <-data.frame(horario_dia_1,horario_dia_2,horario_dia_3) %>%
  as_tibble()

df<-data.frame(df, horario) %>%
  as_tibble() %>%
  select (-horario)

#VISUAL

ui <- dashboardPage(skin = 'purple',
                    dashboardHeader(title = 'Gas Station'),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('geo',
                                        'Choose Filter',
                                        choices = list(
                                          'Locality'='localidad',
                                          'Municipality' = 'municipio')
                            ),
                            textInput('municipality', 
                                      'Introduce',
                                      value = ''),
                            selectInput('combustible',
                                        'Choose gas',
                                        choices = list('Gasoleo A' = 'precio_gasoleo_a',
                                                       'Gasoleo B' = 'precio_gasoleo_b',
                                                       'Gasoleo Premium' = 'precio_gasoleo_premium',
                                                       'Gasolina 95 e5'='precio_gasolina_95_e5',
                                                       'Gasolina 98 e5'='precio_gasolina_98_e5'),
                                        selected ='precio_gasoleo_a' ),
                            
                            sliderInput("precio",
                                        "Price",
                                        min =0,
                                        max =3,
                                        value =1),
                          ),
                          
                          mainPanel(
                            leafletOutput("df_gas_map_client")
                          )
                        )
                      )
                    )
                    
)



server <- function(input, output, ShinySession) {
  
  #MAP APP      
  
  output$df_gas_map_client <- renderLeaflet({
    
    if(!''==(selec_municipality())){
      df <- df %>%
        filter(df[,df_gas_geo(selec_search())]==selec_municipality()) %>% 
        select(!starts_with('precio'),contains(selec_gas())) %>%
        select(!starts_with('mean'),contains(selec_gas())) %>%
        select(!starts_with('assessment'),contains(selec_gas())) %>% 
        mutate(info=paste0('<strong>Brand: </strong>',rotulo,
                           '<br><strong>Adress:</strong> ', direccion))  
      
      df <- df %>%
        filter(df[,26]<=selec_price())
      
      
      df <- df %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        addMarkers(~longitud,~latitud,popup = ~as.character(info))  
      
    }
    else{
      df %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        fitBounds(~min(longitud), ~min(latitud), ~max(longitud), ~max(latitud))}
    
  })
  
  
  
  
  
  #LOCALISATION
  
  df_gas_geo <- function(x){
    if(x=='localidad'){
      return(5)
    }
    else if(x=='municipio'){
      return(8)
    }
  }
  
  
  selec_search <- reactive({
    input$geo
  })
  
  observeEvent(input$geo, {
    updateTextInput(inputId = "municipality",
                    label = paste('Introduce', selec_search(), sep=" ")
    )
  })
  
  
  MAJ <- function(x){ #MAJ FUNCTION
    x <- toupper(x)
  }
  
  correct_word <- function(x){
    if(selec_search()=='localidad'){ #PUT WORDS IN MAJ 
      return(MAJ(x))
    }
    else if(selec_search()=='municipio'){
      return(x) 
    }
  }
  
  selec_municipality <- reactive({
    correct_word(input$municipality)
  })
  
  
  #GAS PRICE  
  
  selec_gas <- reactive({
    input$combustible
  })
  
  
  selec_price <- reactive({
    input$precio
  })
  
  correct_gas_price <- function(x){
    if(selec_gas()=='precio_gasoleo_a'){
      return(14) 
    }
    else if(selec_gas()=='precio_gasoleo_b'){
      return(15)
    }
    else if(selec_gas()=='precio_gasoleo_premium'){
      return(16)
    }
    else if(selec_gas()=='precio_gasolina_95_e5'){
      return(18)
    }
    else if(selec_gas()=='precio_gasolina_98_e5'){
      return(21)
    }
  }
  
  
  
  observeEvent(input$municipality, {
    if(!''==(selec_municipality())){
      observeEvent(input$combustible, {
        df <- df %>%
          filter(df[,df_gas_geo(selec_search())]==selec_municipality())
        updateSliderInput(
          inputId = "precio",
          min=min(df[,correct_gas_price(selec_gas())],
                  na.rm = TRUE),
          max=max(df[,correct_gas_price(selec_gas())],
                  na.rm = TRUE),
          value=max(df[,correct_gas_price(selec_gas())],
                    na.rm = TRUE)
        )
      })
    }
  })
}

shinyApp(ui,server)