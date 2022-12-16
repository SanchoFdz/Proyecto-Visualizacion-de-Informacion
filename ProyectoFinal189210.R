  library(shiny)
  library(leaflet)
  library(leaflet.extras)
  library(ggdendro)
  library(reshape)
  library(dplyr)
  library(fullPage)
  library(readxl)
  library(ggplot2)
  library(plotly)
  library(forcats)
  library(tidyverse)
  library(glue)
  
  
  Hojatotal <- as.data.frame(read_excel("Hojatotal.xlsx"))
  Hojatotal[,c(2:10)] <- sapply( Hojatotal[,c(2:10)], as.numeric )
  colnames(Hojatotal)[1]  <- "EntidadFederativa"
  
  entidades <- unique(unlist(list(Hojatotal$EntidadFederativa)))
  
  df_Final <- as.data.frame(read_excel("DfFinal.xlsx"))
  
  columnas2 <- names(df_Final[,c(10:18)])
  
  GUI <- fullPage(
    menu = c(
      "Introudccion" = "intro",
      "Estadistica Descriptiva" = "eda",
      "Actualidad" = "act",
      "Mapas" = "maps",
      "Conclusiones" = "conc"
    ),
    
    fullSection(
      menu = "intro",
      center = TRUE,
      sidebarLayout(position = "right",
                    sidebarPanel(h2("Objetivo de Desarrollo Sostenible"),
                                 h3("4: Educacion de Calidad. Metas 2030:"),
                                 p("- Acceso a educacion gratuita y de calidad primaria y secundaria,"),
                                 p("- Acceso a servicios de antencion y desarrollo en la primera infancia,"),
                                 p("- Acceso a educacion universitaria tecnica y profesional, igualitaria y de calidad,"),
                                 p("- Aumentar el numero de profesionistas y tecnicos capacitados,"),
                                 p("- Eliminar disparidades de genero, capacidad y origen en el acceso a la educacion,"),
                                 p("- Elevar la alfabetizacion y nocion aritmetica en los jovenes y adultos,"),
                                 p("- Inculcar en los estudiantes el desarrollo sostenible, igualitario, diverso y pacifico,"),
                                 p("- Construir entornos educativos seguros, pacificos, inclusivos y eficaces,"),
                                 p("- Aumentar el numero de becas para paises subdesarrollados,"),
                                 p("- Aumentar el numero de docentes capacitados.")),
                    mainPanel(h1("Introduccion"),
                              h2("La Educacion en Mexico"),
                              br(),
                              p("En el trabajo que se presenta a continuacion se presentan visualizaciones generadas
                                con el uso de datos obtenidos de el INEGI. Estos datos detallan el estado de la educacion
                                en Mexico y en sus entidades federativas y se busca contextualizarlos dentro del marco economico
                                del pais durante un periodo que coincide con el que abarcan los datos educativos."),
                              br(),
                              p("La educacion en Mexico experimento un auge o epoca dorada a comienzos del siglo pasado y finales del antepasado.
                                Propuesta en 1881 por Justo Sierra y fundada en 1910 por Porfirio Diaz, la UNAM represento el primer climax
                                en la educacion mexicana luego de 50 anos de lucha por mejorar un otrora precario sistema colonial con la fundacion
                                de la Escuela Nacional Preparatoria y el impulso de la filosofia positivista desde el oficialismo. Su libertad de 
                                catedra y autonomia financiera representaron el primer gran impulso a la educacion en Mexico."),
                              br(),
                              p("Este golpe inicial cobraria mayor fuerza con la llegada de Jose Vasconcelos, filosofo y educador mexicano,
                                a la SEP como su primer presidente en 1921. El periodo de Construccion de Instituciones luego de la revolucion 
                                se reflejaria tambien en la educacion: Vaconcelos constuiria en 3 anos 5000 escuelas, aumentaria en 9000 la
                                fuerza magisterial y matricularia a 1 millon de alumnos. Su legado iria mas alla al motivar desde la SEP el
                                 arte y la cultura mexicanos y la formacion de grupos artisticos y filosoficos. Este proyecto lo retomaria Obregon
                                como presidente y daria pie al auge del muralismo mexicano asi como a la federalizacion de la educacion en Mexico."),
                              br(),
                              p("Este auge duraria alrededor de 40 anos de la mano de Lazaro Cardenas y su educacion socialista y de la institucion
                                del libro de texto gratuito, uno de los mayores avances en la historia de la educacion mexicana, por parte de Jaime
                                Torres Bodet en 1970. Sin embargo, desde entonces la educacion entraria en un periodo de relativo estancamiento y veria
                                pocos avances en su modernizacion. Los 24 anos de control de Elba Ester Gordillo sobre el SNTE desgastaria las
                                relaciones entre el oficialismo, la SEP y el cuerpo de maestros. Desde entonces, todo intento de reforma es frenado
                                por el sindicato magisterial."),
                              br(),
                              p("Esta situacion ha devenido en una urgente necesidad de reformar, refrescar e impulsar un nuevo auge del sistema
                                educativo en Mexico. Poca cobertura, baja calidad educativa, disidencia sindical y falta de recursos son algunos de los
                                principales problemas que limitan el potencial de los jovenes mexicanos y que, hoy mas que nunca, es urgente solucionar."),
                              br(),
                              p("A lo largo de este proyecto se pretende estudiar el estado actual de la educacion en Mexico y contrextualizarlo dentro
                                del marco de la economia del hogar mexicano y,ante la falta de datos que dejen entrever su verdadero efecto
                                en el mediano y largo plazo, dejando abierta la puerta a un estudio en el marco de la pandemia por COVID-19."))
                    )
    ),
    fullSection(
      menu = "eda",
      center = TRUE,
      h1("Estadistica Descriptiva"),
      h2("Variables recopiladas y la relacion entre ellas"),
      p("A continuacion se presentan las variables recopiladas para este proyecto y la relacion entre estas, 
        el lector debe recordar la maxima a tener en mente durante todo analisis estadistico visual: 'correlacion no
        necesariamente implica causalidad'. Las matrices y diagramas que se presentan a continuacion no pretenden mas que
        acercar al usuario a una comprension mayor del marco dentro del cual se pretende estudiar esta problematica y a un 
        mejor conocimiento de las variables con las que el INEGI se da a la tarea de medir la educacion y economia del hogar en Mexico."),
      br(),
      h3("Matriz de Dispersion"),
      plotOutput("plotIntro"),
      p("Esta matriz de dispersion, con la que podras jugar en la siguiente pestana, nos permite estudiar las variables que
         tienen un correlacion fuerte entre si y estudiar el sentido de esta relacion, negativa o positiva, entre ellas. Se debe tener 
        cuidado de no utilizarla para sacar conclusiones, si no para encontrar variables que potencialmente puedan tener un relacion de 
        causalidad y en las cuales seria interesante profundizar con un ainvestigacion."),
      h3("Mapa de Calor"),
      plotOutput("mapaDeCalor"),
      p("Entre mas claro el color del cuadro, mayor la correlacion entre ese par de variables. Observe que la diagonal 
        principal es de color claro lo cual senala que entre las variables mismas, la correlacion de es total."),
      h3("Dendograma"),
      plotOutput("dendograma"),
      p("Este arbol nos permite estudiar con mayor precision la correlacion entre variables. Aquellas variables que 
        aparecen mas cerca unas de otras (en altura y distancia horizontal) tienen mayor correlacion entre ellas."),
      br(),
      p("Expuesto lo anterior, parece razonable estudiar la relacion entre la razon Alumno/Escuela (alumnos por escuela) y el gasto 
        en cada estado en Educacion y Alimentacion pero tambien estudiar las variables de aumento en pobreza y esperanza educativa comparadas 
        con estas otras dos variables. Otra que parece interesante, es la relacion entre gasto en salud y las tasas de absorcion y abandono 
        escolares, ha aumentado el gasto en salud a raiz de la pandemia y esto se correlaciona con cambios en las tasas educativas? Es una pregunta 
        que analizando este dendograma podria ser interesante estudiar en un futuro de este proyecto.")
    ),
    fullSection(
      menu = "act",
      center = TRUE,
      h1("Actualidad"),
      h2("Visualizaciones y estadisticas para entender la actualidad de la educacion en Mexico."),
      p("Selecciona 2 variables que quieras graficar en una diagrama de dispersion para estudiar su 
        correlacion."),
      selectInput('input1act', "Variable Eje X", columnas2), 
      selectInput('input2act', "Variable Eje Y", columnas2),
      plotlyOutput("scatterPlot"),
      p("Este es un graifco de Plotly por lo que puedes poner el cursor sobre cada punto y recibiras 
        el nombre del estado al que corresponde dicho punto y sus valores para ambas variables. Ademas, puedes 
        hacer click y arrastrar con el cursor para hacer zoom dentro del grafico o ajustar los ejes a tu gusto."),
      p("A continuacion se presenta un grafico no dinamico para entender como se ven estas dos variables graficadas 
        en ambas combinaciones de ejes. "),
      plotOutput("scatterPlotStat")
    ), 
    fullSection(
      menu = "maps",
      center = TRUE,
      h1("Mapas"),
      h2("Mapas que permiten estudiar la variable de tu eleccion"),
      selectInput('inputmap', "Variable", columnas2),
      p("Selecciona la variable que desees estuidar. Puedes hacer click y arrastrar sobre el mapa para hacer zoom sobre algun estado,
        tambien puedes descansar el cursor sobre el estado de tu eleccion y aparecera su nombre y el valor de la variable seleccionada."),
      plotlyOutput("mapa")
    ),
    fullSection(
      menu = "conc",
      center = TRUE,
      h1("Conclusiones"),
      h2("Mejoras futuras y comentarios"),
      p("Hemos visto la situacion precaria en la que, historica y estadisticamente, se encuentra Mexico en materia de educacion. Las cifras y 
        su analisis arrojan un preocupante diagnostico: urge un cambio en Mexico y cuanto antes, mejor. Un cambio en la educacion mexicana no surtiria 
        efecto inmediatamente y por lo tanto su sfrutos mas maduros no serian cosechados si no hasta en un par de decadas, por lo que mientras mas 
        tardemos, mas dificil la tendremos como pais hacia el futuro."),
      br(),
      p("Esperamos que esta aplicacion web sea de utilidad para todos aquellos que buscan una primera aporixmacion al problema de la educacion en 
        Mexico, pues no es posible empezar a trabajar un problema sin antes entender el comportamiento de sus variables, su cambio a lo largo del tiempo, 
        la relacion entre ellas, etc."),
      br(),
      p("Los objetivos de comunicar clara y objetivamente las cifras de la educacion en Mexico se cumplen con este trabajo. De cara al futuro hay un par de 
        cambios que podrian implementar. El primero es un analisis de las encuestas ecovid y de las mismas variables una vez pasados mas anos desde 
        el termino del cenit de la pandemia por la COVID-19.Por ahora es muy pronto para sacar conclusiones sobre los efectos de la pandemia en 
        la educacion pues ha pasado poco tiempo, hay pocos datos y los que hay no son los mas ilustrativos. Otra mejora seria implementar un modelo de regresion 
        para ver cuales son las variables que mejor explican el abandono o absorcion escolar y reportar los resultados con visualizaciones y resumenes 
        estadisticos digeribles en este dashboard. ")
    )
  )

  Funcionalidad <- function(input, output){
    # Lógica del dashboard
  
    misDatosAct <- reactive({
      datos <- Hojatotal
    })

    output$scatterPlot <- renderPlotly({
      ggplot(data = misDatosAct(), aes_string(x = input$input1act, y = input$input2act)) + 
        geom_point()
      
      fig1 <- ggplot(data = misDatosAct(), aes_string(x = input$input1act, y = input$input2act, text = "EntidadFederativa")) +
        geom_point() + ggtitle(glue('{input$input1act} vs {input$input2act}'))
      
      ggplotly(fig1)
    })
    
    output$scatterPlotStat <- renderPlot({
      pairs(Hojatotal[c(input$input1act,input$input2act)])
    })
    
    output$mapaDeCalor <- renderPlot({
      datos <- Hojatotal[,c(2:10)]
      correM <- cor(datos)
      correM2 <- melt(correM)
      distancia <- as.dist((1- correM)^2)
      agrupamiento3 <- hclust(distancia)

      correM_Ordenada <- correM[agrupamiento3$order, agrupamiento3$order]
      correM3 <- melt(correM_Ordenada)
      heatmap3 <- ggplot(correM3, aes(X1,X2, fill=value))+geom_tile()
      heatmap3
      
    })
    
    output$dendograma <- renderPlot({
      datos <- Hojatotal[,c(2:10)]
      correM <- cor(datos)
      correM2 <- melt(correM)
      distancia <- as.dist((1- correM)^2)
      distancia
      agrupamiento3 <- hclust(distancia)
      dend <- as.dendrogram(agrupamiento3)
      dend_data <- dendro_data(dend, type = "rectangle")
      
      p <- ggplot(dend_data$segments) + 
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
        geom_text(data = dend_data$labels, aes(x, y, label = label),
                  hjust = 1, angle = 90, size = 3)+
        ylim(-0.8, 2)
      p
    })
    
    output$mapa <- renderPlotly(
      {
        title <- ""
        if (input$inputmap %in% c("CambioEsperanza","AumentoPobreza")){
          title <- glue("{input$inputmap} (~%) entre 2018-2020")
        }else{
          title <- glue("{input$inputmap}")
        }
        
        mapa1 <- ggplot(df_Final,aes_string("long","lat",group="group",fill=input$inputmap, text="estado")) +
          geom_polygon() +
          scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) +
          ggtitle(title)
        mapa1
        ggplotly(mapa1)
      }
    )
    
    output$plotIntro <- renderPlot({
      pairs(Hojatotal[,c(2:10)])
    })
  
  }
  
  
  
  
  
  shinyApp(GUI, Funcionalidad)
