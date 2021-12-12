library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(padr)
library(png)


# ----------------------------------loading data

load("./got_data.RData")
char_file <-read.csv(file='./data/characters.csv')

s_min = 1
s_max = max(scenes["episodeId"])


shinyServer(function(input, output,session) {
  
  # ----------------------------------death per episode plot
  
  output$deadEplot <- renderPlot({
    
    dead <- scenes %>% filter(episodeId==input$episodeID) %>% group_by(location) %>% summarize(nbdeath = sum(nbdeath)) %>% left_join(scenes_loc) %>% filter(nbdeath>0) %>% st_as_sf()
    
    ggplot() + geom_sf(data = land,fill="antiquewhite") + geom_sf(data = islands,fill="antiquewhite") + geom_sf(data=dead,aes(size=nbdeath*1.5, fill=nbdeath),color="black",pch=21) + theme(panel.background = element_rect(fill = "aliceblue")) + scale_size_area("Number of deaths") +coord_sf(expand = 0,ndiscr = 0)+
      coord_sf(expand = FALSE)+ labs(x = "Longitude",y="Lattitude")+
      geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=3,family="Palatino", fontface="italic")
  })
  
  # ----------------------------------cumulative deaths plot
  
  output$deadCplot <- renderPlot({
    deadc <- scenes %>% filter(episodeId<=input$episodeID) %>% group_by(location) %>% summarize(nbdeath = sum(nbdeath)) %>% left_join(scenes_loc) %>% filter(nbdeath>0) %>% st_as_sf()
    
    ggplot() + geom_sf(data = land,fill="antiquewhite") + geom_sf(data = islands,fill="antiquewhite") + geom_sf(data=deadc,aes(size=nbdeath*1.5, fill=nbdeath),color="black",pch=21) + theme(panel.background = element_rect(fill ="aliceblue" )) + scale_size_area("Number of deaths") +coord_sf(expand = 0,ndiscr = 0)+
      coord_sf(expand = FALSE) + labs(x = "Longitude",y="Lattitude")+
      geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=3,family="Palatino", fontface="italic")
    
    
  })
  
  # ----------------------------------death per episode count
  
  output$dead_e <- renderText({
    dead_e <- scenes %>% filter(episodeId==input$episodeID) %>% summarize(nbdeath = sum(nbdeath))
    dead_e$nbdeath
  })
  # ----------------------------------cumulative deaths count
  
  output$dead_c <- renderText({
    dead_c <- scenes %>% filter(episodeId <= input$episodeID) %>% summarize(nbdeath = sum(nbdeath))
    dead_c$nbdeath
  })
  
  
  
  # ----------------------------------fetching image url
  
  output$img <- renderText({
    
    img <- char_file %>% filter(name == input$characters)
    img$image
  })
  
  # ----------------------------------fetching name
  
  output$name <- renderText({
    name <- char_file %>% filter(name == input$characters)
    name$name[is.na(name$name)] <- " -Name Not specified- "
    name$name
  })
  
  # ----------------------------------fetching gender
  
  output$sex <- renderText({
    sex <- char_file %>% filter(name == input$characters)
    if(length(sex$sex) > 0)
      if(is.na(sex$sex[1])) sex$sex[1] <- ""
      else sex$sex[1] <- paste(", a ",sex$sex[1])
      sex$sex
  })
  
  # ----------------------------------fetching house
  
  output$house <- renderText({
    house <- char_file %>% filter(name == input$characters)
    #house$house[is.na(house$house)] <- " -Not specified- "
    if(length(house$house) > 0)
      if(is.na(house$house[1])) house$house[1] <- ""
      else house$house[1] <- paste(", of house ",house$house[1])
      house$house
  })
  
  # ----------------------------------fetching killedBy
  
  output$killedby <- renderText({
    killedby <- char_file %>% filter(name == input$characters)
    if(length(killedby$killedBy) > 0)
      if(is.na(killedby$killedBy[1])) killedby$killedBy[1] <- ""
      else killedby$killedBy[1] <- paste(", killed by ",killedby$killedBy[1])
      killedby$killedBy
  })
  
  
  # ----------------------------------displaying image
  
  output$myImage <- renderUI({
    img <- char_file %>% filter(name == input$characters)
    tags$img(
      
      src=img$image)
  })
  
  
  
  # ----------------------------------fetching main character name for 3rd panel
  
  output$mainchar <- renderText({
    mainchar <-   appearences%>% filter(name == input$maincharacters)
    mainchar$name[1]
    
  })
  
  # ----------------------------------fetching episode for main character name for 3rd panel
  
  output$mainchar_ep <- renderText({
    mainchar_ep <-   appearences %>% left_join(scenes) %>% filter(episodeId == input$maincharepisodeID)
    if(length(mainchar_ep$episodeId) > 0)
      if(is.na(mainchar_ep$episodeId[1])) mainchar_ep$episodeId[1] <- ""
      else mainchar_ep$episodeId[1] <- paste(", in episode ",mainchar_ep$episodeId[1])
      mainchar_ep$episodeId[1]
      
  })
  
  # ---------------------------------- barchart for duration per location
  
  output$barchar<- renderPlot({
    appearence <-   appearences %>% left_join(scenes) %>% filter(name == input$maincharacters) %>% filter(episodeId == input$maincharepisodeID) %>% group_by(location) %>% summarise(duration = sum(duration/60))
    
    barplot(appearence$duration,name=appearence$location,xlab = "Time appeared ", ylab = "Locations", col="#e6f4e3", las=1)
    
  })
  
})

