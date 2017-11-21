
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(devtools)
library(shiny)
library(dplyr)
library(hexbin)
library(ggplot2)
library(RColorBrewer)
library(curl)
githubURL <- ("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Season2018.rds")
#download.file(githubURL,"Season2018.rds", method="curl")
Season2018 <- readRDS("Season2018.rds")
load("court.rda")
OffShotSeasonGraphTeam<- function(Seasondata, team, nbins = 25, quant = 0.4, type = "PPS", MAX_Y = 280) {
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Seasondata <- dplyr::filter(Seasondata, TEAM_NAME == team)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams
  
  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  
  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))
  
  
  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)
    
    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)
    
    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }
  
  #Transform Hexbins into polygons
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(name = "PPS", midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
    
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")}
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", team, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", team, sep =" ")
  )}
  
  
  return(GRAPH)
}

DefShotSeasonGraphTeam <- function(Seasondata, team, nbins = 25, quant = 0.4, type = "PPS", MAX_Y = 280) {
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Seasondata <- dplyr::filter(Seasondata, HTM == team | VTM == team & TEAM_NAME != team)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams
  
  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  
  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))
  
  
  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)
    
    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)
    
    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }
  
  #Transform Hexbins into polygons
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(name = "PPS", midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(name = "Pct", midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")}
  GRAPH <- GRAPH +  ggtitle(paste("Defensive shot chart of", team, sep =" "))
  
  
  return(GRAPH)
}

OffShotSeasonGraphPlayer <- function(Seasondata, player, nbins = 25, quant = 0.4, type = "PPS", MAX_Y = 280) {
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
  Seasondata <- dplyr::filter(Seasondata, PLAYER_NAME == player)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the players
  
  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  
  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))
  
  
  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)
    
    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)
    
    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }
  
  #Transform Hexbins into polygons
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", player, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", player, sep =" ")
  )}
  
  
  return(GRAPH)
}

PointShotSeasonGraphPlayer <- function(Seasondata, player, Type = "Both") {
  Seasondata <- dplyr::filter(Seasondata, LOC_Y < 280 & LOC_Y > -40)
  Seasondata <- dplyr::filter(Seasondata, LOC_X < 250 & LOC_X > -250)
  Seasondata <- dplyr::filter(Seasondata, PLAYER_NAME == player)
  Seasondata$SHOT_MADE_FLAG <- ifelse(Seasondata$SHOT_MADE_FLAG == "1", "Made", "Missed")
  Seasondata$SHOT_MADE_FLAG <- as.factor(Seasondata$SHOT_MADE_FLAG)
  if (Type == "Made"){
    Seasondata2 <- dplyr::filter(Seasondata, SHOT_MADE_FLAG == "Made")
  }
  
  if (Type == "Missed"){
    Seasondata2 <- dplyr::filter(Seasondata, SHOT_MADE_FLAG == "Missed")
  }
  
  if (Type == "Both"){
    Seasondata2 <- Seasondata
  }
  
  #Make Graph
  GRAPH <- ggplot(Seasondata2, aes(x=LOC_X, y = LOC_Y))+ annotation_custom(court, -250, 250, -52, 418)  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon", show.legend = FALSE, alpha = 0.2) + geom_point(aes(color = SHOT_MADE_FLAG), alpha = 0.8) + scale_fill_gradientn(
      colours = rev( brewer.pal( 7, "Spectral" ) )
    ) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  return(GRAPH)
}

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    OffShotSeasonGraphTeam(Seasondata = Season2018, team = input$OffTeam, type = input$OffType)
  })
  output$defPlot <- renderPlot({
    DefShotSeasonGraphTeam(Seasondata = Season2018, team = input$DefTeam, type = input$DefType)
  })
  
  output$playPlotPoint <- renderPlot({
    PointShotSeasonGraphPlayer(Seasondata = Season2018, player = input$player, Type = input$playType)
  })
  
  plotInput = function() {
    PointShotSeasonGraphPlayer(Seasondata = Season2018, player = input$player, Type = input$playType)
  }
  output$downloadPlayer = downloadHandler(
    filename = paste("Player", Sys.Date(), ".png", sep=""),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  ##DownloadOff
  plotOff = function() {
    OffShotSeasonGraphTeam(Seasondata = Season2018, team = input$OffTeam, type = input$OffType)
  }
  output$downloadOff = downloadHandler(
    filename = paste("Offensive", Sys.Date(), ".png", sep=""),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotOff(), device = device)
    })
  #DownloadDeff
  
  plotDef = function() {
    DefShotSeasonGraphTeam(Seasondata = Season2018, team = input$DefTeam, type = input$DefType)
  }
  output$downloadDef = downloadHandler(
    filename = paste("Offensive", Sys.Date(), ".png", sep=""),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotDef(), device = device)
    })
})