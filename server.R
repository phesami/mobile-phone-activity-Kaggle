#check that the libraries needed are avaialble
neededPackages <- c("visNetwork", "dtplyr", "geosphere", "ggplot2", "maps", "jsonlite", "shiny", "networkD3", "RColorBrewer", "rsconnect", "stringr", "dplyr", "data.table")
newPackages <- neededPackages[!(neededPackages %in% installed.packages()[,"Package"])]
if (length(newPackages)) install.packages(newPackages, dependencies = TRUE) 
library(dtplyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(geosphere)
library(maps)
library(jsonlite)
library(RColorBrewer)
library(rsconnect)
library(stringr)

server <- function(input, output) {

selectedInt_Dom <- reactive({
  input$int_dom
})  
selectedSamplesize <- reactive({
  input$sample_size
})  
selectedAnalysis <- reactive({
  input$analysis
})  
selectedLayout <- reactive({
  input$layout_type
})
selectedAggLevel <- reactive({
  input$agg_level
})
selectedDay <- reactive({
  input$day
})
selectedInt_Edge_Type <- reactive({
  input$int_edge_type
})
selectedDom_Edge_Type <- reactive({
  input$dom_edge_type
})
selectedNode_Label <- reactive({
  input$node_label
})
selectedEdge_Label <- reactive({
  input$edge_label
})
selectedHighlight_edge <- reactive({
  input$highlight_nearesr
})
selectedEdge_Width_Threshold <- reactive({
  input$edge_width_threshold
})
selectedHide_Node <- reactive({
  input$hide_node
})
selectedHide_Edge <- reactive({
  input$hide_edge
})
time_start <- reactive({
  strtoi(unlist(str_split(input$time_interval," "))[1])-1#+24)%%24 # coverting 1:24 to 0:23 to match the data format
})
time_end <- reactive({
  strtoi(unlist(str_split(input$time_interval," "))[2])-1
})
# for zooming in purpose
ranges <- reactiveValues(x = c(-1,1), y =c(-1,1))
input_dir = "../mobile-phone-activity/"
#extracting the country names based on country code from a 3rd source of data
country_codes = read.csv(paste0(input_dir,"countrycodes.csv"), sep = ';', stringsAsFactors = FALSE)[c("Country..en.", "Dialing.prefix")]
names(country_codes) = c("countryname", "countrycode")
country_codes = country_codes[!duplicated(country_codes$countrycode),]
# assiging country code 1 to US only
country_codes[[87,1]] = "USA"  
world_data <- map_data("world")[,c(1,2,5)]
milan_grids_data <- data.frame(fromJSON( paste0(input_dir,"milano-grid.geojson")))[,c(4,7)]
province_data <- map_data("italy")[c(1,2,5)]
province_data$region <- sapply(province_data$region, toupper)

domestic_mod <- reactive({
  domestic_mod_tmp = sample_frac(fread(paste0(input_dir,"mi-to-provinces-2013-11-0",1,".csv")), size = selectedSamplesize())
  domestic_mod_tmp$day = 1
  for (i in c(2:7)){
    domestic = sample_frac(fread(paste0(input_dir,"mi-to-provinces-2013-11-0",i,".csv")), size = selectedSamplesize())
    domestic$day = i
    domestic_mod_tmp = rbind(domestic, domestic_mod_tmp)
  }
  names(domestic_mod_tmp) = c("datetime","CellID", "provinceName", "callout", "callin", "day")
  domestic_mod_tmp[is.na(domestic_mod_tmp)] <- 0
  domestic_mod_tmp$hour = sapply(substr(domestic_mod_tmp$datetime, 12, 13), as.integer)
  domestic_mod_tmp
})

international_mod <- reactive({
  international_mod_tmp = sample_frac(fread(paste0(input_dir,"sms-call-internet-mi-2013-11-0",1,".csv")), size = selectedSamplesize())
  international_mod_tmp$day = 1
  for (i in c(2:7)){
    international = sample_frac(fread(paste0(input_dir,"sms-call-internet-mi-2013-11-0",i,".csv")), size = selectedSamplesize())
    international$day = i
    international_mod_tmp = rbind(international, international_mod_tmp)
  }
  international_mod_tmp$countrycode = sapply(international_mod_tmp$countrycode, as.character)
  international_mod_tmp = international_mod_tmp %>% merge(country_codes, by = 'countrycode')
  international_mod_tmp[is.na(international_mod_tmp)] <- 0
  international_mod_tmp$hour = sapply(substr(international_mod_tmp$datetime, 12, 13), as.integer)
  international_mod_tmp
})


  output$network_proxy_nodes <- renderVisNetwork({
    if (selectedInt_Dom()=="inter"){
      international_mod = international_mod()
      cell_aggregation_level = selectedAggLevel()
      international_mod$CellID = (international_mod$CellID%/%cell_aggregation_level)+1
      
      international_filtered_time = international_mod[international_mod$hour %in% c(time_start(): time_end()) & international_mod$day %in% selectedDay() ,]
      if (dim(international_filtered_time)[1]!=0){
        international_filtered = international_filtered_time[!(international_filtered_time$countryname=="Italy"),]
        nodes_international_country = data.frame(id = unique(international_filtered$countryname))
        nodes_international_country$group = 'international'
        world_data_filtered = world_data %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))
        nodes_international_country_latlong = nodes_international_country %>% merge(world_data_filtered, by.x = "id", by.y = "region", all.x = TRUE, all.y = FALSE)
        
        nodes_international_milan = data.frame(id = unique(international_filtered$CellID))
        nodes_international_milan$group = 'milan'
        milan_grids = data.frame(milan_grids_data$features.properties$cellId)
        names(milan_grids)= "id"
        milan_grids$long = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[1])
        milan_grids$lat = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[6])
        nodes_international_milan_latlong = nodes_international_milan %>% merge(milan_grids, by = "id")
        
        nodes_international = rbind(nodes_international_milan_latlong,nodes_international_country_latlong)
        if (selectedNode_Label()){
          nodes_international$label = nodes_international$id
        }
        edges_international = as.data.frame(international_filtered %>% group_by(CellID,countryname) %>% summarise(smsin = sum(smsin), smsout = sum(smsout), 
                                                                                                                  callin = sum(callin), callout = sum(callout), internet = sum(internet)))
        colnames(edges_international) = c('from','to','smsin','smsout', 'callin', 'callout','internet')
        edges_international$from = sapply(edges_international$from, as.character)
        
        edges_international_selected = edges_international[c('from', 'to', selectedInt_Edge_Type())]
        if (dim(edges_international_selected[selectedInt_Edge_Type()])[2]!=0){
          edge_threshold <- quantile(rowSums(edges_international_selected[selectedInt_Edge_Type()]), selectedEdge_Width_Threshold()/100)
          edges_international_selected$hidden = rowSums(edges_international_selected[selectedInt_Edge_Type()]<=edge_threshold)==length(selectedInt_Edge_Type())
          width = rowSums(edges_international_selected[selectedInt_Edge_Type()])
          edges_international_selected$width = 10*(width-min(width))/(max(width)-min(width))
          if (selectedEdge_Label()){
            edges_international_selected$title = round(edges_international_selected$width, digits = 4)
          }
          edges_international_selected$hoverWidth = edges_international_selected$width
          edges_international_selected$selectWidth = edges_international_selected$width 
          if (selectedAnalysis()=="node-link diagram"){
            if (selectedLayout()=="circ"){
              lo = "layout_in_circle"
              lo_name = "Circular"
            }else if (selectedLayout()=="fd"){
              lo = "layout_with_fr"
              lo_name = "Force-Directed"
            }
          if (selectedLayout()!="spring"){  
              visNetwork(nodes_international, edges_international_selected, main = paste0(lo_name, " Graph Representation of the International Phone Activity in Milan"), height = "1000px", width = "100%")%>% 
                visGroups(groupname = "international", color = "lightblue") %>% 
                visGroups(groupname = "milan", color = "red") %>%
                visLegend(width = 0.2, position = "right", main = "Group") %>%
                visOptions(highlightNearest = list( enabled = selectedHighlight_edge(), hover = selectedHighlight_edge()))%>%
                visPhysics(solver = "barnesHut")  %>%
                visIgraphLayout(layout = lo) %>% 
                visInteraction(tooltipDelay = 0, hover= selectedEdge_Label()) %>%
                visInteraction(hideEdgesOnDrag = selectedHide_Edge()) %>%
                visInteraction(hideNodesOnDrag = selectedHide_Node()) %>%
                visLayout(randomSeed = 12) 
          }else{
            visNetwork(nodes_international, edges_international_selected, main = paste0("Spring-Forced Graph Representation of the International Phone Activity in Milan"), height = "1000px", width = "100%")%>% 
              visGroups(groupname = "international", color = "lightblue") %>% 
              visGroups(groupname = "milan", color = "red") %>%
              visLegend(width = 0.2, position = "right", main = "Group") %>%
              visOptions(highlightNearest = list( enabled = selectedHighlight_edge(), hover = selectedHighlight_edge()))%>%
              visPhysics(solver = "barnesHut")  %>%
              visInteraction(hideEdgesOnDrag = selectedHide_Edge()) %>%
              visInteraction(hideNodesOnDrag = selectedHide_Node()) %>%
              visInteraction(tooltipDelay = 0, hover= selectedEdge_Label()) %>%
              visLayout(randomSeed = 12) 
          }
          }
        }
      }
    }else if (selectedInt_Dom()=="dom"){
      domestic_mod = domestic_mod()
      cell_aggregation_level = selectedAggLevel()
      domestic_mod$CellID = (domestic_mod$CellID%/%cell_aggregation_level)+1
      domestic_filtered_time = domestic_mod[domestic_mod$hour %in% c(time_start(): time_end()) & domestic_mod$day %in% selectedDay() ,]
      if (dim(domestic_filtered_time)[1]!=0){
        domestic_filtered = domestic_filtered_time
        nodes_domestic_province = data.frame(id = unique(domestic_filtered$provinceName))
        nodes_domestic_province$group = 'domestic'
        province_data_filtered = province_data %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))
        nodes_domestic_province_latlong = nodes_domestic_province %>% merge(province_data_filtered, by.x = "id", by.y = "region", all.x = TRUE, all.y = FALSE)
        
        nodes_domestic_milan = data.frame(id = unique(domestic_filtered$CellID))
        nodes_domestic_milan$group = 'milan'
        milan_grids = data.frame(milan_grids_data$features.properties$cellId)
        names(milan_grids)= "id"
        milan_grids$long = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[1])
        milan_grids$lat = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[6])
        nodes_domestic_milan_latlong = nodes_domestic_milan %>% merge(milan_grids, by = "id")
        
        nodes_domestic = rbind(nodes_domestic_milan_latlong,nodes_domestic_province_latlong)
        if (selectedNode_Label()){
          nodes_domestic$label = nodes_domestic$id
        }
        edges_domestic = as.data.frame(domestic_filtered %>% group_by(CellID,provinceName) %>% summarise(callin = sum(callin), callout = sum(callout)))
        colnames(edges_domestic) = c('from', 'to','callin', 'callout')
        edges_domestic$from = sapply(edges_domestic$from, as.character)
        edges_domestic_selected = edges_domestic[c('from', 'to', selectedDom_Edge_Type())]
        if (dim(edges_domestic_selected[selectedDom_Edge_Type()])[2]!=0){
          edge_threshold <- quantile(rowSums(edges_domestic_selected[selectedDom_Edge_Type()]), selectedEdge_Width_Threshold()/100)
          edges_domestic_selected$hidden = rowSums(edges_domestic_selected[selectedDom_Edge_Type()]<=edge_threshold)==length(selectedDom_Edge_Type())
          width = rowSums(edges_domestic_selected[selectedDom_Edge_Type()])
          edges_domestic_selected$width = 10*(width-min(width))/(max(width)-min(width))
          if (selectedEdge_Label()){
            edges_domestic_selected$title = round(edges_domestic_selected$width, digits = 4)
          }
          edges_domestic_selected$hoverWidth = edges_domestic_selected$width
          edges_domestic_selected$selectWidth = edges_domestic_selected$width 
          if (selectedAnalysis()=="node-link diagram"){
            if (selectedLayout()=="circ"){
              lo = "layout_in_circle"
              lo_name = "Circular"
            }else if (selectedLayout()=="fd"){
              lo = "layout_with_fr"
              lo_name = "Force-Directed"
            }
            if (selectedLayout()!="spring"){  
              visNetwork(nodes_domestic, edges_domestic_selected,height = "1000px", main = paste0(lo_name, " Graph Representation of the Domestic Phone Activity in Milan"),width = "100%")%>% 
                visGroups(groupname = "domestic", color = "lightblue") %>% 
                visGroups(groupname = "milan", color = "red") %>%
                visLegend(width = 0.2, position = "right", main = "Group") %>%
                visOptions(highlightNearest = list( enabled = selectedHighlight_edge(), hover = selectedHighlight_edge()))%>%
                visPhysics(solver = "barnesHut")  %>%
                visIgraphLayout(layout = lo) %>% 
                visInteraction(hideEdgesOnDrag = selectedHide_Edge()) %>%
                visInteraction(hideNodesOnDrag = selectedHide_Node()) %>%
                visInteraction(tooltipDelay = 0, hover= selectedEdge_Label()) %>%
                visLayout(randomSeed = 12) 
            }else{
              visNetwork(nodes_domestic, edges_domestic_selected,height = "1000px", main = paste0("Spring-Forced Graph Representation of the Domestic Phone Activity in Milan"),width = "100%")%>% 
                visGroups(groupname = "domestic", color = "lightblue") %>% 
                visGroups(groupname = "milan", color = "red") %>%
                visLegend(width = 0.2, position = "right", main = "Group") %>%
                visOptions(highlightNearest = list( enabled = selectedHighlight_edge(), hover = selectedHighlight_edge()))%>%
                visPhysics(solver = "barnesHut")  %>%
                visInteraction(hideEdgesOnDrag = selectedHide_Edge()) %>%
                visInteraction(hideNodesOnDrag = selectedHide_Node()) %>%
                visInteraction(tooltipDelay = 0, hover= selectedEdge_Label()) %>%
                visLayout(randomSeed = 12) 
            }
          }
        }
      }
    }
  })
  output$great_circles <- renderPlot({
    if (selectedInt_Dom()=="inter"){
      international_mod = international_mod()
      cell_aggregation_level = selectedAggLevel()
      international_mod$CellID = (international_mod$CellID%/%cell_aggregation_level)+1
      
      international_filtered_time = international_mod[international_mod$hour %in% c(time_start(): time_end()) & international_mod$day %in% selectedDay() ,]
      if (dim(international_filtered_time)[1]!=0){
        international_filtered = international_filtered_time[!(international_filtered_time$countryname=="Italy"),]
        nodes_international_country = data.frame(id = unique(international_filtered$countryname))
        nodes_international_country$group = 'international'
        world_data_filtered = world_data %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))
        nodes_international_country_latlong = nodes_international_country %>% merge(world_data_filtered, by.x = "id", by.y = "region", all.x = TRUE, all.y = FALSE)
        
        nodes_international_milan = data.frame(id = unique(international_filtered$CellID))
        nodes_international_milan$group = 'milan'
        milan_grids = data.frame(milan_grids_data$features.properties$cellId)
        names(milan_grids)= "id"
        milan_grids$long = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[1])
        milan_grids$lat = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[6])
        nodes_international_milan_latlong = nodes_international_milan %>% merge(milan_grids, by = "id")
        
        nodes_international = rbind(nodes_international_milan_latlong,nodes_international_country_latlong)
        if (selectedNode_Label()){
          nodes_international$label = nodes_international$id
        }
        #removing countries with no lat/long info
        nodes_international = nodes_international[complete.cases(nodes_international),]
        edges_international = as.data.frame(international_filtered %>% group_by(CellID,countryname) %>% summarise(smsin = sum(smsin), smsout = sum(smsout), 
                                                                                                                  callin = sum(callin), callout = sum(callout), internet = sum(internet)))
        colnames(edges_international) = c('from','to','smsin','smsout', 'callin', 'callout','internet')
        edges_international$from = sapply(edges_international$from, as.character)
        
        edges_international_selected = edges_international[c('from', 'to', selectedInt_Edge_Type())]
        if (dim(edges_international_selected[selectedInt_Edge_Type()])[2]!=0){
          edge_threshold <- quantile(rowSums(edges_international_selected[selectedInt_Edge_Type()]), selectedEdge_Width_Threshold()/100)
          edges_international_selected$hidden = rowSums(edges_international_selected[selectedInt_Edge_Type()]<=edge_threshold)==length(selectedInt_Edge_Type())
          width = rowSums(edges_international_selected[selectedInt_Edge_Type()])
          edges_international_selected$width = 10*(width-min(width))/(max(width)-min(width))
          if (selectedEdge_Label()){
            edges_international_selected$title = round(edges_international_selected$width, digits = 4)
          }
          edges_international_selected$hoverWidth = edges_international_selected$width
          edges_international_selected$selectWidth = edges_international_selected$width 
          if(selectedAnalysis()=="great circles (geo)"){
            colors <- brewer.pal(5,"Dark2")
            names(colors) <- c("smsin","smsout","callin","callout","internet")
            map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.1)
            title( main = "Great Circles Representation of the International Phone Activity in Milan")
            maxwidth <- max(edges_international_selected[selectedInt_Edge_Type()])
            edges_tmp = edges_international_selected[!rowSums(edges_international_selected[selectedInt_Edge_Type()]==0)==length(selectedInt_Edge_Type()),]
            for (j in 1:length(edges_tmp$from)) {
              edge_threshold <- quantile(rowSums(edges_international_selected[selectedInt_Edge_Type()]), selectedEdge_Width_Threshold()/100)
              if (edges_tmp[j,]$width>edge_threshold){
                air1 <- nodes_international[nodes_international$id == edges_tmp[j,]$from,]
                air2 <- nodes_international[nodes_international$id == edges_tmp[j,]$to,]
                if (dim(air1)[1]!=0 & dim(air2)[1]!=0){
                  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
                  if (length(selectedInt_Edge_Type())==1){
                    lines(inter, col=colors[selectedInt_Edge_Type()] , lwd=0.8)
                    legend("topright", legend = selectedInt_Edge_Type(), title = "Data Type", col = colors[selectedInt_Edge_Type()], lty=c(1))
                  }else{
                    lines(inter, col="black" , lwd=0.8)
                    legend("topright", legend = selectedInt_Edge_Type(), title = "Data Type", col = "black", lty=c(1))
                  }
                }
              }
            }
          }
        }
      }
    }else if (selectedInt_Dom()=="dom"){
      domestic_mod= domestic_mod()
      cell_aggregation_level = selectedAggLevel()
      domestic_mod$CellID = (domestic_mod$CellID%/%cell_aggregation_level)+1
      
      domestic_filtered_time = domestic_mod[domestic_mod$hour %in% c(time_start(): time_end()) & domestic_mod$day %in% selectedDay() ,]
      if (dim(domestic_filtered_time)[1]!=0){
        domestic_filtered = domestic_filtered_time
        nodes_domestic_province = data.frame(id = unique(domestic_filtered$provinceName))
        nodes_domestic_province$group = 'domestic'
        province_data_filtered = province_data %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))
        nodes_domestic_province_latlong = nodes_domestic_province %>% merge(province_data_filtered, by.x = "id", by.y = "region", all.x = TRUE, all.y = FALSE)
        
        nodes_domestic_milan = data.frame(id = unique(domestic_filtered$CellID))
        nodes_domestic_milan$group = 'milan'
        milan_grids = data.frame(milan_grids_data$features.properties$cellId)
        names(milan_grids)= "id"
        milan_grids$long = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[1])
        milan_grids$lat = sapply(milan_grids_data$features.geometry$coordinates, function(x) x[6])
        nodes_domestic_milan_latlong = nodes_domestic_milan %>% merge(milan_grids, by = "id")
        
        nodes_domestic = rbind(nodes_domestic_milan_latlong,nodes_domestic_province_latlong)
        if (selectedNode_Label()){
          nodes_domestic$label = nodes_domestic$id
        }
        
        #removing provinces with no lat/long info
        nodes_domestic = nodes_domestic[complete.cases(nodes_domestic),]
        
        edges_domestic = as.data.frame(domestic_filtered %>% group_by(CellID,provinceName) %>% summarise(callin = sum(callin), callout = sum(callout)))
        colnames(edges_domestic) = c('from', 'to','callin', 'callout')
        edges_domestic$from = sapply(edges_domestic$from, as.character)
        edges_domestic_selected = edges_domestic[c('from', 'to', selectedDom_Edge_Type())]
        if (dim(edges_domestic_selected[selectedDom_Edge_Type()])[2]!=0){
          edge_threshold <- quantile(rowSums(edges_domestic_selected[selectedDom_Edge_Type()]), selectedEdge_Width_Threshold()/100)
          edges_domestic_selected$hidden = rowSums(edges_domestic_selected[selectedDom_Edge_Type()]<=edge_threshold)==length(selectedDom_Edge_Type())
          width = rowSums(edges_domestic_selected[selectedDom_Edge_Type()])
          edges_domestic_selected$width = 10*(width-min(width))/(max(width)-min(width))
          if (selectedEdge_Label()){
            edges_domestic_selected$title = round(edges_domestic_selected$width, digits = 4)
          }
          edges_domestic_selected$hoverWidth = edges_domestic_selected$width
          edges_domestic_selected$selectWidth = edges_domestic_selected$width 
          
          if(selectedAnalysis()=="great circles (geo)"){
            colors <- brewer.pal(3,"Dark2")
            names(colors) <- c("callin","callout")
            map("italy", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.1)
            title( main = "Great Circles Representation of the Domestic Phone Activity in Milan")
            maxwidth <- max(edges_domestic_selected[selectedDom_Edge_Type()])
            edges_tmp = edges_domestic_selected[!rowSums(edges_domestic_selected[selectedDom_Edge_Type()]==0)==length(selectedDom_Edge_Type()),]
            for (j in 1:length(edges_tmp$from)) {
              edge_threshold <- quantile(rowSums(edges_domestic_selected[selectedDom_Edge_Type()]), selectedEdge_Width_Threshold()/100)
              if (edges_tmp[j,]$width>edge_threshold){
                air1 <- nodes_domestic[nodes_domestic$id == edges_tmp[j,]$from,]
                air2 <- nodes_domestic[nodes_domestic$id == edges_tmp[j,]$to,]
                if (dim(air1)[1]!=0 & dim(air2)[1]!=0){
                  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
                  if (length(selectedDom_Edge_Type())==1){
                    lines(inter, col=colors[selectedDom_Edge_Type()] , lwd=0.8)
                    legend("topright", legend = selectedDom_Edge_Type(), title = "Data Type", col = colors[selectedDom_Edge_Type()], lty=c(1))
                  }else{
                    lines(inter, col="black" , lwd=0.8)
                    legend("topright", legend = selectedDom_Edge_Type(), title = "Data Type", col = "black", lty=c(1))
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$graph_dblclick, {
    brush <- input$graph_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }else {
      ranges$x <- c(-1,1)
      ranges$y <- c(-1,1)
    }
  })
}