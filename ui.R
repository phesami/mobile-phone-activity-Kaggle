library(visNetwork)
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Mobile Phone Activity in the City of Milan"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Mobile Phone Activity Visualization"),
      selectInput("analysis", label = "Choose the type of the analysis to display",choices = list("node-link diagram", "great circles (geo)")
                  ,selected = "node-link diagram"),
      radioButtons('int_dom', 'Choose the Type of the Data', c(Domestic='dom', International='inter'), selected = 'dom'),
      sliderInput("sample_size", label = "Sample Size", min = 0.01 , max = 1, step=0.01, value = 0.01),
      conditionalPanel(condition = "input.int_dom == 'inter'",
                       checkboxGroupInput("int_edge_type", label = "Choose the type of the international data (edge data)", choices = c("Incoming SMS" = "smsin","Outgoing SMS" = "smsout",
                                                                                                                           "Incoming Calls" = "callin", "Outgoing Calls" = "callout", 
                                                                                                                           "Internet Connections" = "internet"), selected = "smsin")),
      conditionalPanel(condition = "input.int_dom == 'dom'",
                       checkboxGroupInput("dom_edge_type", label = "Choose the type of the domestic data (edge data)", choices = c("Incoming Calls" = "callin", 
                                                                                                                           "Outgoing Calls" = "callout"), selected = "callin")),
      checkboxGroupInput("day", label = "Choose the day", choices = c("Monday"=7,"Tuesday"=1,"Wednesday"=2,"Thursday"=3,"Friday"=4,"Saturday"=5,"Sunday"=6), selected = 1),
      sliderInput("time_interval", label = "Time Interval (Hour)", min = 1 , max = 24, value = c(9,11), step = 1, animate = TRUE),
      sliderInput("agg_level", label = "Cell Aggregation Level", min = 1 , max = 10000, step=100, value = 1000),
      conditionalPanel(condition = "input.analysis == 'node-link diagram'",
                       radioButtons('layout_type', 'Graph Layout', c("Force Directed"='fd', "Circular"='circ', "Spring Forced" = 'spring')),
                       strong("Graph Annotaion"),
                       checkboxInput("node_label", "Node Label", FALSE),
                       checkboxInput("edge_label", "Edge Label", FALSE),
                       checkboxInput("highlight_nearesr", "Highlight Nearest Edge", FALSE),
                       checkboxInput("hide_node", "Hide Nodes on Drag", FALSE),
                       checkboxInput("hide_edge", "Hide Edges on Drag", FALSE)),
      sliderInput("edge_width_threshold", label = "Edge Width Threshold (percentile)", min = 0 , max = 100, step=1, value = 0)),
    mainPanel(
      conditionalPanel(condition = "input.analysis == 'node-link diagram'", visNetworkOutput("network_proxy_nodes", width = "100%", height = "800px")),
      conditionalPanel(condition = "input.analysis == 'great circles (geo)'", plotOutput(outputId = "great_circles", width = "100%", height = "800px",
                                                                            dblclick = "graph_dblclick",brush = brushOpts(id = "graph_brush",resetOnNew = TRUE)))
    ))
))
