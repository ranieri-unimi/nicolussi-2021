library(shiny)
library(shinythemes)
#library(tidyverse) # ggplot
library(rvest)
library(igraph)
library(visNetwork)

# const and utils

uri_site = 'https://www.unimi.it'
uri_prof = '/it/ugov/person/'
uri_exam = '/it/corsi/insegnamenti-dei-corsi-di-laurea/'
call_sleep= 0.15

pure = function(x) {
    return(tolower(gsub("[^a-zA-Z]", "", x)))
}

# breadth first search web scaper 
bfs_prof = function(root, max_depth, delete_self = 1){
    
    #link model
    links = data.frame(
        from = character(),
        to = character(),
        year = character(),
        course  = character(),
        title  = character(),
        hours = character(),
        stringsAsFactors=FALSE
    )
    
    walked = list() # visited link
    discovered = c(paste(uri_prof, root, sep='')) # already queued nodes
    queued = c(paste(uri_prof, root, sep=''))
    
    # pop a prof
    while(length(queued) > 0 && max_depth > 0) {
        id_prof = queued[[1]]
        queued = queued[-1]
        
        # extract his courses
        prof_webpage = read_html(paste(uri_site, id_prof, sep=''))
        Sys.sleep(call_sleep)
        prof_courses = prof_webpage %>% html_nodes(".accordion-cdl") %>% html_nodes("a")
        
        for(i in prof_courses[-1]){
            
            # pop a course
            id_link = i%>% html_attr('href')
            if(!(id_link %in% walked)){
                walked = c(walked, id_link)
                course_page = read_html(paste(uri_site, id_link, sep=''))
                Sys.sleep(call_sleep)
                
                # extract hours, coo-profs, year, name
                hours = course_page %>% html_nodes(".views-label-ore-totali") %>% html_text()
                data_course = tail(strsplit(id_link, "/")[[1]], 2)
                profs_list = course_page %>% html_nodes(".rubrica") %>% html_nodes("a") %>% html_attr('href')
            
                if(delete_self<length(profs_list)){
                    
                    # queue coo-profs
                    for (tmpj in 1:(length(profs_list)-delete_self)){
                        j = profs_list[tmpj]
                        if(!(j %in% discovered)) {
                            discovered = c(discovered, j)
                            queued = c(queued, j)
                        }
                        
                        # generate links
                        for(tmpk in (tmpj+delete_self):length(profs_list)){
                            k = profs_list[tmpk]
                            elem = c(
                                tail(strsplit(j, "/")[[1]], n=1),
                                tail(strsplit(k, "/")[[1]], n=1),
                                data_course[1],
                                data_course[2],
                                paste(data_course[2], data_course[1], sep ='-'),
                                hours
                            )
                            links = rbind(links, elem)
                        }
                    }
                }
            }
        }
        max_depth = max_depth - 1
        print('turn')
    }
    print('closed')
    return(links)
}

# vis.js stylish
paint_net = function(g){
    # gen vis
    vis = toVisNetworkData(g)
    names(vis$edges) =  c("from", "to", "year",'course', 'title', 'hours')
    
    # beauty nodes
    vis$nodes$shape = "dot"
    vis$nodes$shadow = T # Nodes will drop shadow
    vis$nodes$title = vis$nodes$id # Text on click
    vis$nodes$label = vis$nodes$id # Node label
    vis$nodes$size = vis$nodes$value # Node size
    vis$nodes$borderWidth = 2 # Node border width
    vis$nodes$color.background = 'gold' #c("slategrey", "tomato", "gold")[nodes$media.type]
    vis$nodes$color.border = "black"
    vis$nodes$color.highlight.background = "orange"
    vis$nodes$color.highlight.border = "darkred"
    
    # beauty edges
    vis$edges$width = 2
    vis$edges$color = "gray" # line color
    #vis$edges$arrows = "middle" # arrows: 'from', 'to', or 'middle'
    vis$edges$smooth = T # should the edges be curved?
    vis$edges$shadow = T # edge shadow
    vis$edges$title = vis$edges$course # Text on click
    vis$edges$label = vis$edges$year # Node label
    
    print('almost drawed')
    img = visNetwork(
        vis$nodes,
        vis$edges,
        height="100%",
        width="100%",
        background="#ffffff",
        main="UniMi Family Tree",
        submain="Discover your past collaboration whith other professors",
        footer= "Zoom, click and drag to reorder nodes. Hold links to see details"
    )
    print('drawed')
    return(img)
}

ui = fluidPage(
    theme = shinytheme("cerulean"),
    navbarPage(
        "UniMiLy - UniMi FamiLy-tree",
        tabPanel(
            "Home",
            sidebarPanel(
                tags$h4("Scrape a new professor:"),
                textInput("txtName", "Name:", "federica"),
                textInput("txtSurname", "Surname:", "nicolussi"),
                sliderInput(
                    "sldDeep",
                    "How deep shoud scraper search?",
                    min = 1,
                    max = 8,
                    value = 4
                ),
                tags$h6("1 deep ~ 10 sec"),
                actionButton("btnGo", "Go!")
            ),
            mainPanel(
                visNetworkOutput("network")
            )
        ),
        tabPanel(
            "Othe stats",
            titlePanel("Old Faithful Geyser Data"),
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        "bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30
                    )
                ),
                mainPanel(
                    plotOutput("distPlot")
                )
            )
        )
    )
) 

server = function(input, output) {
    
    output$network = renderVisNetwork({
        
        # first attempt in local data
        links = readRDS("localdata/cache.Rda")
        input$btnGo
        
        # parametered scraping
        if(input$btnGo > 0) {
            input$btnGo
            links = bfs_prof(paste(
                pure(isolate(input$txtName)),
                pure(isolate(input$txtSurname)),
                sep = "-"),
                isolate(input$sldDeep))
            
            # who knows
            print('saved')
            saveRDS(links, "localdata/cache.Rda")
        }
        
        # bypass with igraph for metrics
        g = graph_from_data_frame(links, directed = F)
        degg = degree(g)
        g = set_vertex_attr(g, 'value', index=names(degg), degg)
        paint_net(g)
    })
    
}

shinyApp(ui = ui, server = server)

#runGitHub('unimily', 'ranieri-unimi', 'main')
