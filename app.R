library(shiny)
library(shinythemes)
library(rvest)
library(igraph)
library(visNetwork)
library(plotly)

# const and utils
uri_site = 'https://www.unimi.it'
uri_prof = '/it/ugov/person/'
uri_exam = '/it/corsi/insegnamenti-dei-corsi-di-laurea/'
call_sleep = 0.25

data_cache = readRDS("localdata/sample.Rda")
prof_history = distinct(data_cache[c('to','year','hours','title')])[c('to','year','hours')]
all_prof_list = distinct(prof_history['to'])[[1]]

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
    while(length(queued) > 0 && max_depth != 0) {
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
        
        # who knows
        print(max_depth)

    }
    names(links) =  c("from", "to", "year",'course', 'title', 'hours')
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
    vis$edges$smooth = T # should the edges be curved?
    vis$edges$shadow = T # edge shadow
    vis$edges$title = paste(vis$edges$course,'<br>' ,vis$edges$hours,'hours') # Text on click
    vis$edges$label = vis$edges$year # Node label
    
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
    return(img)
}

paint_viol = function(df, alfa, beta){
    fig = df %>% plot_ly(type = 'violin') 
    fig = fig %>% add_trace(
        x = ~hours[df$to == alfa],
        y = ~year[df$to == alfa],
        legendgroup = alfa,
        scalegroup = alfa,
        name = alfa,
        side = 'negative',
        box = list(
            visible = T
        ),
        meanline = list(
            visible = T
        ),
        color = I("orange")
    ) 
    fig = fig %>% add_trace(
        x = ~hours[df$to == beta],
        y = ~year[df$to == beta],
        legendgroup = beta,
        scalegroup = beta,
        name = beta,
        side = 'positive',
        box = list(
            visible = T
        ),
        meanline = list(
            visible = T
        ),
        color = I("purple")
    ) 
    
    fig = fig %>% layout(
        xaxis = list(
            title = "teaching hours of the course"  
        ),
        yaxis = list(
            title = "year of the course",
            zeroline = F
        ),
        violingap = 0,
        violingroupgap = 0,
        violinmode = 'overlay'
    )
    
    return(fig)
}

ui = fluidPage(
    theme = shinytheme("cerulean"),
    navbarPage(
        "UniMiLy - UniMi FamiLy-tree",
        tabPanel(
            "Explore family graph",
            sidebarPanel(
                tags$h4("Scrape a new professor:"),
                textInput("txtName", "Name:", "federica"),
                textInput("txtSurname", "Surname:", "nicolussi"),
                sliderInput(
                    "sldDeep",
                    "How deep shoud scraper search?",
                    min = 1,
                    max = 8,
                    value = 2
                ),
                tags$h6("1 notch of deepness ~ 10 sec"),
                actionButton("btnGo", "Go!")
            ),
            mainPanel(
                visNetworkOutput("network")
            )
        ),
        tabPanel(
            "Compare profile over years",
            titlePanel("Compare exam hours preference between professors"),
            sidebarLayout(
                sidebarPanel(
                    selectInput("sltAlfa", "Select a professor..", choices=all_prof_list),
                    selectInput("sltBeta", "..and another one:", choices = all_prof_list)
                    #choices=colnames(all_prof_list)
                ),
                mainPanel(
                    plotlyOutput("pltViolin")
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
        }
        
        # bypass with igraph for metrics
        g = graph_from_data_frame(links, directed = F)
        degg = degree(g)
        g = set_vertex_attr(g, 'value', index=names(degg), degg)
        paint_net(g)
    })
    
    output$pltViolin = renderPlotly({
        # first attempt with first value of list doubled
        paint_viol(data_cache, input$sltAlfa, input$sltBeta)
    })
}

shinyApp(ui = ui, server = server)