library(shiny)
library(tidyverse)
library(maps)
library(lubridate)
#read data
df<-read.csv("PublicAssistanceFundedProjectsDetails.csv")
df$declarationDate<-as.Date(df$declarationDate)
df$year <- substr(df$declarationDate, 1, 4)
df<-df%>%filter(year>2009,year<2018)
total_declaration<-df%>%group_by(state,year)%>%summarise(total_declaration
                                                    =n())
damage_hurricane<-df%>%filter(incidentType=="Hurricane")
county<-map_data("county")
damage_hurricane<-damage_hurricane%>%group_by(state,county,year)%>%summarise(total_project=sum(projectAmount),
                                                                    total_federal=sum(federalShareObligated),
                                                                    total_total=sum(totalObligated))
damage_hurricane$county<-tolower(damage_hurricane$county)
damage_hurricane$state<-tolower(damage_hurricane$state)
colnames(damage_hurricane)[1]<-"region"
colnames(damage_hurricane)[2]<-"subregion"
county%>%right_join(damage_hurricane,by=c("region","subregion"))->total_map
total_map$project_legend<-total_map$total_project%>%cut(breaks=c(1.356e+03,1.781e+05,1.012e+06,5.603e+06,9.801e+09),
                                                        include.lowest=T)
#define ui
ui <- fluidPage(
    mainPanel(
        headerPanel("Damage by hurricane data"),
        tabsetPanel(
            tabPanel("Declaration by each state",
                     br(),
                     sidebarPanel(
                         selectInput('stateinput','State',
                                     choices=sort(unique(df$state))),
                         selectInput("yearinput","Year",c("ALL",unique(df$year)))
                     ),
                     mainPanel(
                         h5('number of declaration for each state'),
                         plotOutput('plot_state'),
                         h5('Table for damage for each state in particular year'),
                         dataTableOutput('table_state')
                     )),
            tabPanel("Map for hurricane damage",
                     br(),
                     sidebarPanel(
                         selectInput("region","State",choices=sort(unique(total_map$region)))
                     ),
                     mainPanel(
                         h5('Map for each state'),
                         plotOutput('mapstate'),
                     ))
        )
    )
)
            
                     

# Define server logic required to draw plot and table
server <- function(input, output) {
    output$plot_state<-renderPlot({
        hurricane_state<-total_declaration%>%filter(state==input$stateinput)
        total<-ggplot(data=hurricane_state)+geom_bar(aes(x=year,weight=total_declaration,fill=year))+coord_flip()
        total

})
    output$table_state<-renderDataTable({
        hurricane_filtered<-df%>%filter(state==input$stateinput,
                  year==input$yearinput)
        print(hurricane_filtered)
        
})
    output$mapstate<-renderPlot({
        map<-total_map%>%filter(region==input$region)
        map_state<-ggplot()+
            geom_polygon(data=county,aes(long,lat,group=group),colour="black",fill="white")+
            geom_polygon(data=map,aes(long,lat,group=group,fill=project_legend))+
            scale_fill_brewer(palette="Reds")+
            ggtitle("Total project amount by county")+
            theme(plot.title=element_text(hjust=0.5))
        map_state
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
