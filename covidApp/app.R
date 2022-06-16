library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(readr)
library(dplyr)
library(ggplot2)
library(limma)
library(ggiraph)
library(data.table)
library(sf)
library(rnaturalearth)
library(stringr)
library(RColorBrewer)  
library(scales) 
library(DT)
library(plotly)

library(xts)
library(tseries)
library(zoo)
library(forecast)

# geographic map data
world<-ne_countries(50,returnclass='sf') %>% 
   select(geounit) %>% 
   rename(location=geounit) %>%
   mutate(location=replace(location,location=="United States of America","United States")) %>%
   mutate(location=replace(location,location=="Czech Republic","Czechia"))
   
# Read in dataset
covid_full = read_csv(file = "owid-covid-data-160222.csv", col_names = TRUE, na = c("", "n/a","NA", " "))

# List of countries to study
countries = c("United States", "India", "Brazil", "France", "United Kingdom", "Russia", 
              "Turkey", "Italy", "Germany", "Spain", "Argentina",
                "Iran", "Colombia", "Poland", 'Mexico', "Netherlands", 
              "Indonesia", "Ukraine", "South Africa", "Philippines", "Peru", "Belgium",
                "Czechia", "Japan", "Israel")

#all(countries %in% covid_full$location)				

covid<-covid_full %>% filter(location %in% countries) %>% filter(date >= as.Date("2020-03-15") & date <= as.Date("2022-02-14")) %>% setDT()

#covid[,.(paste(range(date),collapse="~")),location]
#covid[,.N,location]  ##702 days

	   
#####
ui <- dashboardPage(
  dashboardHeader(title = "Covid Shiny"),
  
  ################
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "tabPage1",icon=icon('map')),
	  menuItem("Transition", tabName = "t2", icon = icon("th")),
	  menuItem("Forecase", tabName = "t33", icon = icon("th"))
    ),width=120
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "tabPage1",
            
			tags$br(),
			
			fluidRow(column(6,
			dateInput('dateUntil','which day?',
			min='2020-03-15',max='2022-2-14',
			value='2021-12-31')),
			
			column(6,
			selectInput('varonmap','which variable?',
			choices=c('total_cases',
                      'total_cases_per_million',
                      'total_deaths',
                      'total_deaths_per_million',
                      'total_vaccinations',
                      'total_vaccinations_per_hundred'),
			selected='total_cases'))
			),
			
			fluidRow(column(10,withSpinner(girafeOutput('mapPlotout')),offset=1))  ##Output
      ),
	  
	      # Second tab content
      tabItem(tabName = "t2",
        fluidRow(column(4,selectInput('varonlimma','which variable?',
			                choices=c('stringency_index','reproduction_rate'),
			                selected='stringency_index')),
							
		
				 column(4, dateRangeInput("Period1", "Date Interval1",
                           start = "2020-03-15",
                           end   = "2020-12-31",min='2020-03-15',max='2022-2-14')),
				 
				 column(4, dateRangeInput("Period2", "Date Interval2",
                           start = "2021-01-01",
                           end   = "2021-12-31",min='2020-03-15',max='2022-2-14'))
		),
		
	    tags$h4('Top fit table'),
		fluidRow(column(10,withSpinner(DTOutput('limmaDT')),offset=1)), ##Output
		tags$h4('MA plot'),
		fluidRow(column(10,withSpinner(plotlyOutput('limmaMA')),offset=1))##Output
		
      ),
	  
	  # third tab content
      tabItem(tabName = "t33",
        fluidRow(column(4,selectInput('CountryPre','which country?',
			                choices=covid[,unique(location)],
			                selected='United States')),
							
		
				 column(4, dateInput("preStartDate", "From which day?",
                           value = "2020-03-15",
                           min='2020-03-15',max='2021-10-01')),
						   
				column(4, numericInput("preNday", "How many days to predict?",
                           value = 20,
                           min=3,max=100))
		),
		

		tags$h4('Forecast plot'),
		fluidRow(column(12,withSpinner(plotlyOutput('predictionPlot')),offset=0))##Output
		#tags$h4('Forecast table'),
		#fluidRow(column(10,withSpinner(DTOutput('predictionTable')),offset=1)) ##Output
		
      )
	  
	  
    ) ###tabItems
  ), skin='green'  ##body
)
   
	   

server<-function(input,output,session){
###using world map to show 
output$mapPlotout<-renderGirafe({
    colss<-c('location',  'date',input$varonmap)  
    map.data<-covid[,..colss][date==input$dateUntil]
	names(map.data)[3]<-'vv'
    
    map.data<-world %>% 
              left_join(map.data,by='location') %>%
              mutate(label=paste(location,input$varonmap,comma(vv),sep='<br>'))
    
    mapPlot<-ggplot()+
       geom_sf(aes(fill=vv),data=map.data,lwd=0.2)+
       geom_sf_interactive(aes(fill=vv,tooltip=label,data_id=location),
                          data=map.data %>% filter(location %in% countries),lwd=0.2)+
       scale_fill_gradientn(trans='log10',
       na.value="white",
       colours=c(rev(brewer.pal(5,'YlGn')),brewer.pal(5,'YlOrRd')))+
       theme_bw()+
       theme(legend.position='bottom',legend.key.width=unit(2,'cm'))+
       labs(title=str_to_title(str_replace(input$varonmap,"_"," ")),subtitle=input$dateUntil,fill=input$varonmap)
     
    girafe(ggobj = mapPlot,options = list(
        opts_hover(css = "stroke:#1279BF;cursor:pointer;"),
    	opts_toolbar(saveaspng=FALSE),
    	opts_selection(type='none')
      ))
})

###
#############################tab2 limma comparison
getLimmaData<-reactive({
      colss.limma<-c('location',  'date',c('stringency_index','reproduction_rate')) 
      limma.data<-covid[, ..colss.limma]
      setnames(limma.data,input$varonlimma,'limmav')
      limma.data<-limma.data[,.(location,date,limmav)]
      limma.data<-dcast(limma.data,location~date,value.var='limmav')
      limma.matrix<-as.matrix(limma.data[,!'location'])
      row.names(limma.matrix)<-limma.data$location
      
      limma.matrix1<-limma.matrix[,colnames(limma.matrix)>=input$Period1[1] &  colnames(limma.matrix)<=input$Period1[2]]
      limma.matrix2<-limma.matrix[,colnames(limma.matrix)>=input$Period2[1] &  colnames(limma.matrix)<=input$Period2[2]]
      colnames(limma.matrix1)<-paste('T1_',1:ncol(limma.matrix1))
      colnames(limma.matrix2)<-paste('T2_',1:ncol(limma.matrix2))
      time_index <- data.frame( time_index =  c(rep('T1',ncol(limma.matrix1)),rep('T2',ncol(limma.matrix2))) )
      limma.matrix<-cbind(limma.matrix1,limma.matrix2)
      
      								
      ## Calculating t-test for all countries
      design <- model.matrix(~time_index, data = time_index)
      fit <- lmFit(limma.matrix, design)
      fit <- eBayes(fit)
      fit
})



output$limmaDT<-renderDT({
   fit<-getLimmaData()
   tT <- topTable(fit, n = Inf) 
   DT::datatable(round(tT, 2),options=list(dom='tp'))
})


output$limmaMA<-renderPlotly({
   fit<-getLimmaData()
   
   ## Code for MA plot
   #df<- topTable(fit, number=nrow(fit), genelist=rownames(limma.matrix))
   df<- topTable(fit, number=nrow(fit), genelist=countries)
   
   paste(paste(input$Period1,collapse="~"),
         paste(input$Period2,collapse="~"),sep=" Vs. ")->inter.title
   
   p <- ggplot(df, aes(logFC, -log10(P.Value),  text=ID))+
       geom_point(aes(colour=-log10(P.Value)), alpha=1/3, size=1) +
       scale_colour_gradient(low="blue",high="red")+
       labs(x="log2 fold change",y="-log10 p-value",title=str_to_title(str_replace(input$varonlimma,"_"," ")),subtitle=inter.title)+
   	theme_bw()
   	
   plotly::ggplotly(p,tooltip=c('x','y','text')) %>%
     plotly::layout(title=paste0(str_to_title(str_replace(input$varonlimma,"_"," ")),
                                       '<br>',
                                       '<sup>',
                                       inter.title,
                                       '</sup>'))
   
})


###########for prediction
preForcast<-reactive({
   pre.data<-covid[location==input$CountryPre & date>=input$preStartDate,.(date,new_cases)]
   covid4 = zooreg(pre.data[,.(new_cases)], frequency = 1, start = pre.data[1,date])
   
   #ncovid <-diff(covid4)
   #ADF = adf.test(ncovid)
   #ADF
   fit = auto.arima(covid4)
   #fit
   fore.fit<-forecast(fit,input$preNday)
   
   fore.fitDT<-as.data.frame(fore.fit)
   setDT(fore.fitDT)
   list(covid4,fore.fitDT,pre.data)
})


output$predictionPlot<-renderPlotly({
   covid4<-preForcast()[[1]]
   fore.fitDT<-preForcast()[[2]]
   pre.data<-preForcast()[[3]]
   
   fore.days<-tail(seq(as.IDate(tail(index(covid4),1)),by='1 day',length.out=input$preNday+1),-1)
   fore.fitDT[,date:=fore.days]
   
   
   ppre<-ggplot(pre.data,aes(x=date,y=new_cases))+
       geom_line(group='a')+
       theme_bw()+
       scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m",expand=expansion(add=5))+
       theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))+
   	geom_smooth(aes(x=date,y=`Point Forecast`,ymin=`Lo 80`,ymax=`Hi 80`),
   	data=fore.fitDT,color='red',inherit.aes=F,stat='identity')+
   	geom_smooth(aes(x=date,y=`Point Forecast`,ymin=`Lo 95`,ymax=`Hi 95`),
   	data=fore.fitDT,color='red',inherit.aes=F,stat='identity')+
   	labs(x='')
   
   ggplotly(ppre)
})
}

shinyApp(ui,server)
