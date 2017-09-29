# Developed by Mojdeh Shirazi-Manesh-22/06/2017
# To practice and learn RShiny
# Further Improvement: using ggplot2

##Load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(shinydashboard)

#-----------
table<-read.table("data.txt")
#-----------

table_socialClass<-table[,25:29]
#Considering the max value of social class columns as the customer social class.
socClass<-colnames(table_socialClass)[apply(table_socialClass,1,which.max)]
table<-mutate(table,SocialClass=socClass)
#-----------
table_education<-table[,16:18]
#Considering the column with max value among education related columns as the education level.
education<-colnames(table_education)[apply(table_education,1,which.max)]
table<-mutate(table,EducLevel=education)
#-----------

# values of Customer main type (L2), social class, education level and income level
cusMaintype<-c("Successful hedonists","Driven Growers","Average Family","Career Loners","Living well",
               "Cruising Seniors","Retired and Religeous","Family with grown ups","Conservative families","Farmers")
cusSocialCls<-c("Class A","Class B1","Class B2","Class C","Class D")
cusEducLvl<-c("High level","Medium level","Lower level")
cusIncLvl<-c("L0","L1","L2","L3","L4","L5","L6","L7","L8","L9")
#--------------------------------------------------------------------------

ui<-fluidPage(
  titlePanel("Insurance Policies"),
  
  sidebarPanel(
    
    selectInput("policy", label = h3("Select an Insurance Policy"), 
                choices = list("Car"="V68",
                               "Van"="V69",
                               "Motorcycle/Scooter"="V70",
                               "Lorry"="V71",
                               "Trailer"="V72",
                               "Tractor"="V73",
                               "Agricultural Machines"="V74",
                               "Moped"="V75",
                               "Life Insurance"="V76",
                               "Private Accident insurance"="V77",
                               "Family Accidents"="V78",
                               "Disability"="V79",
                               "Fire"="V80",
                               "Surfboard"="V81",
                               "Boat"="V82",
                               "Bicycle"="V83",
                               "Property Insurance"="V84",
                               "Social Security"="V85",
                               "Mobile Home"="V86"
                               
                               
                )
    )),
  
  
  mainPanel( 
    fluidRow(
      box(plotOutput("bar_cusmain", height = 400)),
      box(plotOutput( "bar_socclass", height = 400)),
      box(plotOutput("bar_education", height = 400)),
      box(plotOutput("scatter_income", height = 400)))
  )
)  
############################################################################

server<-function(input, output) {
  
  # based on customer main type
  filteredPolicy_cusmain<-reactive({
    table%>%
      filter(table[input$policy]>0)%>%
      group_by(.,V5)%>%
      summarise(.,n())%>%
      rename(.,Nu_cusmain=`n()`) 
  })
  
  output$bar_cusmain<-renderPlot({
    par(mar=c(4,11,4,4)) 
    a<-TRUE
    namesarg<-cusMaintype
    u<-unique(filteredPolicy_cusmain()$V5)
    if (length(u)<10){ 
      a<-(1:10 %in% u)
      
      namesarg<-namesarg [which(a==TRUE)]
    }
    
    barplot(filteredPolicy_cusmain()$Nu_cusmain,main="Customer Main Type",horiz=TRUE,
            names.arg=namesarg,
            las=1,
            xlab="# of customers",  
            col="deepskyblue3"
    )}) 
  #----------------------------------------------------------------------------
  # based on social classes
  filteredPolicy_socclass<-reactive({
    table%>%
      filter(table[input$policy]>0)%>%
      group_by(.,SocialClass)%>%
      summarise(.,n())%>%
      rename(.,Nu_socclass=`n()`)
  })
  
  
  output$bar_socclass<-renderPlot({
    a<-TRUE
    namesarg<-cusSocialCls
    u<-unique(filteredPolicy_socclass()$SocialClass)
    if (length(u)<5){ 
      a<-(c("V25","V26","V27","V28","V29") %in% u)
      
      namesarg<-namesarg [which(a==TRUE)]
    }
    
    slices <- filteredPolicy_socclass()$Nu_socclass/sum(filteredPolicy_socclass()$Nu_socclass)
    lbls <- namesarg
    pie(slices, labels = lbls, main="Customer Social Class")
  })
  #----------------------------------------------------------------------------
  # based on education level
  filteredPolicy_educ<-reactive({
    table%>%
      filter(table[input$policy]>0)%>%
      group_by(.,EducLevel)%>%
      summarise(.,n())%>%
      rename(.,Nu_educ=`n()`)
  })
  
  output$bar_education<-renderPlot({
    a<-TRUE
    namesarg<-cusEducLvl
    u<-unique(filteredPolicy_educ()$EducLevel)
    if (length(u)<3){ 
      a<-(c("V16","V17","V18") %in% u)
      
      namesarg<-namesarg [which(a==TRUE)]
    }
    
    barplot(filteredPolicy_educ()$Nu_educ,main="Customer Education level",
            names.arg=namesarg,
            las=1,
            ylab="# of customers" , 
            col="palevioletred2"
    )}) 
  
  #-----------------------------------------------------------------------------
  # based on average income level
  filteredPolicy_inc<-reactive({
    table%>%
      filter(table[input$policy]>0)%>%
      group_by(.,V42)%>%
      summarise(.,n())%>%
      rename(.,Nu_inc=`n()`)
  })
  output$scatter_income<-renderPlot({
    par(mar=c(4,11,4,4)) 
    a<-TRUE
    namesarg<-cusIncLvl
    # cat(namesarg)
    u<-unique(filteredPolicy_inc()$V42)
    if (length(u)<10){ 
      a<-(0:9 %in% u)
      
      namesarg<-namesarg [which(a==TRUE)]
    }
    y<-filteredPolicy_inc()$Nu_inc
    l<-length(namesarg)

    plot(0:(l-1),y,xaxt = 'n',type="b", xlab="Income Level", lty=2, pch=19, col="blue", ylab="# of Customers",
         main="Customer Income Level" )
    axis(side=1,at=0:(l-1),labels=namesarg)
    
  }) 
}

shinyApp(ui=ui,server=server)
