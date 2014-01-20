




library(shiny)
library(rCharts)


shinyUI(pageWithSidebar(

  # Application title
  headerPanel("The Burden of Segregated Fees at UW"),

  sidebarPanel(

    conditionalPanel(
  	condition = "input.tabs1=='Seg fees past & future'",
			
#    numericInput("seg.growth", "Annual % growth of seg fees:", 3),    
      
      sliderInput("seg.growth", "Annual % growth of seg fees:", 
                min=1, max=15, value=5),
      
      numericInput("credits", "Number of academic credits per semester:", 8),
      
      numericInput("appt.pct", "TA/PA/RA appointment percentage, i.e. 50%, 33%, etc.:", 33),
    
      numericInput("inflation", "Annual inflation in percent:", 2),
    
      selectInput("end.year", "End year of graph:",
                  list(
                    "2014" = "2014",
                    "2015" = "2015",
                    "2016" = "2016",
                    "2017" = "2017",
                    "2018" = "2018",
                    "2019" = "2019",
                    "2020" = "2020", 
                    "2021" = "2021",
                    "2022" = "2022",
                    "2023" = "2023",
                    "2024" = "2024",
                    "2025" = "2025",
                    "2030" = "2030"
                       ), selected="2020"),
    
      checkboxInput("zero.y.lim", "Start graph axis at $0", FALSE),
    
      checkboxInput("tabledisplay", "Show table instead", FALSE),
    
      checkboxInput("seg.line", "Show seg fees", FALSE),
    
    
      helpText(HTML('<br> The base wage in 2013 is 0.5 * $29,492 * (appointment %).
0.5 is because this is for one semester, and $29,492 is from the <a href=\"http://www.ohr.wisc.edu/polproced/utg/SalRng.html#stuasst\">TA appointment salary</a>.
<br>
<br>

\"Full seg fee offset\" means that we do not pay seg fees now or ever. The \"No action\" line subtracts seg fees from our wages, now and into the future. The projections are in constant 2013 dollars, so the purchasing power of our wages declines with inflation, which the user can set at different rates.'))

    ), 
    
#     conditionalPanel(
#     condition = "input.tabs1=='Other stuff!'",
#     
#       sliderInput("obs", 
#                   "Number of observations:", 
#                   min = 1,
#                   max = 1000, 
#                   value = 200),
#       
#       selectInput("correlation", "Correlation:",
#                 list("High" = "0.85", 
#                      "Higher" = ".95", 
#                      "Ultra" = ".99"))
#     ),
    
        conditionalPanel(
    condition = "input.tabs1=='Fees eating our budgets'",
      
          
          
          numericInput("housing", "Monthly housing expenses", 899),
          
          numericInput("groceries", "Monthly grocery expenses", 249),
          
          numericInput("clothing", "Monthly clothing expenses", 65),
          
          numericInput("transportation", "Monthly transportation expenses", 282) , # NA
          
          numericInput("credits2", "Number of academic credits per semester:", 8),
          
          helpText(HTML('<br> This graph shows the numbers of months of various expenses that grad students could pay for if they did not have to pay seg fees. The default values ($899, $249, $65, $282, respectively) are based on Consumer Expenditure Survey responses of people with characteristics similar to graduate students at UW. See <a href=\"some link\">here</a> for methodological details. You can input your own monthly expenses to calculate your seg fee burden, too. Seg fees are based on credit load, and include both fall and spring semesters.'))

    
    
#       numericInput("obs", 
#                   "Number of observations:", 
#                   0),
#       
#       selectInput("correlation", "Correlation:",
#                 list("High" = "0.85", 
#                      "Higher" = ".95", 
#                      "Ultra" = ".99"))
#           
#          #, submitButton(text = "Apply Changes")
#          , actionButton("inputId", "label")
          
    ),
    
    conditionalPanel(
    condition = "input.tabs1=='UW losing to rival schools'",

      numericInput("appt.pct.peer", "TA/PA/RA appointment percentage, i.e. 50%, 33%, etc.:", 33),
      checkboxInput("PeerCOLA", "Account for cost-of-living differences", TRUE),
      checkboxInput("PeerFees", "Subtract fees from gross earnings", TRUE),
      
      selectInput("PeerGroup", "Comparison group:",
                list("Big 10" = "big10", 
                     "Faculty salary peers" = "salaryPeers")),
      
      checkboxInput("PeerBarplot", "Barplot instead", FALSE),
      
      helpText(HTML('<br>Low grad student pay means UW has a harder time attracting the best students when they have better financial offers from elsewhere.<br><br>The Big 10 are our sports rivals. The <a href=\"http://apir.wisc.edu/compensation/FacultySalaryComparison201213.pdf\">faculty salary peer group</a> was defined by the Governor\'s Commission on Faculty Compensation in 1984. The two groups overlap a lot. A few schools were excluded due to data unavailability. Fee totals assume a full course load and reflects any fee remission. '))
      
      
      
#      ,
#      conditionalPanel(
#        condition = "input.PeerGroup == 'big10'",
#          checkboxInput("PeerCOLA", "This is where each Big 10 school will go", TRUE)
#      ),
#      conditionalPanel(
#        condition = "input.PeerGroup == 'salaryPeers'",
#          checkboxInput("PeerCOLA", "This is where each salary peers school will go", TRUE)
#      )
      
    
)
      
      
      
    ),
  
    
    
    



mainPanel(
  
  tabsetPanel( id ="tabs1",
    
    tabPanel("Seg fees past & future",
  
     conditionalPanel(
        condition = "input.tabledisplay == false",
        plotOutput("take.home.plot") 
      ),
  
      conditionalPanel(
        condition = "input.tabledisplay == true",
        tableOutput("take.home.table")
      )
      
    ),
    
    tabPanel("UW losing to rival schools",
      plotOutput("competitivenessPlot")
 
    ),
    
    tabPanel("Fees eating our budgets",
      #plotOutput("distPlot")
      plotOutput("expensesPlot")
 
    )
    

#    ,   
#    tabPanel("Other stuff II",
#      showOutput("rchartsPlot", "Morris") # "polycharts" 
#    )
    
  )
  
)
  
#  mainPanel(
    
#    plotOutput("take.home.plot"),
#    tableOutput("take.home.table")
    
#    )
  
))
