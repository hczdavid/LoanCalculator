
library(shiny)
library(DT)
library(tidyverse)
source("mortgage.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Loan Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("PV","Original Principle Value:", value = 2e5,width = '100%'),
            sliderInput("rate","Original Interest Rate (%):",0, 5, step = 0.05, value = 3,width = '100%'),
            selectInput("nterm", "Original Term (years)", choices = c(3, 5, 6, 15,30), selected = 15,width = '100%'),
            submitButton("Calculate"),
            
            tags$hr(),
            
            tags$h3("Tools"),
            
            tabsetPanel(
                tabPanel("Refinance",
                         numericInput("cPV","Current Principle Value:", value = 100000,width = '100%'),
                         sliderInput("newrate","New Interest Rate (%):",0, 5, step = 0.05, value = 2,width = '100%'),
                         numericInput("newterm", "New Term (years)", value = 10,width = '100%')),
                tabPanel("Lump Sum",
                         numericInput("cPV2","Current Principle Value:", value = 100000,width = '100%'),
                         numericInput("ls","Lump Sum Payment:", value = 10000,width = '100%'),
                         tags$hr(),
                         numericInput("lsmin","Min:", value = 10000,width = '50%'),
                         numericInput("lsmax","Max:", value = 50000,width = '50%')
                         
                         ),
                tabPanel("Add Monthly Payment",
                         numericInput("cPV3","Current Principle Value:", value = 100000,width = '100%'),
                         numericInput("addp","Additional Monthly Principle Payment:", value = 200,width = '100%'),
                         tags$hr(),
                         numericInput("addpmin","Min:", value = 100,width = '50%'),
                         numericInput("addpmax","Max:", value = 500,width = '50%'))
                
            
            ),
            submitButton("Apply"),
            tags$h6("Please email (lymnadia2016@gmail.com) us you have any question!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Original Data",
                         tags$hr(),
                         span(textOutput("monthlypay"), style= {"color: red;font-size:150%"}),
                         tags$hr(),
                         plotOutput("PvsI"),
                         tags$hr(),
                         DTOutput("monthlytable")),
                tabPanel("Refinance",
                         tags$hr(),
                         span(textOutput("refpay"), style= {"color: red;font-size:150%"}),
                         tags$hr(),
                         plotOutput("ratesave"),
                         tags$hr(),
                         DTOutput("reftable")
                         ),
                tabPanel("Lump Sum",
                         tags$hr(),
                         span(textOutput("lspay"), style= {"color: red;font-size:150%"}),
                         tags$hr(),
                         plotOutput("lssave"),
                         plotOutput("lssavetime")
                ),
                tabPanel("Add Monthly Payment",
                         tags$hr(),
                         span(textOutput("adp"), style= {"color: red;font-size:150%"}),
                         tags$hr(),
                         plotOutput("adpsave"),
                         plotOutput("adpsavetime")
                )
            )
    
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    PV    <- reactive(input$PV)
    nterm <- reactive({
        
       myterm <-  input$nterm %>% as.numeric()
       myterm <- myterm * 12
        })
    rate <- reactive({
        
        myrate <- input$rate %>% as.numeric()
        myrate <- myrate/100
         })
    
    cPV    <- reactive(input$cPV)
    newterm <- reactive({
        
        newmyterm <-  input$newterm %>% as.numeric()
        newmyterm <- newmyterm * 12
    })
    
    newrate <- reactive({
        
        newmyrate <- input$newrate %>% as.numeric()
        newmyrate <- newmyrate/100
    })
    
    
    mypayment <- reactive(cal_monthlypayment(pv = PV(), nterm = nterm(), rate = rate()/12))
    mydetail  <- reactive(cal_IntPrin(PV = PV(), payment = mypayment(), rate = rate()))
    refinance <- reactive({
        if(cPV()==0)return(NULL)
        cal_refinance(PV = cPV(),oldpayment = mypayment(),oldrate = rate(),newrate = newrate(),newterm = newterm())})
     
    
    output$monthlypay <- renderText({
        c(paste0("Your monthly payment is $", mypayment(),";"),
          paste0("Your total interest is $", mydetail()[["tot_int"]],"."))})
    
    
    output$PvsI <- renderPlot({
        plotdata <- mydetail()[["fulltable"]][c(1,2,4)]
        colnames(plotdata) <- c("Month", "Interest", "Principle")
        newplotdata <- plotdata %>% 
            gather(key="PI",value = "all", Principle,Interest)
        
        ggplot(data=newplotdata, aes(x=Month, y=all, fill=PI)) +
            geom_bar(stat="identity") + 
            ylab("Dollars")+
            labs(title="Principle and Interest Proportion in Your Monthly Payment")+
            geom_hline(yintercept = mypayment()/2, linetype = "dashed", color='yellow')+
            theme(legend.title = element_blank()) 
    })
                                                        
    
    
    output$monthlytable <- renderDataTable({
        mydetail()[["fulltable"]]
    })
    
    
    output$refpay <- renderText({
        
        c(paste0("Your saved interest is $", refinance()[[1]],"."),
          paste0("Your new monthly payment is $", refinance()[[2]],"."))})
    
    
    output$reftable <- renderDataTable({
        refinance()[["newInt_Prin"]][["fulltable"]]
    })
    
    output$ratesave <- renderPlot({
        
        raterange <- seq(0.01, rate(), by=0.0001)
        allref <- c()
        for(i in 1:length(raterange)){
           allref[i] <-  cal_refinance(PV = cPV(),oldpayment = mypayment(),oldrate = rate(),newrate = raterange[i],newterm = newterm())$Saveint
        }
        ratesavedata <- data.frame(rate = raterange, save = allref)
        
        ggplot(data=ratesavedata, aes(x=rate, y=save)) +
            geom_bar(stat="identity") + 
            ylab("Dollars")+
            labs(title="Rate vs. Save interest")+
            geom_hline(yintercept = 0, linetype = "dashed", color='red')+
            geom_vline(xintercept = raterange[which.min(abs(allref))] ,linetype = "dashed", color='red')+
            theme_classic() 
    })
    
    
    
    output$lspay <- renderText({
        
        req(input$cPV2)
        myls <- cal_onetimepay(PV = input$cPV2, payment = mypayment(), rate = rate(), onetimepay = input$ls)
        
        c(paste0("Your saved interest is $", myls[[1]],";"),
          paste0("Your saved time is ", myls[[2]],"years."))})
    
    output$lssave <- renderPlot({
        req(input$cPV2)
        lsmin <- input$lsmin
        lsmax <- input$lsmax
        
        lsseq <- seq(lsmin,lsmax,1000)
        ls_int  <- c()
        ls_time <- c()
        for(j in 1:length(lsseq)){
            
            myls <- cal_onetimepay(PV = input$cPV2, payment = mypayment(), rate = rate(), onetimepay = lsseq[j])
            ls_int[j] <- myls[[1]]
            ls_time[j] <- myls[[2]]
        }
        lsplot <- data.frame(LS = lsseq, saveint=ls_int, saveyear =  ls_time )
        
        ggplot(data=lsplot, aes(x=LS, y=saveint)) +
            geom_bar(stat="identity") + 
            ylab("Dollars")+
            xlab("Lump Sum")+
            labs(title="Lump Sum vs. Saved Interest")+
            theme_classic() 
        })
    
    output$lssavetime <- renderPlot({
        req(input$cPV2)
        lsmin <- input$lsmin
        lsmax <- input$lsmax
        
        lsseq <- seq(lsmin,lsmax,1000)
        ls_int  <- c()
        ls_time <- c()
        for(j in 1:length(lsseq)){
            
            myls <- cal_onetimepay(PV = input$cPV2, payment = mypayment(), rate = rate(), onetimepay = lsseq[j])
            ls_int[j] <- myls[[1]]
            ls_time[j] <- myls[[2]]
        }
        lsplot <- data.frame(LS = lsseq, saveint=ls_int, saveyear =  ls_time )
        
        ggplot(data=lsplot, aes(x=LS, y=saveyear)) +
            geom_bar(stat="identity") + 
            ylab("Time (years)")+
            xlab("Lump Sum")+
            labs(title="Lump Sum vs. Save year")+
            theme_classic() 
    })
    
    ############
    output$adp <- renderText({
        
        req(input$cPV3)
        myadp <- cal_incmonthpay(PV = input$cPV3, payment = mypayment(), rate = rate(), incmonthpay  = input$addp)
        
        c(paste0("Your saved interest is $", myadp[[1]],";"),
          paste0("Your saved time is ", myadp[[2]],"years."))})
    
    output$adpsave <- renderPlot({
        req(input$cPV3)
        addpmin <- input$addpmin
        addpmax <- input$addpmax
        
        adseq <- seq(addpmin,addpmax,50)
        adp_int  <- c()
        adp_time <- c()
        for(j in 1:length(adseq)){
            
            myadp <- cal_incmonthpay(PV = input$cPV3, payment = mypayment(), rate = rate(), incmonthpay = adseq[j])
            adp_int[j] <- myadp[[1]]
            adp_time[j] <- myadp[[2]]
        }
        adpplot <- data.frame(ADP = adseq, saveintadp=adp_int, saveyearadp =  adp_time )
        
        ggplot(data=adpplot, aes(x=ADP, y=saveintadp)) +
            geom_bar(stat="identity") + 
            ylab("Dollars")+
            xlab("Lump Sum")+
            labs(title="Lump Sum vs. Save Interest")+
            theme_classic() 
    })
    
    output$adpsavetime <- renderPlot({
        req(input$cPV3)
        addpmin <- input$addpmin
        addpmax <- input$addpmax
        
        adseq <- seq(addpmin,addpmax,50)
        adp_int  <- c()
        adp_time <- c()
        for(j in 1:length(adseq)){
            
            myadp <- cal_incmonthpay(PV = input$cPV3, payment = mypayment(), rate = rate(), incmonthpay = adseq[j])
            adp_int[j] <- myadp[[1]]
            adp_time[j] <- myadp[[2]]
        }
        adpplot <- data.frame(ADP = adseq, saveintadp=adp_int, saveyearadp =  adp_time )
        
        ggplot(data=adpplot, aes(x=ADP, y=saveyearadp)) +
            geom_bar(stat="identity") + 
            ylab("Time (year)")+
            xlab("Lump Sum")+
            labs(title="Lump Sum vs. Save Time")+
            theme_classic() 
    })
    
    
    
    
    
    #output$monthlypay <- renderText(nterm())
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
