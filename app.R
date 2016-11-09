library(BTYD)
library(shiny)
library(dplyr)
library(BTYDplus)
library(data.table)
library(ggplot2)
library(tidyr)

cdnow <- data.table(read.csv(file = 'cdnow_elog.csv', stringsAsFactors = F)) # hello
cdnow <- cdnow[, date := as.Date(date, '%Y-%m-%d')]
cdnow <- cdnow[, first := min(date), by='cust']
cdnow <- arrange(cdnow,date)

grocery <- data.table(read.csv(file = 'grocery-elog.csv', stringsAsFactors = F))
grocery <- grocery[, date := as.Date(date, '%Y-%m-%d')]
grocery$first <- as.Date(grocery$first)
grocery <- arrange(grocery,date)


donations <- data.table(read.csv('donations-elog.csv', stringsAsFactors = F))
donations[, date := as.Date(date, format='%m/%d/%Y')]
donations[, first := min(date), by='cust']
donations <- arrange(donations,date)



ui <- fluidPage(
    headerPanel('BTYDplus package'),
    sidebarPanel(
        helpText("Options to choose the dataset, the model and calibration period in %(optional)."),
        selectInput('Data','Select the dataset',choices = c('CDNOW','Donations','Grocery')),
        selectInput('Model','Select the model',choices = c('ParetoNBD', 'BG/NBD', 'MBG/NBD', 'MBG/CNBD-k')),
        sliderInput("cal_per",value = 0.5,max = 0.90,min = 0.1,label = "% of total data for calibration period")
    ),
    mainPanel(tabsetPanel(
        tabPanel(title = 'Descriptive Summary Statistic',
                 h4('Descriptive Statistics'),
                 tableOutput('descr_stats'),

                 h4('Plot Of Timing Patterns'),
                 plotOutput('tim_pat')),
        tabPanel(title = 'Cohort Level Analysis',
                 h4('Estimated Parameters '),
                 tableOutput('est_param'),

                 h4('Tracking Incremental & Cumulative Weekly'),
                 plotOutput('incr_weekly'),
                 plotOutput('cum_weekly'),

                 h4('Frequency and Recency vs Holdout Transactions'),
                 plotOutput('freq_trans'),
                 plotOutput('rec_trans'),



                 h4('distribution of P(alive)'),
                 plotOutput('p_alive')),

        tabPanel(title = 'Customer Level Analysis',
                 h4('Mean Absolute Error & Bias'),
                 tableOutput('stats'),

                 h4('Sufficient Statistic Matrix'),
                 tableOutput('suf_mat'))
    )
    )

)

server <- function(input,output){

    data <- reactive({
        cbs_cdnow <- elog2cbs(cdnow, units = 'week', T.cal = unique(cdnow$date)[length(unique(cdnow$date))*input$cal_per])
        cbs_cdnow <- as.data.frame(cbs_cdnow)

        cbs_grocery <- elog2cbs(grocery, T.cal = unique(grocery$date)[length(unique(grocery$date))*input$cal_per], T.tot = as.Date('2007-12-31'))
        cbs_grocery <- as.data.frame(cbs_grocery)
        #Take the length of the unique dates in dataset and * on the number from 0.1 till 0.9 => new date for calibration period.
        cbs_donations <- elog2cbs(donations, units='week', T.cal = unique(donations$date)[length(unique(donations$date))*input$cal_per], T.tot = as.Date('2006-07-01'))

        data <- list(cbs_cdnow,cbs_grocery,cbs_donations)
    })

    output$descr_stats <- renderTable({
        cbs_cdnow <- data()[[1]]
        cbs_grocery <- data()[[2]]
        cbs_donations <- data()[[3]]
        #a)dataset
        dataset <- c("CDs","Grocery","Donations")
        #b)Cohort size
        coh.size <- c(nrow(cbs_cdnow),nrow(cbs_grocery),nrow(cbs_donations))
        #c)Period length in weeks(Calibration)
        per.length.cal <- c(round(max(cbs_cdnow$T.cal)),round(max(cbs_grocery$T.cal)),round(max(cbs_donations$T.cal)))
        #d)Period length in weeks(holdout)
        per.length.hold <- c(round(cbs_cdnow$T.star[1]),round(cbs_grocery$T.star[1]),round(cbs_donations$T.star[1]))
        #e)share of inactive customer(calibr)
        sh.in.cust.cal <- c(round(sum(cbs_cdnow$x == 0)/nrow(cbs_cdnow),2),round(sum(cbs_grocery$x == 0)/nrow(cbs_grocery),2),
                            round(sum(cbs_donations$x == 0)/nrow(cbs_donations),2))
        #f)share of inactive customer(holdout)
        sh.in.cust.hold <- c(round(sum(cbs_cdnow$x.star == 0)/nrow(cbs_cdnow),2),round(sum(cbs_grocery$x.star == 0)/nrow(cbs_grocery),2),
                             round(sum(cbs_donations$x.star == 0)/nrow(cbs_donations),2))
        #g)share of customers with 4 or > transactions(calibration)
        sh.cust.4.plus.cal <- c(round((sum(cbs_cdnow$x >=4)/nrow(cbs_cdnow)),2),round(sum((cbs_grocery$x >=4)/nrow(cbs_grocery)),2),round(sum((cbs_donations$x >=4)/nrow(cbs_donations)),2))
        #h)share of customers with 4 or > transactions(holdout)
        sh.cust.4.plus.hold <- c(round((sum(cbs_cdnow$x.star >=4)/nrow(cbs_cdnow)),2),round(sum((cbs_grocery$x.star >=4)/nrow(cbs_grocery)),2),round(sum((cbs_donations$x.star >=4)/nrow(cbs_donations)),2))
        #i)mean 0f number of purchases(calibr)
        num.purch.cal <- c(round(mean(cbs_cdnow$x),2),round(mean(cbs_grocery$x),2),round(mean(cbs_donations$x),2))
        #j)mean 0f number of purchases(holdout)
        num.purch.hold <- c(round(mean(cbs_cdnow$x.star),2),round(mean(cbs_grocery$x.star),2),round(mean(cbs_donations$x.star),2))
        #k)mean 0f number of purchases active cust(calibr)
        num.purch.cal.act <- c(round(mean(cbs_cdnow$x[cbs_cdnow$x>0]),2),round(mean(cbs_grocery$x[cbs_grocery$x>0]),2),round(mean(cbs_donations$x[cbs_donations$x>0]),2))
        #l)mean 0f number of purchases active cust(hold)
        num.purch.hold.act <- c(round(mean(cbs_cdnow$x.star[cbs_cdnow$x.star>0]),2),round(mean(cbs_grocery$x.star[cbs_grocery$x.star>0]),2),round(mean(cbs_donations$x.star[cbs_donations$x.star>0]),2))
        #m)wheat
        wheat <- c(round(estimateRegularity(cdnow, method = "wheat",plot = FALSE),2),round(estimateRegularity(grocery, method = "wheat",plot = FALSE),2),round(estimateRegularity(donations, method = "wheat",plot = FALSE),2))
        #Table:
        des_stat <- c()
        des_stat<- cbind(des_stat ,dataset,coh.size,per.length.cal,per.length.hold,sh.in.cust.cal,sh.in.cust.hold,
                         sh.cust.4.plus.cal,sh.cust.4.plus.hold,num.purch.cal,num.purch.hold,
                         num.purch.cal.act,num.purch.hold.act,wheat)
        des_stat <- as.data.frame(des_stat)
        des_stat <- data.frame(lapply(des_stat, as.character), stringsAsFactors=FALSE)
        d <- cbind(des_stat[1:2], do.call(cbind, lapply(c(3,5,7,9,11), function(i) do.call(paste, c(des_stat[i:(i+1)], sep="/")))))
        d <- cbind(d,wheat)
        d <- as.data.frame(d)

        colnames(d) <- c("Dataset","Cohort size","Period length (Calibration/Holdout)","Share of inactive customer(cal/hol)",
                         "Share of customers with 4 or > transactions(cal/hol)","Mean of № of purchases(cal/hol)",
                         "Mean of № of purchases active cust(cal/hol)",'wheat')
        d
    })

    output$tim_pat <- renderPlot({
        if(input$Data == "CDNOW"){
            plotTimingPatterns(cdnow, n = 30, T.cal = unique(cdnow$date)[length(unique(cdnow$date))*input$cal_per],
                               headers = c("Calibration Period", "Holdout Period"), title = "")
        }else if(input$Data == "Grocery"){
            plotTimingPatterns(grocery, n = 30, T.cal = unique(grocery$date)[length(unique(cdnow$date))*input$cal_per],
                               headers = c("Calibration Period", "Holdout Period"), title = "")

        }else if(input$Data == "Donations"){
            plotTimingPatterns(donations, n = 30, T.cal = unique(donations$date)[length(unique(cdnow$date))*input$cal_per],
                               headers = c("Calibration Period", "Holdout Period"), title = "")

        }

    })

    output$p_alive <- renderPlot({
        if(input$Data == "CDNOW"){
            if(input$Model == "ParetoNBD"){
                cbs_cdnow <- data()[[1]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
                palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                plot <- ggplot(as.data.frame(palive.pnbd),aes(x=palive.pnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "BG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
                palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                plot <- ggplot(as.data.frame(palive.bgnbd),aes(x=palive.bgnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "MBG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)
                palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                plot <- ggplot(as.data.frame(palive.mbgnbd),aes(x=palive.mbgnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "MBG/CNBD-k"){
                cbs_cdnow <- data()[[1]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)
                palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                plot <- ggplot(as.data.frame(palive.mbgcnbd),aes(x=palive.mbgcnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }
        }else if(input$Data == "Grocery"){
            if(input$Model == "ParetoNBD"){
                cbs_grocery <- data()[[2]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
                palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                plot <- ggplot(as.data.frame(palive.pnbd),aes(x=palive.pnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "BG/NBD"){
                cbs_grocery <- data()[[2]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
                palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                plot <- ggplot(as.data.frame(palive.bgnbd),aes(x=palive.bgnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "MBG/NBD"){
                cbs_grocery <- data()[[2]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)
                palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                plot <- ggplot(as.data.frame(palive.mbgnbd),aes(x=palive.mbgnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "MBG/CNBD-k"){
                cbs_grocery <- data()[[2]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)
                palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                plot <- ggplot(as.data.frame(palive.mbgcnbd),aes(x=palive.mbgcnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }
        }else if(input$Data == "Donations"){
            if(input$Model == "ParetoNBD"){
                cbs_donations <- data()[[3]]
                ##works very slow
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
                palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                plot <- ggplot(as.data.frame(palive.pnbd),aes(x=palive.pnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "BG/NBD"){
                cbs_donations <- data()[[3]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
                palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                plot <- ggplot(as.data.frame(palive.bgnbd),aes(x=palive.bgnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "MBG/NBD"){
                cbs_donations <- data()[[3]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)
                palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                plot <- ggplot(as.data.frame(palive.mbgnbd),aes(x=palive.mbgnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }else if(input$Model == "MBG/CNBD-k"){
                cbs_donations <- data()[[3]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)
                palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                plot <- ggplot(as.data.frame(palive.mbgcnbd),aes(x=palive.mbgcnbd))+
                    geom_histogram(binwidth =0.01,colour="black",fill="orange")+
                    ylab("Number of Customers")+
                    xlab("Probability Customer is 'Live'")+
                    theme_minimal()
                plot
            }
        }
    })

    output$freq_trans <-renderPlot({
        if(input$Data == "CDNOW"){
            if(input$Model == "ParetoNBD"){
                cbs_cdnow <- data()[[1]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
                pnbd.PlotFreqVsConditionalExpectedFrequency(params.pnbd,cbs_cdnow$T.star[1],cbs_cdnow,cbs_cdnow$x.star,censor =7)

            }else if(input$Model == "BG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
                bgnbd.PlotFreqVsConditionalExpectedFrequency(params.bgnbd,cbs_cdnow$T.star[1],cbs_cdnow,cbs_cdnow$x.star,censor =7)
            }else if(input$Model == "MBG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_cdnow <- data()[[1]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)

            }
        }else if(input$Data == "Grocery"){
            if(input$Model == "ParetoNBD"){
                cbs_grocery <- data()[[2]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
                pnbd.PlotFreqVsConditionalExpectedFrequency(params.pnbd,cbs_grocery$T.star[1],cbs_grocery,cbs_grocery$x.star,censor =7)
            }else if(input$Model == "BG/NBD"){
                cbs_grocery <- data()[[2]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
                bgnbd.PlotFreqVsConditionalExpectedFrequency(params.bgnbd,cbs_grocery$T.star[1],cbs_grocery,cbs_grocery$x.star,censor =7)
            }else if(input$Model == "MBG/NBD"){
                cbs_grocery <- data()[[2]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_grocery <- data()[[2]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)

            }
        }else if(input$Data == "Donations"){
            if(input$Model == "ParetoNBD"){
                cbs_donations <- data()[[3]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
                pnbd.PlotFreqVsConditionalExpectedFrequency(params.pnbd,cbs_donations$T.star[1],cbs_donations,cbs_donations$x.star,censor =7)
            }else if(input$Model == "BG/NBD"){
                cbs_donations <- data()[[3]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
                bgnbd.PlotFreqVsConditionalExpectedFrequency(params.bgnbd,cbs_donations$T.star[1],cbs_donations,cbs_donations$x.star,censor =7)
            }else if(input$Model == "MBG/NBD"){
                cbs_donations <- data()[[3]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_donations <- data()[[3]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)

            }
        }
    })


    output$incr_weekly <-renderPlot({
        if(input$Data == "CDNOW"){
            if(input$Model == "ParetoNBD"){
                cbs_cdnow <- data()[[1]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
                pnbd.PlotTrackingInc(params.pnbd,cbs_cdnow$T.cal,T.tot = 78,actual.inc.tracking.data =elog2inc(cdnow))


            }else if(input$Model == "BG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
                bgnbd.PlotTrackingInc(params.bgnbd,cbs_cdnow$T.cal,T.tot = 78,actual.inc.tracking.data =elog2inc(cdnow))


            }else if(input$Model == "MBG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)
                mbgcnbd.PlotTrackingInc(params.mbgnbd,cbs_cdnow$T.cal,T.tot = 78,actual.inc.tracking.data =elog2inc(cdnow))


            }else if(input$Model == "MBG/CNBD-k"){
                cbs_cdnow <- data()[[1]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)
                mbgcnbd.PlotTrackingInc(params.mbgcnbd,cbs_cdnow$T.cal,T.tot = 78,actual.inc.tracking.data =elog2inc(cdnow))

            }
        }else if(input$Data == "Grocery"){
            if(input$Model == "ParetoNBD"){
                cbs_grocery <- data()[[2]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
                pnbd.PlotTrackingInc(params.pnbd,cbs_grocery$T.cal,T.tot = 104,actual.inc.tracking.data =elog2inc(grocery))


            }else if(input$Model == "BG/NBD"){
                cbs_grocery <- data()[[2]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
                bgnbd.PlotTrackingInc(params.bgnbd,cbs_grocery$T.cal,T.tot = 104,actual.inc.tracking.data =elog2inc(grocery))


            }else if(input$Model == "MBG/NBD"){
                cbs_grocery <- data()[[2]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)
                mbgcnbd.PlotTrackingInc(params.mbgnbd,cbs_grocery$T.cal,T.tot = 104,actual.inc.tracking.data =elog2inc(grocery))

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_grocery <- data()[[2]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)
                mbgcnbd.PlotTrackingInc(params.mbgcnbd,cbs_grocery$T.cal,T.tot = 104,actual.inc.tracking.data =elog2inc(grocery))

            }
        }else if(input$Data == "Donations"){
            if(input$Model == "ParetoNBD"){
                cbs_donations <- data()[[3]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
                pnbd.PlotTrackingInc(params.pnbd,cbs_donations$T.cal,T.tot = 234,actual.inc.tracking.data =elog2inc(donations))


            }else if(input$Model == "BG/NBD"){
                cbs_donations <- data()[[3]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
                bgnbd.PlotTrackingInc(params.bgnbd,cbs_donations$T.cal,T.tot = 234,actual.inc.tracking.data =elog2inc(donations))


            }else if(input$Model == "MBG/NBD"){
                cbs_donations <- data()[[3]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)
                mbgcnbd.PlotTrackingInc(params.mbgnbd,cbs_donations$T.cal,T.tot = 234,actual.inc.tracking.data =elog2inc(donations))

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_donations <- data()[[3]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)
                mbgcnbd.PlotTrackingInc(params.mbgcnbd,cbs_donations$T.cal,T.tot = 234,actual.inc.tracking.data =elog2inc(donations))

            }
        }
    })

    output$cum_weekly <- renderPlot({
        if(input$Data == "CDNOW"){
            if(input$Model == "ParetoNBD"){
                cbs_cdnow <- data()[[1]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
                pnbd.PlotTrackingCum(params.pnbd,cbs_cdnow$T.cal,T.tot = 78,actual.cu.tracking.data =elog2cum(cdnow))


            }else if(input$Model == "BG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
                bgnbd.PlotTrackingCum(params.bgnbd,cbs_cdnow$T.cal,T.tot = 78,actual.cu.tracking.data =elog2cum(cdnow))


            }else if(input$Model == "MBG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)
                mbgcnbd.PlotTrackingCum(params.mbgnbd,cbs_cdnow$T.cal,T.tot = 78,actual.cu.tracking.data =elog2cum(cdnow))


            }else if(input$Model == "MBG/CNBD-k"){
                cbs_cdnow <- data()[[1]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)
                mbgcnbd.PlotTrackingCum(params.mbgcnbd,cbs_cdnow$T.cal,T.tot = 78,actual.cu.tracking.data =elog2cum(cdnow))

            }
        }else if(input$Data == "Grocery"){
            if(input$Model == "ParetoNBD"){
                cbs_grocery <- data()[[2]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
                pnbd.PlotTrackingCum(params.pnbd,cbs_grocery$T.cal,T.tot = 104,actual.cu.tracking.data =elog2cum(grocery))


            }else if(input$Model == "BG/NBD"){
                cbs_grocery <- data()[[2]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
                bgnbd.PlotTrackingCum(params.bgnbd,cbs_grocery$T.cal,T.tot = 104,actual.cu.tracking.data =elog2cum(grocery))


            }else if(input$Model == "MBG/NBD"){
                cbs_grocery <- data()[[2]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)
                mbgcnbd.PlotTrackingCum(params.mbgnbd,cbs_grocery$T.cal,T.tot = 104,actual.cu.tracking.data =elog2cum(grocery))

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_grocery <- data()[[2]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)
                mbgcnbd.PlotTrackingCum(params.mbgcnbd,cbs_grocery$T.cal,T.tot = 104,actual.cu.tracking.data =elog2cum(grocery))

            }
        }else if(input$Data == "Donations"){
            if(input$Model == "ParetoNBD"){
                cbs_donations <- data()[[3]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
                pnbd.PlotTrackingCum(params.pnbd,cbs_donations$T.cal,T.tot = 234,actual.cu.tracking.data =elog2cum(donations))


            }else if(input$Model == "BG/NBD"){
                cbs_donations <- data()[[3]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
                bgnbd.PlotTrackingCum(params.bgnbd,cbs_donations$T.cal,T.tot = 234,actual.cu.tracking.data =elog2cum(donations))


            }else if(input$Model == "MBG/NBD"){
                cbs_donations <- data()[[3]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)
                mbgcnbd.PlotTrackingCum(params.mbgnbd,cbs_donations$T.cal,T.tot = 234,actual.cu.tracking.data =elog2cum(donations))

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_donations <- data()[[3]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)
                mbgcnbd.PlotTrackingCum(params.mbgcnbd,cbs_donations$T.cal,T.tot = 234,actual.cu.tracking.data =elog2cum(donations))

            }
        }

    })

    output$est_param <- renderTable({
        if(input$Data == "CDNOW"){
            if(input$Model == "ParetoNBD"){
                cbs_cdnow <- data()[[1]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
                palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                d <- as.data.frame(cbind(mean(cbs_cdnow$t.x),mean(cbs_cdnow$litt),mean(palive.pnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "BG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
                palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                d <- as.data.frame(cbind(mean(cbs_cdnow$t.x),mean(cbs_cdnow$litt),mean(palive.bgnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "MBG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)
                palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                d <- as.data.frame(cbind(mean(cbs_cdnow$t.x),mean(cbs_cdnow$litt),mean(palive.mbgnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_cdnow <- data()[[1]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)
                palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                d <- as.data.frame(cbind(mean(cbs_cdnow$t.x),mean(cbs_cdnow$litt),mean(palive.mbgcnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d
            }
        }else if(input$Data == "Grocery"){
            if(input$Model == "ParetoNBD"){
                cbs_grocery <- data()[[2]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
                palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                d <- as.data.frame(cbind(mean(cbs_grocery$t.x),mean(cbs_grocery$litt),mean(palive.pnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "BG/NBD"){
                cbs_grocery <- data()[[2]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
                palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                d <- as.data.frame(cbind(mean(cbs_grocery$t.x),mean(cbs_grocery$litt),mean(palive.bgnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "MBG/NBD"){
                cbs_grocery <- data()[[2]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)
                palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                d <- as.data.frame(cbind(mean(cbs_grocery$t.x),mean(cbs_grocery$litt),mean(palive.mbgnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_grocery <- data()[[2]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)
                palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                d <- as.data.frame(cbind(mean(cbs_grocery$t.x),mean(cbs_grocery$litt),mean(palive.mbgcnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d
            }
        }else if(input$Data == "Donations"){
            if(input$Model == "ParetoNBD"){
                cbs_donations <- data()[[3]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
                palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                d <- as.data.frame(cbind(mean(cbs_donations$t.x),mean(cbs_donations$litt),mean(palive.pnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "BG/NBD"){
                cbs_donations <- data()[[3]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
                palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                d <- as.data.frame(cbind(mean(cbs_donations$t.x),mean(cbs_donations$litt),mean(palive.bgnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "MBG/NBD"){
                cbs_donations <- data()[[3]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)
                palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                d <- as.data.frame(cbind(mean(cbs_donations$t.x),mean(cbs_donations$litt),mean(palive.mbgnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_donations <- data()[[3]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)
                palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                d <- as.data.frame(cbind(mean(cbs_donations$t.x),mean(cbs_donations$litt),mean(palive.mbgcnbd)))
                colnames(d) <- c("mean(lifetime)","mean(itt)","mean(palive)")
                d
            }
        }


    })

    output$rec_trans <-renderPlot({
        if(input$Data == "CDNOW"){
            if(input$Model == "ParetoNBD"){
                cbs_cdnow <- data()[[1]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
                pnbd.PlotRecVsConditionalExpectedFrequency(params.pnbd,cbs_cdnow,cbs_cdnow$T.star[1],cbs_cdnow$x.star)


            }else if(input$Model == "BG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
                bgnbd.PlotRecVsConditionalExpectedFrequency(params.bgnbd,cbs_cdnow,cbs_cdnow$T.star[1],cbs_cdnow$x.star)


            }else if(input$Model == "MBG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_cdnow <- data()[[1]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)

            }
        }else if(input$Data == "Grocery"){
            if(input$Model == "ParetoNBD"){
                cbs_grocery <- data()[[2]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
                pnbd.PlotRecVsConditionalExpectedFrequency(params.pnbd,cbs_grocery,cbs_grocery$T.star[1],cbs_grocery$x.star)


            }else if(input$Model == "BG/NBD"){
                cbs_grocery <- data()[[2]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
                bgnbd.PlotRecVsConditionalExpectedFrequency(params.bgnbd,cbs_grocery,cbs_grocery$T.star[1],cbs_grocery$x.star)


            }else if(input$Model == "MBG/NBD"){
                cbs_grocery <- data()[[2]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_grocery <- data()[[2]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)

            }

        }else if(input$Data == "Donations"){
            if(input$Model == "ParetoNBD"){
                cbs_donations <- data()[[3]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
                pnbd.PlotRecVsConditionalExpectedFrequency(params.pnbd,cbs_donations,cbs_donations$T.star[1],cbs_donations$x.star)


            }else if(input$Model == "BG/NBD"){
                cbs_donations <- data()[[3]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
                bgnbd.PlotRecVsConditionalExpectedFrequency(params.bgnbd,cbs_donations,cbs_donations$T.star[1],cbs_donations$x.star)


            }else if(input$Model == "MBG/NBD"){
                cbs_donations <- data()[[3]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)

            }else if(input$Model == "MBG/CNBD-k"){
                cbs_donations <- data()[[3]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)

            }
        }

    })
    ##no sales data in Donations and Grocery
    output$suf_mat <- renderTable({
        if(input$Data == "CDNOW"){
            if(input$Model == "ParetoNBD"){
                cbs_cdnow <- data()[[1]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
                cbs_cdnow$palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                cbs_cdnow$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                    params  = params.pnbd,
                    T.star  = cbs_cdnow$T.star,
                    x       = cbs_cdnow$x,
                    t.x     = cbs_cdnow$t.x,
                    T.cal   = cbs_cdnow$T.cal)
                cbs_cdnow <- arrange(cbs_cdnow,desc(xstar.pnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_cdnow),cbs_cdnow[1179:1183,],tail(cbs_cdnow)))
                data_cbs
            }else if(input$Model == "BG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
                cbs_cdnow$palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                cbs_cdnow$xstar.bgnbd <- BTYD::bgnbd.ConditionalExpectedTransactions(
                    params  = params.bgnbd,
                    T.star  = cbs_cdnow$T.star,
                    x       = cbs_cdnow$x,
                    t.x     = cbs_cdnow$t.x,
                    T.cal   = cbs_cdnow$T.cal)
                cbs_cdnow <- arrange(cbs_cdnow,desc(xstar.bgnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_cdnow),cbs_cdnow[1179:1183,],tail(cbs_cdnow)))
                data_cbs
            }else if(input$Model == "MBG/NBD"){
                cbs_cdnow <- data()[[1]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)
                cbs_cdnow$palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                cbs_cdnow$xstar.mbgnbd <- mbgcnbd.ConditionalExpectedTransactions(
                    params  = params.mbgnbd,
                    T.star  = cbs_cdnow$T.star,
                    x       = cbs_cdnow$x,
                    t.x     = cbs_cdnow$t.x,
                    T.cal   = cbs_cdnow$T.cal)
                cbs_cdnow <- arrange(cbs_cdnow,desc(xstar.mbgnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_cdnow),cbs_cdnow[1179:1183,],tail(cbs_cdnow)))
                data_cbs
            }else if(input$Model == "MBG/CNBD-k"){
                cbs_cdnow <- data()[[1]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)
                cbs_cdnow$palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_cdnow$x, cbs_cdnow$t.x, cbs_cdnow$T.cal)
                cbs_cdnow$xstar.mbgnbd <- mbgcnbd.ConditionalExpectedTransactions(
                    params  = params.mbgcnbd,
                    T.star  = cbs_cdnow$T.star,
                    x       = cbs_cdnow$x,
                    t.x     = cbs_cdnow$t.x,
                    T.cal   = cbs_cdnow$T.cal)

                cbs_cdnow <- arrange(cbs_cdnow,desc(xstar.mbgnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_cdnow),cbs_cdnow[1179:1183,],tail(cbs_cdnow)))
                data_cbs
            }
        }else if(input$Data == "Grocery"){
            if(input$Model == "ParetoNBD"){
                cbs_grocery <- data()[[2]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
                cbs_grocery$palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                cbs_grocery$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                    params  = params.pnbd,
                    T.star  = cbs_grocery$T.star,
                    x       = cbs_grocery$x,
                    t.x     = cbs_grocery$t.x,
                    T.cal   = cbs_grocery$T.cal)
                cbs_grocery <- arrange(cbs_grocery,desc(xstar.pnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_grocery),cbs_grocery[763:767,],tail(cbs_grocery)))
                data_cbs
            }else if(input$Model == "BG/NBD"){
                cbs_grocery <- data()[[2]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
                cbs_grocery$palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                cbs_grocery$xstar.bgnbd  <- BTYD::bgnbd.ConditionalExpectedTransactions(
                    params  = params.bgnbd ,
                    T.star  = cbs_grocery$T.star,
                    x       = cbs_grocery$x,
                    t.x     = cbs_grocery$t.x,
                    T.cal   = cbs_grocery$T.cal)
                cbs_grocery <- arrange(cbs_grocery,desc(xstar.bgnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_grocery),cbs_grocery[763:767,],tail(cbs_grocery)))
                data_cbs
            }else if(input$Model == "MBG/NBD"){
                cbs_grocery <- data()[[2]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)
                cbs_grocery$palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                cbs_grocery$xstar.mbcnbd  <- mbgcnbd.ConditionalExpectedTransactions(
                    params  = params.mbgnbd ,
                    T.star  = cbs_grocery$T.star,
                    x       = cbs_grocery$x,
                    t.x     = cbs_grocery$t.x,
                    T.cal   = cbs_grocery$T.cal)
                cbs_grocery <- arrange(cbs_grocery,desc(xstar.mbcnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_grocery),cbs_grocery[763:767,],tail(cbs_grocery)))
                data_cbs
            }else if(input$Model == "MBG/CNBD-k"){
                cbs_grocery <- data()[[2]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)
                cbs_grocery$palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_grocery$x, cbs_grocery$t.x, cbs_grocery$T.cal)
                cbs_grocery$xstar.mbgcnbd  <- mbgcnbd.ConditionalExpectedTransactions(
                    params  = params.mbgcnbd ,
                    T.star  = cbs_grocery$T.star,
                    x       = cbs_grocery$x,
                    t.x     = cbs_grocery$t.x,
                    T.cal   = cbs_grocery$T.cal)
                cbs_grocery <- arrange(cbs_grocery,desc(xstar.mbgcnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_grocery),cbs_grocery[763:767,],tail(cbs_grocery)))
                data_cbs
            }
        }else if(input$Data == "Donations"){
            if(input$Model == "ParetoNBD"){
                cbs_donations <- data()[[3]]
                params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
                cbs_donations$palive.pnbd <- BTYD::pnbd.PAlive(params = params.pnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                cbs_donations$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                    params  = params.pnbd,
                    T.star  = cbs_donations$T.star,
                    x       = cbs_donations$x,
                    t.x     = cbs_donations$t.x,
                    T.cal   = cbs_donations$T.cal)
                cbs_donations <- arrange(cbs_donations,desc(xstar.pnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_donations),cbs_donations[10583:10587,],tail(cbs_donations)))
                data_cbs <- arrange(data.cbs,xstar.pnbd)
                data_cbs
            }else if(input$Model == "BG/NBD"){
                cbs_donations <- data()[[3]]
                params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
                cbs_donations$palive.bgnbd <- BTYD::bgnbd.PAlive(params = params.bgnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                cbs_donations$xstar.bgnbd <- BTYD::bgnbd.ConditionalExpectedTransactions(
                    params  = params.bgnbd,
                    T.star  = cbs_donations$T.star,
                    x       = cbs_donations$x,
                    t.x     = cbs_donations$t.x,
                    T.cal   = cbs_donations$T.cal)
                cbs_donations <- arrange(cbs_donations,desc(xstar.bgnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_donations),cbs_donations[10583:10587,],tail(cbs_donations)))
                data_cbs
            }else if(input$Model == "MBG/NBD"){
                cbs_donations <- data()[[3]]
                params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)
                cbs_donations$palive.mbgnbd <- mbgcnbd.PAlive(params = params.mbgnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                cbs_donations$xstar.mbgnbd <- mbgcnbd.ConditionalExpectedTransactions(
                    params  = params.mbgnbd,
                    T.star  = cbs_donations$T.star,
                    x       = cbs_donations$x,
                    t.x     = cbs_donations$t.x,
                    T.cal   = cbs_donations$T.cal)
                cbs_donations <- arrange(cbs_donations,desc(xstar.mbgnbd))
                data_cbs <- as.data.frame(rbind(head(cbs_donations),cbs_donations[10583:10587,],tail(cbs_donations)))
                data_cbs
            }else if(input$Model == "MBG/CNBD-k"){
                cbs_donations <- data()[[3]]
                params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)
                cbs_donations$palive.mbgcnbd <- mbgcnbd.PAlive(params = params.mbgcnbd, cbs_donations$x, cbs_donations$t.x, cbs_donations$T.cal)
                cbs_donations$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
                    params  = params.mbgcnbd,
                    T.star  = cbs_donations$T.star,
                    x       = cbs_donations$x,
                    t.x     = cbs_donations$t.x,
                    T.cal   = cbs_donations$T.cal)
                cbs_donations <- arrange(cbs_donations,desc(xstar.mbgcnbd))
                data_cbs<- as.data.frame(rbind(head(cbs_donations),cbs_donations[10583:10587,],tail(cbs_donations)))
                data_cbs

            }
        }
    })
    output$stats <- renderTable({
        measures <- c(
            "MAE" = function(a, f) mean(abs(a - f)),
            "MSLE" = function(a, f) mean(((log(a + 1) - log(f + 1)))^2),
            "BIAS" = function(a, f) sum(f)/sum(a) - 1)
        models <- c(
            "NBD" = "nbd",
            "Pareto/NBD" = "pnbd",
            "BG/CNBD-k" = "bgcnbd",
            "MBG/CNBD-k" = "mbgcnbd")

        if(input$Data == "CDNOW"){
            cbs_cdnow <- data()[[1]]
            params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_cdnow,max.param.value = 100)
            cbs_cdnow$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                params  = params.pnbd,
                T.star  = cbs_cdnow$T.star,
                x       = cbs_cdnow$x,
                t.x     = cbs_cdnow$t.x,
                T.cal   = cbs_cdnow$T.cal)
            params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_cdnow)
            cbs_cdnow$xstar.bgnbd <- BTYD::bgnbd.ConditionalExpectedTransactions(
                params  = params.bgnbd,
                T.star  = cbs_cdnow$T.star,
                x       = cbs_cdnow$x,
                t.x     = cbs_cdnow$t.x,
                T.cal   = cbs_cdnow$T.cal)
            params.mbgnbd <- mbgnbd.EstimateParameters(cbs_cdnow)
            cbs_cdnow$xstar.mbgnbd <- mbgcnbd.ConditionalExpectedTransactions(
                params  = params.mbgnbd,
                T.star  = cbs_cdnow$T.star,
                x       = cbs_cdnow$x,
                t.x     = cbs_cdnow$t.x,
                T.cal   = cbs_cdnow$T.cal)
            params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_cdnow)
            cbs_cdnow$xstar.mbgnbd <- mbgcnbd.ConditionalExpectedTransactions(
                params  = params.mbgcnbd,
                T.star  = cbs_cdnow$T.star,
                x       = cbs_cdnow$x,
                t.x     = cbs_cdnow$t.x,
                T.cal   = cbs_cdnow$T.cal)
            sapply(measures, function(measure) {
                sapply(models, function(model) {
                    err <- do.call(measure, list(a = cbs_cdnow$x.star, f = cbs_cdnow[[paste0("xstar.", model)]]))
                    round(err, 3)
                })
            })

        }else if(input$Data == "Grocery"){
            cbs_grocery <- data()[[2]]
            params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_grocery,max.param.value = 100)
            cbs_grocery$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                params  = params.pnbd,
                T.star  = cbs_grocery$T.star,
                x       = cbs_grocery$x,
                t.x     = cbs_grocery$t.x,
                T.cal   = cbs_grocery$T.cal)
            params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_grocery)
            cbs_grocery$xstar.bgnbd  <- BTYD::bgnbd.ConditionalExpectedTransactions(
                params  = params.bgnbd ,
                T.star  = cbs_grocery$T.star,
                x       = cbs_grocery$x,
                t.x     = cbs_grocery$t.x,
                T.cal   = cbs_grocery$T.cal)
            params.mbgnbd <- mbgnbd.EstimateParameters(cbs_grocery)
            cbs_grocery$xstar.mbcnbd  <- mbgcnbd.ConditionalExpectedTransactions(
                params  = params.mbgnbd ,
                T.star  = cbs_grocery$T.star,
                x       = cbs_grocery$x,
                t.x     = cbs_grocery$t.x,
                T.cal   = cbs_grocery$T.cal)
            params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_grocery)
            cbs_grocery$xstar.mbgcnbd  <- mbgcnbd.ConditionalExpectedTransactions(
                params  = params.mbgcnbd ,
                T.star  = cbs_grocery$T.star,
                x       = cbs_grocery$x,
                t.x     = cbs_grocery$t.x,
                T.cal   = cbs_grocery$T.cal)
            sapply(measures, function(measure) {
                sapply(models, function(model) {
                    err <- do.call(measure, list(a = cbs_grocery$x.star, f = cbs_grocery[[paste0("xstar.", model)]]))
                    round(err, 3)
                })
            })

        }else if(input$Data == "Donations"){
            cbs_donations <- data()[[3]]
            params.pnbd <- BTYD::pnbd.EstimateParameters(cbs_donations,max.param.value = 100)
            cbs_donations$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                params  = params.pnbd,
                T.star  = cbs_donations$T.star,
                x       = cbs_donations$x,
                t.x     = cbs_donations$t.x,
                T.cal   = cbs_donations$T.cal)
            params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs_donations)
            cbs_donations$xstar.bgnbd <- BTYD::bgnbd.ConditionalExpectedTransactions(
                params  = params.bgnbd,
                T.star  = cbs_donations$T.star,
                x       = cbs_donations$x,
                t.x     = cbs_donations$t.x,
                T.cal   = cbs_donations$T.cal)
            params.mbgnbd <- mbgnbd.EstimateParameters(cbs_donations)
            cbs_donations$xstar.mbgnbd <- mbgcnbd.ConditionalExpectedTransactions(
                params  = params.mbgnbd,
                T.star  = cbs_donations$T.star,
                x       = cbs_donations$x,
                t.x     = cbs_donations$t.x,
                T.cal   = cbs_donations$T.cal)

            params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs_donations)
            cbs_donations$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
                params  = params.mbgcnbd,
                T.star  = cbs_donations$T.star,
                x       = cbs_donations$x,
                t.x     = cbs_donations$t.x,
                T.cal   = cbs_donations$T.cal)
            sapply(measures, function(measure) {
                sapply(models, function(model) {
                    err <- do.call(measure, list(a = cbs_donations$x.star, f = cbs_donations[[paste0("xstar.", model)]]))
                    round(err, 3)
                })
            })
        }

    })
}

shinyApp(ui = ui, server = server)
