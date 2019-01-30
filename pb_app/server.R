############## BDT Passing-Bablok Regression                  ##############
############## User Interface                                 ##############
############## Author: SeokChan Yoo                           ##############
############## Version. Beta                                  ##############

# MIT License
# 
# Copyright (c) [2019/01/21] [Seokchan Yoo]
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library("xlsx")
library("shiny")
library("mcr")
library("shinydashboard")
library("DT")
library("rhandsontable")
library("rmarkdown")

# Validate  with custom 'need' function for correct input
# NULL = "PASS"
# FALSE = "Fail silently"
# String = "Fail"

# df_check <- function(input){
#   for(idx in length(input$X)){
#     if(class(input$X[idx])=="numeric" & class(input$Y[idx])== "numeric"){
#       NULL
#     }
#     else if(class(input$X[idx])=="" & class(input$Y[idx])== ""){
#       FALSE
#     }
#     else {
#       "Please enter correct values"
#     }
#   }
# }

# Initialize sample X & Y data 
X <- c(3.07,3.98,	5.86,	13.62,	4.01,	11.31,	10.29	,1.69	,3.2,	4.95,	1.29,	3.32,	6.74,	8.98,	3.04,	1.24,	5.94,	5.25,	2.7	,1.46,	8.46,	1.75,	0.494,	0.345,	6.95,	2.11,	2.1,	1.75,	3.47,	1.31,	15.29,	3.96,	5.95,	6.23,	3.11,	2.21,	16.92,	5,	6.12,	1.23,	4.28,	6.83,	4.08,	2.1	,3.61,	2.67,	2.71,	1.43,	0.585,	1.55,	1.71,	3.09,	3.19,	1.84,	6.36,	4.51,	18.28,	1.12)
Y <- c(3.53,	3.76,	5.56,	11.27	,4.19,	11.12,	8.95,	1.60,	4.06,	6.01,	1.24,	3.19	,6.48	,8.86,	2.32,	1.09,	5.57,	5.74,	1.93,	1.29,	8.28,	1.71,	0.46,	0.30,	6.38,	2.00,	1.77,	1.89,	3.50,	1.10,	13.47,	3.89,	7.14,	6.21,	3.05,	1.96,	16.76,	5.10,	5.57,	1.29,	3.04,	6.12,	3.38,	2.30,	4.13,	2.91,	3.30,	1.28,	0.44,	1.30,	1.81,	3.01,	3.18,	1.75,	8.28,	4.98,	20.11,	1.25)
df <- data.frame(X, Y)


server <- function(input, output){

  data_values <- reactiveValues(data=df)
  
  output$d_tbl <- renderRHandsontable({
    # if(is.null(input$d_tbl)){
    #   DF = data.frame(X=c(0,0,0,0,0,0,0),Y=c(0,0,0,0,0,0,0))
    # } else{
    #   DF = hot_to_r(input$d_tbl)
    # }
    rhandsontable(data_values$data, stretchH="all") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) %>%
      hot_rows(rowHeights=30)
      #hot_validate_numeric(col="X",min=-10,max=1000, allowInvalid=FALSE)
    # Somehow, the app isn't recognizing validate numric.....
      
  })
  
  # Observe Event for the Scatter Plot
  observeEvent(input$submit,{
    data_values$data <- hot_to_r(input$d_tbl)
    dat_x <- data_values$data[["X"]]
    dat_y <- data_values$data[["Y"]]
    
    scat_pl <- function(){
      pl1 <- plot(dat_x,dat_y, main="Scatter Plot", xlab = paste(input$x_name," [",input$units,"]", sep=""), ylab = paste(input$y_name," [",input$units,"]", sep=""))
      abline(a=0,b=1, col="blue")
      legend("bottomright", legend="x=y", col="blue", lty=1)
      pl1
    }
    
    # Render plot - PB_Regression Plot
    output$scat_plot <- renderPlot({
        scat_pl()
    })
  })
  
  ########## Need calculation for Bias Estimation #########
  # Observe Event for the Difference Plot
  observeEvent(input$to_diff_anal,{
    data_values$data <- hot_to_r(input$d_tbl)
    dat_x <- data_values$data[["X"]]
    dat_y <- data_values$data[["Y"]]
    
    PB.reg <- mcreg(dat_x,dat_y, method.reg = "PaBa", alpha=input$alpha)
    
    if(input$diff_plt_opt =="Ba"){
          output$unit_diff <- renderPlot({
            plotDifference(PB.reg, plot.type=3, main="Units Difference Plot", xlab=paste("Average Concentration"," [",input$units,"]", sep=""), ylab=paste("Difference", " [",input$units,"]", sep=""))
          })
          output$per_diff <- renderPlot({
            plotDifference(PB.reg, plot.type=4, main="Percent Difference Plot", xlab=paste("Average Concentration"," [",input$units,"]", sep=""), ylab="Difference (%)")
          })
    }else if(input$diff_plt_opt == "single"){
          output$unit_diff <- renderPlot({
            plotDifference(PB.reg, plot.type=1, main="Units Difference Plot", xlab=paste("Comparative MP's Concentration"," [",input$units,"]", sep=""), ylab=paste("Difference", " [",input$units,"]", sep=""))
          })
          output$per_diff <- renderPlot({
            plotDifference(PB.reg, plot.type=2, main="Percent Difference Plot", xlab=paste("Comparative MP's Concentration"," [",input$units,"]", sep=""), ylab="Difference (%)")
          })
    }else{
      validate(
        need(is.null(input$diff_plt_opt), "Please select the option for Difference Plot!")
      )
    }
  })
  
  # Observe Event for the Regression Plot
  # Case 1. Constant SD  --> Plot OLR and Deming   (CI Calculation = Anal for Lin & JackKnife for Deming)
  # Case 2. Constant CV --> Plot wOLR and wDeming  (CI Calculation = Anal for Lin & JackKnife for Deming)
  # Case 3. Mixed --> Plot Passing-Bablok (CI Calculation = Bootstrap)
  observeEvent(input$to_reg_anal,{
    data_values$data <- hot_to_r(input$d_tbl)
    dat_x <- data_values$data[["X"]]
    dat_y <- data_values$data[["Y"]]
    

        reg_sum <- function(){
          
          tt <- c("Regression Equation", "Intercept A", "Intercept 95% CI", "Slope B", "Slope 95% CI")
          bb <- c(paste("y=",round(PB.reg@para[1],digits=input$sci_digit),"+", round(PB.reg@para[2],digits=input$sci_digit),"x"), round(PB.reg@para[1],digits=input$sci_digit),
                  paste(round(PB.reg@para[5],digits=input$sci_digit), "to", round(PB.reg@para[7],digits=input$sci_digit)),
                  round(PB.reg@para[2],digits=input$sci_digit),paste(round(PB.reg@para[6],digits=input$sci_digit), "to", round(PB.reg@para[8],digits=input$sci_digit))
                  )
          
          # SD for RSD
          # rsd <- sd(getResiduals(PB.reg)$optimized)
          # l_rsd <- rsd*-1.96
          # h_rsd <- rsd*1.96
          # 
          # # Calculate cusum p-value for linearity test
          # cusum <- calcCUSUM(PB.reg)$max.cusum

          # Create a dataframe for stat summary
          # tt <- c("Regression Equation", "Intercept A", "95% CI", "Slope B", "95% CI" , "Residual Standard Deviation (RSD)", "1.96 RSD Interval", "Cusum test for Linearity")
          # bb <- c(paste("y=",round(PB.reg@para[1],digits=input$sci_digit),"+", round(PB.reg@para[2],digits=input$sci_digit),"x"), round(PB.reg@para[1],digits=input$sci_digit),
          #         paste(round(PB.reg@para[5],digits=input$sci_digit), "to", round(PB.reg@para[6],digits=input$sci_digit)),
          #         round(PB.reg@para[2],digits=input$sci_digit),paste(round(PB.reg@para[7],digits=input$sci_digit), "to", round(PB.reg@para[8],digits=input$sci_digit)),
          #         round(rsd,digits=input$sci_digit), paste(round(l_rsd,digits=input$sci_digit), "to", round(h_rsd,digits=input$sci_digit)), round(cusum,digits=input$sci_digit) )
          dff <- data.frame(tt,bb)
          return(dff)
        }

    if(input$reg_opt =="cSD-1"){
      PB.reg <- mcreg(dat_x,dat_y, method.reg = "LinReg", alpha=input$alpha, method.ci = "analytical")
      output$reg_plt <- renderPlot({
        # plot(dat_x,dat_y, main = "Ordinary Linear Regression", xlab = paste(input$x_name," [",input$units,"]", sep=""), ylab = paste(input$y_name," [",input$units,"]", sep=""))
        # abline(PB.reg@para[1:2], col = "blue")
        # abline(c(PB.reg@para[5],PB.reg@para[7]), col = "red", lty=3)
        # abline(c(PB.reg@para[6],PB.reg@para[8]), col = "red", lty=3)
        # legend("bottomright", legend=c("OLR","LowCI","UpperCI"), col=c("blue","red","red"), lty=c(1,3,3))
        # legend("topleft", legend=c(paste("Slope: ",round(PB.reg@para[2],digits=input$sci_digit)), paste("Intercept: ",round(PB.reg@para[1],digits=input$sci_digit))))
            plot(PB.reg, alpha=input$alpha, main="Ordinary Linear Regression",equal.axis=TRUE, ci.area=FALSE, add.legend=TRUE, ci.border=TRUE, ci.border.col="green",ci.border.lty=6, ci.border.lwd=1, x.lab= paste(input$x_name," [",input$units,"]", sep=""), y.lab= paste(input$y_name," [",input$units,"]", sep=""), add.grid=FALSE, sub="")
        })
        output$view <- renderTable({
            reg_sum()
          }, include.colnames=FALSE)
    }else if(input$reg_opt == "cSD-2"){
      PB.reg <- mcreg(dat_x,dat_y, method.reg = "Deming", alpha=input$alpha, method.ci = "jackknife")
      output$reg_plt <- renderPlot({
        # plot(dat_x,dat_y, main = "Deming Regression", xlab = paste(input$x_name," [",input$units,"]", sep=""), ylab = paste(input$y_name," [",input$units,"]", sep=""))
        # abline(PB.reg@para[1:2], col = "blue")
        # abline(PB.reg@para[5:6], col = "red", lty=3)
        # abline(PB.reg@para[7:8], col = "red", lty=3)
        # legend("bottomright", legend=c("Deming","LowCI","UpperCI"), col=c("blue","red","red"), lty=c(1,3,3))
        # legend("topleft", legend=c(paste("Slope: ",round(PB.reg@para[2],digits=input$sci_digit)), paste("Intercept: ",round(PB.reg@para[1],digits=input$sci_digit))))
        plot(PB.reg, alpha=input$alpha, main="Deming Regression", equal.axis=TRUE, ci.area=FALSE, add.legend=TRUE, ci.border=TRUE, ci.border.col="green",ci.border.lty=6, ci.border.lwd=1, x.lab= paste(input$x_name," [",input$units,"]", sep=""), y.lab= paste(input$y_name," [",input$units,"]", sep=""), add.grid=FALSE, sub="")
      })
      output$view <- renderTable({
        reg_sum()
      }, include.colnames=FALSE)
    }else if(input$reg_opt =="cCV-1"){
      PB.reg <- mcreg(dat_x,dat_y, method.reg = "WLinReg", alpha=input$alpha, method.ci = "analytical")
      output$reg_plt <- renderPlot({
        # plot(dat_x,dat_y, main = "Weigthed Linear Regression", xlab = paste(input$x_name," [",input$units,"]", sep=""), ylab = paste(input$y_name," [",input$units,"]", sep=""))
        # abline(PB.reg@para[1:2], col = "blue")
        # abline(PB.reg@para[5:6], col = "red", lty=3)
        # abline(PB.reg@para[7:8], col = "red", lty=3)
        # legend("bottomright", legend=c("wOLR","LowCI","UpperCI"), col=c("blue","red","red"), lty=c(1,3,3))
        # legend("topleft", legend=c(paste("Slope: ",round(PB.reg@para[2],digits=input$sci_digit)), paste("Intercept: ",round(PB.reg@para[1],digits=input$sci_digit))))
        plot(PB.reg, alpha=input$alpha, main="Weigthed Linear Regression", equal.axis=TRUE, ci.area=FALSE, add.legend=TRUE, ci.border=TRUE, ci.border.col="green",ci.border.lty=6, ci.border.lwd=1, x.lab= paste(input$x_name," [",input$units,"]", sep=""), y.lab= paste(input$y_name," [",input$units,"]", sep=""), add.grid=FALSE, sub="")
      })
      output$view <- renderTable({
        reg_sum()
      }, include.colnames=FALSE)
    }else if(input$reg_opt == "cCV-2"){
      PB.reg <- mcreg(dat_x,dat_y, method.reg = "WDeming", alpha=input$alpha, method.ci = "jackknife")
      output$reg_plt <- renderPlot({
        # plot(dat_x,dat_y, main = "Weigthed Deming Regression", xlab = paste(input$x_name," [",input$units,"]", sep=""), ylab = paste(input$y_name," [",input$units,"]", sep=""))
        # abline(PB.reg@para[1:2], col = "blue")
        # abline(PB.reg@para[5:6], col = "red", lty=3)
        # abline(PB.reg@para[7:8], col = "red", lty=3)
        # legend("bottomright", legend=c("wDeming","LowCI","UpperCI"), col=c("blue","red","red"), lty=c(1,3,3))
        # legend("topleft", legend=c(paste("Slope: ",round(PB.reg@para[2],digits=input$sci_digit)), paste("Intercept: ",round(PB.reg@para[1],digits=input$sci_digit))))
        plot(PB.reg, alpha=input$alpha, main="Weigthed Deming Regression", equal.axis=TRUE, ci.area=FALSE, add.legend=TRUE, ci.border=TRUE, ci.border.col="green",ci.border.lty=6, ci.border.lwd=1, x.lab= paste(input$x_name," [",input$units,"]", sep=""), y.lab= paste(input$y_name," [",input$units,"]", sep=""), add.grid=FALSE, sub="")
      })
      output$view <- renderTable({
        reg_sum()
      }, include.colnames=FALSE)
    }else if(input$reg_opt == "PaBa"){
      PB.reg <- mcreg(dat_x,dat_y, method.reg = "PaBa", alpha=input$alpha, method.ci = "bootstrap")
      output$reg_plt <- renderPlot({
              # plot(dat_x,dat_y, main = "Passing-Bablok Regression", xlab = paste(input$x_name," [",input$units,"]", sep=""), ylab = paste(input$y_name," [",input$units,"]", sep=""))
              # abline(PB.reg@para[1:2], col = "blue")
              # abline(PB.reg@para[5:6], col = "red", lty=3)
              # abline(PB.reg@para[7:8], col = "red", lty=3)
              # legend("bottomright", legend=c("Passing-Bablok","LowCI","UpperCI"), col=c("blue","red","red"), lty=c(1,3,3))
              # legend("topleft", legend=c(paste("Slope: ",round(PB.reg@para[2],digits=input$sci_digit)), paste("Intercept: ",round(PB.reg@para[1],digits=input$sci_digit))))
        plot(PB.reg, alpha=input$alpha, main="Passing-Bablok Regression", equal.axis=TRUE, ci.area=FALSE, add.legend=TRUE, ci.border=TRUE, ci.border.col="green",ci.border.lty=6, ci.border.lwd=1, x.lab= paste(input$x_name," [",input$units,"]", sep=""), y.lab= paste(input$y_name," [",input$units,"]", sep=""), add.grid=FALSE, sub="")
        #abline(a=PB.reg@para[5],b=PB.reg@para[6], col="black")
        })
      output$view <- renderTable({
        reg_sum()
      }, include.colnames=FALSE)
    }
    else{
      validate(
        need(is.null(input$reg_opt), "Please select the option for Regression Analysis!")
      )
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('report', sep = '.', switch(
        input$format, PDF = 'pdf', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}
  

    
