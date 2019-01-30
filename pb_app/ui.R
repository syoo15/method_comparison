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


# Begin UI Design

ui <- dashboardPage(
  dashboardHeader(title = "Method Comparison"),
  dashboardSidebar(
    sidebarMenu(
    menuItem(
      "Information",
      tabName = "info",
      icon = icon("info", lib = "font-awesome")
    ),
    menuItem(
      "Analysis",
      tabName = "anal",
      icon = icon("chart-line", lib = "font-awesome"),
      startExpanded = TRUE,
        menuSubItem("Step 1. Insert Data", tabName="data"),
        menuSubItem("Step 2. Scatter Plot", tabName="scat_plt"),
        menuSubItem("Step 3. Difference Plot", tabName="diff_anal"),
        menuSubItem("Step 4. Regression Analysis", tabName="reg_anal")
    ),
    menuItem(
      "Download Report",
      tabName = "report",
      icon = icon("cloud-download-alt", lib = "font-awesome"),
      badgeLabel = "dev",
      badgeColor = "yellow"
    )
    )
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "info",
            h2("Method Comparison Beta version"),
            h3(
              "Following the CLSI EP09-A3 recommendations"),
            p("Must proceed each step carefully, otherwise the app will crash"),
            p("Step1. Insert the data for Method Comparison"),
            p("Open source Dashboard & Boditech Med."),
            p("Copyrighted by Seokchan Yoo")
            ),
    tabItem(tabName = "data",
            fluidRow(
              box(
                h4("Data Table"),
                rHandsontableOutput("d_tbl")
              ),
              box(
                title="Instruction"
              )
            )),
    tabItem(tabName = "scat_plt",
            fluidRow(
              box(
                title="Scatter Plot",
                collapsible = TRUE,
                plotOutput("scat_plot")),
              box(
                title="Basic Configuration",
                collapsible = TRUE,
                h4("Method Names"),
                textInput("x_name", "X", "Reference Method"),
                textInput("y_name", "Y", "New Method"),
                textInput("units", "Cocentration Unit:", "ng/mL"),
                sliderInput(
                  "alpha",
                  "Confidence level:",
                  value = 0.05,
                  min = 0.01,
                  max = 0.20,
                  step = 0.01
                ),
                numericInput(
                  "sci_digit",
                  "Number of digits:",
                  4,
                  min = 1,
                  max = 10
                ),
                actionButton("submit", "Start Analysis")
              )
              )
            ),
    tabItem(tabName="diff_anal",
            fluidRow(
              box(
                title="Inspect Plot for underlying characteristics",
                collapsible = TRUE,
                selectInput("diff_plt_opt","Plot Option",
                            c("Bland-Altman; Average on X-axis"= "Ba",
                              "Comparative MP on X-axis"= "single"
                            ),
                            selected=NULL),
                actionButton("to_diff_anal", "Next Step"),
                width=12
                
              ),
              box(
                title="Unit Difference Plot",
                collapsible = TRUE,
                plotOutput("unit_diff")),
              box(
                title="Percentage Difference Plot",
                collapsible = TRUE,
                plotOutput("per_diff")
              ),
              box(
                title="Bias Estimation",
                collapsible = TRUE,
                h3("What is the trend of your data?:")
              )
            )),
    tabItem(tabName="reg_anal",
            fluidRow(
              box(
                title="Regression Option",
                collapsible = TRUE,
                selectInput("reg_opt","Regression Option",
                            c("Constant SD: Linear Regression"= "cSD-1",
                              "Constant SD: Deming" = "cSD-2",
                              "Constant CV: weigthed Linear Regression" = "cCV-1",
                              "Constant CV: weighted Deming"= "cCV-2",
                              "Mixed: Passing-Bablok" = "PaBa"
                            ),
                            selected=NULL),
                actionButton("to_reg_anal", "Next Step"),
                width=12
              ),
              box(
                title="Regression Analysis",
                collapsible = TRUE,
                plotOutput("reg_plt")
                ),
              box(
                title="Statistics",
                collapsible=TRUE,
                tableOutput("view")
              )
            )),
    tabItem(tabName = "report",
            fluidRow(
              box(
                downloadButton('downloadReport'))))    

    
  ))
)

