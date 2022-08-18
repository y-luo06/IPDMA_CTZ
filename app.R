library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(egg)

stage1 <- read.csv("coef_stage1.csv")
stage1 <- stage1[,-1]
stage2.lda <- read.csv("lda_stage2.csv")
stage2.acr <- read.csv("acr_stage2.csv")
stage2.lda$var <- as.factor(stage2.lda$var)
stage2.acr$var <- as.factor(stage2.acr$var)

ui <- fluidPage(
  fluidRow(
    headerPanel(
      h2("Predicting the Treatment Effect for Individual Rheumatoid Arithritis Patients"), 
      windowTitle = "Predicting the Treatment Effect for Individual Rheumatoid Arithritis Patients")
  ),
  fluidRow(
    navbarPage(theme = shinytheme("flatly"), "Menu",
      tabPanel("Introduction of the study", 
        style = "position:absolute; margin-left: 80px; margin-right: 100px",
        h3("Predicting the add-on treatment effect of certolizumab for patients with rheumatoid arthritis: a two-stage individual participant data meta-analysis prediction model", align = "center"),
        br(),
        p("Current evidence remains ambiguous regarding whether biologics should be prescribed for 
          a rheumatoid arthritis (RA) patient with a specific combination of baseline characteristics. 
          RA is a chronic disease characterized by erosive peripheral arthritis with recurrent flare and 
          remission episodes. To delay structural joint destruction and irreversible deformity on a 
          long-term basis, “treat-to-target” strategy has been recommended to guide treatment selection,
          where the treatment target is to attain low disease activity or remission within three to six months. 
          Previous studies have identified some risk factors of poor prognosis, but some were contradictory 
          and the precise impact of various combinations of such risk factors is still uncertain. Moreover, 
          most previous studies were intended to predict long-term outcomes, which were not straightforward 
          to guide drug selection. Lacking of such precise evidence may lead to the “trial-and-error” based 
          treatment selection optimal treatment delay. It may also cause overuse of biologics, leading to 
          increased risk of side effects, difficulties in treatment selection for future flares, 
          loss of response due to anti-drug antibodies, and increased medical expenditures", 
          style = "font-size:19px"),
        p(strong("Therefore, our study aims to predict the accurate benefit of adding biologics to csDMARDs 
                 (conventional synthetic disease-modifying anti-rheumatic drug) for any specific RA patients 
                 to help with treatment selection. ", style = "color:steelblue"), 
          "We take certolizumab (CTZ) as an example because it is a TNF-alpha inhibitor, the most classic 
          and widely used biologic for RA, and CTZ had plenty of IPD at the time we planned the study. 
          To achieve this goal, we developed a prediction model to predict the probability of reaching the 
          treatment target (i.e., low disease activity or remission at three months) when CTZ was added to 
          csDMARDs or when csDMARDs were used alone, respectively, given the individual baseline characteristics. 
          We fitted the model using a two-stage approach based on IPD-MA (individual participant data meta-analysis).", style = "font-size:19px")
                ),
      
      tabPanel("Predicted results from the model",
        sidebarLayout(
          sidebarPanel(width = 3,
            strong("Input your baseline charateristics", style = "font-size: 16px"),
            br(),
            tags$hr(style = "border-color: lightgray;"),
            radioButtons("sex", label = "Sex",
                         choices = c(Male = 1, Female = 0), selected = 0),
            textInput("age", label = "Age (year)", placeholder = 40, value = 40),
            p("*Age should be a number between 18 to 90.", style = "font-size:12px; color:grey"),
            textInput("height", label = "Height (cm)", placeholder = 165, value = 165),
            textInput("weight", label = "Weight (kg)", placeholder = 55, value = 55),
            textInput("duration", label = "How long have you been diagnosed with rheumatoid arthritis? (year)", placeholder = 1.5, value = 1.5),
            strong("I have been treated with the following drugs before:", style = "font-size:15px"),
            checkboxInput("dmard", label = "csDMARDs, such as methotrexate, leflunomide, sulfasalazine, hydroxychloroquine, etc.", value = TRUE),
            checkboxInput("biologic", label = "bDMARDs, any biologics including TNFi, IL-6i, etc.", value = TRUE),
            textInput("tjc", label = "Number of tender joints (0~28)", placeholder = 20, value = 20),
            textInput("sjc", label = "Number of swollen joints (0~28)", placeholder = 10, value = 10),
            textInput("crp", label = "CRP (mg/L)", placeholder = 10.0, value = 10.0),
            textInput("esr", label = "ESR (mm/h)", placeholder = 30.0, value = 30.0),
            textInput("rf", label = "RF (IU/mL)", placeholder = 25.0, value = 25.0),
            sliderInput("pain", label = "How would you rate your pain? (0~100)", min = 0, max = 100, value = 80),
            sliderInput("patg", label = "How would you rate your overall disease activity? (0~100)", min = 0, max = 100, value = 80),
            sliderInput("phyg", label = "How would your doctor rate your overall disease activity? (0~100)", min = 0, max = 100, value = 80),
            sliderInput("fatig", label = "How would you rate your fatigue? (0~10)", min = 0, max = 10, value = 8),
            textInput("haq", label = "Can you tell us your HAQ-DI score? (0~3)", placeholder = 1.5, value = 1.5),
            p("*If you don't know your HAQ-DI score, please assess it first by clicking the HAQ-DI tab.", style = "font-size:14px; color:steelblue")
                  ),
          
          mainPanel(
            tabsetPanel(
              tabPanel("Low disease activity or remission",
                fluidRow(style = "padding-left:20px",
                  h3("Predicted results:"),
                  h4(uiOutput("pLDA")),
                  br(),
                  div(plotOutput("barplot_lda",width = "80%", height = "550px"), align = "center"),
                  br(),
                  br()
                ),
                fluidRow(style = "padding-left:10px; padding-top:10px; border-left:2px solid steelblue; border-top:2px solid steelblue; border-right:2px solid steelblue;margin-border:10px", 
                  column(7,
                    p("*Prediction process and predicted results for any patients are shown in the plots below.", style = "font-weight:bold;font-size:18px;color:lightslategray"),
                    textOutput("base_riskLDA"),
                    tags$head(tags$style("#base_riskLDA{font-size:16px;color:lightslategray}")),
                    p("(For your reference, the plot on the right shows the predicted baseline expected probabilities in our study population.)", 
                      style = "font-size:14px; font-style:italic; color:darkgray"),
                    p("(2) At stage two, the probability of low disease activity or remission when adding CTZ or csDMARDs alone respectively ",
                      strong("(Plot A below)", style = "font-style:italic"), 
                      " and the risk difference between the two treatments ",
                      strong("(Plot B below)", style = "font-style:italic"), "are predicted. The ", 
                      strong("pink dashed line ", style = "font-style:italic;color:#EE6AA7"),
                      "shows the predicted baseline expected probability of the outcome at stage one. It will change interactively when altering the baseline characteristics.",
                      style = "font-size:16px;color:lightslategray")),
                  column(5,
                         br(), 
                         div(img(src = "baserisk_lda.png", height = "140%", width = "100%"), align = "center"))
                ),
                fluidRow(
                  style='border-left:2px solid steelblue; border-right:2px solid steelblue; border-bottom:2px solid steelblue; padding-bottom:10px; ', 
                  br(),
                  div(plotOutput("plot_lda", width = "80%", height = "1000px"), align = "center")
                )
              ),
              tabPanel("ACR50 response",
                       fluidRow(style = "padding-left:20px",
                                h3("Predicted results:"),
                                h4(uiOutput("pACR")),
                                br(),
                                div(plotOutput("barplot_acr",width = "80%", height = "550px"), align = "center"),
                                br()
                       ),
                       fluidRow(style = "padding-left:10px; padding-top:10px; border-left:2px solid steelblue; border-top:2px solid steelblue; border-right:2px solid steelblue;margin-border:10px", 
                         column(7, 
                                p("Prediction process and predicted results for any patients are shown in the plots below.", style = "font-weight:bold;font-size:18px;color:lightslategray"),
                                textOutput("base_riskACR"),
                                tags$head(tags$style("#base_riskACR{font-size:16px}")),
                                p("(For your reference, the plot on the right shows the predicted baseline expected probabilities in our study population.)", 
                                  style = "font-size:14px; font-style:italic; color:darkgray"),
                                p("(2) At stage two, the probability of ACR50 response when adding CTZ or csDMARDs alone respectively ",
                                  strong("(Plot A below)", style = "font-style:italic"), 
                                  " and the risk difference between the two treatments ",
                                  strong("(Plot B below)", style = "font-style:italic"), "are predicted. The ", 
                                  strong("pink dashed line ", style = "font-style:italic;color:#EE6AA7"),
                                  "shows the predicted baseline expected probability of the outcome at stage one. It will change interactively when altering the baseline characteristics.",
                                  style = "font-size:16px;color:lightslategray")),
                         column(5,
                                br(), 
                                div(img(src = "baserisk_acr.png", height = "140%", width = "100%"), align = "center"))
                       ),
                       fluidRow(
                         style='border-left:2px solid steelblue; border-right:2px solid steelblue; border-bottom:2px solid steelblue; padding-bottom:10px; ', 
                         br(),
                         div(plotOutput("plot_acr", width = "80%", height = "1000px"), align = "center")
                       )
              ),
              tabPanel("About the model",
                br(),
                h4("Stage 1:"),
                p("This is a penalized logistic regression model containing 16 variables selected from the literature. 
                  The penalized maximum likelihood shrinkage method is used to shrink the coefficients to avoid extreme predictions. 
                  The model was trained on 3790 patients from 5 randomized controlled trials.", style = "font-size:19px"),
                strong("Model performance (Bootstrap optimism corrected): ", style = "font-size:19px"), 
                p("(1) Model of low disease activity or remission: AUC = 0.72; calibration intercept = 0.03, slope = 0.98.", style = "font-size:19px"),
                br(),
                div(img(src = "stage1_lda.png", height = "80%", width = "60%"), align = "center"),
                br(),
                p("(2) Model of ACR50: AUC = 0.68; calibration intercept = 0.05, slope = 1.12.", style = "font-size:19px"),
                br(),
                div(img(src = "stage1_acr.png", height = "80%", width = "60%"), align = "center"),
                br(),
                tags$hr(style = "border-color: lightgray;"),
                br(),
                h4("Stage 2:"),
                p("This is a Bayesian IPD meta-regression model. The baseline expected probability of the outcome estimated at Stage 1 was used as 
                  both a prognostic factor and an effect modifier. The intercept is assumed independent across trials, 
                  and all the other coefficients are assumed exchangeable across trials. The equations can be found in the Appendix of the paper.", style = "font-size:19px")
                )
            )
          )
        )
      ),
        tabPanel("Appendix: HAQ-DI",
          sidebarLayout(
            sidebarPanel(width = 9,
              strong("Please check the ONE BEST description for your abilities ", 
                     span("OVER THE PAST WEEK:", style = "color:steelblue"), style = "font-size: 16px"),
              tags$hr(style = "border-color:gray;"),
              fluidRow(
                column(5,
                  strong("1. DRESSING & GROOMING", style = "font-size: 15px"),
                  selectInput("q11", label = "(1) Were you able to dress yourself, including tying shoelaces and doing buttons?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                       "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q12", label = "(2) Were you able to shampoo your hair?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q1a", label = "I needed AIDS OR DEVICES to help with dressing or grooming (e.g., button hook, zipper puller).", value = FALSE),
                  checkboxInput("q1h", label = "I needed HELP FROM ANOTHER PERSON for dressing or grooming.", value = FALSE),
                  tags$hr(style = "border-color:lightgray;"),
                  strong("2. ARISING", style = "font-size: 15px"),
                  selectInput("q21", label = "(1) Were you able to stand up from an armless chair?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q22", label = "(2) Were you able to get in and out of bed?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q2a", label = "I needed AIDS OR DEVICES to help with arising.", value = FALSE),
                  checkboxInput("q2h", label = "I needed HELP FROM ANOTHER PERSON for arising.", value = FALSE),
                  tags$hr(style = "border-color:lightgray;"),
                  strong("3. EATING", style = "font-size: 15px"),
                  selectInput("q31", label = "(1) Were you able to cut your meat?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q32", label = "(2) Were you able to lift a full cup or glass to your mouth?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q33", label = "(3) Were you able to open a new milk carton?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q3a", label = "I needed AIDS OR DEVICES to help with eating (e.g., built-up or special utensils).", value = FALSE),
                  checkboxInput("q3h", label = "I needed HELP FROM ANOTHER PERSON for eating.", value = FALSE),
                  tags$hr(style = "border-color:lightgray;"),
                  strong("4. WALKING", style = "font-size: 15px"),
                  selectInput("q41", label = "(1) Were you able to walk outdoors on flat ground?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q42", label = "(2) Were you able to climb up five steps?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q4a", label = "I needed AIDS OR DEVICES to help with walking (e.g., cane, walker, wheelchair, etc.).", value = FALSE),
                  checkboxInput("q4h", label = "I needed HELP FROM ANOTHER PERSON for walking.", value = FALSE)),
                column(5, offset = 1,
                  strong("5. HYGIENE", style = "font-size: 15px"),
                  selectInput("q51", label = "(1) Were you able to wash and dry your entire body?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q52", label = "(2) Were you able to take a tub bath?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q53", label = "(3) Were you able to get on and off the toilet?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q5a", label = "I needed AIDS OR DEVICES to help with hygiene (e.g., bathtub seat/bar or raised toilet seat).", value = FALSE),
                  checkboxInput("q5h", label = "I needed HELP FROM ANOTHER PERSON for hygiene.", value = FALSE),
                  tags$hr(style = "border-color:lightgray;"),
                  strong("6. REACH", style = "font-size: 15px"),
                  selectInput("q61", label = "(1) Were you able to reach and get a 5 pound object (such as a bag of sugar) from just above your head?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q62", label = "(2) Were you able to bend down and pick up clothing from the floor?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q6a", label = "I needed AIDS OR DEVICES to help with reach (e.g., long-handled appliances).", value = FALSE),
                  checkboxInput("q6h", label = "I needed HELP FROM ANOTHER PERSON for reach.", value = FALSE),
                  tags$hr(style = "border-color:lightgray;"),
                  strong("7. GRIP", style = "font-size: 15px"),
                  selectInput("q71", label = "(1) Were you able to open car doors?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q72", label = "(2) Were you able to open jars which have been previously opened?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q73", label = "(3) Were you able to turn faucets on and off?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q7a", label = "I needed AIDS OR DEVICES to help with gripping (e.g., jar openers).", value = FALSE),
                  checkboxInput("q7h", label = "I needed HELP FROM ANOTHER PERSON for gripping and opening things.", value = FALSE),
                  tags$hr(style = "border-color:lightgray;"),
                  strong("8. ACTIVITIES", style = "font-size: 15px"),
                  selectInput("q81", label = "(1) Were you able to run errands and shop?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q82", label = "(2) Were you able to get in and out of a car?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  selectInput("q83", label = "(3) Were you able to do chores such as vacuuming, yard work?",
                              choices = c("Without ANY difficulty" = 0, "With SOME difficulty" = 1,
                                          "With MUCH difficulty" = 2, "Unable to do" = 3), selected = 0),
                  checkboxInput("q8a", label = "I needed AIDS OR DEVICES to help with the above activities.", value = FALSE),
                  checkboxInput("q8h", label = "I needed HELP FROM ANOTHER PERSON for activities.", value = FALSE),
                  tags$hr(style = "border-color: lightgray;"),
                  actionButton("submit", label = "Submit")
              )
              )
                   ),
            
            mainPanel(width = 3,
              p("The HAQ-DI (Health Assessment Questionnaire Disability Index) is commonly used to measure the functional ability
                among rheumatoid arthritis patients [1].", style = "font-size:19px"),
              br(),
              p("Your score will be shown below, after you finish checking all the items, and submit your answers.", style = "font-size:16px; color:steelblue"),
              br(),
              br(),
              uiOutput("haq"),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              tags$hr(style = "border-color:lightgray;"),
              p("[1] Fries JF, Spitz P, Kraines RG, Holman HR. Measurement of patient outcome in arthritis. Arthritis Rheum 1980;23:137-45.", 
                style = "font-size: 14px; color: grey")
            )
         )
      )
    )
  ),
  fluidRow(column(2,div(style = "height:700px;background-color:rgba(0,0,0,0);"))),
  fluidRow(
    fillRow(div(style = "height:110px;background-color:rgba(229,232,235,1);")),
    br(),
    column(1),
    column(1,
           br()),
    column(10, 
           strong("Please cite:", style = "color:rgba(21,45,70,1)"),
           p("Luo Y, Chalkou K, Funada S, Salanti G, Furukawa TA. 
             Predicting the Add-on Treatment Effect of Certolizumab for Patients with Rheumatoid Arthritis: 
             A Two-Stage Individual Participant Data Meta-Analysis Prediction Model.", em(" (Submitted)"), style = "font-size: 14px; color:rgba(21,45,70,1)")
    )
  )
)

server <- function(input, output, session) {
  
  stage1Input <- reactive({
    bmi <- as.numeric(input$weight)/(as.numeric(input$height)/100)^2
    stage1$input <- c(1, as.numeric(input$sex), log(as.numeric(input$age)), as.numeric(bmi), 
                      log(as.numeric(input$duration)), as.numeric(input$dmard), as.numeric(input$biologic), 
                      (as.numeric(input$tjc)+1)^0.6, as.numeric(input$sjc), as.numeric(input$patg),
                      as.numeric(input$phyg), as.numeric(input$pain), log1p(as.numeric(input$crp)), as.numeric(input$esr), 
                      1/(as.numeric(input$rf)+1)^0.1, as.numeric(input$haq), as.numeric(input$fatig))
    base_logitLDA <- sum(as.numeric(stage1$input)*as.numeric(stage1$coef.lda))
    base_riskLDA <- exp(base_logitLDA)/(1+exp(base_logitLDA))
    base_logitACR <- sum(stage1$input*stage1$coef.acr)
    base_riskACR <- exp(base_logitACR)/(1+exp(base_logitACR))
    stage1 <- c("base_riskLDA"=base_riskLDA, "base_riskACR"=base_riskACR,
                "base_logitLDA"=base_logitLDA, "base_logitACR"=base_logitACR)
  })
  
  stage2Input <- reactive({
    
    p_placeboLDA <- stage2.lda$estimate[stage2.lda$var=="p_new_placebo" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    p_placeboLDA.lo <- stage2.lda$lower[stage2.lda$var=="p_new_placebo" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    p_placeboLDA.up <- stage2.lda$upper[stage2.lda$var=="p_new_placebo" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    p_ctzLDA <- stage2.lda$estimate[stage2.lda$var=="p_new_ctz" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    p_ctzLDA.lo <- stage2.lda$lower[stage2.lda$var=="p_new_ctz" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    p_ctzLDA.up <- stage2.lda$upper[stage2.lda$var=="p_new_ctz" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    rdLDA <- stage2.lda$estimate[stage2.lda$var=="rd" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    rdLDA.lo <- stage2.lda$lower[stage2.lda$var=="rd" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    rdLDA.up <- stage2.lda$upper[stage2.lda$var=="rd" & stage2.lda$risk.bas==round(stage1Input()[["base_riskLDA"]],2)]
    
    p_placeboACR <- stage2.acr$estimate[stage2.acr$var=="p_new_placebo" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    p_placeboACR.lo <- stage2.acr$lower[stage2.acr$var=="p_new_placebo" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    p_placeboACR.up <- stage2.acr$upper[stage2.acr$var=="p_new_placebo" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    p_ctzACR <- stage2.acr$estimate[stage2.acr$var=="p_new_ctz" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    p_ctzACR.lo <- stage2.acr$lower[stage2.acr$var=="p_new_ctz" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    p_ctzACR.up <- stage2.acr$upper[stage2.acr$var=="p_new_ctz" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    rdACR <- stage2.acr$estimate[stage2.acr$var=="rd" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    rdACR.lo <- stage2.acr$lower[stage2.acr$var=="rd" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    rdACR.up <- stage2.acr$upper[stage2.acr$var=="rd" & stage2.acr$risk.bas==round(stage1Input()[["base_riskACR"]],2)]
    
    stage2 <- c("p_placeboLDA"=p_placeboLDA, "p_placeboLDA.lo"=p_placeboLDA.lo, "p_placeboLDA.up"=p_placeboLDA.up,  
                "p_ctzLDA"=p_ctzLDA, "p_ctzLDA.lo"=p_ctzLDA.lo, "p_ctzLDA.up"=p_ctzLDA.up, 
                "p_placeboACR"=p_placeboACR, "p_placeboACR.lo"=p_placeboACR.lo, "p_placeboACR.up"=p_placeboACR.up,
                "p_ctzACR"=p_ctzACR, "p_ctzACR.lo"=p_ctzACR.lo, "p_ctzACR.up"=p_ctzACR.up,
                "rdLDA"=rdLDA, "rdLDA.lo"=rdLDA.lo, "rdLDA.up"=rdLDA.up,
                "rdACR"=rdACR, "rdACR.lo"=rdACR.lo, "rdACR.up"=rdACR.up)
    
  })
  
  stage2df_lda <- reactive({
    df <- data.frame("treat" = c("Placebo+csDMARDs", "CTZ+csDMARDs"),
                     "p" = c(round(stage2Input()[["p_placeboLDA"]]*100, 2), round(stage2Input()[["p_ctzLDA"]]*100, 2)),
                     "lo" = c(round(stage2Input()[["p_placeboLDA.lo"]]*100, 2), round(stage2Input()[["p_ctzLDA.lo"]]*100, 2)),
                     "up" = c(round(stage2Input()[["p_placeboLDA.up"]]*100, 2), round(stage2Input()[["p_ctzLDA.up"]]*100, 2)))
  })
  
  stage2df_acr <- reactive({
    df <- data.frame("treat" = c("Placebo+csDMARDs", "CTZ+csDMARDs"),
                     "p" = c(round(stage2Input()[["p_placeboACR"]]*100, 2), round(stage2Input()[["p_ctzACR"]]*100, 2)),
                     "lo" = c(round(stage2Input()[["p_placeboACR.lo"]]*100, 2), round(stage2Input()[["p_ctzACR.lo"]]*100, 2)),
                     "up" = c(round(stage2Input()[["p_placeboACR.up"]]*100, 2), round(stage2Input()[["p_ctzACR.up"]]*100, 2)))
  })
  
  p1ldaInput <- reactive({
    pred_p_plot <- subset(stage2.lda, var == "p_new_ctz" | var == "p_new_placebo" )
    df_breaks <- data.frame(start=c(-Inf, 0.0288238, 0.8680566),
                            end=c(0.0288238, 0.8680566, Inf),
                            fill=c("out", "in", "out"))
    
    p1 <- ggplot(data = pred_p_plot, aes(x=risk.bas, y=estimate)) +
      ggtitle("(A) Predicted probability of the outcome after 3-month treatment") +
      xlab("Baseline expected probability of low disease activity or remission at stage one") +
      ylab("Predicted probability of low disease activity or remission after treatment at 3-month") +
      scale_y_continuous(limits = c(-0.03, 1), breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
      geom_point(aes(group=var, col=var), size=1, alpha=0.7) +
      scale_color_manual(name="Treatment", 
                         values=c("deepskyblue3", "goldenrod1"),
                         labels=c("CTZ+csDMARDs", "Placebo+csDMARDs")) +
      geom_ribbon(aes(ymin=lower, ymax=upper, group=var, fill=var), alpha=0.1) +
      scale_fill_manual(values=c("deepskyblue1", "goldenrod1"), guide="none") +
      geom_vline(xintercept=c(0.0288238, 0.8680566), col="darkgray", lty=2) +
      new_scale_fill() +
      geom_rect(data = df_breaks, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=fill), 
                inherit.aes = FALSE) + scale_fill_manual(values = alpha(c("white","gray"), 0.2), guide="none") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.2),
                         axis.title = element_text(size = 13), legend.text = element_text(size = 12),
                         axis.title.x = element_text(margin = margin(t = 15))) +
      coord_cartesian(clip = "off") 
    
  })
  
  p2ldaInput <- reactive({
    rd_plot <- subset(stage2.lda, var == "rd")
    df_breaks <- data.frame(start=c(-Inf, 0.0288238, 0.8680566),
                            end=c(0.0288238, 0.8680566, Inf),
                            fill=c("out", "in", "out"))
    
    p2 <- ggplot() +
      ggtitle("(B) Predicted risk difference between two treatments after 3-month") +
      xlab("Baseline expected probability of low disease activity or remission at stage one)") +
      ylab("Predicted risk difference after treatment at 3-month") +
      scale_y_continuous(limits = c(-0.03, 1), breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
      geom_point(data = rd_plot, aes(x=risk.bas, y=estimate, group=var, col=var), size=2, alpha=0.7) +
      scale_color_manual(name="Effect measure", 
                         values=c("red2"),
                         labels=c("Risk difference")) +
      geom_vline(xintercept=c(0.0288238, 0.8680566), col="darkgray", lty=2) +
      geom_ribbon(data=rd_plot, aes(x=risk.bas, y=estimate, ymin=lower, ymax=upper), fill="red2", alpha=0.1) +
      geom_rect(data = df_breaks, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=fill), 
                inherit.aes = FALSE) + scale_fill_manual(values = alpha(c("white","gray"), 0.2), guide="none") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.2),
                         axis.title = element_text(size = 13), legend.text = element_text(size = 12),
                         axis.title.x = element_text(margin = margin(t = 15))) +
      coord_cartesian(clip = "off") 
    
  })
  
  p1acrInput <- reactive({
    pred_p_plot <- subset(stage2.acr, var == "p_new_ctz" | var == "p_new_placebo" )
    df_breaks <- data.frame(start=c(-Inf, 0.0727, 0.7922),
                            end=c(0.0727, 0.7922, Inf),
                            fill=c("out", "in", "out"))
    
    p1 <- ggplot(data = pred_p_plot, aes(x=risk.bas, y=estimate)) +
      ggtitle("(A) Predicted probability of the outcome after 3-month treatment") +
      xlab("Baseline expected probability of ACR50 response at stage one") +
      ylab("Predicted probability of ACR50 response after treatment at 3-month") +
      scale_y_continuous(limits = c(-0.15, 1), breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
      geom_point(aes(group=var, col=var), size=1, alpha=0.7) +
      scale_color_manual(name="Treatment", 
                         values=c("deepskyblue3", "goldenrod1"),
                         labels=c("CTZ+csDMARDs", "Placebo+csDMARDs")) +
      geom_ribbon(aes(ymin=lower, ymax=upper, group=var, fill=var), alpha=0.1) +
      scale_fill_manual(values=c("deepskyblue1", "goldenrod1"), guide="none") +
      geom_vline(xintercept=c(0.0727, 0.7922), col="darkgray", lty=2) +
      new_scale_fill() +
      geom_rect(data = df_breaks, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=fill), 
                inherit.aes = FALSE) + scale_fill_manual(values = alpha(c("white","gray"), 0.2), guide="none") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.2),
                         axis.title = element_text(size = 13), legend.text = element_text(size = 12),
                         axis.title.x = element_text(margin = margin(t = 15))) +
      coord_cartesian(clip = "off")
    
  })
  
  p2acrInput <- reactive({
    rd_plot <- subset(stage2.acr, var == "rd")
    df_breaks <- data.frame(start=c(-Inf, 0.0727, 0.7922),
                            end=c(0.0727, 0.7922, Inf),
                            fill=c("out", "in", "out"))
    
    p2 <- ggplot() +
      ggtitle("(B) Predicted risk difference between two treatments after 3-month") +
      xlab("Baseline expected probability of ACR50 response at stage one)") +
      ylab("Predicted risk difference after treatment at 3-month") +
      scale_y_continuous(limits = c(-0.15, 1), breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
      geom_point(data = rd_plot, aes(x=risk.bas, y=estimate, group=var, col=var), size=2, alpha=0.7) +
      scale_color_manual(name="Effect measure", 
                         values=c("red2"),
                         labels=c("Risk difference")) +
      geom_vline(xintercept=c(0.0727, 0.7922), col="darkgray", lty=2) +
      geom_ribbon(data=rd_plot, aes(x=risk.bas, y=estimate, ymin=lower, ymax=upper), fill="red2", alpha=0.1) +
      geom_rect(data = df_breaks, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=fill), 
                inherit.aes = FALSE) + scale_fill_manual(values = alpha(c("white","gray"), 0.2), guide="none") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.2),
                         axis.title = element_text(size = 13), legend.text = element_text(size = 12),
                         axis.title.x = element_text(margin = margin(t = 15))) +
      coord_cartesian(clip = "off") 
    
  })
  
 
  output$base_riskLDA <- renderText({
    validate(
      need(as.numeric(input$age)>=18 & as.numeric(input$age)<=100, "Please input your age (18~100 years old)."),
      need(as.numeric(input$height)>100 & as.numeric(input$height)<300, "Please input your height in terms of cm."),
      need(as.numeric(input$weight)>=0, "Please input your weight in terms of kg."),
      need(as.numeric(input$duration)>=0 & as.numeric(input$duration)<=100, "Please input how many years you have been diagnosed with rheumatoid arthritis."),
      need(as.numeric(input$tjc)>=0 & as.numeric(input$tjc)<=28, "Please input a valid tender joint count: 0~28."),
      need(as.numeric(input$sjc)>=0 & as.numeric(input$sjc)<=28, "Please input a valid swollen joint count: 0~28."),
      need(as.numeric(input$crp)>=0, "Please input the CRP result in terms of mg/L."),
      need(as.numeric(input$esr)>=0, "Please input the ESR result in terms of mm/h."),
      need(as.numeric(input$rf)>=0, "Please input the RF result in terms of IU/mL."),
      need(as.numeric(input$haq)>=0 & as.numeric(input$haq)<=3, "Please input a valid HAQ-DI score: 0~3.")
    )
    paste("(1) At stage one, a baseline expected probability of the outcome regardless of treatment is predicted: ", round(stage1Input()[["base_riskLDA"]]*100, digits = 2), "%.")
  })
  
  output$base_riskACR <- renderText({
    validate(
      need(as.numeric(input$age)>=18 & as.numeric(input$age)<=100, "Please input your age (18~100 years old)."),
      need(as.numeric(input$height)>100 & as.numeric(input$height)<300, "Please input your height in terms of cm."),
      need(as.numeric(input$weight)>=0, "Please input your weight in terms of kg."),
      need(as.numeric(input$duration)>=0 & as.numeric(input$duration)<=100, "Please input how many years you have been diagnosed with rheumatoid arthritis."),
      need(as.numeric(input$tjc)>=0 & as.numeric(input$tjc)<=28, "Please input a valid tender joint count: 0~28."),
      need(as.numeric(input$sjc)>=0 & as.numeric(input$sjc)<=28, "Please input a valid swollen joint count: 0~28."),
      need(as.numeric(input$crp)>=0, "Please input the CRP result in terms of mg/L."),
      need(as.numeric(input$esr)>=0, "Please input the ESR result in terms of mm/h."),
      need(as.numeric(input$rf)>=0, "Please input the RF result in terms of IU/mL."),
      need(as.numeric(input$haq)>=0 & as.numeric(input$haq)<=3, "Please input a valid HAQ-DI score: 0~3.")
    )
    paste("(1) At stage one, a baseline expected probability of the outcome regardless of treatment is predicted: ", round(stage1Input()[["base_riskACR"]]*100, digits = 2), "%.")
  })
  
  
  output$pLDA <- renderUI({
    validate(
      need(as.numeric(input$age)>=18 & as.numeric(input$age)<=100, "Please input your age (18~100 years old)."),
      need(as.numeric(input$height)>100 & as.numeric(input$height)<300, "Please input your height in terms of cm."),
      need(as.numeric(input$weight)>=0, "Please input your weight in terms of kg."),
      need(as.numeric(input$duration)>=0 & as.numeric(input$duration)<=100, "Please input how many years you have been diagnosed with rheumatoid arthritis."),
      need(as.numeric(input$tjc)>=0 & as.numeric(input$tjc)<=28, "Please input a valid tender joint count: 0~28."),
      need(as.numeric(input$sjc)>=0 & as.numeric(input$sjc)<=28, "Please input a valid swollen joint count: 0~28."),
      need(as.numeric(input$crp)>=0, "Please input the CRP result in terms of mg/L."),
      need(as.numeric(input$esr)>=0, "Please input the ESR result in terms of mm/h."),
      need(as.numeric(input$rf)>=0, "Please input the RF result in terms of IU/mL."),
      need(as.numeric(input$haq)>=0 & as.numeric(input$haq)<=3, "Please input a valid HAQ-DI score: 0~3.")
    )

    placebo.str <- paste("If you use ", "<font color=\"#FFC125\">", "csDMARDs alone", "</font>", ", the probability of achieving low disease activity or remission in 3 months will be", 
                         round(stage2Input()[["p_placeboLDA"]]*100, digits = 2), "% (95%CrI, ", 
                         round(stage2Input()[["p_placeboLDA.lo"]]*100, digits = 2), "% to ",
                         round(stage2Input()[["p_placeboLDA.up"]]*100, digits = 2), "%).")
    ctz.str <- paste("If you use ", "<font color=\"#009ACD\">", "csDMARDs plus certolizumab", "</font>", ", the probability of achieving low disease activity or remission in 3 months will be", 
                     round(stage2Input()[["p_ctzLDA"]]*100, digits = 2), "% (95%CrI, ",
                     round(stage2Input()[["p_ctzLDA.lo"]]*100, digits = 2), "% to ",
                     round(stage2Input()[["p_ctzLDA.up"]]*100, digits = 2), "%).")
    rd.str <- paste("The ", "<font color=\"#EE0000\">", "risk difference", "</font>", " is ", 
                    round(stage2Input()[["rdLDA"]]*100, digits = 2), "% (95%CrI, ", 
                    round(stage2Input()[["rdLDA.lo"]]*100, digits = 2), "% to ", 
                    round(stage2Input()[["rdLDA.up"]]*100, digits = 2), "%).")
    HTML(paste(placebo.str, ctz.str, rd.str, sep = '<br/><br/>'))
  })
  
  output$pACR <- renderUI({
    validate(
      need(as.numeric(input$age)>=18 & as.numeric(input$age)<=100, "Please input your age (18~100 years old)."),
      need(as.numeric(input$height)>100 & as.numeric(input$height)<300, "Please input your height in terms of cm."),
      need(as.numeric(input$weight)>=0, "Please input your weight in terms of kg."),
      need(as.numeric(input$duration)>=0 & as.numeric(input$duration)<=100, "Please input how many years you have been diagnosed with rheumatoid arthritis."),
      need(as.numeric(input$tjc)>=0 & as.numeric(input$tjc)<=28, "Please input a valid tender joint count: 0~28."),
      need(as.numeric(input$sjc)>=0 & as.numeric(input$sjc)<=28, "Please input a valid swollen joint count: 0~28."),
      need(as.numeric(input$crp)>=0, "Please input the CRP result in terms of mg/L."),
      need(as.numeric(input$esr)>=0, "Please input the ESR result in terms of mm/h."),
      need(as.numeric(input$rf)>=0, "Please input the RF result in terms of IU/mL."),
      need(as.numeric(input$haq)>=0 & as.numeric(input$haq)<=3, "Please input a valid HAQ-DI score: 0~3.")
    )
    
    placebo.str <- paste("If you use ", "<font color=\"#FFC125\">", "csDMARDs alone", "</font>", ", the probability of achieving 50% improvement (ACR50) in 3 months will be", 
                         round(stage2Input()[["p_placeboACR"]]*100, digits = 2), "% (95%CrI, ", 
                         round(stage2Input()[["p_placeboACR.lo"]]*100, digits = 2), "% to ",
                         round(stage2Input()[["p_placeboACR.up"]]*100, digits = 2), "%).")
    ctz.str <- paste("If you use ", "<font color=\"#009ACD\">", "csDMARDs plus certolizumab", "</font>", ", the probability of achieving 50% improvement (ACR50) in 3 months will be", 
                     round(stage2Input()[["p_ctzACR"]]*100, digits = 2), "% (95%CrI, ",
                     round(stage2Input()[["p_ctzACR.lo"]]*100, digits = 2), "% to ",
                     round(stage2Input()[["p_ctzACR.up"]]*100, digits = 2), "%).")
    rd.str <- paste("The ", "<font color=\"#EE0000\">", "risk difference", "</font>", " is ", 
                    round(stage2Input()[["rdACR"]]*100, digits = 2), "% (95%CrI,", 
                    round(stage2Input()[["rdACR.lo"]]*100, digits = 2), "% to ", 
                    round(stage2Input()[["rdACR.up"]]*100, digits = 2), "%).")
    HTML(paste(placebo.str, ctz.str, rd.str, sep = '<br/><br/>'))
  })
  
  output$barplot_lda <- renderPlot({
    validate(
      need(as.numeric(input$age)>=18 & as.numeric(input$age)<=100, "Please input your age (18~100 years old)."),
      need(as.numeric(input$height)>100 & as.numeric(input$height)<300, "Please input your height in terms of cm."),
      need(as.numeric(input$weight)>=0, "Please input your weight in terms of kg."),
      need(as.numeric(input$duration)>=0 & as.numeric(input$duration)<=100, "Please input how many years you have been diagnosed with rheumatoid arthritis."),
      need(as.numeric(input$tjc)>=0 & as.numeric(input$tjc)<=28, "Please input a valid tender joint count: 0~28."),
      need(as.numeric(input$sjc)>=0 & as.numeric(input$sjc)<=28, "Please input a valid swollen joint count: 0~28."),
      need(as.numeric(input$crp)>=0, "Please input the CRP result in terms of mg/L."),
      need(as.numeric(input$esr)>=0, "Please input the ESR result in terms of mm/h."),
      need(as.numeric(input$rf)>=0, "Please input the RF result in terms of IU/mL."),
      need(as.numeric(input$haq)>=0 & as.numeric(input$haq)<=3, "Please input a valid HAQ-DI score: 0~3.")
    )
    
    ggplot(data = stage2df_lda(), aes(x=treat, y=p, fill=treat)) +
      ylab("Probability of low disease activity or remission after 3-month treatment") +
      scale_x_discrete("Treatment", c("Placebo+csDMARDs", "CTZ+csDMARDs")) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0,20,40,60,80,100)) + 
      scale_fill_manual(values = alpha(c("deepskyblue2", "goldenrod1"), 0.8), guide = "none") +
      geom_bar(stat = "identity", width=0.7) +
      geom_errorbar(aes(ymin=lo, ymax=up), width=0.2) +
      theme_bw() + theme(axis.text = element_text(size = 13),
                         axis.title = element_text(size = 15))
  })
  
  output$barplot_acr <- renderPlot({
    validate(
      need(as.numeric(input$age)>=18 & as.numeric(input$age)<=100, "Please input your age (18~100 years old)."),
      need(as.numeric(input$height)>100 & as.numeric(input$height)<300, "Please input your height in terms of cm."),
      need(as.numeric(input$weight)>=0, "Please input your weight in terms of kg."),
      need(as.numeric(input$duration)>=0 & as.numeric(input$duration)<=100, "Please input how many years you have been diagnosed with rheumatoid arthritis."),
      need(as.numeric(input$tjc)>=0 & as.numeric(input$tjc)<=28, "Please input a valid tender joint count: 0~28."),
      need(as.numeric(input$sjc)>=0 & as.numeric(input$sjc)<=28, "Please input a valid swollen joint count: 0~28."),
      need(as.numeric(input$crp)>=0, "Please input the CRP result in terms of mg/L."),
      need(as.numeric(input$esr)>=0, "Please input the ESR result in terms of mm/h."),
      need(as.numeric(input$rf)>=0, "Please input the RF result in terms of IU/mL."),
      need(as.numeric(input$haq)>=0 & as.numeric(input$haq)<=3, "Please input a valid HAQ-DI score: 0~3.")
    )
    
    ggplot(data = stage2df_acr(), aes(x=treat, y=p, fill=treat)) +
      ylab("Probability of ACR50 response after 3-month treatment") +
      scale_x_discrete("Treatment", c("Placebo+csDMARDs", "CTZ+csDMARDs")) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0,20,40,60,80,100)) + 
      scale_fill_manual(values = alpha(c("deepskyblue2", "goldenrod1"), 0.8), guide = "none") +
      geom_bar(stat = "identity", width=0.7) +
      geom_errorbar(aes(ymin=lo, ymax=up), width=0.2) +
      theme_bw() + theme(axis.text = element_text(size = 13),
                         axis.title = element_text(size = 15))
  })
  
 
  output$plot_lda <- renderPlot({
    p1 <- p1ldaInput() + geom_vline(xintercept=stage1Input()[["base_riskLDA"]], col="hotpink2", lty=2, size=0.8)
    p2 <- p2ldaInput() + geom_vline(xintercept=stage1Input()[["base_riskLDA"]], col="hotpink2", lty=2, size=0.8)
    p <- ggarrange(p1, p2, ncol=1)
    
  })
  
  output$plot_acr <- renderPlot({
    p1 <- p1acrInput() + geom_vline(xintercept=stage1Input()[["base_riskACR"]], col="hotpink2", lty=2, size=0.8)
    p2 <- p2acrInput() + geom_vline(xintercept=stage1Input()[["base_riskACR"]], col="hotpink2", lty=2, size=0.8)
    
    p <- ggarrange(p1, p2, ncol=1)
    
  })
  
  
  haqInput <- reactive({
    df.tmp <- data.frame("q1" = c(input$q11, input$q12, -1, input$q1a, input$q1h), "q2" = c(input$q21, input$q22, -1, input$q2a, input$q2h),
                         "q3" = c(input$q31, input$q32, input$q33, input$q3a, input$q3h), "q4" = c(input$q41, input$q42, -1, input$q4a, input$q4h),
                         "q5" = c(input$q51, input$q52, input$q53, input$q5a, input$q5h), "q6" = c(input$q61, input$q62, -1, input$q6a, input$q6h),
                         "q7" = c(input$q71, input$q72, input$q73, input$q7a, input$q7h), "q8" = c(input$q81, input$q82, input$q83, input$q8a, input$q8h))
    
  })

  observeEvent(input$submit, {
    output$haq <- renderUI({
      haq.list <- c()
      for (i in 1:8) {
        haq.list[[i]] <- max(haqInput()[c(1:3), paste0("q",i)])
        
        if ((haq.list[[i]]<2) & (haqInput()[4, paste0("q",i)]==TRUE | haqInput()[5, paste0("q",i)]==TRUE)) {
          haq.list[[i]] <- 2
        } else {
          haq.list[[i]] <- haq.list[[i]]
        }
      }
      haq.m <- mean(as.numeric(unlist(haq.list)))
      haq.m <- (round(haq.m*8))/8  # round to 0.125
      paste("Your HAQ-DI score is ", haq.m, ".")
    })
  })
  
  
  
}

shinyApp(ui = ui, server = server)

