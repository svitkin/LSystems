library(shiny)
library(shinyjs)


source("lsys_functions.R")
alphabet <- c("F", "G", "f", "X", "Y", "+", "-")


# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    shiny::tags$script(HTML("
    $(document).ready(function() {
        console.log(new Date(), 'adding rule event handler');
        $(document).on('shiny:sessioninitialized', function(event) {
            var numRules = 1;
            Shiny.onInputChange('numRules', numRules);
        });
        
        $('#sidebar').on('DOMNodeInserted', function(e) {
            if ( $(e.target).hasClass('rule') ) {
                var numRules = $('.rule').length;
                Shiny.onInputChange('numRules', numRules);
                console.log('rule added');
                console.log(numRules);
            }
        });
        $('#sidebar').on('DOMNodeRemoved', function(e) {
            if ( $(e.target).hasClass('rule') ) {
                var numRules = $('.rule').length - 1;
                Shiny.onInputChange('numRules', numRules);
                console.log('rule deleted');
                console.log(numRules);
            }
        });
        
    })")),
    tags$head(
        tags$style(HTML('#genViz {
                            background-color: blue;
                            color: white;
                            text-align: center;
                        }
                        body {
                            background-color: #fbfcdd;
                        }
                        #sidebar {
                            background-color: #fbfcdd;
                            border-left: none;
                            border-top: none;
                            border-bottom: none;
                            border-right: 2px solid grey;
                        }
                        h2 {
                            border: none;
                        }'))
    ),
    # Application title
    titlePanel("L-System"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(uiOutput("sidebar"),
                  mainPanel(plotOutput("lsysViz"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    presets <- read.csv("preset_systems.csv", stringsAsFactors = FALSE)
    
    output$sidebar <-
        renderUI({
            sidebarPanel(id = "sidebar",
                         helpText("Alphabet:  ", paste(alphabet, collapse = ", ")),
                         selectizeInput("presets", "Choose a predefined system",
                                        c("", sort(unique(presets$preset_name)))),
                         textInput("axiom", 
                                   "Starting axiom", 
                                   ""),
                         numericInput("angle", 
                                      "Angle", 
                                      min = 1, 
                                      max = 360,
                                      value = 90),
                         div(class = "rule",
                             id = "rule1",
                             div(style="display:inline-block",
                                 textInput("to_rep1", "Rule 1", "")),
                             div(style="display:inline-block",
                                 textInput("rep1", "", ""))),
                         
                         div(style="display:inline-block", actionButton("addRule", "Add Rule")), 
                         div(style="display:inline-block", actionButton("delRule", "Delete Rule")),
                         sliderInput("depth",
                                     "Depth",
                                     min = 1,
                                     max = 12,
                                     value = 1)
            )
        })
    
    axiom_reactive <- reactive({ input$axiom })
    numRules_reactive <- reactive({ req(input$numRules); input$numRules })
    depth_reactive <- reactive({ input$depth })
    angle_reactive <- reactive({ input$angle })
    presets_reactive <- reactive({ input$presets })
    
    remove_preset <- function() { updateSelectizeInput(session, "presets", selected = "")}
    
    # Invalidate preset
    #observe({
    #    axiom_reactive()
    #    depth_reactive()
    #    angle_reactive()
    #    
    #    remove_preset()
    #})
    
    observeEvent(presets_reactive(), {
        if (presets_reactive() != "") {
            #browser()
            preset <-
                presets %>% 
                filter(preset_name == presets_reactive())
            
            axiom <-
                preset %>% 
                filter(key == "axiom") %>% 
                pull(value)
            
            angle <-
                preset %>% 
                filter(key == "angle") %>% 
                pull(value)
            
            num_rules_data <-
                preset %>% 
                filter(str_detect(key, "rule")) %>% 
                pull(key) %>% 
                length()
            
            rules_to_make <- 1:num_rules_data
            
            # Add rules for the predefined system
            for (rulenum in rules_to_make) {
                # rule values
                to_rep_val <-
                    preset %>% 
                    filter(key == paste0("rule", rulenum)) %>% 
                    pull(value)
                
                rep_val <-
                    preset %>% 
                    filter(key == paste0("rep", rulenum)) %>% 
                    pull(value)
                
                # If the rule ui elements don't exist, then add them
                # Otherwise update existing ui elements
                if (rulenum > numRules_reactive()) {
                    insertUI(selector = paste0("#rule", rulenum - 1), 
                             where = "afterEnd",
                             ui = div(class = "rule", 
                                      id = paste0("rule", rulenum),
                                      div(style="display:inline-block",
                                          textInput(paste0("to_rep", rulenum), 
                                                    paste0("Rule ", rulenum), 
                                                    to_rep_val)),
                                      div(style="display:inline-block",
                                          textInput(paste0("rep", rulenum), 
                                                    "", 
                                                    rep_val))),
                             session = session)
                } else {
                    updateTextInput(session, paste0("to_rep", rulenum), value = to_rep_val)
                    updateTextInput(session, paste0("rep", rulenum), value = rep_val)
                }   
            }
            
            # If there are extra rule ui elements, delete them
            if (num_rules_data < numRules_reactive()) {
                for (extra_rule in (num_rules_data+1):numRules_reactive()) {
                    message(extra_rule)
                    removeUI(paste0("#rule", extra_rule))
                }
            }
            
            # Reset inputs according to presets value
            updateTextInput(session, "axiom", value = axiom)
            updateNumericInput(session, "angle", value = as.numeric(angle))
            updateSliderInput(session, "depth", value = 1)
            
        }
    })
    
    observeEvent(input$addRule, {
        insertUI(selector = paste0("#rule", numRules_reactive()), 
                 where = "afterEnd",
                 ui = div(class = "rule", 
                          id = paste0("rule", numRules_reactive() + 1),
                          div(style="display:inline-block",
                              textInput(paste0("to_rep", numRules_reactive() + 1), 
                                        paste0("Rule ", numRules_reactive() + 1), 
                                        "")),
                          div(style="display:inline-block",
                              textInput(paste0("rep", numRules_reactive() + 1), 
                                        "", 
                                        ""))),
                 session = session)
    })
    
    observeEvent(input$delRule, {
        removeUI(paste0("#rule", numRules_reactive()))
    })
    
    # TODO: Make delete button disabled to start
    observe({
        shinyjs::toggleState("delRule", condition = numRules_reactive() > 1)
    })
    
    processRules <- function(input, numRules) {
        rules <-
            lapply(1:numRules, function(rule_num) {
                input[[paste0("rep", rule_num)]] 
            })
        
        names(rules) <- 
            lapply(paste0("to_rep", 1:numRules), function(to_rep) {
                input[[to_rep]]
            }) %>% 
            unlist()
        
        rules
    }
        
    output$lsysViz <- renderPlot({
        req(numRules_reactive(),
            axiom_reactive(),
            angle_reactive(),
            depth_reactive())
        
        lapply(1:numRules_reactive(), function(rule_num) {
            input[[paste0("to_rep", rule_num)]]
            input[[paste0("rep", rule_num)]]
        })
        
        rules <- processRules(input, numRules_reactive())
        message(expand_system(axiom_reactive(), rules, depth_reactive()))
        # if (presets_reactive() == "Dragon Curve") browser()
        expand_system(axiom_reactive(), rules, depth_reactive()) %>% 
            # TODO: change hardcoding of length
            render_LSystem_string(len = 0.15/depth_reactive(), angle = angle_reactive()) 
        
    })
    
    # TODO: Make error checking on input validation
    # TODO: Make sure visualizations are correct (they're almost definitely not right now)
    # TODO: Change view so all lines are always in middle
    # TODO: Add break process button
    # TODO: When you change preset values, it makes the preset name ""
}

# Run the application 
shinyApp(ui = ui, server = server)
