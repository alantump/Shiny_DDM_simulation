ui= fluidPage(    shinyjs::useShinyjs(),
                  headerPanel("Drift diffusion parameters"),
                  sidebarPanel(sliderInput("ndt_value", label = "Non decision time", min=0.1,max=1, value=0.1, step=0.1),
                               sliderInput("bias_value", label = "Bias", min=0.1,max=0.9, value=0.5, step=0.05),
                               sliderInput("drift_value", label = "Drift", min=-2,max=2, value=0, step=0.1),
                               sliderInput("bs_value", label = "Boundary seperation", min=0.1,max=5, value=2, step=0.2),
                               sliderInput("n_agents", label = "Simulated agents", min=1,max=50, value=10, step=2)),
                  #textInput ("value2", label = HTML("<br /><br /><br /><br /><br /> <br /> Bitte nichts eingeben:"))),
                  mainPanel(plotOutput(outputId = "distPlot")))