# ui
fluidPage(theme = shinytheme("flatly"),
  useShinydashboard(),
  useShinyjs(),
  titlePanel(title = div(img(src="wendy.PNG", width ='120'), 'participatory mapping of nature values'), windowTitle = "ES maps" ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Load study", value = "p0",
                          h5("Welcome and thanks for your interest in this study. Please provide your study id that you received from the study administrator"),
                          selectInput("in_siteID","Select a WENDY study area",choices = studies$siteID),
                          actionButton("sub","check access"),
                          uiOutput("cond")
                          ),
              tabPanel(title = "Start", value = "p1",
                       h3("Welcome"),
                       h5("With your information you help us to understand where in Trondheim you and other people use and benefit from various values that nature provides to us as individuals or to our society. After some general information about you and some instructions, you are asked to show us on an interactive map of Trondheim, where you know good areas to benefit form a specific nature value. The different nature values will be explained before you start mapping. In a second round, that will take place a few days later, you will see where others benefit from nature values and if you wish, you can modify and update your areas based on others knowledge."),
                       br(),
                       h5("To contact you as soon as the second mapping round is open, we ask you hereinafter to provide us your email address. We will not use your email address for any other purpose and will anonymize all your data. However, if you don’t want to provide your email address you are welcome to complete just the first mapping round."),
                       br(),
                       h4("About your data"),
                       h5("- Within this study we do not collect any sensitive information"),
                       h5("- We do not present and publish your individual data but statistics and summaries of all participants"),
                       h5("- We ask you about your email address but will anonymize all other data rigorously."),
                       h5("- Your email address will not be shared within any other context outside this study and will be deleted after the survey is completed  date:XY"),
                       h5("- We reuse and share only anonymized data for within the research project pareus"),
                       h5("- We store the data on a google big query database owned by the Norwegian Institute of Nature research (NINA)."),
                       h5("- You have the right to withdraw your data at any time and thus exclude it from the study."),
                       h5("LINK TO GDPR"),
                       br(),
                       checkboxInput("gdpr","I read and understood the form, use and storage of my data",value = F),
                       br(),
                       uiOutput("cond_0")
                       # textInput("email","email"),
                       # uiOutput("cond_1")
                       # actionButton("sub1","start")
              ),
              tabPanel(title = "About you", value = "p2",
                       withSpinner(mod_questionnaire_ui("questionnaire"))
              ),
              tabPanel(title = "Mapping instructions", value = "p3",
                       mod_instructions_ui("training_1")
              )
  ),
  uiOutput("tabs"),
  # uiOutput("ahp_group"),
  # uiOutput("ahp_single"),
  # uiOutput("ahp_dist"),
  # uiOutput("final")

)


