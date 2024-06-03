# server
## server

function(input, output, session) {
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")

  rv<-reactiveValues(
    u = reactive({}),
    v = reactive({}),
    w = reactive({}),
    x = reactive({}),
    y = reactive({}),
    z = reactive({})
  )

  ## load study site according to siteID
  # studies<-eventReactive(input$sub,{
  #   studies<-tbl(con_admin, "studSite")
  #   studies<-studies%>%collect()
  #   # sel_study<-studies%>%filter(siteID == input$in_siteID)
  #
  # })

  # validate site id
  observeEvent(input$sub,{
    # req(studies)
    # studies<-studies()
    studies<-studies%>%filter(siteID == input$in_siteID)%>%
      arrange(desc(as.POSIXct(siteCREATETIME)))%>%first()
    if(input$in_siteID %in% studies$siteID & studies$siteSTATUS == 1){
      output$cond<-renderUI(
        tagList(
          h5("You have access to this study"),
          actionButton("sub0","load study")
        )
      )

    }else{
      output$cond<-renderUI(
        h5("invalid id or study not active, contact the administrator")
      )
    }
  })

  ## when sub0
  ## extracte site_id
  site_id<-eventReactive(input$sub0,{
    # req(studies)
    # studies<-studies()
    site_id<-input$in_siteID
  })

  ##extract site_type
  site_type<-eventReactive(input$sub0,{
    studies<-studies%>%filter(siteID == input$in_siteID)%>%
      arrange(desc(as.POSIXct(siteCREATETIME)))%>%first()
    site_type<-as.character(studies$siteTYPE)
  })

  sf_stud_geom<-eventReactive(input$sub0,{
    # req(studies)
    # studies<-studies()
    site_id <- site_id()
    sf_stud_geom<-studies%>%filter(siteID == site_id  & siteSTATUS == 1)
    # area_name<-sf_stud_geom$siteDESCR
    sf_stud_geom <- sf::st_as_sf(sf_stud_geom, wkt = "geometry" )%>%st_set_crs(4326)
  })

  grd<-eventReactive(input$sub0,{
    req(sf_stud_geom)
    sf_stud_geom<-sf_stud_geom()
    grd<-st_make_grid(sf_stud_geom, cellsize = 0.05,
                      offset = st_bbox(sf_stud_geom)[1:2],  what = "polygons")
  })



  #switch tab 1
  observeEvent(input$sub0,{

    updateTabsetPanel(session, "inTabset",
                      selected = "p1")
    hideTab(inputId = "inTabset",
            target = "p0")
    showTab(inputId = "inTabset", target = "p1")
  })


  #checkbox gdpr
  observeEvent(input$gdpr,{
    if(input$gdpr == TRUE){
      #remove checkbox and render email
      removeUI(
        selector = "div:has(>> #gdpr)"
      )
      output$cond_0<-renderUI({
        tagList(
          textInput("email","If you want to participate also in the second mapping round, please enter your email address"),
          actionButton("check_mail","submit"),
          uiOutput("cond_1")
        )#/tag

      })#/ui
    }#/if
  })

  ## here check if user is not already present in DB
  observeEvent(input$check_mail,{
    #if no email is provided
    if(input$email == ""){
      removeUI(
        selector = "#check_mail")
      output$cond_1<-renderUI({
        actionButton("sub1","Start the study", class='btn-primary')
      })
    }else{
      show_modal_spinner(
        text = "check mail"
      )
      req(site_id)
      site_id <- site_id()
      user_conf<-tbl(con_admin, "user_conf")%>%collect()
      user_conf<-user_conf%>%filter(siteID == site_id)
      remove_modal_spinner()
      #email already present
      if(input$email %in% user_conf$userMAIL){
        output$cond_1<-renderUI({
          h5("email for this study already present")
        })
      }else{
        removeUI(
          selector = "#check_mail")
        output$cond_1<-renderUI({
          actionButton("sub1","Start the study", class='btn-primary')
        })
      }
    }
  })

  ##create a user ID as soon as start is pressed
  userID<-eventReactive(input$sub1,{
    nchar<-round(runif(1,8,13),0)
    userID<-stri_rand_strings(1, nchar, pattern = "[A-Za-z0-9]")
  })

  ## save user data in the DB and open the questionnaire module
  observeEvent(input$sub1,{
    sf_stud_geom<-sf_stud_geom()
    grd<-grd()
    site_id <- site_id()
    site_type<-site_type()
    show_modal_spinner(
      text = "update data base"
    )
    req(userID)
    userID<-userID()

    ## save user_conf
    user_conf_df<-data.frame(
      userID = userID,
      userMAIL = input$email,
      userTLOG = Sys.time(),
      siteID = site_id,
      projID = project
    )




    #
    user_conf_tab = bq_table(project = "eu-wendy", dataset = dataset, table = 'user_conf')
    bq_table_upload(x = user_conf_tab, values = user_conf_df, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
    #

    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
    hideTab(inputId = "inTabset",
            target = "p1")
    showTab(inputId = "inTabset", target = "p2")
    remove_modal_spinner()
    rv$u<-mod_questionnaire_server("questionnaire",userID, site_id, sf_stud_geom, grd, site_type, con_admin)

    ## create a random subset of all ES in the es table


  })

  ## chreate a randomized order of ES
  # stud_es<-eventReactive(rv$u(),{
  #   stud_es<-stud_es[sample(nrow(stud_es)),]
  # })

  ### instructions
  # training module
  observeEvent(rv$u(),{
    # num_tabs<-num_tabs()
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
    hideTab(inputId = "inTabset",
            target = "p2")
    showTab(inputId = "inTabset", target = "p3")
    rv$v<-mod_instructions_server("training_1")
  })

  pred<-eventReactive(rv$u(),{
    sf_stud_geom<-sf_stud_geom()
    pred <- elevatr::get_elev_raster(locations = sf_stud_geom, z = 9, clip = "locations")
    pred<- load_var(pred)
  })

  #######################
  ### create N tabs

  observeEvent(rv$v(),{
    hideTab(inputId = "inTabset",
            target = "p3")
    # num_tabs<-num_tabs()
    output$tabs <- renderUI({
      do.call(tabsetPanel, c(id="tabs_content",
                             lapply(1:num_tabs, function(i) {
                               tabPanel(title = paste("Mapping ", i), value = paste0("map_", i),
                                        mod_delphi_round1_ui(paste0("mapping_",i))

                               )#/tabpanel
                             })#/lapply
      ))#/do.call
    })#/UI render
  })


  ## hide tabs
  observeEvent(input$tabs_content, {
    # num_tabs<-num_tabs()
    pred<-pred()
    rand_es_sel<-as.data.frame(stud_es[sample(nrow(stud_es)),])
    # rand_es_sel<-stud_es
    sf_stud_geom<-sf_stud_geom()
    # comb<-comb()
    userID<-userID()
    site_id<-site_id()
    # bands<-bands()
    site_type<-site_type()

    for (i in 2:num_tabs) {
      runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
    }

    lapply(1:num_tabs, function(i) {
      rv<-reactiveValues(
        a = reactive({})
      )


      rv$a<-mod_delphi_round1_server(paste0("mapping_",i),
                                     sf_stud_geom,
                                     # comb,
                                     # bands,
                                     rand_es_sel,
                                     as.numeric(i),
                                     userID,
                                     site_id,
                                     # table_con,
                                     site_type,
                                     var_lang,
                                     pred)
      #reactive value from module as event
      observeEvent(rv$a(), {
        next_tab <- i+1


        runjs(paste("$('.nav-tabs li:nth-child(", next_tab, ")').show();"))
        if(next_tab<=num_tabs){
          updateTabsetPanel(session, "tabs_content", selected=paste0("map_",next_tab) )
          runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
        }else{
          removeUI("#tabs")
          # output$ahp_group<-renderUI({
          #   tagList(
          #     mod_ahp_group_ui("ahp_group_1")
          #   )
          # })
        }

      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  }, once = TRUE)



}#/server
