# Load required libraries ----
library(future)
library(promises)
library(rsconnect)
library(shiny)
library(bslib)
library(StatsBombR)
library(dplyr)
library(DT)
library(glue)
library(ggsoccer)
library(ggplot2)
library(tidyr)
library(ggforce)
# For nicer progress messages and disabling buttons:
library(shinyjs)
library(shinydashboard)
library(reticulate)

use_python('C:/Users/barry/AppData/Local/Programs/Python/Python312')  # Adjust this path to your Python installation
py_install("statsbombpy")
py_install("pandas")
py_install("numpy")
py_install("openai")

# Configure future for better memory management
plan(sequential)  # Use sequential plan instead of multisession
options(future.globals.maxSize = 500 * 1024^2)  # Set memory limit to 500MB
options(future.wait.timeout = 60)  # Set timeout to 60 seconds

# Create the UI Object ----
ui <- page_fillable(
  useShinyjs(),  # Enables shinyjs functions to disable/enable buttons
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  navset_card_pill(
    selected = "Landing Page", # creates the tabbed multi-page structure
    #tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    ## Landing Page Content Output ----
    nav_panel("Landing Page",
              titlePanel(HTML("<h1>Stats Portal</h1>"), windowTitle = "Stats Portal"),
              HTML("<h3>Explore matches within the StatsBomb Open Data archive to view individual player statistics.<br><br>
          Follow the instructions below to get started.</h3>
          <h4>
            Instructions: <br><br>
            1. Click 'Get Started' to navigate to the 'Select a Match' page.<br><br>
            2. Use the left dropdown list to select the competition you wish to explore.<br><br>
            3. Click the 'Select' button to confirm your choice.<br><br>
            4. Use the right dropdown list to select the desired season.<br><br>
            5. Select any match from the table and click 'Import Data'.<br><br>
            6. Once imported, click the 'View Data' button to navigate to the player statistics page. Use the tabs to swap between the home and away pages.<br><br>
            7. Click on any player's name to generate statistics and charts. Use the buttons to change the chart type.<br>
            8. Repeat as many times as you like. <br><br>
          </h4>"),
              actionButton(inputId = "btnstart", class = "btnstart", label = "Get Started"),
              HTML("<h5><br><br><br>Data Source: <br></h5>"),
              img(src = "SBLogo.png", class = "logocontainer")
    ),
    
    ## Match Selection Page Content Output ----
    nav_panel("Select a Match",
              layout_columns(
                card(
                  selectInput(
                    inputId = "compDropdown", 
                    label = "Select options below:", 
                    choices = list("Bundesliga" = "1. Bundesliga", "English Premier League" = "Premier League", "Champions League" = "Champions League"),
                    selected = "Bundesliga"
                  ),
                  height = 250,
                ),
                card(
                  actionButton(inputId = "btncompselect", class = "compbtn", label = "Select"),
                  actionButton(inputId = "btnimport", class = "importbtn", label = "Import Data"),
                  uiOutput("viewdata"),
                  height = 250
                ),
                card(
                  uiOutput("seasons"),
                  height = 250
                )
              ),
              dataTableOutput("matchlist")
    ),
    
    ## Match Report
    nav_panel("Match Report", 
        fluidRow(
        column(12,
             div(id = "report_container",
                 style = "margin-top: 20px; padding: 20px; background-color: #f8f9fa; border-radius: 5px;",
                 htmlOutput("match_report"))))),
    
    ## Team Comparison Page ----
    nav_panel("Team Comparison",
              fluidRow(
                # Team Statistics Comparison
                column(12,
                  card(
                    title = "Team Statistics Comparison",
                    HTML("<table class='comparison-table'>
                           <tr>
                             <th style='width: 40%'>Home Team</th>
                             <th style='width: 20%'>Statistic</th>
                             <th style='width: 40%'>Away Team</th>
                           </tr>
                           <tr>
                             <td id='home-passes'></td>
                             <td>Total Passes</td>
                             <td id='away-passes'></td>
                           </tr>
                           <tr>
                             <td id='home-pass-acc'></td>
                             <td>Pass Accuracy</td>
                             <td id='away-pass-acc'></td>
                           </tr>
                           <tr>
                             <td id='home-shots'></td>
                             <td>Total Shots</td>
                             <td id='away-shots'></td>
                           </tr>
                           <tr>
                             <td id='home-sot'></td>
                             <td>Shots on Target</td>
                             <td id='away-sot'></td>
                           </tr>
                           <tr>
                             <td id='home-goals'></td>
                             <td>Goals</td>
                             <td id='away-goals'></td>
                           </tr>
                           <tr>
                             <td id='home-xg'></td>
                             <td>Expected Goals (xG)</td>
                             <td id='away-xg'></td>
                           </tr>
                           <tr>
                             <td id='home-possession'></td>
                             <td>Possession Events</td>
                             <td id='away-possession'></td>
                           </tr>
                           <tr>
                             <td id='home-pressures'></td>
                             <td>Pressures</td>
                             <td id='away-pressures'></td>
                           </tr>
                           <tr>
                             <td id='home-tackles'></td>
                             <td>Tackles</td>
                             <td id='away-tackles'></td>
                           </tr>
                           <tr>
                             <td id='home-interceptions'></td>
                             <td>Interceptions</td>
                             <td id='away-interceptions'></td>
                           </tr>
                         </table>")
                  )
                ),
                # Team Performance Timeline
                column(12,
                  card(
                    title = "Shot Maps",
                    fluidRow(
                      column(6, 
                        h4("Home Team", align = "center"),
                        plotOutput("hometeamshotmap", height = "400px")
                      ),
                      column(6, 
                        h4("Away Team", align = "center"),
                        plotOutput("awayteamshotmap", height = "400px")
                      )
                    )
                  )
                ),
                # Passing Networks
                column(12,
                  card(
                    title = "Passing Networks",
                    fluidRow(
                      column(6, 
                        h4("Home Team", align = "center"),
                        plotOutput("hometeampassnetwork", height = "500px")
                      ),
                      column(6, 
                        h4("Away Team", align = "center"),
                        plotOutput("awayteampassnetwork", height = "500px")
                      )
                    )
                  )
                )
              )
    ),
    
    ## Home Player Statistics Page ----
    nav_panel("Home Players",
              tags$div(
                layout_columns(
                  tags$div(dataTableOutput("homelineuptable")),
                  tags$div(
                    actionButton(inputId = "homepassplotbtn", class = "changeplotbtn", label = "View Passmap"),
                    actionButton(inputId = "hometouchplotbtn", class = "changeplotbtn", label = "View Touchmap"),
                    plotOutput("homechart"),
                    uiOutput("homeplayertable")
                  )
                )
              )
    ),
    
    ## Away Player Statistics Page ----
    nav_panel("Away Players",
              tags$div(
                layout_columns(
                  tags$div(dataTableOutput("awaylineuptable")),
                  tags$div(
                    actionButton(inputId = "awaypassplotbtn", class = "changeplotbtn", label = "View Passmap"),
                    actionButton(inputId = "awaytouchplotbtn", class = "changeplotbtn", label = "View Touchmap"),
                    plotOutput("awaychart"),
                    uiOutput("awayplayertable")
                  )
                )
              )
    ),
    
    nav_menu(
      "Other links", 
      "----", 
      "Description:", 
      nav_item(
        a("Shiny", href = "https://shiny.posit.co", target = "_blank")
      )
    ),
    id = "tab"
  )
)

# Server logic ----
server <- function(input, output, session) {
  
  # Position groups as constants
  DEFENDER_POSITIONS <- c("Left Center Back", "Left Back", "Right Back", "Left Wing Back", "Right Wing Back", "Center Back", "Right Center Back")
  MIDFIELDER_POSITIONS <- c("Right Attacking Midfield", "Right Center Midfield", "Left Center Midfield", "Center Defensive Midfield", "Left Attacking Midfield", "Right Defensive Midfield", "Left Defensive Midfield")
  FORWARD_POSITIONS <- c("Center Forward", "Left Center Forward", "Right Center Forward", "Left Wing", "Right Wing")
  
  # Allow reconnections to avoid disconnects on shinyapps.io during long computations
  session$allowReconnect(TRUE)
  
  # Cleanup function for session end
  onSessionEnded(function() {
    # Clear all reactive values
    selected_events$data <- NULL
    player_summary$table <- NULL
    home_lineup$lineup <- NULL
    away_lineup$lineup <- NULL
    selected_player$homeplayer <- ""
    selected_player$awayplayer <- ""
    
    # Force garbage collection
    gc()
  })
  
  ## Get Started Button ----
  observeEvent(input$btnstart, {
    updateTabsetPanel(session, inputId = "tab", selected = "Select a Match")
  })
  
  ## Display landing page texts (using HTML for proper rendering) ----
  # titleText output moved directly into the titlePanel; the below outputs
  # are used for other texts if needed.
  
  ## Functions to select competitions and matches ----
  
  # API call to get a dataframe of free competitions with error handling
  comps <- tryCatch({
    FreeCompetitions()
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("Could not load competitions:", e$message),
      easyClose = TRUE,
      footer = NULL
    ))
    NULL
  })
  
  ## Create reactive values for data storage ----
  matchlists <- reactiveValues(matchlist = NULL)
  selected_match <- reactiveValues(match_ID = NULL)
  selected_events <- reactiveValues(data = NULL)
  player_summary <- reactiveValues(table = NULL)
  home_lineup <- reactiveValues(lineup = NULL)
  away_lineup <- reactiveValues(lineup = NULL)
  selected_player <- reactiveValues(
    homeplayer = "", homedata = "", homeposition = "",
    awayplayer = "", awaydata = "", awayposition = ""
  )
  
  ## Competition selection ----
  observeEvent(input$btncompselect, {
    req(input$compDropdown)
    req(!is.null(comps))
    
    # Extract competition ID and season names from comps
    chosen_comp_id <- comps %>% 
      filter(competition_name == input$compDropdown) %>% 
      pull(competition_id) %>% unique()
    
    seasons <- comps %>% 
      filter(competition_name == input$compDropdown) %>% 
      select(season_name) %>% unique()
    
    # Create a dynamic seasons dropdown list
    output$seasons <- renderUI({
      if (nrow(seasons) == 0) return(NULL)
      selectInput("seasonchoice", "Select Season", choices = seasons$season_name)
    })
  })
  
  ## Show matches in selected season ----
  observeEvent(input$seasonchoice, {
    req(input$seasonchoice)
    req(!is.null(comps))
    
    chosen_comp_id <- comps %>% 
      filter(competition_name == input$compDropdown) %>% 
      pull(competition_id) %>% unique()
    
    comp <- comps %>% 
      filter(competition_id == chosen_comp_id, season_name == input$seasonchoice)
    
    # Fetch match list from API and update reactive value with error handling
    match_df <- tryCatch({
      FreeMatches(comp) %>% 
        mutate(Score = paste(home_score, "-", away_score)) %>% 
        select(match_date, home_team.home_team_name, away_team.away_team_name, Score, match_id) %>% 
        rename(Date = match_date, "Home Team" = home_team.home_team_name, "Away Team" = away_team.away_team_name) %>% 
        arrange(desc(Date))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Could not load matches:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      NULL
    })
    matchlists$matchlist <- match_df
    
    output$matchlist <- renderDataTable({
      req(matchlists$matchlist)
      datatable(matchlists$matchlist, 
                selection = list(mode = "single", target = "row"),
                options = list(pageLength = 10)
      )
    }, rownames = FALSE)
  })
  
  ## When a match is selected from the table ----
  observeEvent(input$matchlist_rows_selected, {
    req(matchlists$matchlist)
    req(input$matchlist_rows_selected)
    selected_match$match_ID <- matchlists$matchlist[input$matchlist_rows_selected,5]
    
    
    # Re-enable the import button and clear viewdata when a new match is selected
    enable("btnimport")
    output$viewdata <- renderUI({ NULL })
  })
  
  ## Import Button: fetch match event data asynchronously ----
  observeEvent(input$btnimport, {
    req(selected_match$match_ID)
    match_id <- selected_match$match_ID
    disable("btnimport")

    # Show spinner immediately
    output$viewdata <- renderUI({
      tags$div(class = "spinner", tags$div(class = "loader"))
    })

    withProgress(message = "Importing match events...", value = 0, {
      incProgress(0.1)

      # Create a promise that will be resolved with the data
      data_promise <- future({
        # Simple error handling with retry logic
        max_retries <- 3
        retry_count <- 0
        
        while(retry_count < max_retries) {
          tryCatch({
            # Get the events data
            events <- StatsBombFreeEvents(MatchesDF = match_id, Parallel = FALSE)
            
            # Clean the data
            result <- allclean(events)
            
            # If we get here, the operation was successful
            return(result)
          }, error = function(e) {
            retry_count <<- retry_count + 1
            if(retry_count >= max_retries) {
              return(list(error = TRUE, message = paste("Failed after", max_retries, "attempts:", e$message)))
            }
            # Wait a bit before retrying
            Sys.sleep(1)
          })
        }
      })

      # Handle the promise result
      data_promise %...>% (function(result) {
        if (is.list(result) && !is.null(result$error) && result$error) {
          showModal(modalDialog(
            title = "Error",
            paste("There was a problem importing the data:", result$message),
            easyClose = TRUE,
            footer = NULL
          ))
          enable("btnimport")
          output$viewdata <- renderUI({
            tags$div(style = 'color: red; font-weight: bold;', paste("Error:", result$message))
          })
          return()
        }
        
        # Store the processed data
        selected_events$data <- result
        
        incProgress(0.7)
        
        output$viewdata <- renderUI({
          tagList(
            tags$div('Data Imported!'),
            actionButton('btnviewdata', 'View Data', class = 'viewdatabtn')
          )
        })
      }) %...!% (function(err) {
        error_message <- if (inherits(err, "interrupted")) {
          "The data import was interrupted. Please try again."
        } else {
          paste("There was a problem importing the data:", err$message)
        }
        
        showModal(modalDialog(
          title = "Error",
          error_message,
          easyClose = TRUE,
          footer = NULL
        ))
        output$viewdata <- renderUI({
          tags$div(style = 'color: red; font-weight: bold;', error_message)
        })
      }) %...>% finally(function() {
        enable("btnimport")
      })
      
      incProgress(0.2)
    })
  })
  
  # Observe the 'View Data' button to process data and switch to Home Players tab
  observeEvent(input$btnviewdata, {
    req(selected_events$data)
    
    withProgress(message = "Processing match data...", value = 0, {
      incProgress(0.2)
      
      # Process data in chunks to manage memory
      tryCatch({
        # Build player summary table
        player_summary$table <- selected_events$data %>%
          group_by(player.name) %>%
          summarise(
            Carries = sum(type.name == "Carry", na.rm = TRUE),
            Shots = sum(type.name == "Shot", na.rm = TRUE),
            PassesComplete = sum(type.name == "Pass", na.rm = TRUE),
            Pressures = sum(type.name == "Pressure", na.rm = TRUE),
            FoulsWon = sum(type.name == "Foul Won", na.rm = TRUE),
            Interceptions = sum(type.name == "Interception", na.rm = TRUE),
            Dispossessed = sum(type.name == "Dispossessed", na.rm = TRUE),
            Blocks = sum(type.name == "Block", na.rm = TRUE),
            xG = round(sum(shot.statsbomb_xg, na.rm = TRUE), 1),
            Ave_Passes_Length = round(mean(pass.length, na.rm = TRUE), 1),
            Assists = sum(pass.shot_assist == "TRUE", na.rm = TRUE),
            DuelWon = sum(duel.outcome.name %in% c("Won", "Success In Play"), na.rm = TRUE),
            CompleteDribbles = sum(dribble.outcome.name == "Complete", na.rm = TRUE),
            Shots_on_Target = sum(shot.outcome.name %in% c("Goal", "Saved"), na.rm = TRUE),
            Touches = sum(type.name %in% c("Pass", "Shot", "Ball Receipt", "Carry", "Dribble", "Interception"), na.rm = TRUE),
            Tackles = sum(type.name == "Tackle", na.rm = TRUE),
            Clearances = sum(type.name == "Clearance", na.rm = TRUE),
            Goals = sum(type.name == "Shot" & shot.outcome.name == "Goal", na.rm = TRUE),
            BallRecoveries = sum(type.name == "Ball Recovery", na.rm = TRUE), 
            DribblesComplete = sum(type.name == "Dribble" & dribble.outcome.name == "Complete", na.rm = TRUE),
            ShotsFaced = sum(goalkeeper.type.name == "Shot Faced", na.rm = TRUE),
            ShotsSaved = sum(goalkeeper.type.name == "Shot Saved", na.rm = TRUE),
            SuccessfulClaims = sum((goalkeeper.type.name == "Collected" & goalkeeper.outcome.name == "Success") | 
                                     (goalkeeper.type.name == "Keeper Sweeper" & goalkeeper.outcome.name == "Claim"), na.rm = TRUE),
            SuccessfulClearancesGK = sum((goalkeeper.type.name == "Punch" & goalkeeper.outcome.name == "In Play Safe") | 
                                           (goalkeeper.type.name == "Keeper Sweeper" & goalkeeper.outcome.name == "Clear"), na.rm = TRUE),
            GoalsConceded = sum(goalkeeper.type.name == "Goal Conceded", na.rm = TRUE)
          )
        
        gc()
        
        incProgress(0.4)
        
        # Get home and away teams
        home_team <- selected_events$data[1, "team.name"] %>% pull()
        away_team <- selected_events$data[2, "team.name"] %>% pull()
        
        # Get lineups
        get_lineup <- function(match_events, team) {
          lineup <- match_events %>% filter(team.name == team)
          if (nrow(lineup) == 0 || is.null(lineup[1, "tactics.lineup"]) || is.na(lineup[1, "tactics.lineup"])) return(data.frame(Player = character(), Position = character()))
          lineup <- lineup[1, "tactics.lineup"] %>% pull() %>% 
            as.data.frame() %>% 
            select(player.name, position.name) %>% 
            rename(Player = player.name, Position = position.name)
          return(lineup)
        }
        
        home_lineup$lineup <- get_lineup(selected_events$data, home_team)
        away_lineup$lineup <- get_lineup(selected_events$data, away_team)
        
        gc()
        
        incProgress(0.4)
        
        # Switch to Team Comparison tab instead of Home Players
        updateTabsetPanel(session, inputId = "tab", selected = "Team Comparison")
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("There was a problem processing the data:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })
  })
  
  ## Render home and away lineups ----
  output$homelineuptable <- renderDataTable({
    req(home_lineup$lineup)
    datatable(home_lineup$lineup, 
              selection = list(mode = "single", target = "row"),
              options = list(pageLength = 15))
  }, rownames = FALSE)
  
  output$awaylineuptable <- renderDataTable({
    req(away_lineup$lineup)
    datatable(away_lineup$lineup, 
              selection = list(mode = "single", target = "row"),
              options = list(pageLength = 15))
  }, rownames = FALSE)
  
  ## Functions to generate stats tables for different positions ----
  get_defender_table <- function(match_summary, player) {
    player_data <- match_summary %>% filter(player.name == player)
    HTML(glue("<table class='cardtable' align='center'> 
                <tr><th>Touches</th><td>{player_data$Touches}</td></tr>
                <tr><th>Passes</th><td>{player_data$PassesComplete}</td></tr>
                <tr><th>Shots</th><td>{player_data$Shots}</td></tr>
                <tr><th>Duels Won</th><td>{player_data$DuelWon}</td></tr>
                <tr><th>Tackles</th><td>{player_data$Tackles}</td></tr>
                <tr><th>Clearances</th><td>{player_data$Clearances}</td></tr>
                <tr><th>Blocks</th><td>{player_data$Blocks}</td></tr>
                <tr><th>Interceptions</th><td>{player_data$Interceptions}</td></tr>
                <tr><th>Ball Recoveries</th><td>{player_data$BallRecoveries}</td></tr>
                <tr><th>Goals</th><td>{player_data$Goals}</td></tr>
              </table>"))
  }
  
  get_midfielder_table <- function(match_summary, player) {
    player_data <- match_summary %>% filter(player.name == player)
    HTML(glue("<table class='cardtable' align='center'> 
                <tr><th>Touches</th><td>{player_data$Touches}</td></tr>
                <tr><th>Passes</th><td>{player_data$PassesComplete}</td></tr>
                <tr><th>Shots</th><td>{player_data$Shots}</td></tr>
                <tr><th>Duels Won</th><td>{player_data$DuelWon}</td></tr>
                <tr><th>Assists</th><td>{player_data$Assists}</td></tr>
                <tr><th>Avg Pass Length</th><td>{player_data$Ave_Passes_Length}</td></tr>
                <tr><th>Carries</th><td>{player_data$Carries}</td></tr>
                <tr><th>Goals</th><td>{player_data$Goals}</td></tr>
                <tr><th>Dribbles Completed</th><td>{player_data$DribblesComplete}</td></tr>
                <tr><th>Pressures</th><td>{player_data$Pressures}</td></tr>
              </table>"))
  }
  
  get_forward_table <- function(match_summary, player) {
    player_data <- match_summary %>% filter(player.name == player)
    HTML(glue("<table class='cardtable' align='center'> 
                <tr><th>Goals</th><td>{player_data$Goals}</td></tr>
                <tr><th>Touches</th><td>{player_data$Touches}</td></tr>
                <tr><th>Passes</th><td>{player_data$PassesComplete}</td></tr>
                <tr><th>Shots</th><td>{player_data$Shots}</td></tr>
                <tr><th>Assists</th><td>{player_data$Assists}</td></tr>
                <tr><th>xG</th><td>{player_data$xG}</td></tr>
                <tr><th>Fouls Won</th><td>{player_data$FoulsWon}</td></tr>
                <tr><th>Dribbles Completed</th><td>{player_data$DribblesComplete}</td></tr>
                <tr><th>Shots on Target</th><td>{player_data$Shots_on_Target}</td></tr>
                <tr><th>Dispossessed</th><td>{player_data$Dispossessed}</td></tr>
              </table>"))
  }
  
  get_keeper_table <- function(match_summary, player) {
    player_data <- match_summary %>% filter(player.name == player)
    HTML(glue("<table class='cardtable' align='center'> 
                <tr><th>Shots Faced</th><td>{player_data$ShotsFaced}</td></tr>
                <tr><th>Shots Saved</th><td>{player_data$ShotsSaved}</td></tr>
                <tr><th>Successful Claims</th><td>{player_data$SuccessfulClaims}</td></tr>
                <tr><th>Successful Clearances</th><td>{player_data$SuccessfulClearancesGK}</td></tr>
                <tr><th>Goals Conceded</th><td>{player_data$GoalsConceded}</td></tr>
                <tr><th>Touches</th><td>{player_data$Touches}</td></tr>
                <tr><th>Passes</th><td>{player_data$PassesComplete}</td></tr>
              </table>"))
  }
  
  ## Functions to generate charts ----
  get_player_touchmap <- function(match_events, player) {
    player_data <- match_events %>% 
      filter(player.name == player & type.name %in% c("Pressure", "Foul Won", "Foul Committed", "Duel", "Ball Recovery", "Shot"))
    
    # Limit to 500 points for performance
    if (nrow(player_data) > 500) player_data <- player_data[1:500, ]
    
    # Create a color palette for different event types
    event_colors <- c(
      "Pressure" = "#FF6B6B",
      "Foul Won" = "#4ECDC4",
      "Foul Committed" = "#FFE66D",
      "Duel" = "#1A535C",
      "Ball Recovery" = "#7B2CBF",
      "Shot" = "#FF0000"
    )
    
    ggplot(player_data, aes(x = location.x, y = location.y, shape = type.name, color = type.name)) + 
      annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "seagreen3", limits = FALSE) +
      geom_point(size = 4, alpha = 0.7) + 
      scale_y_reverse() +
      scale_color_manual(values = event_colors) +
      scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
      theme_pitch() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5)
      ) +
      labs(
        title = paste("Touch Map -", player),
        subtitle = "Player Event Locations",
        color = "Event Type",
        shape = "Event Type"
      )
  }
  
  get_player_passmap <- function(match_events, player) {
    player_data <- match_events %>% 
      filter(player.name == player & type.name == "Pass") %>% 
      mutate(
        pass_outcome = replace_na(pass.outcome.name, "Completed")
      )
    
    # Limit to 500 points for performance
    if (nrow(player_data) > 500) player_data <- player_data[1:500, ]
    
    # Calculate pass statistics
    total_passes <- nrow(player_data)
    completed_passes <- sum(player_data$pass_outcome == "Completed")
    completion_rate <- round(completed_passes / total_passes * 100, 1)
    
    ggplot(player_data) + 
      annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "seagreen3", limits = FALSE) +
      geom_segment(
        aes(x = location.x, y = location.y, 
            xend = pass.end_location.x, yend = pass.end_location.y,
            color = pass_outcome),
        arrow = arrow(length = unit(0.2, "cm"), type = "closed")
      ) +
      scale_y_reverse() +
      scale_color_manual(values = c("Completed" = "#4ECDC4", "Incomplete" = "darkorange")) +
      theme_pitch() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5),
        plot.caption = element_text(color = "black", size = 10, hjust = 0.5)
      ) +
      labs(
        title = paste("Pass Map -", player),
        subtitle = paste("Pass Completion Rate:", completion_rate, "%"),
        color = "Pass Outcome",
        caption = paste("Total Passes:", total_passes, "| Completed:", completed_passes)
      )
  }
  
  ## Home player selection ----
  observeEvent(input$homelineuptable_rows_selected, {
    req(home_lineup$lineup, player_summary$table)
    selected_player$homeplayer <- home_lineup$lineup[input$homelineuptable_rows_selected, "Player"]
    selected_player$homeposition <- home_lineup$lineup[input$homelineuptable_rows_selected, "Position"]
    
    # Decide which stat table function to use based on position:
    pos <- selected_player$homeposition
    if(pos %in% DEFENDER_POSITIONS) {
      selected_player$homedata <- get_defender_table(player_summary$table, selected_player$homeplayer)
    } else if (pos %in% MIDFIELDER_POSITIONS) {
      selected_player$homedata <- get_midfielder_table(player_summary$table, selected_player$homeplayer)
    } else if (pos %in% FORWARD_POSITIONS) {
      selected_player$homedata <- get_forward_table(player_summary$table, selected_player$homeplayer)
    } else if (pos == "Goalkeeper") {
      selected_player$homedata <- get_keeper_table(player_summary$table, selected_player$homeplayer)
    }
  })
  
  output$homechart <- renderPlot({
    req(selected_events$data, selected_player$homeplayer)
    get_player_passmap(selected_events$data, selected_player$homeplayer)
  })
  
  output$homeplayertable <- renderUI({
    req(selected_player$homedata)
    div(selected_player$homedata)
  })
  
  observeEvent(input$hometouchplotbtn, {
    output$homechart <- renderPlot({
      req(selected_events$data, selected_player$homeplayer)
      get_player_touchmap(selected_events$data, selected_player$homeplayer)
    })
  })
  
  observeEvent(input$homepassplotbtn, {
    output$homechart <- renderPlot({
      req(selected_events$data, selected_player$homeplayer)
      get_player_passmap(selected_events$data, selected_player$homeplayer)
    })
  })
  
  ## Away player selection ----
  observeEvent(input$awaylineuptable_rows_selected, {
    req(away_lineup$lineup, player_summary$table)
    selected_player$awayplayer <- away_lineup$lineup[input$awaylineuptable_rows_selected, "Player"]
    selected_player$awayposition <- away_lineup$lineup[input$awaylineuptable_rows_selected, "Position"]
    
    pos <- selected_player$awayposition
    if(pos %in% DEFENDER_POSITIONS) {
      selected_player$awaydata <- get_defender_table(player_summary$table, selected_player$awayplayer)
    } else if (pos %in% MIDFIELDER_POSITIONS) {
      selected_player$awaydata <- get_midfielder_table(player_summary$table, selected_player$awayplayer)
    } else if (pos %in% FORWARD_POSITIONS) {
      selected_player$awaydata <- get_forward_table(player_summary$table, selected_player$awayplayer)
    } else if (pos == "Goalkeeper") {
      selected_player$awaydata <- get_keeper_table(player_summary$table, selected_player$awayplayer)
    }
  })
  
  output$awaychart <- renderPlot({
    req(selected_events$data, selected_player$awayplayer)
    get_player_passmap(selected_events$data, selected_player$awayplayer)
  })
  
  observeEvent(input$awaytouchplotbtn, {
    output$awaychart <- renderPlot({
      req(selected_events$data, selected_player$awayplayer)
      get_player_touchmap(selected_events$data, selected_player$awayplayer)
    })
  })
  
  observeEvent(input$awaypassplotbtn, {
    output$awaychart <- renderPlot({
      req(selected_events$data, selected_player$awayplayer)
      get_player_passmap(selected_events$data, selected_player$awayplayer)
    })
  })
  
  output$awayplayertable <- renderUI({
    req(selected_player$awaydata)
    div(selected_player$awaydata)
  })
  
  ## Team Analysis Functions ----
  
  # Function to generate team statistics
  get_team_stats <- function(match_events, team_name) {
    team_data <- match_events %>% 
      filter(team.name == team_name) %>%
      summarise(
        Total_Passes = sum(type.name == "Pass", na.rm = TRUE),
        Completed_Passes = sum(type.name == "Pass" & is.na(pass.outcome.name), na.rm = TRUE),
        Pass_Accuracy = round(Completed_Passes / Total_Passes * 100, 1),
        Total_Shots = sum(type.name == "Shot", na.rm = TRUE),
        Shots_on_Target = sum(type.name == "Shot" & shot.outcome.name %in% c("Goal", "Saved"), na.rm = TRUE),
        Goals = sum(type.name == "Shot" & shot.outcome.name == "Goal", na.rm = TRUE),
        xG = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2),
        Possession_Time = sum(type.name %in% c("Pass", "Carry", "Dribble"), na.rm = TRUE),
        Pressures = sum(type.name == "Pressure", na.rm = TRUE),
        Tackles = sum(type.name == "Tackle", na.rm = TRUE),
        Interceptions = sum(type.name == "Interception", na.rm = TRUE),
        
      )
    return(team_data)
  }
  
  # Update comparison table when data is loaded
  observeEvent(selected_events$data, {
    req(selected_events$data)
    
    # Get home and away team names
    home_team <- selected_events$data[1, "team.name"] %>% pull()
    away_team <- selected_events$data[2, "team.name"] %>% pull()
    
    # Calculate statistics for both teams
    home_stats <- get_team_stats(selected_events$data, home_team)
    away_stats <- get_team_stats(selected_events$data, away_team)
    
    # Update table cells with team statistics
    runjs(sprintf("
      document.getElementById('home-passes').textContent = '%d';
      document.getElementById('away-passes').textContent = '%d';
      document.getElementById('home-pass-acc').textContent = '%.1f%%';
      document.getElementById('away-pass-acc').textContent = '%.1f%%';
      document.getElementById('home-shots').textContent = '%d';
      document.getElementById('away-shots').textContent = '%d';
      document.getElementById('home-sot').textContent = '%d';
      document.getElementById('away-sot').textContent = '%d';
      document.getElementById('home-goals').textContent = '%d';
      document.getElementById('away-goals').textContent = '%d';
      document.getElementById('home-xg').textContent = '%.2f';
      document.getElementById('away-xg').textContent = '%.2f';
      document.getElementById('home-possession').textContent = '%d';
      document.getElementById('away-possession').textContent = '%d';
      document.getElementById('home-pressures').textContent = '%d';
      document.getElementById('away-pressures').textContent = '%d';
      document.getElementById('home-tackles').textContent = '%d';
      document.getElementById('away-tackles').textContent = '%d';
      document.getElementById('home-interceptions').textContent = '%d';
      document.getElementById('away-interceptions').textContent = '%d';
    ",
    home_stats$Total_Passes, away_stats$Total_Passes,
    home_stats$Pass_Accuracy, away_stats$Pass_Accuracy,
    home_stats$Total_Shots, away_stats$Total_Shots,
    home_stats$Shots_on_Target, away_stats$Shots_on_Target,
    home_stats$Goals, away_stats$Goals,
    home_stats$xG, away_stats$xG,
    home_stats$Possession_Time, away_stats$Possession_Time,
    home_stats$Pressures, away_stats$Pressures,
    home_stats$Tackles, away_stats$Tackles,
    home_stats$Interceptions, away_stats$Interceptions
    ))
  })
  
  # Render team performance charts
  output$hometeamshotmap <- renderPlot({
    req(selected_events$data)
    home_team <- selected_events$data %>% 
      filter(row_number() == 1) %>% 
      pull(team.name)
    get_shot_map(selected_events$data, home_team)
  })
  
  output$awayteamshotmap <- renderPlot({
    req(selected_events$data)
    away_team <- selected_events$data %>% 
      filter(row_number() == 2) %>% 
      pull(team.name)
    get_shot_map(selected_events$data, away_team)
  })
  
  output$hometeampassnetwork <- renderPlot({
    req(selected_events$data)
    home_team <- selected_events$data %>% 
      filter(row_number() == 1) %>% 
      pull(team.name)
    get_passing_network(selected_events$data, home_team)
  })
  
  output$awayteampassnetwork <- renderPlot({
    req(selected_events$data)
    away_team <- selected_events$data %>% 
      filter(row_number() == 2) %>% 
      pull(team.name)
    get_passing_network(selected_events$data, away_team)
  })
  
  # Function to generate shot map
  get_shot_map <- function(match_events, team_name) {
    # Get all shots for the team
    shots <- match_events %>%
      filter(team.name == team_name & type.name == "Shot")
    
    # Handle empty shots data
    if(nrow(shots) == 0) {
      # Return empty plot with message
      return(
        ggplot() + 
          annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "white", limits = FALSE) +
          theme_pitch() +
          theme(
            plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5)
          ) +
          labs(title = paste("No Shot Data Available For", team_name))
      )
    }
    
    # Create the shot map with proper error handling for missing values
    shots <- shots %>%
      mutate(
        outcome = case_when(
          shot.outcome.name == "Goal" ~ "Goal",
          shot.outcome.name == "Saved" ~ "Saved",
          TRUE ~ "Missed"
        ),
        # Add a default xG value if missing
        xg_value = ifelse(is.na(shot.statsbomb_xg), 0.05, shot.statsbomb_xg)
      )
    
    ggplot(shots, aes(x = location.x, y = location.y)) +
      annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "white", limits = FALSE) +
      geom_point(aes(color = outcome, size = xg_value), alpha = 0.7) +
      scale_color_manual(values = c("Goal" = "#00FF00", "Saved" = "#FF0000", "Missed" = "orange")) +
      scale_size_continuous(range = c(3, 8)) +
      scale_y_reverse() +
      theme_pitch() +
      theme(
        panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5)
      ) +
      labs(
        title = paste("Shot Map -", team_name),
        color = "Shot Outcome",
        size = "xG"
      )
  }
  
  # Function to generate passing network
  get_passing_network <- function(match_events, team_name) {
    passes <- match_events %>%
      filter(team.name == team_name & type.name == "Pass")
    
    if(nrow(passes) == 0) {
      return(
        ggplot() + 
          annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#3B7D32", limits = FALSE) +
          theme_pitch() +
          theme(
            plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5)
          ) +
          labs(title = paste("No Pass Data Available For", team_name))
      )
    }
    
    has_recipient_data <- "pass.recipient.name" %in% names(passes) && 
                         sum(!is.na(passes$pass.recipient.name)) > 10
    
    if(has_recipient_data) {
      pass_connections <- passes %>%
        filter(!is.na(pass.recipient.name)) %>%
        group_by(passer = player.name, receiver = pass.recipient.name) %>%
        summarise(
          count = n(),
          .groups = "drop"
        ) %>%
        filter(count >= 3)
      
      total_passes_by_player <- bind_rows(
        pass_connections %>% 
          group_by(player = passer) %>% 
          summarise(passes_out = sum(count)),
        pass_connections %>% 
          group_by(player = receiver) %>% 
          summarise(passes_in = sum(count))
      ) %>%
      group_by(player) %>%
      summarise(
        total_passes = sum(passes_out, na.rm = TRUE) + sum(passes_in, na.rm = TRUE)
      )
    }
    
    avg_positions <- passes %>%
      group_by(player.name) %>%
      summarise(
        avg_x = mean(location.x, na.rm = TRUE),
        avg_y = mean(location.y, na.rm = TRUE),
        passes_made = n()
      ) %>%
      filter(!is.na(player.name))
    
    if(has_recipient_data && exists("total_passes_by_player") && nrow(total_passes_by_player) > 0) {
      avg_positions <- avg_positions %>%
        left_join(total_passes_by_player, by = c("player.name" = "player")) %>%
        mutate(
          node_size = ifelse(is.na(total_passes), passes_made, total_passes)
        )
    } else {
      avg_positions <- avg_positions %>%
        mutate(node_size = passes_made)
    }
    
    if(nrow(avg_positions) == 0) {
      return(
        ggplot() + 
          annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#3B7D32", limits = FALSE) +
          theme_pitch() +
          theme(
            plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5)
          ) +
          labs(title = paste("No Player Position Data Available For", team_name))
      )
    }
    
    max_node_size <- max(avg_positions$node_size)
    min_node_size <- min(avg_positions$node_size)
    node_size_range <- c(8, 25)
    
    scale_node_size <- function(x) {
      if(max_node_size == min_node_size) return(mean(node_size_range))
      scaled <- (x - min_node_size) / (max_node_size - min_node_size) * 
                (node_size_range[2] - node_size_range[1]) + node_size_range[1]
      return(scaled)
    }
    
    p <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "white", limits = FALSE) +
      annotate("text", x = -5, y = 50, label = "Attack", angle = 90, size = 4, color = "black") +
      geom_circle(aes(x0 = 60, y0 = 40, r = 9.15), color = "black", fill = NA)
    
    if(has_recipient_data && nrow(pass_connections) > 0) {
      p <- p + 
        geom_segment(
          data = pass_connections,
          aes(
            x = avg_positions$avg_x[match(passer, avg_positions$player.name)],
            y = avg_positions$avg_y[match(passer, avg_positions$player.name)],
            xend = avg_positions$avg_x[match(receiver, avg_positions$player.name)],
            yend = avg_positions$avg_y[match(receiver, avg_positions$player.name)],
            size = count,
            alpha = count
          ),
          color = "darkblue",
          lineend = "round"
        ) +
        scale_size_continuous(range = c(1, 5)) +
        scale_alpha_continuous(range = c(0.3, 0.8))
    }
    
    p <- p +
      geom_point(
        data = avg_positions,
        aes(x = avg_x, y = avg_y, size = node_size),
        color = "darkblue",
        fill = "darkblue",
        shape = 1,
        alpha = 0.9
      ) +
      geom_text(
        data = avg_positions,
        aes(x = avg_x, y = avg_y - 5, label = player.name),
        color = "black",
        size = 4, 
        fontface = "plain",
        vjust = 1
      ) +
      scale_y_reverse() +
      guides(size = "none", alpha = "none") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey50", size = 10, hjust = 0)
      ) +
      labs(
        title = paste("Passing Network -", team_name),
        subtitle = "Passes from minutes 1' to 90'"
      )
    
    return(p)
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)