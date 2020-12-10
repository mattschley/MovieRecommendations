## server.R

library(data.table)
library(dplyr)
# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  test = value_list
  user_rating_list = rep(0, 3686)
  for(i in 1:3883){
    if(i %in% dat$MovieID){
      user_rating_list[i] = dat$Rating[i]
    }
  }

  #browser()
  set.seed(101)
  train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
  train = ratings[train.id, ]
  head(train)

  test = ratings[-train.id, ]
  head(test)

  i = paste0('u', train$UserID)
  j = paste0('m', train$MovieID)
  x = train$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  new.Rmat = rbind(Rmat, user_rating_list)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  #rownames(new.Rmat) = levels(tmp2$i)
  #colnames(new.Rmat) = levels(tmp2$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  new.Rmat = new('realRatingMatrix', data = new.Rmat)

  #browser()
  rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))
  recom = predict(rec_UBCF, 
                new.Rmat[6041], type = 'ratings')

  user_recom = as(recom, 'matrix')[1, 1:3686]
  ordered = order(-user_recom, na.last=NA)[1:10]
  recommended_movie_ids = names(user_recom[ordered])
  stripped = gsub('m', '', recommended_movie_ids)
  final_ids = noquote(stripped)

  #browser()
  
  final_ids
}

get_user_genre = function(genre) {
  # value_list is a name i.e. 'Action'... map to a genre id
  # genre_id = genre_list[genre_name]
  
  system1 = ratings %>% group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), 
            ave_ratings = round(mean(Rating), dig=3)) %>%
  inner_join(movies, by = 'MovieID') %>%
  filter(Genres %like% genre) %>%
  filter(ratings_per_movie > 1000) %>%
  top_n(10, ave_ratings) %>%
  mutate(Image = paste0('<img src="', 
                        small_image_url, 
                        MovieID, 
                        '.jpg?raw=true"></img>')) %>%
  select('Image', 'Title','Genres', 'ave_ratings', 'MovieID') %>%
  arrange(desc(-ave_ratings))
  
  system1
  #datatable(class = "nowrap hover row-border", 
  #        escape = FALSE, 
  #        options = list(dom = 't',
  #                       scrollX = TRUE, autoWidth = TRUE))
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
    
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        #browser()
        user_results = (1:10)/10
        user_predicted_ids = 1:10
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = movies$MovieID[as.integer(user_ratings)], 
                                    Title = movies$Title[as.integer(user_ratings)], 
                                    Predicted_rating =  user_results)
        
    }) # still busy
    
  }) # clicked on button

  df2 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        #value_list <- reactiveValuesToList(input)

        genre = input$genre

        cat(file=stderr(), "genre", genre, "XXX", "\n")

        genre_based_ids = get_user_genre(input$genre)

        #browser()
        #user_results = (1:10)/10
        #user_predicted_ids = 1:10

        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = genre_based_ids$MovieID, 
                                    Title = movies$Title[genre_based_ids$Title], 
                                    Predicted_rating =  genre_based_ids$MovieID)
         #browser()
        
    }) # still busy
    
  }) # clicked on button1
  

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    test <- movies
    #browser()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
          div(style = "text-align:center", 
              a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
             )
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function

  output$results2 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df2()
    #browser()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
          div(style = "text-align:center", 
              a(img(src = movies[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j],]$image_url, height = 150))
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j],]$Title)
             )
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
