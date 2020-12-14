# Init env ----------------------------------------------------------------
library(rvest)
library(jsonlite)
library(data.table)
library(stringr)
library(tidyverse)
library(tibble)
library(purrr)
library(ggthemes)
rm(list = ls())

path <- "/Users/Dominik/OneDrive - Central European University/1st_trimester/C2-Webscraping/assignment2/"

# Functions ---------------------------------------------------------------
# Create function to extract JSON from the HTML Doc
get_json_values <- function(my_page , my_look_for, ful_name) {
  #my_look_for <- 'reactInitialState'
  #ful_name <- 'window.__reactInitialState__ = '
  script_list <- 
    my_page %>%
    html_nodes("script")
  
  for (i in script_list) {
    my_t <- i %>% 
      html_text()
    my_t_h <- substring(my_t, 1,100)
    #print(my_t)
    if(grepl(my_look_for, my_t_h)){
      res <- substring(str_trim(my_t), nchar(ful_name)+1)
      res <- gsub('\\</b>', '',gsub('\\<b>', '', gsub('}};', '}}', res, fixed = T), fixed = T), fixed = T)
      return(fromJSON(res))
    }
  }
}

# Create function to get ingredients from nested lists
get_ingredients <- function() {
  stages<-recipe_json$recipeReducer$recipe$stagesWithoutLinks
  ing <- c()
  for (stage in 1:length(stages)){
    for (aux in 1:length(stages[stage,"ingredients"])){
      for (ingredient in 1:length(stages[stage,"ingredients"][[aux]]$foods)){
        ing <- c(ing,stages[stage,"ingredients"][[aux]]$foods[[ingredient]]$title)
      }
    }
  }
  return(ing)
}

# Create function that returns NA when list is empty and with element if not
get_if_not_empty <- function(list,row,col) {
  if (is_empty(list)){
    return(NA)
  } else{
    return(list[row,col]) 
  }
}

# Create function that returns NA when item is null and with itself if not
get_if_not_null <- function(item) { 
  if (is_null(item)) {
    return(NA)
  } else{
    return(item)
  }
}

# Scraping all BBC Food Recipes -------------------------------------------

# Find BBC Food page to get letter page letters
t <- read_html('https://www.bbc.co.uk/food/recipes/a-z/a/1#featured-content')
write_html(t, paste0(path, "bbc_food_page.html"))
d <- "https://www.bbc.co.uk"

init_json <- get_json_values(t, 'reactInitialState', 'window.__reactInitialState__ = ')
letters<- names(init_json$azPageReducer$azCounts)

# Initialize output table

df<-data.frame(
  "category"= character(),
  "title"= character(),
  "description"= character(),
  "diet1" = character(),
  "diet2" = character(),
  "diet3" = character(),
  "collection" = character(),
  "chef" = character(),
  "prep_time" = character(),
  "cook_time" = character(),
  "food" = character(),
  "rating_no" = numeric(),
  "rating_val" = numeric()
)

stages <- list()
ingredients <- list()
instructions <- list()
occasions <- list()

# Iterate through initial letter pages
for (l in letters) {
  print(paste0("Getting recipes with: ",l))
  lpage <- read_html(paste0("https://www.bbc.co.uk/food/recipes/a-z/",l,"/1#featured-content"))
  dir.create(paste0(path,"/htmls/",as.character(l)))
  lpage_json <- get_json_values(lpage, 'reactInitialState', 'window.__reactInitialState__ = ')
  pages<-1:lpage_json$azPageReducer$pages
  print(pages)
  # Iterate through pages
  for (p in pages) {
    print(paste0("Getting recipes on page: ", p))
    dir.create(paste0(path,"/htmls/",l,"/",p))
    page <- read_html(paste0("https://www.bbc.co.uk/food/recipes/a-z/",l,"/",p,"#featured-content"))
    page_json <- get_json_values(page, 'reactInitialState', 'window.__reactInitialState__ = ')
    recipes <- page_json$azPageReducer$promos$url
    print(recipes)
    # Iterate through recipes 
    for (r in recipes) {
      print(paste0("Getting recipe for: ", r))
      r_page <- read_html(paste0(d,r))
      recipe_json <- get_json_values(r_page, 'reactInitialState', 'window.__reactInitialState__ = ')
      print("json ok")
      r_id <- recipe_json$recipeReducer$recipe$id
      write_html(page, paste0(path,"/htmls/",l,"/",p,"/",r_id,".html"))
      df<-df %>% add_row(
        "category"= get_if_not_null(recipe_json$recipeReducer$recipe$course$title),
        "title"= get_if_not_null(recipe_json$recipeReducer$recipe$title),
        "description"= get_if_not_null(recipe_json$recipeReducer$recipe$description),
        "diet1" = get_if_not_empty(recipe_json$recipeReducer$recipe$diets,1,"title"),
        "diet2" = get_if_not_empty(recipe_json$recipeReducer$recipe$diets,2,"title"),
        "collection" = get_if_not_empty(recipe_json$recipeReducer$recipe$collections,1,"title"),
        "chef" = get_if_not_null(recipe_json$recipeReducer$recipe$chefDetails$name),
        "prep_time" = get_if_not_null(recipe_json$recipeReducer$recipe$metadata$prepTimeMeta),
        "cook_time" = get_if_not_null(recipe_json$recipeReducer$recipe$metadata$cookTimeMeta),
        "food" = get_if_not_null(recipe_json$recipeReducer$recipe$food$title),
        "rating_no" = get_if_not_null(recipe_json$recipeReducer$rating$total),
        "rating_val" = get_if_not_null(recipe_json$recipeReducer$rating$value)
      )
      print("non list variables ok")
      stages<-append(stages, list(recipe_json$recipeReducer$recipe$stages$title))
      ingredients<-append(ingredients, list(get_ingredients()))
      instructions<-append(instructions, list(recipe_json$recipeReducer$recipe$methods$text))
      occasions<-append(occasions, list(recipe_json$recipeReducer$recipe$occasions$title))
      print("list variables ok")
    }
  }
}

# Add the lists to the data frame
df$stages <- stages
df$ingredients <- ingredients
df$instructions <- instructions
df$occasions <- occasions

df$no_stages <- unlist(lapply(df$stages,length))
df$no_ingredients <- unlist(lapply(df$ingredients,length))
df$no_instructions <- unlist(lapply(df$instructions,length))
df$no_occasions <- unlist(lapply(df$occasions,length))


# Export scraped data -----------------------------------------------------
tibble_with_lists_to_csv <- function(tibble_object, file_path_name) {
  set_lists_to_chars <- function(x) { 
    if(class(x) == 'list') { y <- paste(unlist(x[1]), sep='', collapse=', ') } else { y <- x  } 
    return(y) }
  new_frame <- data.frame(lapply(tibble_object, set_lists_to_chars), stringsAsFactors = F)
  write.csv(new_frame, file=file_path_name)
}

tibble_with_lists_to_csv(df,paste0(path,"/data/bbc_food_recipes.csv"))

# EDA ---------------------------------------------------------------------

# Ratings
df[,c("rating_no","rating_val")] %>%
  filter(rating_no!=0) %>% 
  filter(rating_no<200) %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_economist() + 
  scale_fill_economist()

summary(df[,c("rating_no","rating_val")])

# Categories
cat_df <- df[,c("category","rating_val","no_stages","no_ingredients","no_instructions")] %>% 
  group_by(category) %>% 
  summarize(
    no_recipes = n(),
    avg_rating_val = mean(rating_val, na.rm = TRUE),
    avg_no_stages = mean(no_stages, na.rm = TRUE),
    avg_no_ingredients = mean(no_ingredients, na.rm = TRUE),
    avg_no_instructions = mean(no_instructions, na.rm = TRUE)
  )

ggplot(cat_df, aes(x = reorder(category, no_recipes), y = no_recipes, fill= no_recipes)) +
  geom_bar(stat="identity")+
  coord_flip() + 
  theme_base() +
  labs(title = 'Recipe categories by frequency',
       y = 'Frequency',
       x = 'Category')

# Get all ingredients
all_ingredients <- flatten(df$ingredients) %>% 
  lapply(function(x) tolower(trimws(x))) %>% 
  unlist()
all_ingredients <- unlist(all_ingredients)

# Get ingredient occurrences
ing_df <- table(all_ingredients)

# Exclude "other" ingredient
ing_df <- ing_df[names(ing_df)!="(other)"]

# Get top 10 most common ingredients
top10 <- data.frame(sort(ing_df, decreasing=T)[1:10],)
colnames(top10) <- c("ingredient", "frequency")

ggplot(top10, aes(x = reorder(ingredient, frequency), y = frequency, fill= frequency)) +
  geom_bar(stat="identity")+
  coord_flip() + 
  theme_base() +
  labs(title = 'Top 10 ingredient by occurance',
       y = 'Frequency',
       x = 'Ingredient')

# Chef performance
chef_df <- df[,c("chef","rating_val","no_ingredients")] %>% 
  group_by(chef) %>% 
  summarize(
    no_recipes = n(),
    avg_rating_val = mean(rating_val, na.rm = TRUE),
    avg_no_ingredients = mean(no_ingredients, na.rm = TRUE)
  ) %>% 
  filter(avg_rating_val!=0)

ggplot(chef_df, aes(x = avg_no_ingredients, y = avg_rating_val)) +
  geom_point( size=2, color="steelblue")+
  theme_base() +
  labs(title = 'Relationship between recipe rating and complexity (avg. # of ingredients)',
       y = 'Average Rating',
       x = 'Average # of ingredients')

reg<-lm(avg_rating_val~avg_no_ingredients, chef_df)
summary(reg)