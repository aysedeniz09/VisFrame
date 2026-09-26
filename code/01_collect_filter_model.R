# Setup ----

Output_folder = "../output/"
Data_folder = "../data/"
Project_name = "VisFrame_"

# Create local folders for intermediate files and results (not shared)
dir.create(Output_folder, showWarnings = FALSE)
dir.create(Data_folder, showWarnings = FALSE)

# Run this script from the code/ folder (paths below are relative to it).

## Libraries ----

# === File I/O and Data Handling ===
library(readr)        # Reading delimited files (CSV, TSV, etc.)
library(readxl)       # Reading .xls/.xlsx Excel files
library(openxlsx)     # Alternative for reading/writing Excel files

# === Text and Language Processing ===
library(quanteda)     # Text analysis and corpus management
library(tidytext)     # Tokenization, sentiment dictionaries, tf-idf, etc.
library(cld2)         # Language detection
library(stringi)      # High-performance string processing (Unicode-aware)
library(stringr)      # Tidy string manipulation (part of tidyverse)

# === Topic Modeling and NLP Utilities ===
library(ldaOptim)  # Latent Dirichlet Allocation (LDA), CTM, etc.

# === Data Manipulation and Visualization ===
library(tidyverse)    # Collection of core tidy tools (ggplot2, dplyr, tidyr, etc.)
library(dplyr)        # Data manipulation (also in tidyverse)
library(tidyr)        # Data tidying (also in tidyverse)
library(ggplot2)      # Grammar of Graphics plotting
library(scales)       # Axis formatting, rescaling (for ggplot2)
library(ggpubr)

# === Date and Time Handling ===
library(lubridate)    # Simplified date and time manipulation

# === Parallel and Efficient Computing ===
library(doParallel)   # Parallel backend for foreach

# === Network and Graph Analysis ===
library(igraph)       # Network analysis and graph theory

# === Image Processing (if used for thumbnails/visuals) ===
library(magick)       # Image reading, editing, conversion
library(daiR)         # OCR w/ Google Document AI
library(httr)

# === Stats ===
library(broom)


# Data Collection ----
# Script to get popular articles from the NewsWhip database using the API.
# First we call the API to get the articles we are interested in.

## NewsWhip API Setup ----

num_articles <- 5000

# create a variable that has the API key in it
api_key <- Sys.getenv("NEWSWHIP_API_KEY")

get_newswhip_articles <- function(api_key, limit, start_time, end_time) {
  api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)          
  data <- paste0('{\"filters\": [\"language:en AND country_code:us AND (publisher:nytimes.com OR publisher:nypost.com)\"], 
                           \"size\": ', limit, ', 
                           \"from\": ', start_time, ',
                           \"to\": ', end_time, ',
                           \"search_full_text\": true,
                           \"find_related\": false}')
  r <- httr::POST(api_endpoint, body = data)
  httr::stop_for_status(r)         
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$articles          
}

## Daily Collection Loop ----
# You only need to change the days.

start_date <- as.Date("2025-06-01")
end_date <- as.Date("2025-12-31")
days <- as.character(as.Date(as.Date(start_date):as.Date(end_date), origin="1970-01-01"))

mylist <- list()

for (i in days) {
  print("now running days:")
  print(i)
  
  start_loop_time <- Sys.time()
  
  Sys.sleep(runif(1, 0.3, 10))
  start_time <- as.numeric(as.POSIXct(paste(i, "00:00:00 EST", sep=" "))) * 1000 # datetime conversion
  end_time <- as.numeric(as.POSIXct(paste(as.Date(paste(i)) + 1,  "00:00:00 EST", sep=" "))) * 1000 - 1 # datetime conversion
  
  elapsed_time <- Sys.time() - start_loop_time
  if (elapsed_time > 10) {
    print(paste("Skipping day", i, "because it took more than 10 seconds"))
    next
  }
  
  data_temp <- get_newswhip_articles(api_key = api_key, limit = num_articles, start_time = start_time, end_time = end_time) # datetime conversion
  data_temp$date_time <- as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01") # datetime conversion
  data_temp$date <- as.Date(as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01")) # datetime conversion
  data_temp$relatedStories <- NULL
  # data_temp$topics <- NULL
  data_temp$authors <- NULL
  data_temp$entities <- NULL
  data_temp$videos <- NULL
  
  try(data_temp <- data_temp |> dplyr::select(
    uuid, 
    publication_timestamp, 
    link, 
    headline, 
    excerpt, 
    keywords, 
    image_link, 
    has_video, 
    fb_data.total_engagement_count, 
    fb_data.likes, 
    fb_data.comments, 
    fb_data.shares, 
    tw_data.tw_count, 
    source.publisher, 
    source.domain, 
    source.link, 
    source.country, 
    source.country_code, 
    source.language, 
    date_time, 
    date, topics))
  
  mylist[[i]] <- data_temp
}

## Combine and Deduplicate ----

##add month and year name, or if you are pulling for a full year just put year
data_temp <- do.call("rbind",mylist) |> data.frame()

data_temp$topics <- sapply(data_temp$topics, function(df) {
  paste(df$name, collapse = "; ")
})


df <- data_temp

min(df$date)
max(df$date)

df<- df |> 
  distinct(link, .keep_all = TRUE)

# Add a unique article index as the first column. It is kept through filtering,
# captioning, and modeling, and serves as the document ID in the LDA model.
df <- df |> 
  mutate(index = as.character(row_number()), .before = 1)

sum(is.na(df$image_link))


# Filtering ----

## Headline Filter ----

df2 <- df
df2$has_protest <- grepl("protest|no kings|anti[- ]?ice", tolower(df2$headline), ignore.case = TRUE)

sum(df2$has_protest, na.rm = TRUE)

df2 <- df2[df2$has_protest, ]

nrow(df2)

table(df2$source.publisher)

save.image(paste0(Data_folder, Project_name, "-raw-data.Rdata"))

head(df2$image_link)

## Image Filter ----

df2$is_likely_logo <- grepl(
  "logo|icon|favicon|avatar|sprite|placeholder|thumb(nail)?|badge|watermark|/assets/|/static-assets/|/branding/|/theme/",
  tolower(df2$image_link)
)

df2 |> count(is_likely_logo)

data <- df2 |> filter(!is_likely_logo) |> 
  rename("url" = "link")
nrow(data)

write.csv(data, 
          paste0(Output_folder, Project_name,  "-to-scrape.csv"),
          row.names = FALSE)

rm(list = setdiff(ls(), c("Data_folder", "Output_folder", "Project_name",
                          "data")))

save.image(paste0(Data_folder, Project_name, "-raw-data-post-image.Rdata"))


# Load Captions and Merge ----

captions <- read_csv("../output/captions_file.csv")

captions <- captions |> distinct(image_path, .keep_all = TRUE)

captions$uuid <- sub("\\.[^.]*$", "", basename(captions$image_path))


data <- left_join(data, captions)

data2 <- data |> 
  filter(caption != "Failed to get caption") |> 
  drop_na(caption)

table(data2$source.publisher)

data2 <- data2 |> 
  filter(source.publisher == "nypost.com" | source.publisher == "nytimes.com")

save.image(paste0(Data_folder, Project_name, "-raw-data-post-captions.Rdata"))

rm(captions)


# Preprocessing ----

## Language and Length ----

## clean for specific language (only keep english text)
data2$text <- data2$caption
# data2$index is created after NewsWhip collection (see Combine and Deduplicate)
data2$CLD2<-cld2::detect_language(data2$text)
table(data2$CLD2)
# data2<-data2 |> filter(CLD2=="en") 

## calculate and examine text length
data2$nwords <- str_count(data2$text, "\\w+")
nrow(data2)
hist(data2$nwords, breaks = 100)
meanwc = mean(data2$nwords, na.rm = TRUE)
meanwc
sdwc = sd(data2$nwords, na.rm = TRUE)
sdwc

data3<-data2
data3<-data3[which(data3$nwords<(meanwc+2*sdwc)),]
data3<-data3[which(data3$nwords>(meanwc-2*sdwc)),] 
hist(data3$nwords, breaks = 50)
mean(data3$nwords)
sd(data3$nwords) #656

data3$text <- str_squish(str_trim(data3$text, side = "both"))

save.image(paste0(Data_folder, Project_name,  "_post-preprocess.Rdata"))

rm(list=setdiff(ls(), c("Project_name", "Data_folder",
                        "Output_folder", "data3")))


# Tidy Preprocessing ----

## Stopwords ----

mystopwords <- c(stopwords("en"), stopwords(source = "smart"),
                 "http", "https", "image", "picture", "scene", "atmosphere", "setting",
                 "colors", "colored", "colorful", "red", "blue", "green", "yellow", "orange", "purple", "pink", "brown",
                 "black", "white", "gray", "cyan", "magenta", "maroon", "lime", "navy", "teal", "silver",
                 "dark", "background", "blurry", "blurred", "visible", "fuzzy", "reads", "overlay", "caption",
                 "subtitles", "bright",
                 "reads", "read", "reading", "overlaid", "letters", "shapes", "foreground",
                 "top", "bottom", "front", "back", "left", "right", "center", "side",
                 "text", "logo",
                 "wearing", "wears", "wear", "dressed", "attire",
                 "appears", "present", "presents", "shows", "displayed", "featuring", "indicating",
                 "holding", "holds", "engaged", "focused", "expression",
                 "number", "including", "nearby", "large",
                 "features", "individuals", "group",
                 # Additional caption-boilerplate noise observed in the k=27 FREX output:
                 "shown", "show", "display", "displays", "images", "imagery",
                 "related", "elements", "element", "representing", "represents",
                 "centered", "prominently", "inset", "partially", "similar", "part",
                 "suggesting", "suggests", "surrounded", "surrounding", "structure",
                 "backdrop", "labeled", "label", "arranged", "multiple", "captioned",
                 "style")

mystopwords <- unique(mystopwords)
mystopwords <- tolower(mystopwords)

## Tokenization and Pruning ----

##### USING SAMPLE DATA ##########

# creating tidy tokens
tidy_data<-data3 |>
  unnest_tokens(word, text) |> # tokenizing
  anti_join(data.frame(word=mystopwords)) |>
  mutate(nchar=nchar(word)) |> #counting the number of letters
  filter(nchar>2) |> # remove from minumum number of chars
  filter(!grepl("[0-9]{1}", word)) |> # removing numbers
  filter(!grepl("\\W", word))  # removing any word containing non letter/number

# choosing top words by tf-idf
maxndoc=0.5
minndoc=0.00001

# filter to tokens not too common and not too rare
templength<-length(unique(tidy_data$index))

good_common_words <- tidy_data |>
  count(index, word, sort = TRUE) |>
  group_by(word) |>
  summarize(doc_freq=n()/templength) |>
  filter(doc_freq<maxndoc) |>
  filter(doc_freq>minndoc)

# clean tidy to fit the tokens - NOTE: this is where you might lost indexes
tidy_data_pruned<-tidy_data |> inner_join(good_common_words)

tidy_data_pruned |>
  group_by(word) |>
  dplyr::summarise(n=n()) |>
  arrange(desc(n)) |>
  mutate(word = reorder(word, n)) |>
  top_n(100) |>
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

ggsave(paste0(Output_folder, Project_name,"top-words-forstopwords.jpeg"), bg="white", width=12, height=12, dpi=300)


# STOP HERE - check words - correct stopwords - continue only if happy

## Document-Feature Matrix ----

# DFM-ing it (termdoc)
tidy_dfm <- tidy_data_pruned |>
  count(index, word) |>
  cast_dfm(index, word, n)

# feed to lda object

full_data<- convert(tidy_dfm, to = "topicmodels")

rm(good_common_words, tidy_data, tidy_data_pruned, tidy_dfm, maxndoc, minndoc, templength, mystopwords)
gc()
save.image(paste0(Data_folder, Project_name, "_post-tidy-presearchk_", format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))


# Hyperparameter Search ----

## Alpha Search ----

alpha_results <- lda_find_alpha(
  dtm = full_data,
  candidate_alpha = c(10, 5, 2, 1),
  candidate_k = c(seq(5, 100, by = 5), seq(125, 200, by = 25)),
  folds = 5
  # ncores auto-detected (uses detectCores() - 2 by default)
  # Or specify manually: ncores = 4
)

suggest_alpha_elbow(alpha_results)

temp <- suggest_alpha_elbow(alpha_results)
write.csv(temp,
          file = paste0(Output_folder, Project_name, "-suggest_alpha_elbow.csv"),
          row.names = FALSE)
# Save results
save.image(paste0(Data_folder, Project_name, "_post-searchk_",
                  format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))

# # closeAllConnections()

### Visualize Alpha Results ----

plot_alpha_crossval(alpha_results)
ggsave(paste0(Output_folder, Project_name, "FindAlpha_CV_BoxPlot.jpeg"),
       bg = "white",
       width = 12, height = 8, dpi = 300)
plot_alpha_smooth(alpha_results)
ggsave(paste0(Output_folder, Project_name, "FindAlpha_CV_Smothed.jpeg"),
       bg = "white",
       width = 12, height = 8, dpi = 300)


plot_alpha_second_derivative(alpha_results, alpha_value = "1", vline_at = 35)
ggsave(paste0(Output_folder, Project_name, "FindAlpha_CV_2ndDerivative.jpeg"),
       bg = "white",
       width = 12, height = 8, dpi = 300)

## Topic Number Search ----

my_alpha = 1/35

topic_results <- lda_find_topics(
  dtm = full_data,
  topics = seq(2, 200, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  control = list(alpha = my_alpha),  # Use optimal alpha from Stage 1
  mc.cores = detectCores() - 4,
  verbose = TRUE
)

print("saving results")
save.image(paste0(Data_folder, Project_name, "_post-searchk_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))

### Visualize Topic-Number Results ----

print("plotting results")

# Visualize all four metrics
plot_topics_metrics(topic_results)
ggsave(paste0(Output_folder, Project_name, "FindTopics_CV_Metrics.jpeg"), 
       bg = "white",
       width = 12, height = 8, dpi = 300)

print("saving results")
save.image(paste0(Data_folder, Project_name, "_post-searchk_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))


suggest_topics_elbow(topic_results)
temp <- suggest_topics_elbow(topic_results)
write.csv(temp,
          file = paste0(Output_folder, Project_name, "-suggest_topic_elbow.csv"),
          row.names = FALSE)

## Combined Panel Figure ----

p1 <- plot_alpha_crossval(alpha_results)
p2 <- plot_alpha_smooth(alpha_results)
p3 <- plot_alpha_second_derivative(alpha_results, alpha_value = "1", vline_at = 35)
p4 <- plot_topics_metrics(topic_results)

ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
                  labels = c("A", "B", "C", "D"))

ggsave(paste0(Output_folder, Project_name, "ALL-Metrics.jpeg"),
       bg = "white",
       width = 12, height = 8, dpi = 300)

print("saving results")
save.image(paste0(Data_folder, Project_name, "_post-searchk_",
                  format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))


# Run LDA Model ----

## Fit Final Model ----

models <- lda_run_models(
  dtm = full_data,
  k_values = c(19),
  alpha_divisor = 1,
  control = list(iter = 2000, seed = 123)
)


save.image(paste0(Data_folder, Project_name, "_post-runlda_",
                  format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))

## Export Topic Results ----

# Export all results for one model
export_lda_results(
  model = models$k_19,
  doc_data = data3,
  n_words = 30,
  n_docs = 30,
  output_prefix = paste0(Project_name, "LDA_K19_"),
  output_dir = Output_folder
)


# Topic Image Grids ----

## get_top_images() Function ----

get_top_images<-function(LDAmodel) {
  
  print("Starting images:")
  print(LDAmodel@k)
  
  # Ensure mydata$index is character and has image_path
  mydata$index <- as.character(mydata$index)
  print("Converted index to character")
  
  # Extract tidy gamma matrix
  tidy_gamma <- tidy(LDAmodel, matrix = "gamma")
  tidy_gamma$document <- as.character(tidy_gamma$document)
  print("Extracted tidy gamma")
  
  print("Gamma sample:")
  print(head(tidy_gamma))
  
  # Join to get image path
  gamma_with_paths <- tidy_gamma |>
    left_join(mydata |> select(index, image_path), by = c("document" = "index"))
  print("Join completed")
  
  print("Checking for valid image paths:")
  print(sum(!is.na(gamma_with_paths$image_path)))
  
  # Get top image paths for each topic
  top_images_long <- gamma_with_paths |> 
    group_by(topic) |>
    slice_max(gamma, n = nimage, with_ties = FALSE) |>
    arrange(topic, -gamma) |>
    mutate(rank = row_number()) |>
    select(topic, rank, image_path) |>
    ungroup()
  
  print("Top images extracted")
  
  
  # Pivot to wide format *after* sorting
  toppaths <- top_images_long |>
    select(topic, rank, image_path) |>
    pivot_wider(names_from = topic, values_from = image_path) |>
    arrange(rank) |>
    select(-rank)
  
  print("Toppaths built:")
  print(dim(toppaths))
  print("Column names:")
  print(colnames(toppaths))
  
  # Write Excel file of image paths
  openxlsx::write.xlsx(toppaths, paste0(Output_folder, Project_name, "_TopPATHSk_", LDAmodel@k, ".xlsx"))
  
  print("Starting saving images")
  
  ## row names to make pretty
  dirname<-paste0("TOPIC_IMAGES_K",LDAmodel@k)
  try(dir.create(dirname))
  
  for(topicnum in 1:ncol(toppaths)) {
    print(topicnum)
    
    imagelist<-list()
    
    # paths<-toppaths[,topicnum]
    paths <- as.character(toppaths[[topicnum]])
    
    
    # paths<-paste0("../",paths)
    paths <- paths
    
    for (i in 1:length(paths)) {
      imagelist[[i]] <- tryCatch({
        image_convert(
          tryCatch(image_strip(image_read(paths[i])),
                   error = function(e) image_read(paths[i])), 
          "jpeg")
      }, error = function(e) {
        message("Skipping unusable file: ", paths[i])
        NULL
      })
    }
    
    imagelist <- Filter(Negate(is.null), imagelist)
    
    if (length(imagelist) == 0L) next
    
    add0 <- function(input){
      ifelse(input < 10, paste0("0", input), input)
    }
    
    png(filename=paste0(dirname,"/Topic_",add0(topicnum),".png"), width = 3000, height = 2400, units = "px")
    
    par(mfrow = c(9, 5), mar = c(1, 1, 1, 1))
    
    for (j in imagelist) {plot(j)}
    
    topfrextoprint<-topFREX[,topicnum]
    
    #mtext(paste0("Topic #: ",topicnum), side = 3, line = - 4, outer=TRUE,cex=4)
    
    topfrextoprint<-paste(topfrextoprint,collapse=", ")
    
    topfrextoprint<-paste0("Topic ",topicnum,": ",topfrextoprint)
    
    topfrextoprint<-str_wrap(topfrextoprint, width = 80)
    
    mtext(topfrextoprint, side = 1, line = -2, outer=TRUE,cex=5)
    
    dev.off()
  }
  print("Getting PDF")
  
  allimagesforpdf<-list.files(dirname,full.names = T)
  
  image_to_pdf(allimagesforpdf,paste0(Project_name,"_Topimages_",LDAmodel@k,".pdf"))
  
  # dev.off()
  return(toppaths)
}

## Run get_top_images() ----

mydata<-data3 # change if you used for your corpus some other data like data3, trump or X
nimage<-40 #### NOTE: If changed then also mfraw in images print needs to change to accomodate

topFREX <- openxlsx::read.xlsx("../output/VisFrame_LDA_K19__TopFREX_k19.xlsx")
mydata$image_path <- paste0("../", mydata$image_path)
topimages<-get_top_images(models$k_19)

toppaths <- openxlsx::read.xlsx("../output/VisFrame__TopPATHSk_19.xlsx")

save.image(paste0(Data_folder, Project_name, "_post-runlda_",
                  format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))


# Meta Theta Data Frame ----

missing_docs<-setdiff(as.character(data3$index),as.character(models$k_19@documents))

## we filter out all docs not in both
data33 <- data3 |>  dplyr::filter(!(as.character(data3$index) %in% missing_docs))
# data33<-data33[,c(which(colnames(data33)=="index"),textcolumn)]

## now we can add together the gamma and data (notice we only add TEXt from original data)
TEMP_gamma<-(cbind.data.frame(models$k_19@documents, models$k_19@gamma))

colnames(TEMP_gamma)[1]<-"index"
data33$index <- as.character(data33$index)
meta_theta_df <-left_join(data33, TEMP_gamma, by = "index")
#meta_theta_df<-meta_theta_df[,-c(which(colnames(meta_theta_df)=="index"))]
rm(TEMP_gamma)
rm(data33)

save.image(paste0(Data_folder, Project_name, "_post-meta-theta-df_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".Rdata"))


# ANTMN: Topic Network and Community Detection ----

## Topic Labels ----

#@@@@@@@@@@@@@@@@@@@@@@@@@@
#chatgptlabels$label_both<-paste0(chatgptlabels$chatgpt_label_bytext," *OR* ",chatgptlabels$chatgpt_label_bywords)
#topic_labels<-chatgptlabels$label_both
topic_size<-colSums(meta_theta_df[,c((ncol(data3)+1):(ncol(data3)+models$k_19@k))])      

# Enter by hand
#topic_labels<-c("name1","name2")

# or from file:

topic_labels <- c(
  "No Kings 1",
  "DELETE: Miscellaneous 1",
  "Political Speech and Institutions",
  "Police Confrontation",
  "No Kings 2",
  "Legitimacy and Voice",
  "Walls, Graffiti, and Protest",
  "Political Speech and Press",
  "Foreign Conflicts Iran Israel Palestine",
  "Football and Soccer Games",
  "Riots and Conflict",
  "DELETE: Miscellaneous 2",
  "Night Police and Streets",
  "Political Figures Trump focused",
  "Resist Tyranny Symbolism",
  "Militarized and National Guard",
  "Persons of Interest",
  "Crowd Gatherings and Rallies International",
  "DELETE: Miscellaneous3"
)

## network_from_LDA() Function ----

network_from_LDA<-function(LDAobject,deleted_topics=c(),topic_names=c(),save_filename="",topic_size=c(),bbone=FALSE) {
  # Importing needed packages
  require(lsa) # for cosine similarity calculation
  require(dplyr) # general utility
  require(igraph) # for graph/network managment and output
  require(corpustools)
  
  print("Importing model")
  
  # first extract the theta matrix form the topicmodel object
  theta<-LDAobject@gamma
  # adding names for culumns based on k
  colnames(theta)<-c(1:LDAobject@k)
  
  # claculate the adjacency matrix using cosine similarity on the theta matrix
  mycosine<-cosine(as.matrix(theta))
  colnames(mycosine)<-colnames(theta)
  rownames(mycosine)<-colnames(theta)
  
  # Convert to network - undirected, weighted, no diagonal
  
  print("Creating graph")
  
  topmodnet<-graph_from_adjacency_matrix(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames
  # add topicnames as name attribute of node - importend from prepare meta data in previous lines
  if (length(topic_names)>0) {
    print("Topic names added")
    V(topmodnet)$name<-topic_names
  } 
  # add sizes if passed to funciton
  if (length(topic_size)>0) {
    print("Topic sizes added")
    V(topmodnet)$topic_size<-topic_size
  }
  newg<-topmodnet
  
  # delete 'garbage' topics
  if (length(deleted_topics)>0) {
    print("Deleting requested topics")
    
    newg<-delete_vertices(topmodnet, deleted_topics)
  }
  
  # Backbone
  if (bbone==TRUE) {
    print("Backboning")
    
    nnodesBASE<-length(V(newg))
    for (bbonelvl in rev(seq(0,1,by=0.05))) {
      #print (bbonelvl)
      nnodes<-length(V(backbone_filter(newg,alpha=bbonelvl)))
      if(nnodes>=nnodesBASE) {
        bbonelvl=bbonelvl
        #  print ("great")
      }
      else{break}
      oldbbone<-bbonelvl
    }
    
    newg<-backbone_filter(newg,alpha=oldbbone)
    print(paste("Topic-net backbone alpha:", oldbbone, "| Nodes:", vcount(newg), "| Edges:", ecount(newg)))   # <-- ADD THIS LINE
    
  }
  
  # run community detection and attach as node attribute
  print("Calculating communities")
  
  mylouvain<-(cluster_louvain(newg)) 
  mywalktrap<-(cluster_walktrap(newg)) 
  # myspinglass<-(cluster_spinglass(newg)) 
  myfastgreed<-(cluster_fast_greedy(newg)) 
  myeigen<-(cluster_leading_eigen(newg)) 
  
  V(newg)$louvain<-mylouvain$membership 
  V(newg)$walktrap<-mywalktrap$membership 
  # V(newg)$spinglass<-myspinglass$membership 
  V(newg)$fastgreed<-myfastgreed$membership 
  V(newg)$eigen<-myeigen$membership 
  
  # if filename is passsed - saving object to graphml object. Can be opened with Gephi.
  if (nchar(save_filename)>0) {
    print("Writing graph")
    write_graph(newg,paste0(save_filename,".graphml"),format="graphml")
  }
  
  # graph is returned as object
  return(newg)
}

## Build the Network ----

deleted_topics=grep("DELETE:",topic_labels)
print(deleted_topics)

set.seed(123)

mynewnet<-network_from_LDA(LDAobject=models$k_19,
                           #deleted_topics=c(10,6,20,21,38),
                           deleted_topics=grep("DELETE:",topic_labels),
                           save_filename=paste0(Output_folder, Project_name, "K19"),
                           topic_names=topic_labels,
                           topic_size = topic_size, 
                           bbone=TRUE)

## Modularity Comparison ----

mod_scores <- data.frame(
  Method = c("Louvain", "Walktrap", "FastGreedy", "Eigen"),
  Modularity = c(
    modularity(mynewnet, membership = V(mynewnet)$louvain),
    modularity(mynewnet, membership = V(mynewnet)$walktrap),
    modularity(mynewnet, membership = V(mynewnet)$fastgreed),
    modularity(mynewnet, membership = V(mynewnet)$eigen)
  ),
  NumCommunities = c(
    length(unique(V(mynewnet)$louvain)),
    length(unique(V(mynewnet)$walktrap)),
    length(unique(V(mynewnet)$fastgreed)),
    length(unique(V(mynewnet)$eigen))
  )
)

mod_scores #louvain 

write.xlsx(mod_scores, 
           file = paste0(Output_folder, Project_name, "Modulariy_Scores_Network.xlsx"))

save.image(paste0(Data_folder, Project_name, "post-antmn.rdata"))


# Louvain Community Labeling ----

## Extract Membership ----

finalnet <- mynewnet

membership <- setNames(V(finalnet)$louvain, V(finalnet)$name)

V(finalnet)$name

df <- data.frame(
  label = V(finalnet)$name,
  cluster = membership
)

df <- df |> arrange(cluster)

table(df$cluster)

write.csv(df,
          file = paste0(Output_folder, Project_name, "topics_clusters-louvain.csv"),
          row.names = FALSE)

## Identify DELETE Topics ----

topic_labels

is_delete <- grepl("^DELETE:", topic_labels)
table(is_delete)

## Build Full Topic-Community Table ----

# ---- Load Top FREX Words (19 topics) ----
top_words_df <- topFREX

community_map <- list(
  `1` = list(name = "(A) Conflict and Law Enforcement",
             color = "purple"),
  `2` = list(name = "(B) Political Voice and Institutions",
             color = "orange"),
  `3` = list(name = "(C) Mass Gatherings and Spectacle",
             color = "green")
)

# ---- Build Output for All 19 Topics ----
full_output <- lapply(1:19, function(i) {
  topic_label <- topic_labels[i]
  comm_label <- if (topic_label %in% names(membership)) {
    comm_id <- as.character(membership[[topic_label]])
    comm <- community_map[[comm_id]]
    paste0(comm$name, " (", comm$color, ")")
  } else {
    "Delete"
  }
  
  list(
    `Topic Number` = i,
    `Topic Label` = topic_label,
    `Top 10 FREX Words` = paste(na.omit(top_words_df[[i]][1:10]), collapse = ", "),
    `Community (Color)` = comm_label
  )
}) |> bind_rows()

# ---- Format and Output ----
final_table <- full_output |>
  dplyr::select(`Community (Color)`, `Topic Label`, `Top 10 FREX Words`) |>
  arrange(`Community (Color)`)

final_table

write.csv(final_table,
          file = paste0(Output_folder, Project_name, "final_topic_community_table.csv"),
          row.names = FALSE)

rm(df, final_table, full_output)


# Supervised Coding Setup ----

## Copy Top Images per Topic ----

topics_needed <- c("4", "6", "11", "16")
N_PER_TOPIC <- 40 

for (t in topics_needed) {
  dest_dir <- file.path("test_images", paste0("topic", t))
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  top_paths <- as.character(toppaths[[t]][seq_len(N_PER_TOPIC)])  # top-N images for this topic
  
  for (top_path in top_paths) {
    if (file.exists(top_path)) {
      file.copy(top_path, file.path(dest_dir, basename(top_path)), overwrite = TRUE)
      message("Copied topic ", t, ": ", basename(top_path))
    } else {
      message("Missing file for topic ", t, ": ", top_path)
    }
  }
}


# Frame Package Prevalence Over Time ----

## Compute Per-Document Package Theta ----

# Louvain community colors
louvain_colors <- c(
  "1"  = "#7D77CE",
  "2"  = "#C9821C",
  "3"  = "#68C73F"
)

meta_theta_df_comm <- meta_theta_df

which(grepl("^1", colnames(meta_theta_df_comm))) #### -1 for rowsums
meta_theta_df_comm$Ltheme1<-rowSums(meta_theta_df_comm[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$louvain == 1)]))+30]) # 1
meta_theta_df_comm$Ltheme2<-rowSums(meta_theta_df_comm[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$louvain == 2)]))+30]) # 2
meta_theta_df_comm$Ltheme3<-rowSums(meta_theta_df_comm[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$louvain == 3)]))+30]) # 3

min(meta_theta_df_comm$date)
max(meta_theta_df_comm$date)

save.image(paste0(Output_folder, Project_name, "-post-meta-theta-df-comm.Rdata"))

## Daily Sum Visualization ----

meta_theta_df_daily <- meta_theta_df_comm |>
  group_by(date) |>
  summarise(
    across(
      Ltheme1:Ltheme3,
      sum,
      .names = "sum_{.col}"
    )
  ) |>
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "Cluster",
    values_to = "sum_gamma"
  ) |>
  mutate(
    Cluster = gsub("sum_", "", Cluster),
    Cluster = gsub("_", " ", Cluster),
    Cluster = case_when(
      Cluster == "Ltheme1" ~ "(A) Conflict and Law Enforcement",
      Cluster == "Ltheme2" ~ "(B) Political Voice and Institutions",
      Cluster == "Ltheme3" ~ "(C) Mass Gatherings and Spectacle",
      TRUE ~ Cluster
    )
  ) |>
  ungroup()

write.csv(meta_theta_df_daily,
          file = paste0(Output_folder, Project_name, "daily-gamma-per-Cluster.csv"),
          row.names = FALSE)


meta_theta_df_daily |>
  ggplot(aes(x = date, y = sum_gamma, color = Cluster)) +
  geom_smooth(method = "loess", span = 0.05, se = FALSE, linewidth = 1) +
  geom_point(size = 1) +
  scale_color_manual(
    values = c(
      "(A) Conflict and Law Enforcement"          = "#7D77CE",
      "(B) Political Voice and Institutions"      = "#C9821C",
      "(C) Mass Gatherings and Spectacle"         = "#68C73F"
    ),
    guide = guide_legend(nrow = 2)
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_minor_breaks = "1 week",
    date_labels = "%b %Y"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12, family = "Times"),
    title = element_text(size = 12, family = "Times"),
    axis.text.x = element_text(size = 12, family = "Times"),
    axis.text.y = element_text(size = 12, family = "Times"),
    axis.title.x = element_text(vjust = -0.25, size = 12, family = "Times"),
    axis.title.y = element_text(vjust = -0.25, size = 12, family = "Times"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    legend.key = element_rect(fill = "white"),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(size = 12, family = "Times")
  ) +
  labs(
    x = "Date",
    y = "Share of Content",
    caption = "Note: Louvain Clustered Topics"
  )


ggsave(paste0(Output_folder, Project_name, "_Clusters_Daily_Sum.png"),
       bg="white",
       width=12,
       height=8, dpi=300)

## Daily Average Visualization ----

meta_theta_df_daily_av <- meta_theta_df_comm |>
  group_by(date) |>
  summarise(
    across(
      Ltheme1:Ltheme3,
      mean,
      .names = "mean_{.col}"
    )
  ) |>
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "Cluster",
    values_to = "mean_gamma"
  ) |>
  mutate(
    Cluster = gsub("mean_", "", Cluster),
    Cluster = gsub("_", " ", Cluster),
    Cluster = case_when(
      Cluster == "Ltheme1" ~ "(A) Conflict and Law Enforcement",
      Cluster == "Ltheme2" ~ "(B) Political Voice and Institutions",
      Cluster == "Ltheme3" ~ "(C) Mass Gatherings and Spectacle",
      TRUE ~ Cluster
    )
  ) |>
  ungroup()

write.csv(meta_theta_df_daily_av,
          file = paste0(Output_folder, Project_name, "daily-gamma-per-ave-Cluster.csv"),
          row.names = FALSE)

meta_theta_df_daily_wide <- meta_theta_df_daily_av |>
  select(date, Cluster, mean_gamma) |>
  pivot_wider(
    names_from = Cluster,
    values_from = mean_gamma
  ) |>
  mutate(across(where(is.numeric), ~round(.x, 3)))

openxlsx::write.xlsx(meta_theta_df_daily_wide,
                     file = paste0(Output_folder, Project_name, "daily-gamma-per-ave-Cluster_WIDE.xlsx"))

meta_theta_df_daily_av |>
  ggplot(aes(x = date, y = mean_gamma, color = Cluster)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, linewidth = 1) +
  # geom_point(size = 1) +
  scale_color_manual(
    values = c(
      "(A) Conflict and Law Enforcement"          = "#7D77CE",
      "(B) Political Voice and Institutions"      = "#C9821C",
      "(C) Mass Gatherings and Spectacle"         = "#68C73F"
    ),
    guide = guide_legend(nrow = 2)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_minor_breaks = "1 week",
    date_labels = "%d %b %Y"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12, family = "Times"),
    title = element_text(size = 12, family = "Times"),
    axis.text.x = element_text(size = 12, family = "Times", angle = 90),
    axis.text.y = element_text(size = 12, family = "Times"),
    axis.title.x = element_text(vjust = -0.25, size = 12, family = "Times"),
    axis.title.y = element_text(vjust = -0.25, size = 12, family = "Times"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    legend.key = element_rect(fill = "white"),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(size = 12, family = "Times")
  ) +
  labs(
    x = "Date",
    y = "Share of Content",
    caption = "Note: Louvain Clustered Topics"
  )


ggsave(paste0(Output_folder, Project_name, "_Clusters_Daily_Average.png"),
       bg="white",
       width=12,
       height=8, dpi=300)

save.image(paste0(Data_folder, Project_name,"_post-visualization.rdata"))


# Kim & Chen (2026) Codebook Results (Stage 4) ----

## Read and Parse Coded Images ----

kimchen_files <- c(
  "Police Confrontation"          = "../output/test_out_topic4_kimchen.csv",
  "Legitimacy and Voice"          = "../output/test_out_topic6_kimchen.csv",
  "Riots and Conflict"            = "../output/test_out_topic11_kimchen.csv",
  "Militarized and National Guard" = "../output/test_out_topic16_kimchen.csv"
)

topic_order <- c("Police Confrontation", "Legitimacy and Voice",
                 "Riots and Conflict", "Militarized and National Guard")

read_kimchen <- function(path, topic_name) {
  df <- read_csv(path, show_col_types = FALSE)
  
  df <- df |>
    mutate(topic = topic_name,
           failed = str_detect(caption, "Failed"))
  
  # Extract each code field regardless of spacing (PL:1 or PL: 1)
  for (code in c("PL", "PT", "VA", "EW", "SA", "MO")) {
    df[[code]] <- as.integer(str_match(df$caption, paste0(code, ":\\s*(\\d)"))[, 2])
  }
  
  df
}

all_codes <- bind_rows(
  lapply(names(kimchen_files), function(t) read_kimchen(kimchen_files[[t]], t))
)

# ---- Sanity check: how many failed per topic ----
all_codes |>
  group_by(topic) |>
  summarise(n_total = n(), n_failed = sum(failed), n_valid = sum(!failed))

## Compute Percentages by Topic ----

valid_codes <- all_codes |> filter(!failed)

summary_table <- valid_codes |>
  group_by(topic) |>
  summarise(
    n_valid = n(),
    n_total = n() + sum(all_codes$topic == first(topic) & all_codes$failed),
    `Police present`      = round(100 * mean(PL == 1, na.rm = TRUE), 1),
    `Protesters present`  = round(100 * mean(PT == 1, na.rm = TRUE), 1),
    `Contestation`        = round(100 * mean(VA == 1, na.rm = TRUE), 1),
    `No weapon visible`   = round(100 * mean(EW == 3, na.rm = TRUE), 1),
    `Solidarity action`   = round(100 * mean(SA == 1, na.rm = TRUE), 1),
    `No police visible (MO)` = round(100 * mean(MO == 3, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  mutate(`n (valid/total)` = paste0(n_valid, "/", n_total)) |>
  select(topic, `Police present`, `Protesters present`, `Contestation`,
         `No weapon visible`, `Solidarity action`, `No police visible (MO)`,
         `n (valid/total)`)

## Reshape into Table 2 Layout ----

numeric_part <- summary_table |>
  select(-`n (valid/total)`) |>
  mutate(across(-topic, as.character)) |>
  pivot_longer(-topic, names_to = "Item", values_to = "value")

n_part <- summary_table |>
  select(topic, value = `n (valid/total)`) |>
  mutate(Item = "n (valid/total)")

table2 <- bind_rows(numeric_part, n_part) |>
  pivot_wider(names_from = topic, values_from = value) |>
  select(Item, all_of(topic_order))

table2 <- table2 |>
  mutate(Item = factor(Item, levels = c(
    "Police present", "Protesters present", "Contestation",
    "No weapon visible", "Solidarity action", "No police visible (MO)",
    "n (valid/total)"
  ))) |>
  arrange(Item) |>
  mutate(Item = as.character(Item))

table2

## Save Output ----

write.csv(table2,
          file = paste0(Output_folder, Project_name, "_Table2_KimChen_results.csv"),
          row.names = FALSE)

write.csv(all_codes,
          file = paste0(Output_folder, Project_name, "_KimChen_per_image_codes.csv"),
          row.names = FALSE)

save.image(paste0(Data_folder, Project_name,"_post-manualcoding.rdata"))


# Comparative Framing Across Outlets ----

## Part A: Inductive Frame Packages (Wilcoxon Rank-Sum, by Outlet) ----
# Uses meta_theta_df_comm directly -- it already has Ltheme1/Ltheme2/Ltheme3
# (per-document theta summed within each Louvain package) alongside
# source.publisher, so no rebuild from tidy_gamma is needed.

### Reshape Ltheme columns to long format ----

package_by_doc <- meta_theta_df_comm |>
  select(index, source.publisher, Ltheme1, Ltheme2, Ltheme3) |>
  pivot_longer(
    cols = starts_with("Ltheme"),
    names_to = "Cluster",
    values_to = "theta"
  ) |>
  mutate(
    package = case_when(
      Cluster == "Ltheme1" ~ "(A) Conflict and Law Enforcement",
      Cluster == "Ltheme2" ~ "(B) Political Voice and Institutions",
      Cluster == "Ltheme3" ~ "(C) Mass Gatherings and Spectacle",
      TRUE ~ Cluster
    )
  )

### Run one Wilcoxon rank-sum test per package, NYT vs NY Post ----

wilcox_by_package <- package_by_doc |>
  group_by(package) |>
  summarise(
    n_nyt    = sum(source.publisher == "nytimes.com"),
    n_nypost = sum(source.publisher == "nypost.com"),
    median_nyt    = median(theta[source.publisher == "nytimes.com"]),
    median_nypost = median(theta[source.publisher == "nypost.com"]),
    wilcox_p = wilcox.test(
      theta[source.publisher == "nytimes.com"],
      theta[source.publisher == "nypost.com"]
    )$p.value,
    .groups = "drop"
  ) |>
  mutate(wilcox_p_adj = p.adjust(wilcox_p, method = "BH"))  # correct for testing 3 packages

print(wilcox_by_package)

### Save output ----

write.csv(wilcox_by_package,
          file = paste0(Output_folder, Project_name, "_wilcox_packages_by_outlet.csv"),
          row.names = FALSE)


## Part B: Deductive Codebook (Chi-Square / Fisher's Exact, by Outlet) ----
# Requires: all_codes (from the Kim & Chen parsing section above), with
# $image_path, $PL, $PT, $VA, $EW, $SA, $MO, $topic, $failed

### Join image codes back to outlet ----
# Matches on the shared UUID filename (e.g.
# "54f24f80-46b0-11f0-b07d-a99e97adb144.jpg") that both all_codes$image_path
# and data3$image_path carry, regardless of which folder prefix each has.

codes_with_outlet <- all_codes |>
  filter(!failed) |>
  mutate(image_file = basename(image_path)) |>
  left_join(
    data3 |> mutate(image_file = basename(image_path)) |>
      select(image_file, source.publisher),
    by = "image_file"
  )

### Run chi-square (or Fisher's exact where cell counts are small) per item ----

test_item_by_outlet <- function(df, code_col, present_value) {
  tab <- table(
    present = df[[code_col]] == present_value,
    outlet  = df$source.publisher
  )
  if (any(tab < 5)) {
    ft <- fisher.test(tab)
    tibble(test = "fisher.exact", p_value = ft$p.value)
  } else {
    ct <- chisq.test(tab)
    tibble(test = "chi.square", statistic = ct$statistic, p_value = ct$p.value)
  }
}

items <- list(
  "Police present"     = c("PL", 1),
  "Protesters present" = c("PT", 1),
  "Contestation"       = c("VA", 1),
  "No weapon visible"  = c("EW", 3),
  "Solidarity action"  = c("SA", 1),
  "No police visible"  = c("MO", 3)
)

deductive_outlet_tests <- bind_rows(lapply(names(items), function(item_name) {
  code_col <- items[[item_name]][1]
  present_value <- as.integer(items[[item_name]][2])
  result <- test_item_by_outlet(codes_with_outlet, code_col, present_value)
  result$item <- item_name
  result
})) |>
  select(item, everything()) |>
  mutate(p_value_adj = p.adjust(p_value, method = "BH"))  # correct for testing 6 items

print(deductive_outlet_tests)

### Save output ----

write.csv(deductive_outlet_tests,
          file = paste0(Output_folder, Project_name, "_deductive_codes_by_outlet.csv"),
          row.names = FALSE)

save.image(paste0(Data_folder, Project_name,"_poststats.rdata"))
