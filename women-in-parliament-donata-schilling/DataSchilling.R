library(legislatoR)
library(dplyr)
library(igraph)
library(xml2)
library(stringr)
library(rvest)
library(magrittr)
library(ggplot2)
library(rvest)
library(ggthemes)
library(scales)



# Women in parliaments worldwide ------------------------------------------

url <- read_html("http://archive.ipu.org/wmn-e/arc/classif010418.htm")
parliaments <- html_table(url, header = TRUE, fill = TRUE) %>% .[[3]]
index <- as.vector(parliaments[2,1:10])
names(parliaments) <- index
parliaments <-  parliaments[3:195,1:6] %>% .[seq(1,195,5),]
parliaments$`% W` <- parliaments$`% W` %>% str_replace_all("%", "") %>%  as.numeric()/100

p <- ggplot(parliaments, aes(x=reorder(Country, desc(`% W`)), y=`% W`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  theme_fivethirtyeight() +
  geom_segment(aes(x="Germany", xend ="Germany", y=0.34, yend=0.32),size = 1, colour = "red", arrow = arrow(length = unit(0.1, "cm"))) +
  theme(axis.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1), plot.caption = element_text(size = 8), plot.title = element_text(size = 10)) +
  labs(y='Share of Women in Parliament', title = "Share of Women in Parliament in Selected Countries*", caption = "\n*Every fifth entry from a list of 195 countries for readability")
p

ggsave("figures/globalfemaleshare.pdf", width = 15, height = 15, units = "cm")

# Import data -------------------------------------------------------------

ger_politicians <- inner_join(x=get_core(legislature = "germany"), filter(get_political(legislature = "germany")),id="pageid")


# Correct Wikipedia article names ----------------------------------------------------------

ger_politicians$wikititle[ger_politicians$wikititle == "Werner_Kuhn_(CDU)"] <- "Werner_Kuhn_(Politiker,_1955)"
ger_politicians$wikititle[ger_politicians$wikititle == "Stefan_Müller_(CSU)"] <- "Stefan_Müller_(Politiker,_1975)"
ger_politicians$wikititle[ger_politicians$wikititle == "Martin_Mayer_(CSU)"] <- "Martin_Mayer_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Rudolf_Kraus_(CSU)"]<- "Rudolf_Kraus_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Michael_Kretschmer_(Politiker)"] <- "Michael_Kretschmer"
ger_politicians$wikititle[ger_politicians$wikititle == "Gerd_Müller_(CSU)"] <- "Gerd_Müller_(Politiker,_1955)"
ger_politicians$wikititle[ger_politicians$wikititle == "Heinz_Wiese_(CDU)"] <- "Heinz_Wiese_(Politiker,_1945)"
ger_politicians$wikititle[ger_politicians$wikititle == "Dieter_Steinecke_(SPD)"] <- "Dieter_Steinecke_(Politiker,_1954)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hans_Friedrich_(FDP)"] <- "Hans_Friedrich_(Politiker,_1917)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hermann_Götz_(CDU)"] <- "Hermann_Götz_(Politiker,_1914)"
ger_politicians$wikititle[ger_politicians$wikititle == "Curt_Hoffmann_(FDP)"] <- "Curt_Hoffmann_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Werner_Jacobi_(SPD)"] <- "Werner_Jacobi_(Politiker,_1907)"
ger_politicians$wikititle[ger_politicians$wikititle == "Franz_Marx_(SPD)"] <- "Franz_Marx_(Politiker,_1903)"
ger_politicians$wikititle[ger_politicians$wikititle == "Erich_Meyer_(SPD)"] <- "Erich_Meyer_(Politiker,_1900)"
ger_politicians$wikititle[ger_politicians$wikititle == "Kurt_Neubauer_(SPD)"] <- "Kurt_Neubauer_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Otto_Fürst_von_Bismarck_(CDU)"] <- "Otto_Fürst_von_Bismarck"
ger_politicians$wikititle[ger_politicians$wikititle == "Hans_Geiger_(SPD)"] <- "Hans_Geiger_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Georg_Schneider_(CDU)"] <- "Georg_Schneider_(Politiker,_1892)"
ger_politicians$wikititle[ger_politicians$wikititle == "Heinrich_Schneider_(FDP)"] <- "Heinrich_Schneider_(Politiker,_1907)"
ger_politicians$wikititle[ger_politicians$wikititle == "Werner_Schwarz_(CDU)"] <- "Werner_Schwarz_(Politiker,_1900)"
ger_politicians$wikititle[ger_politicians$wikititle == "Friedrich_Winter_(CSU)"] <- "Friedrich_Winter_(Politiker,_1902)"
ger_politicians$wikititle[ger_politicians$wikititle == "Franz_Wittmann_(CSU)"] <- "Franz_Wittmann_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Heinrich_Zimmermann_(Politiker)"] <- "Heinrich_Zimmermann_(Politiker,_1893)"
ger_politicians$wikititle[ger_politicians$wikititle == "Wolfgang_Bartels_(CDU)"] <- "Wolfgang_Bartels_(Politiker,_1903)"
ger_politicians$wikititle[ger_politicians$wikititle == "Ernst_Keller_(FDP)"] <- "Ernst_Keller_(Politiker,_September_1900)"
ger_politicians$wikititle[ger_politicians$wikititle == "Friedrich_Kraus_(SPD)"] <- "Friedrich_Kraus_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hans_Krüger_(CDU)"] <- "Hans_Krüger_(Politiker,_1902)"
ger_politicians$wikititle[ger_politicians$wikititle == "Otto_Schmidt_(CDU)"] <- "Otto_Schmidt_(Politiker,_1902)"
ger_politicians$wikititle[ger_politicians$wikititle == "Heinrich_Junker_(SPD)"] <- "Heinrich_Junker_(Politiker,_1923)"
ger_politicians$wikititle[ger_politicians$wikititle == "Günter_Klein_(SPD)"] <- "Günter_Klein_(Politiker,_1900)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hermann_Schmidt_(SPD)"] <- "Hermann_Schmidt_(Politiker,_1917)"
ger_politicians$wikititle[ger_politicians$wikititle == "Gustav_Stein_(CDU)"] <- "Gustav_Stein_(Politiker,_1903)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hans_Wellmann_(SPD)"] <- "Hans_Wellmann_(Politiker,_1911)"
ger_politicians$wikititle[ger_politicians$wikititle == "Werner_Marx_(CDU)"] <- "Werner_Marx_(Politiker,_1924)"
ger_politicians$wikititle[ger_politicians$wikititle == "Martin_Wendt_(SPD)"] <- "Martin_Wendt_(Politiker,_1935)"
ger_politicians$wikititle[ger_politicians$wikititle == "Günter_Böhme_(CDU)"] <- "Günter_Böhme_(Politiker,_1925)"
ger_politicians$wikititle[ger_politicians$wikititle == "Manfred_Schmidt_(CDU)"] <- "Manfred_Schmidt_(Politiker,_1929)"
ger_politicians$wikititle[ger_politicians$wikititle == "Manfred_Schmidt_(SPD)"] <- "Manfred_Schmidt_(Politiker,_1936)"
ger_politicians$wikititle[ger_politicians$wikititle == "Karl_Becker_(CDU)"] <- "Karl_Becker_(Politiker,_1923)"
ger_politicians$wikititle[ger_politicians$wikititle == "Klaus_Beckmann_(FDP)"] <- "Klaus_Beckmann_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Günther_Jansen_(Politiker)"] <- "Günther_Jansen_(Politiker,_1936)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hermann_Schmidt_(SPD)"] <- "Hermann_Schmidt_(Politiker,_1917)"
ger_politicians$wikititle[ger_politicians$wikititle == "Volker_Jung_(SPD)"] <- "Volker_Jung_(Politiker,_1942)"
ger_politicians$wikititle[ger_politicians$wikititle == "Michael_Fischer_(CDU)"] <- "Michael_Fischer_(Politiker,_1947)"
ger_politicians$wikititle[ger_politicians$wikititle == "Johannes_Singer_(Jurist)"] <- "Johannes_Singer_(Politiker)"
ger_politicians$wikititle[ger_politicians$wikititle == "Heinz_Wagner_(CDU)"] <- "Heinz_Wagner_(Politiker,_1939)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hans_Zimmermann_(CDU)"] <- "Hans_Zimmermann_(Politiker,_1948)"
ger_politicians$wikititle[ger_politicians$wikititle == "Günter_Klein_(CDU)"] <- "Günter_Klein_(Politiker,_1930)"
ger_politicians$wikititle[ger_politicians$wikititle == "Hanna_Wolf_(SPD)"] <- "Hanna_Wolf_(Politikerin,_1908)"
ger_politicians$wikititle[ger_politicians$wikititle == "Rudolf_Braun_(CDU)"] <- "Rudolf_Braun_(Politiker,_1955)"
ger_politicians$wikititle[ger_politicians$wikititle == "Peter_Enders_(SPD)"] <- "Peter_Enders_(Politiker,_1942)"

ger_politicians$session_start <- str_replace(ger_politicians$session_start, "-[0-9]{2}-[0-9]{2}", "")


# Filter into separate dfs ------------------------------------------------

list_politicians <- list()
for (i in 1:18) {
  list_politicians[i] <- list(filter(ger_politicians, session == i))
  list_politicians[[i]]$session_start <- first(list_politicians[[i]]$session_start)
  assign(paste('ger_politicians',i,sep=''),as.data.frame(list_politicians[i]))
}


# Download Wikipages ------------------------------------------------------

#Create list of urls, folders and names

baseurl <- "https://de.wikipedia.org/wiki/"

urls <- list()
for (i in 1:18) {
  urls[i] <- list(paste0(rep(baseurl, times = length(list_politicians[[i]]$wikititle)), list_politicians[[i]]$wikititle))
  assign(paste('urls',i,sep=''),unlist(urls[i]))
  if(!dir.exists(paste0("politicians_htmls_",i))) {dir.create(paste0("politicians_htmls_",i))
    }
  
}

folder <- list()
names <- list()
for (i in 1:18) {
  folder[i] <- paste0("politicians_htmls_",i,"/")
  assign(paste('folder',i,sep=''),unlist(folder[i]))
  names[i] <- list(paste0(list_politicians[[i]][["wikititle"]], ".html"))
  assign(paste0("names",i), unlist(names[i]))
}


# Download ---------------------------------------------------

# Download files

# for(x in 1:18) {
#   for(i in 1:length(urls[x])) {
#     if(!file.exists(paste0(folder[x], names[x][i]))) {
#       try(download.file(unlist(urls[x][i]), destfile = paste0(rep(folder[x], length(names[x][i])), unlist(names[x][i]))), silent = T)
#       Sys.sleep(runif(1,0,1))
#     }
#   }
# }


#Separate downloads, in case the loop crashes R

for(i in 1:length(urls1)) {
  if(!file.exists(paste0(folder1, names1[i]))) {
    try(download.file(urls1[i], destfile = paste0(folder1, names1[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}


for(i in 1:length(urls2)) {
  if(!file.exists(paste0(folder2, names2[i]))) {
    try(download.file(urls2[i], destfile = paste0(folder2, names2[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls3)) {
  if(!file.exists(paste0(folder3, names3[i]))) {
    try(download.file(urls3[i], destfile = paste0(folder3, names3[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}


for(i in 1:length(urls4)) {
  if(!file.exists(paste0(folder4, names4[i]))) {
    try(download.file(urls4[i], destfile = paste0(folder4, names4[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls5)) {
  if(!file.exists(paste0(folder5, names5[i]))) {
    try(download.file(urls5[i], destfile = paste0(folder5, names5[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls6)) {
  if(!file.exists(paste0(folder6, names6[i]))) {
    try(download.file(urls6[i], destfile = paste0(folder6, names6[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls7)) {
  if(!file.exists(paste0(folder7, names7[i]))) {
    try(download.file(urls7[i], destfile = paste0(folder7, names7[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}


for(i in 1:length(urls8)) {
  if(!file.exists(paste0(folder8, names8[i]))) {
    try(download.file(urls8[i], destfile = paste0(folder8, names8[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls9)) {
  if(!file.exists(paste0(folder9, names9[i]))) {
    try(download.file(urls9[i], destfile = paste0(folder9, names9[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls10)) {
  if(!file.exists(paste0(folder10, names10[i]))) {
    try(download.file(urls10[i], destfile = paste0(folder10, names10[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls11)) {
  if(!file.exists(paste0(folder11, names11[i]))) {
    try(download.file(urls11[i], destfile = paste0(folder11, names11[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls12)) {
  if(!file.exists(paste0(folder12, names12[i]))) {
    try(download.file(urls12[i], destfile = paste0(folder12, names12[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls13)) {
  if(!file.exists(paste0(folder13, names13[i]))) {
    try(download.file(urls13[i], destfile = paste0(folder13, names13[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls14)) {
  if(!file.exists(paste0(folder14, names14[i]))) {
    try(download.file(urls14[i], destfile = paste0(folder14, names14[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls15)) {
  if(!file.exists(paste0(folder15, names15[i]))) {
    try(download.file(urls15[i], destfile = paste0(folder15, names15[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls16)) {
  if(!file.exists(paste0(folder16, names16[i]))) {
    try(download.file(urls16[i], destfile = paste0(folder16, names16[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls17)) {
  if(!file.exists(paste0(folder17, names17[i]))) {
    try(download.file(urls17[i], destfile = paste0(folder17, names17[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}

for(i in 1:length(urls18)) {
  if(!file.exists(paste0(folder18, names18[i]))) {
    try(download.file(urls18[i], destfile = paste0(folder18, names18[i])), silent = T)
    Sys.sleep(runif(1,0,1))
  }
}


##Calculate PageRank ---------------------------------------

files_parsed <- list()
files <- list()
article_url <- list()
for (i in 1:18) {
  folder[i] <- str_replace(folder[i],"/", "")
  assign(paste0("folder",i),unlist(folder[i]))
  files[i] <- list(list.files(as.character(folder[i]), full.names = T))
  assign(paste0("files",i),unlist(files[i]))
  #files_parsed[i] <- list(lapply(as.character(files[i]), read_html))
  #names(files_parsed[i] <- list(basename(files[i])))
  #article_url[i] <- list(paste0("/wiki/", str_replace(basename(files[i]), ".html$", "")))
}

# 1 -----------------------------------------------------------------------

files_parsed <- lapply(files1, read_html)
names(files_parsed) <- basename(files1)
article_url <- paste0("/wiki/", str_replace(basename(files1), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki1 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)

article_name <- str_replace(basename(files1), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki1 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank1 <- merge(ger_politicians1, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank1, file="pagerank1.RDa")


# 2 -----------------------------------------------------------------------

files_parsed <- lapply(files2, read_html)
names(files_parsed) <- basename(files2)
article_url <- paste0("/wiki/", str_replace(basename(files2), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki2 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files2), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki2 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank2 <- merge(ger_politicians2, pagerank_df, by.x = "wikititle", by.y = "page_url")

save(ger_pagerank2, file="pagerank2.RDa")

# 3 -----------------------------------------------------------------------

files_parsed <- lapply(files3, read_html)
names(files_parsed) <- basename(files3)
article_url <- paste0("/wiki/", str_replace(basename(files3), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki3 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files3), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki3 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank3 <- merge(ger_politicians3, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank3, file="pagerank3.RDa")


# 4 -----------------------------------------------------------------------

files_parsed <- lapply(files4, read_html)
names(files_parsed) <- basename(files4)
article_url <- paste0("/wiki/", str_replace(basename(files4), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki4 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files4), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki4 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank4 <- merge(ger_politicians4, pagerank_df, by.x = "wikititle", by.y = "page_url")

save(ger_pagerank4, file="pagerank4.RDa")


# 5 -----------------------------------------------------------------------

files_parsed <- lapply(files5, read_html)
names(files_parsed) <- basename(files5)
article_url <- paste0("/wiki/", str_replace(basename(files5), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki5 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files5), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki5 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank5 <- merge(ger_politicians5, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank5, file="pagerank5.RDa")


# 6 -----------------------------------------------------------------------

files_parsed <- lapply(files6, read_html)
names(files_parsed) <- basename(files6)
article_url <- paste0("/wiki/", str_replace(basename(files6), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki6 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files6), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki6 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank6 <- merge(ger_politicians6, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank6, file="pagerank6.RDa")

# 7 -----------------------------------------------------------------------

files_parsed <- lapply(files7, read_html)
names(files_parsed) <- basename(files7)
article_url <- paste0("/wiki/", str_replace(basename(files7), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki7 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files7), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki7 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank7 <- merge(ger_politicians7, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank7, file="pagerank7.RDa")


# 8 -----------------------------------------------------------------------

files_parsed <- lapply(files8, read_html)
names(files_parsed) <- basename(files8)
article_url <- paste0("/wiki/", str_replace(basename(files8), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki8 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files8), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki8 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank8 <- merge(ger_politicians8, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank8, file="pagerank8.RDa")


# 9 -----------------------------------------------------------------------

files_parsed <- lapply(files9, read_html)
names(files_parsed) <- basename(files9)
article_url <- paste0("/wiki/", str_replace(basename(files9), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki9 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files9), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki9 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank9 <- merge(ger_politicians9, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank9, file="pagerank9.RDa")


# 10 ----------------------------------------------------------------------

files_parsed <- lapply(files10, read_html)
names(files_parsed) <- basename(files10)
article_url <- paste0("/wiki/", str_replace(basename(files10), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki10 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files10), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki10 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank10 <- merge(ger_politicians10, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank10, file="pagerank10.RDa")


# 11 ----------------------------------------------------------------------

files_parsed <- lapply(files11, read_html)
names(files_parsed) <- basename(files11)
article_url <- paste0("/wiki/", str_replace(basename(files11), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki11 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files11), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki11 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank11 <- merge(ger_politicians11, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank11, file="pagerank11.RDa")


# 12 ----------------------------------------------------------------------

files_parsed <- lapply(files12, read_html)
names(files_parsed) <- basename(files12)
article_url <- paste0("/wiki/", str_replace(basename(files12), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki12 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files12), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki12 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank12 <- merge(ger_politicians12, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank12, file="pagerank12.RDa")


# 13 ----------------------------------------------------------------------

files_parsed <- lapply(files13, read_html)
names(files_parsed) <- basename(files13)
article_url <- paste0("/wiki/", str_replace(basename(files13), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki13 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)

article_name <- str_replace(basename(files13), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki13 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank13 <- merge(ger_politicians13, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank13, file="pagerank13.RDa")



# 14 ----------------------------------------------------------------------

files_parsed <- lapply(files14, read_html)
names(files_parsed) <- basename(files14)
article_url <- paste0("/wiki/", str_replace(basename(files14), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki14 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)


article_name <- str_replace(basename(files14), ".html$", "")


## compute pageRank ----------
pagerank <- graph_wiki14 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank14 <- merge(ger_politicians14, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank14, file="pagerank14.RDa")

# 15 ----------------------------------------------------------------------

files_parsed <- lapply(files15, read_html)
names(files_parsed) <- basename(files15)
article_url <- paste0("/wiki/", str_replace(basename(files15), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki15 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files15), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki15 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank15 <- merge(ger_politicians15, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank15, file="pagerank15.RDa")


# 16 ----------------------------------------------------------------------

files_parsed <- lapply(files16, read_html)
names(files_parsed) <- basename(files16)
article_url <- paste0("/wiki/", str_replace(basename(files16), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki16 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files16), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki16 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank16 <- merge(ger_politicians16, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank16, file="pagerank16.RDa")


# 17 ----------------------------------------------------------------------

files_parsed <- lapply(files17, read_html)
names(files_parsed) <- basename(files17)
article_url <- paste0("/wiki/", str_replace(basename(files17), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki17 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files17), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki17 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank17 <- merge(ger_politicians17, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank17, file="pagerank17.RDa")


# 18 ----------------------------------------------------------------------

files_parsed <- lapply(files18, read_html)
names(files_parsed) <- basename(files18)
article_url <- paste0("/wiki/", str_replace(basename(files18), ".html$", ""))

connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%100==0) { print(paste0(i, " cases"))}
}

# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)

## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
graph_wiki18 <- graph_from_edgelist(as.matrix(connections), directed = TRUE)



article_name <- str_replace(basename(files18), ".html$", "")



## compute pageRank ----------
pagerank <- graph_wiki18 %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, stringsAsFactors = FALSE)
pagerank_df %>% arrange(desc(pagerank)) %>% head(10)


# Combine dfs -------------------------------------------------------------

ger_pagerank18 <- merge(ger_politicians18, pagerank_df, by.x = "wikititle", by.y = "page_url")
save(ger_pagerank18, file="pagerank18.RDa")



# Rank and add sex dummies -------------------------------------------------------------

ger_politicians$dummy <- ifelse(ger_politicians$sex == "female", 1,0)
ger_politicians$session <- as.numeric(ger_politicians$session)

# ger_pagerank <- list()
# for (i in 1:18) {
#   ger_pagerank[i] <- lapply(paste0("ger_pagerank",i),get)
#   ger_pagerank[i] <- list(arrange(as.data.frame(ger_pagerank[i]), desc(pagerank)))
#   ger_pagerank[i] <- list(ifelse(get(paste0("ger_pagerank",i,"$dummy"))=="female",1,0))
#   assign(paste0("ger_pagerank",i),as.data.frame(ger_pagerank[i]))
# }

ger_pagerank1 <-ger_pagerank1 %>% arrange(desc(pagerank))
ger_pagerank2 <-ger_pagerank2 %>% arrange(desc(pagerank))
ger_pagerank4 <-ger_pagerank4 %>% arrange(desc(pagerank))
ger_pagerank6 <-ger_pagerank6 %>% arrange(desc(pagerank))
ger_pagerank8 <-ger_pagerank8 %>% arrange(desc(pagerank))
ger_pagerank10 <-ger_pagerank10 %>% arrange(desc(pagerank))
ger_pagerank12 <-ger_pagerank12 %>% arrange(desc(pagerank))
ger_pagerank14 <-ger_pagerank14 %>% arrange(desc(pagerank))
ger_pagerank16 <-ger_pagerank16 %>% arrange(desc(pagerank))
ger_pagerank18 <-ger_pagerank18 %>% arrange(desc(pagerank))
ger_pagerank3 <-ger_pagerank3 %>% arrange(desc(pagerank))
ger_pagerank5 <-ger_pagerank5 %>% arrange(desc(pagerank))
ger_pagerank7 <-ger_pagerank7 %>% arrange(desc(pagerank))
ger_pagerank9 <-ger_pagerank9 %>% arrange(desc(pagerank))
ger_pagerank11 <-ger_pagerank11 %>% arrange(desc(pagerank))
ger_pagerank13 <-ger_pagerank13 %>% arrange(desc(pagerank))
ger_pagerank15 <-ger_pagerank15 %>% arrange(desc(pagerank))
ger_pagerank17 <-ger_pagerank17 %>% arrange(desc(pagerank))


ger_pagerank1$dummy <- ifelse(ger_pagerank1$sex == "female", 1,0)
ger_pagerank2$dummy <- ifelse(ger_pagerank2$sex == "female", 1,0)
ger_pagerank4$dummy <- ifelse(ger_pagerank4$sex == "female", 1,0)
ger_pagerank6$dummy <- ifelse(ger_pagerank6$sex == "female", 1,0)
ger_pagerank8$dummy <- ifelse(ger_pagerank8$sex == "female", 1,0)
ger_pagerank10$dummy <- ifelse(ger_pagerank10$sex == "female", 1,0)
ger_pagerank12$dummy <- ifelse(ger_pagerank12$sex == "female", 1,0)
ger_pagerank14$dummy <- ifelse(ger_pagerank14$sex == "female", 1,0)
ger_pagerank16$dummy <- ifelse(ger_pagerank16$sex == "female", 1,0)
ger_pagerank18$dummy <- ifelse(ger_pagerank18$sex == "female", 1,0)
ger_pagerank3$dummy <- ifelse(ger_pagerank3$sex == "female", 1,0)
ger_pagerank5$dummy <- ifelse(ger_pagerank5$sex == "female", 1,0)
ger_pagerank7$dummy <- ifelse(ger_pagerank7$sex == "female", 1,0)
ger_pagerank9$dummy <- ifelse(ger_pagerank9$sex == "female", 1,0)
ger_pagerank11$dummy <- ifelse(ger_pagerank11$sex == "female", 1,0)
ger_pagerank13$dummy <- ifelse(ger_pagerank13$sex == "female", 1,0)
ger_pagerank15$dummy <- ifelse(ger_pagerank15$sex == "female", 1,0)
ger_pagerank17$dummy <- ifelse(ger_pagerank17$sex == "female", 1,0)

# pagerankshare10 <- vector()
# for (i in 1:18) {
#   pagerankshare10[i] <- list(get(paste0("ger_pagerank",i,"$dummy")))
#   pagerankshare10[i] <- pagerankshare10[i][1:10,]
#   %>% .$dummy %>% sum()/10
# }


# Calculate share of women in top 100 ------------------


pagerankshare100 <- vector()
pagerankshare100[1] <- ger_pagerank1[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[2] <- ger_pagerank2[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[4] <- ger_pagerank4[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[6] <- ger_pagerank6[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[8] <- ger_pagerank8[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[10] <- ger_pagerank10[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[12] <- ger_pagerank12[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[14] <- ger_pagerank14[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[16] <- ger_pagerank16[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[18] <- ger_pagerank18[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[3] <- ger_pagerank2[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[5] <- ger_pagerank4[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[7] <- ger_pagerank6[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[9] <- ger_pagerank8[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[11] <- ger_pagerank10[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[13] <- ger_pagerank12[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[15] <- ger_pagerank14[1:100,] %>% .$dummy %>% sum()/100
pagerankshare100[17] <- ger_pagerank16[1:100,] %>% .$dummy %>% sum()/100

ger_politicians$session_start <- str_replace(ger_politicians$session_start, "-[0-9]{2}-[0-9]{2}", "")


# Tables --------------------------------------------------------------------

ger_pagerank1$Rank <- seq(1,nrow(ger_pagerank1),1)
ger_pagerank2$Rank <- seq(1,nrow(ger_pagerank2),1)
ger_pagerank3$Rank <- seq(1,nrow(ger_pagerank3),1)
ger_pagerank4$Rank <- seq(1,nrow(ger_pagerank4),1)
ger_pagerank5$Rank <- seq(1,nrow(ger_pagerank5),1)
ger_pagerank6$Rank <- seq(1,nrow(ger_pagerank6),1)
ger_pagerank7$Rank <- seq(1,nrow(ger_pagerank7),1)
ger_pagerank8$Rank <- seq(1,nrow(ger_pagerank8),1)
ger_pagerank9$Rank <- seq(1,nrow(ger_pagerank9),1)
ger_pagerank10$Rank <- seq(1,nrow(ger_pagerank10),1)
ger_pagerank11$Rank <- seq(1,nrow(ger_pagerank11),1)
ger_pagerank12$Rank <- seq(1,nrow(ger_pagerank12),1)
ger_pagerank13$Rank <- seq(1,nrow(ger_pagerank13),1)
ger_pagerank14$Rank <- seq(1,nrow(ger_pagerank14),1)
ger_pagerank15$Rank <- seq(1,nrow(ger_pagerank15),1)
ger_pagerank16$Rank <- seq(1,nrow(ger_pagerank16),1)
ger_pagerank17$Rank <- seq(1,nrow(ger_pagerank17),1)
ger_pagerank18$Rank <- seq(1,nrow(ger_pagerank18),1)

names <- rbind(ger_pagerank1,ger_pagerank2,ger_pagerank3,ger_pagerank4,ger_pagerank5,ger_pagerank6,ger_pagerank7,ger_pagerank8,ger_pagerank9,ger_pagerank10,ger_pagerank11,ger_pagerank12,ger_pagerank13,ger_pagerank14,ger_pagerank15,ger_pagerank16,ger_pagerank17,ger_pagerank18) %>% group_by(session_start) %>% arrange(desc(pagerank), .by_group = T) %>% dplyr::select(c(Rank, Name = name, Year = session_start, PageRank = pagerank, Sex = sex))

names$Sex <- as.factor(names$Sex)
names$Year <- as.factor(names$Year)
names$Rank <- as.factor(names$Rank)

datatable(names, filter = "top", rownames = F, options = list(pageLength = 10, dom = "t"), autoHideNavigation = T) %>% formatRound(columns=c('PageRank'), digits=4)


# Plot --------------------------------------------------------------------

dat <- ger_politicians %>% group_by(session) %>% summarise(share = mean(dummy), session_start=first(session_start))

dat <- cbind(dat, pagerankshare100)

f1 <- list(size = 14, color = "grey")
f2 <- list(size = 12, color = "grey")

p <- plot_ly(dat, x=~session_start, y=~share, name = "Bundestag", type = "scatter", mode = "lines", line=list(color = 'dimgrey')) %>% 
  add_trace(x=~session_start, y=~pagerankshare100, name = "Top100 PageRank", mode = "lines", line=list(color = 'firebrick')) %>% 
  layout(font=list(family ="Helvetica"), yaxis = list(title = "Share", tickformat = "%", titlefont = f1, tickfont = f2),
         xaxis = list(titlefont = f2, tickangle = 35, tickfont = f2), showlegend = F, hovermode = "compare") %>% 
  config(displayModeBar = FALSE)
p