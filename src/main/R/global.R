suppressMessages(library("rjson"))
suppressMessages(library("RCurl")) 

GetRepos <- function(organization) {

# get the data from GitHub
repos.json <- fromJSON(getURLContent(paste0("https://api.github.com/orgs/", organization, "/repos?per_page=1000"), 
    ssl.verifypeer = FALSE, useragent = "R"))

project.name <- c()
project.fullname <- c()
project.description <- c()
project.lastupdate <- c()
project.creationdate <- c()
project.language <- c()
project.forks <- c()
project.stars <- c()

# create a dataframe with the repos
for(i in 1:length(repos.json)) {
  repo <- repos.json[[i]]
  
  if (repo$private == FALSE) {
    
    project.name <- append(project.name, repo$name)
    project.fullname <- append(project.fullname, repo$full_name)
    project.description <- append(project.description, repo$description)
    project.lastupdate <- append(project.lastupdate, repo$updated_at)
    project.creationdate  <- append(project.creationdate , repo$created_at)
    project.language <- append(project.language, ifelse(length(repo$language) > 0, repo$language, "")) 
    project.forks <- append(project.forks , repo$forks_count)
    project.stars <- append(project.stars , repo$stargazers_count)
     
  }
}


repo.frame <- data.frame(Project=project.name, 
  Description=project.description, 
  Language=project.language,
  LastTimeUpdate=project.lastupdate,
  CreationTimeDate=project.creationdate, 
  ProjectFullName=project.fullname,
  Forks=project.forks,
  Stars=project.stars,
  stringsAsFactors=FALSE)

return(repo.frame)

}

GetPublicMembers <- function(organization) {

  # get the data from GitHub
  members.json <- fromJSON(getURLContent(paste0("https://api.github.com/orgs/", organization, "/public_members?per_page=1000"),
        ssl.verifypeer = FALSE, useragent = "R"))

  tmp.df <- as.data.frame(do.call("rbind", members.json))
 
  members.df <- as.data.frame(cbind(unlist(tmp.df$login), unlist(tmp.df$avatar_url), unlist(tmp.df$html_url)))
  
  colnames(members.df) <- c("login", "avatar", "url")

  return(members.df)
}

