chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)

install.packages("ggplot2");install.packages("maps");install.packages("ggmap");install.packages("mapproj");install.packages("readxl");install.packages("stringr")
library(ggplot2); library(maps); library(ggmap); library(mapproj);library(readxl);library(stringr)
#installs and allows us to use nessecary packages 


Deliverable_1 <- read_excel("C:/Users/Student/Downloads/Deliverable_1.xlsx")

political_data<-data.frame(Deliverable_1)
View(political_data)
#reads the file

insight<-data.frame(state.name)
political_data$place<-political_data$place
insight$state.name


Democrats<-political_data[str_which("Democratic",political_data$party),]
Democrats$index<-str_which("Democratic",political_data$party)
Republicans<-political_data[str_which("Republican", political_data$party),]
Republicans$index<-str_which("Republican",political_data$party)

listr<-list()
listd<-list()
listo<-list()
for (i in insight$state.name){
  r<-0
  d<-0
  o<-0
  print(i)
  for (i2 in (1:nrow(political_data))) {
    if (grepl(i, political_data$place[i2], fixed=TRUE)){
      if (i2 %in% Democrats$index){d<-d+1}
      else if (i2 %in% Republicans$index){r<-r+1}
      else{o<-o+1}
    }
  }
  listr<-append(listr,r)
  listd<-append(listd,d)
  listo<-append(listo,o)
}
insight$r<-as.numeric(unlist(listr))
insight$d<-as.numeric(unlist(listd))
insight$o<-as.numeric(unlist(listo))

listcolor<-list()
for (i3 in 1:nrow(insight)) {
  if (insight$r[i3]==0 && insight$d[i3]==0 && insight$o[i3]==0){
    listcolor<-append(listcolor, "white")
  }
  else if (insight$d[i3]<insight$r[i3] && insight$o[i3]<insight$r[i3]){
    listcolor<-append(listcolor, "red")
  }
  else if (insight$r[i3]<insight$d[i3] && insight$o[i3]<insight$d[i3]){
    listcolor<-append(listcolor, "blue")
  }
  else if (insight$r[i3]<insight$o[i3] && insight$o[i3]>insight$d[i3]){
    listcolor<-append(listcolor, "green")
  }
  else if (insight$r[i3]==insight$d[i3] && insight$o[i3]==insight$d[i3]){
    listcolor<-append(listcolor, "black")
  }
  else if (insight$r[i3]==insight$o[i3] ){
    listcolor<-append(listcolor, "yellow")
  }
  else if (insight$o[i3]==insight$d[i3]){
    listcolor<-append(listcolor, "orange")
  }
  else if (insight$r[i3]==insight$d[i3]){
    listcolor<-append(listcolor, "purple")
  }
  else {
    listcolor<-append(listcolor, "cyan")
  }
}
listcolor
insight$color<-listcolor

us <- map_data("state")
#sets us to data for states that is needed to map USA
us$state_name <- tolower(us$region)
#makes a new column that is the regions but lowercase so it can be used

insight$state.name<-tolower(insight$state.name)
indexes<-sapply(insight$state.name, function(y) grep(y,us$state_name))
fullcolors<-list()
for (i4 in (1:nrow(insight))) {
  if (i4 == 2 || i4 == 11){next}
  else{
    x<-min(as.numeric(unlist(indexes[i4])))
    y<-max(as.numeric(unlist(indexes[i4])))
    while (x<=y) {
      fullcolors<-append(fullcolors,insight$color[i4])
      x<-x+1
    }
  }
}
fullcolors2<-list()
for (i5 in (1:15537)){
  fullcolors2<-append(fullcolors2, fullcolors[i5])
}
us$color<-fullcolors2
View(us)




str_match_all(insight$state.name,us$state_name)
str(fullcolors)
us$color<-fullcolors

map <- ggplot(us, aes(map_id= state_name))
map <- map + aes(x=long, y=lat, group=group) + geom_polygon(fill=us$color,color="black") 
map <- map + expand_limits(x=us$long, y=us$lat)
map <- map + coord_map() + ggtitle("USA Map")
map
