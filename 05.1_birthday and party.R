library(ggplot2);library(readxl)


Deliverable_1 <- read_excel("C:/Users/Student/Downloads/Deliverable_1.xlsx", 
                            col_types = c("text", "text", "date", 
                                          "text", "text"))
View(Deliverable_1)
political_data<-data.frame(Deliverable_1)
View(political_data)


g3<-ggplot(political_data,aes(x=political_data$bday,y=political_data$party)) + 
  geom_boxplot() 