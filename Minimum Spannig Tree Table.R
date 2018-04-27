#Insalling Packages
pcks<-c("Hmisc", "readxl", "Igraph")
install.packages(pcks)

#Reading Tidy Data 
library(readxl)                                       
Pre_Crise <- read_excel("D:/Pre_Crise.xlsx",            #My file´s name is Pre_Crise and has 199 columns, one for each asset. 
            col_types = c("skip", rep("numeric", 199))) #The first column, the date, skiped

# Building the Matrices
library(Hmisc)
correlation<-rcorr(as.matrix(Pre_Crise), type="pearson") #Pearson Correlation Matrix
p_value<-as.data.frame(as.table(correlation$P))          #Table of p-values for the correlations
p_value_matrix<-as.data.frame(correlation$P)             #Matrix of p-values for the correlations
write.csv(p_value, file="p_value.csv")                   #Save de p-value CSv file
dist_cor<-sqrt(2*(1-correlation$r))                      #Creates the distance correlation matrix, based on GOWER,1966
links<-as.data.frame(as.table(dist_cor))                 #creates a table of three columns for each correlation among two variables
colnames(links)[3]="weight"                              #Rename the third columns as "weight"
write.csv(links, file="links.csv")                       #Save the table CSV file

#Building the Networks
library(igraph)
nodes<-links[1:199,]                                 #A vector of the vertices
mst1<-mst(graph_from_data_frame(d=links,             #Creates a non directed Minimum Spanning Three called mst1 by using Prim algorithm
                    vertices=nodes, directed=F))     #With links in "links" vector, the vertices in "nodes" vector 
MST<-as_data_frame(mst1)                             #Creates a table of three columns of the mst1
write.csv(MST,  file="MST.csv")                      #Save the table as CSV file

