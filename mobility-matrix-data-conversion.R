#this code converts the raw mobility data into a R-INLA adjacency matrix. It also orders the data so the order of municipalities are linked with the order of municipalities in the shpfile

library(data.table)
library(dplyr)

#====link matrix muni names to ids in the shapefile====
#import raw data
text<- readLines("mat_names.txt")

#remove areas outside of SP
orig_names<-data.frame(names=text) %>%
  filter(!names %in% c("MINAS GERAIS","RIO DE JANEIRO","PARANÁ",
                       "MATO GROSSO DO SUL")) %>%
  mutate(names=as.character(names))

#arrange names alphabetically 
names<-data.frame(names=text) %>%
       arrange(names) %>%
       filter(!names %in% 
                c("MINAS GERAIS","RIO DE JANEIRO","PARANÁ",
                         "MATO GROSSO DO SUL"))
       #mutate(repeats=duplicated(names)) 
      #filter(repeats==TRUE

#import shapefile and rearrange muni names alphabetically
muni_codes<-geobr::read_municipality(code_muni="SP",year=2018,simplified=T)
muni_codes<- muni_codes %>%
  arrange(name_muni)

#join list of matrix munis with shapefile
munis<-cbind(names,muni_codes) %>%
  select(code_muni,name_muni,names,geom) %>%
  mutate(idarea=1:nrow(.)) %>%
  mutate(names=as.character(names))

#save id file to be used on martrix 
id<-left_join(orig_names,munis[c("names","idarea","code_muni")],by="names")

#export as a csv
#then copy the id numbers manually in excel to mobility_mat.xlsx
write.csv(id,"code_muni_names.csv")

#====reorganise mobility matrix====

mob<-readxl::read_excel("mobility_mat.xlsx")

#convert to dataframe
mat <- as.data.frame(mob)

#use stack to reorder columns
mob_df <- data.frame(start= mat$col,stack(mat,select=-col)) %>%
       mutate(end=ind,trips=values) %>%
       select(-ind,-values)

# #create a df containing only trips between municipalities
# i<-which(!(mob_df$start==mob_df$end))
# mob_btwn_muni<-mob_df[i, ]

# #convert to matrix 
# mat<- matrix(unlist(matrix(mob)), ncol = 645, byrow = TRUE,)

#remove first row which contain the id numbers
# mat<-mat[-1,]

#create a df which subs trips within municipalities as 0
#then convert it to a matrix
ind <- mob_df$start==mob_df$end
mob_df2<-mob_df 
mob_df2[ind, "trips"] <- 0 
mob_mat<-matrix(sapply(mob_df2$trips, as.numeric),ncol = 645, byrow = TRUE) 

#set trip threshold value
thresh <-175

#convert matrix to a binary matrix where 1 is created when the value in the 
#matrix is greater than the threshold
# mat_bin<- matrix(unlist(as.numeric(mat>thresh)), ncol = 645, byrow = TRUE)

mat_bin<- matrix(unlist(as.numeric(mob_mat>thresh)), ncol = 645, byrow = TRUE)

#write this to a csv, add column and row identifiers, which are based on municipality codes, in Excel 
write.csv(mat_bin,"mat_bin_175.csv")

#read matrix as a inla matrix then plot it 
#library(INLA)
g <- INLA::inla.read.graph(mat_bin)
image(inla.graph2matrix(g),xlab="",ylab="")  
inla.spy(g)

#also plot original matrix based on nearest neighbour
polyg<-geobr::read_municipality(year=2018,simplified=F,code_muni="SP")
nb<-spdep::poly2nb(polyg)
head(nb)
spdep::nb2INLA("map_muni.adj", nb)
m <- INLA::inla.read.graph(filename = "map_muni.adj")
image(inla.graph2matrix(m),xlab="",ylab="")  
m_mat<-inla.graph2matrix(m)
inla.spy(m,factor=1/10)
