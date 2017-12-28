### Referral Source - RAF
options(java.parameters = "- Xmx1024m")
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(XLConnect)

df1 <- readWorksheetFromFile("B:\\Misc Projects\\AZ_Misc_Files\\Marketing\\Mapping RAF Referrals.xlsx",
                              sheet = 'Survey Data', header = TRUE)

find_count <- function(regex_letter) {
  #' This function takes a regular expression and returns the
  #' total number of times that regex appears in the Count column.   
  keep_tally <- 0
  regex_matches <- grepl(regex_letter, df1$Relationship, ignore.case = TRUE, perl = TRUE)
  for(i in 1:length(regex_matches)) {
    if (regex_matches[i] == TRUE) {
      keep_tally = keep_tally + df1$Count[i]
    }
  }
  return(keep_tally) 
}

find_indices <- function(regex_letter) {
  #' This function takes a regular expression and returns the
  #' indices where that regex appears in the Count column.   
  letter_indices <- grep(regex_letter, df1$Relationship, ignore.case = TRUE, perl = TRUE)
  
  return(letter_indices) 
}
## Regex Searches
a <- ".* friend.*|friend.*|.*mate.*|.*acquaintance.*" # Friend
b <- ".*colleague.*|.*customer.*|.*client.*|.*boss.*|.*business.*|.*employee.*|.*work.*|.*patient.*" # Work
c <- "^(?=.*(brother|sister|sibling))((?!(in.?law)).)*$" # Sibling
d <- ".*mother.*|.?mom.*|.*parent.*|.*father.*|.*dad.*" # Parents
e <- ".*neighbor.*|.*neighbour.*|neoghbor" # Neighbor
f <- ".*daughter.*|son.*" # Son/ Daughter
g <- ".*husband.*|.*wife.*|.*partner.*|.*girlfriend.*|.*boyfriend.*|.*spouse.*" # Partner
h <- ".*cousin.*|.*aunt.*|.*uncle.*|family|.*niece.*|.*nephew.*|.*relative.*|.*in.?law.*|family member" # Other family
i <- "dog trainer|.*dog walker.*|.*dog.*vet.*|.*dog park.*|veterinarian|.*related dog.*" # Dog thing
j <- "^$|---.*" # Blank


## Create vectors for df
my_strings <- c(a,b,c,d,e,f,g,h,i,j)
counts_vec <- sapply(my_strings, find_count, USE.NAMES = FALSE)

## Create some category names
my_groups <- c('Friend', 'Work', 'Sibling', 'Parents', 'Neighbor', 'Offspring',
               'Partner', 'Other Family', 'Dog Activities', 'Blank')

## Create df that will be used to make the bar plot
groups_df <- data.frame(my_groups, counts_vec)
##groups_df <- groups_df[order(groups_df$counts_vec),]###
## Create Graph

plot <- ggplot(groups_df, aes(x = reorder(my_groups, -counts_vec), y = counts_vec, label = counts_vec)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, nudge_y = 8) +
  labs(title = "Who Referred you to Trupanion?") +
  xlab("Grouping Description") +
  ylab("Frequency") +
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5),
    axis.text.x=element_text(angle=90,vjust=0.5, hjust = 1),
    axis.title=element_text(size=10,face="bold"))

plot


### Now we need to create a Description RollUp field and add it to the database and then write it to a .csv

cats_binary <- function(regex_letter) {
  #' This function takes a regular expression and returns a
  #' vector of booleans indicating if that index is matched.   
  letter_indices <- grepl(regex_letter, df1$Relationship, ignore.case = TRUE, perl = TRUE)
  if (length(letter_indices) != nrow(df1)) {
    Print("Aw crap, something is wrong, don't use this vector!")
    break
  }
  letter_indices
}

## Create a new df with binary variables for RollUps

a_binary <- cats_binary(a)
b_binary <- cats_binary(b)
c_binary <- cats_binary(c)
d_binary <- cats_binary(d)
e_binary <- cats_binary(e)
f_binary <- cats_binary(f)
g_binary <- cats_binary(g)
h_binary <- cats_binary(h)
i_binary <- cats_binary(i)
j_binary <- cats_binary(j)

df_binary <- data.frame(a_binary, b_binary, c_binary, d_binary, e_binary,
                        f_binary, g_binary, h_binary, i_binary, j_binary)
colnames(df_binary) <- my_groups
# Best trick of ALL TIME! multiply the logical matrix by 1 and you get a numeric matrix!!! WHAAAAA?!??!?!?!?
df_binary <- df_binary*1


df_full <- cbind(df1, df_binary)
df_full <- subset(df_full, select = -Count)

#Let's finally create that rollUp column
df_binary <- as.matrix(df_binary)
rollups_matrix <- matrix(nrow = nrow(df_binary), ncol = ncol(df_binary))


#This line takes the empty rollups_matrix and fills it in with the category name using the binary matrix df_binary
for(i in 1:nrow(rollups_matrix)) {
  for(j in 1:ncol(rollups_matrix)) {
    if(df_binary[i,j]==1){
      rollups_matrix[i,j]=colnames(df_binary)[j]
    }
  }
}


collapse_row <- function(row){
  #' This function collapses a row into as few "places" as possible
  #' by replacing the NAs (if there are any) in the first place(s) with 
  #' the non-NA values from the rest of the row. It's fine just run it.
  a_vector <- c()
  for(j in 1:ncol(rollups_matrix)) {
    if(is.na(j)==FALSE){
      a_vector <- append(a_vector, row[j])
      a_vector <- as.character(na.omit(a_vector))
    }
    length(a_vector) <- ncol(rollups_matrix)
  }
  return(a_vector)
}

# Let's create a blank canvas and then fill it in with our collapsed matrix.
col_cats <- data.frame(matrix(ncol = ncol(rollups_matrix), nrow = nrow(rollups_matrix)))
collapse_matrix <- function(matrix){
  #' This function basically just applies the function above
  #' over a whole matrix. Collapsing a single star now allows
  #' us to collapse entire galaxies. It's fine just run it.
  for(i in 1:nrow(rollups_matrix)) {
    col_cats[i,] = collapse_row(rollups_matrix[i,])
  }
  return(col_cats)
}

collapsed_cats <- collapse_matrix(rollups_matrix)


##Let's combine everything and fill empty Categories with Other
df_full <- cbind(df1, df_binary, collapsed_cats)
df_full <- subset(df_full, select = -c(Count, X3, X4, X5, X6, X7, X8, X9, X10))
colnames(df_full)[colnames(df_full)=="X1"] <- "Category_1"
colnames(df_full)[colnames(df_full)=="X2"] <- "Category_2"

# Call unassigned ones, "Other"
df_full$Category_1[is.na(df_full$Category_1)] <- "Other"


# And finally write to an .xlsx file
writeWorksheetToFile(file = "B:\\Misc Projects\\AZ_Misc_Files\\Marketing\\Mapping RAF Referrals.xlsx",
                     data = df_full, sheet = 'RollUps')


