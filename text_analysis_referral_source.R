### Referral Source
options(java.parameters = "- Xmx1024m")
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(XLConnect)

howd <- readWorksheetFromFile("C:\\Users\\Austin.Zaccor\\Documents\\howd_you_hear.xlsx", sheet = 'Data', header = TRUE)


find_count <- function(regex_letter) {
  #' This function takes a regular expression and returns the
  #' total number of times that regex appears in the Count column.   
  keep_tally <- 0
  regex_matches <- grepl(regex_letter, howd$Description, ignore.case = TRUE, perl = TRUE)
  for(i in 1:length(regex_matches)) {
    if (regex_matches[i] == TRUE) {
      keep_tally = keep_tally + howd$Count[i]
    }
  }
  return(keep_tally) 
}

find_indices <- function(regex_letter) {
  #' This function takes a regular expression and returns the
  #' indices where that regex appears in the Count column.   
  letter_indices <- grep(regex_letter, howd$Description, ignore.case = TRUE, perl = TRUE)
  
  return(letter_indices) 
}
## Regex Searches
b <- ".*trainer.*|.*(puppy|doggy|dog|doggie).?(school|class|training|bootcamp|day.?care).*|.*bark.?buster.*"
c <- ".*friend.*vet.*|.*vet.*friend.*|.*friend.*hospital.*|.*hospital.*friend.*" # For your Vet friend
d <- "^(?=.*(friend|neighbor))((?!(vet|animal)).)*$" # For your friend who is NOT your vet
e <- "^(?=.*(googl|internet|web|http|online|interweb))((?!(review|report)).)*$" #web search
f <- ".*report.*|.*review.*|.*yelp.*|.*research.*" #Online reviews
g <- ".*puppy.?spot.*|.*pure.*" #puppyspot or purebredbreeders 
h <- ".*vested.*|.*stock.*|.*equity.*|.*share.?holder.*|.*ipo.*" #Vested interest, stock holder, etc.
i <- ".*i.*work.*vet.*|i.*am.*vet.*|.*i'm.a.*vet.*|.*rvt.*|.*lvt.*|.*dmv.*|.*cvt.*|.*certified.*" #If you are a vet
j <- "^(?=.*(vet|hospital))((?!(vca|friend|school|sister|brother|niece|nephew|son|daughter|cousin|nephew|web)).)*$" # Your vet
k <- ".*co.?worker.*|.*colleague.*|work$"
l <- "^(?=.*breeder)((?!pure).)*$" #Breeder not puppyspot
m <- ".*huma.*|.*society.*|.*shelter.*|.*rescue.*|.*spca.*|.*kennel.*" #humane society, rescue, spca, or shelter
n <- ".*groomer.*"
o <- ".*walker.*"
p <- ".*previous.*|.*prior.*|.*(have|had).*(insur|policy|pet|before).*|.*died.*|.*passed.?away.*|.*decease.*" #previous policy
q <- ".*word.of.mouth.*"
r <- ".*park.*"
s <- ".*instagram.*|.*facebook.*|.*fb.*|.*pinterest.*|.*reddit.*" #Social Media
t <- ".*vca.*"
u <- ".*family.*|.*daughter.*|.* son.*|.*father.*|.*mother.*|.*brother.*|.*sister.*|cousin.*" #family
v <- ".*seeing.?eye.*"
w <- ".*blue.?pearl.*"
x <- "^(?=.*(other.(plan|policy|pet|dog)))((?!(had|decease|passed|used.to)).)*$" #Add a pet


#find_count(y)
#find_indices(y)

#sample(setdiff(vet_indices, not_vet_indices), 40

## Create vectors for df
my_strings <- c(b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
counts_vec <- sapply(my_strings, find_count, USE.NAMES = FALSE)
## Manually adjust the vet one
counts_vec[9] <- counts_vec[9]-counts_vec[8]

## Create some category names
my_groups <- c('Trainer/Puppy School', 'Vet Friend', 'Regular Friend', 'Search Engine', 'Online Reviews', 'PuppySpot',
               'Shareholder', 'I am a Vet', 'My vet', 'Coworker', 'Other Breeder', 'Shelter or Rescue', 'Groomer',
               'Dog Walker', 'Former Policy Holder', 'Word of Mouth', 'Dog Park', 'Social Media', 'VCA', 'Family',
               'The Seeing Eye', 'Blue Pearl', 'Add a Pet')

## Create df
groups_df <- data.frame(my_groups, counts_vec)
##groups_df <- groups_df[order(groups_df$counts_vec),]###
## Create Graph

plot <- ggplot(groups_df, aes(x = reorder(my_groups, -counts_vec), y = counts_vec, label = counts_vec)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, nudge_y = 8) +
  labs(title = "How did you hear about Trupanion?") +
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
  letter_indices <- grepl(regex_letter, howd$Description, ignore.case = TRUE, perl = TRUE)
  if (length(letter_indices) != nrow(howd)) {
    Print("Aw crap, something is wrong, don't use this vector!")
    break
  }
  letter_indices
}

## Create a new df with binary variables for RollUps

b_binary <- cats_binary(b)
c_binary <- cats_binary(c)
d_binary <- cats_binary(d)
e_binary <- cats_binary(e)
f_binary <- cats_binary(f)
g_binary <- cats_binary(g)
h_binary <- cats_binary(h)
i_binary <- cats_binary(i)
j_binary <- cats_binary(j)
for (i in 1:length(j_binary)){
  if(i_binary[i]==TRUE|c_binary[i]==TRUE){
    j_binary[i]=FALSE
  }
}
k_binary <- cats_binary(k)
l_binary <- cats_binary(l)
m_binary <- cats_binary(m)
n_binary <- cats_binary(n)
o_binary <- cats_binary(o)
p_binary <- cats_binary(p)
q_binary <- cats_binary(q)
r_binary <- cats_binary(r)
s_binary <- cats_binary(s)
t_binary <- cats_binary(t)
u_binary <- cats_binary(u)
v_binary <- cats_binary(v)
w_binary <- cats_binary(w)
x_binary <- cats_binary(x)

df_binary <- data.frame(b_binary, c_binary, d_binary, e_binary, f_binary, g_binary, h_binary, i_binary, j_binary,
                        k_binary, l_binary, m_binary, n_binary, o_binary, p_binary, q_binary, r_binary, s_binary,
                        t_binary, u_binary, v_binary, w_binary, x_binary)
colnames(df_binary) <- my_groups
# Best trick of ALL TIME! multiply the logical matrix by 1 and you get a numeric matrix!!! WHAAAAA?!??!?!?!?
df_binary <- df_binary*1


df_full <- cbind(howd, df_binary)
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
df_full <- cbind(howd, df_binary, collapsed_cats)
df_full <- subset(df_full, select = -c(Count, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13,
                                       X14, X15, X16, X17, X18, X19, X20, X21, X22, X23))
colnames(df_full)[colnames(df_full)=="X1"] <- "Category_1"
colnames(df_full)[colnames(df_full)=="X2"] <- "Category_2"
colnames(df_full)[colnames(df_full)=="X3"] <- "Category_3"
# Call unassigned ones, "Other"
df_full$Category_1[is.na(df_full$Category_1)] <- "Other"


# And finally write to an .xlsx file
# WARNING: THIS TAKES A LONG TIME!!!! Like 7 minutes, must be uninterrupted too.
writeWorksheetToFile(file = "C:\\Users\\Austin.Zaccor\\Documents\\howd_you_hear.xlsx", data = df_full,
                     sheet = 'RollUps')
# I'm sorry, but writing a worksheet to a file is not an efficient process unless it's a .csv


indexx <- c(255, 182, 260, 966, 2313, 2089, 564, 2059, 2015, 1757, 2423, 1297, 465, 2416, 158,
            1644, 1160, 1531, 1235, 248, 46, 1977, 2199, 1274, 1951, 2212, 869, 886, 1540, 207,
            715, 467, 1597, 173, 1214, 395, 744, 452, 253, 230)
sort(indexx, decreasing = FALSE)

### Aw jeeze, how many of these genuises show up in multiple categories?
# The sum of all counts in the chart is:
sum(counts_vec)
#The sum of all counts in the original table is:
sum(howd$Count)
#Percent of counts that made it in a category:
100*sum(counts_vec)/sum(howd$Count)
#The number of unique indices included in the chart is:
indices_vec <- unique(unlist(lapply(my_strings, find_indices)))
length(indices_vec)
#The number of indices in the original table is:
nrow(howd)
#That percent of indices captured:
100*length(indices_vec)/nrow(howd)
#How many people are double counted?
repeat_indices <- unlist(lapply(my_strings, find_indices))
double_counts <- repeat_indices[duplicated(repeat_indices)]
length(double_counts)
#What percent is that?
100*length(double_counts)/length(repeat_indices)



