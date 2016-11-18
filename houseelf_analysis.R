library(stringr)
library(dplyr)

library(ShortRead)
#download data
elves <- read.csv("houseelf-earlength-dna-data.csv", stringsAsFactors = FALSE)
View(elves)


##create  functions


earlength <- function(ear){
  ##function that deems ear size large or small
  if (ear >10){
    return("large")
  }else if (ear <= 10){
    return("small")
  }
}


GC_counter <- function(sequences){
  #counts number of Gs and Cs in DNA string
  Gs <- str_count(sequences, "g")
  gs <- str_count(sequences, "G")
  Cs <- str_count(sequences, "c")
  cs <- str_count(sequences, "C")
  gc_content <- (Gs + Cs+cs+gs) / str_length(sequences) * 100 
  return(gc_content)
}

##create data frame

elf_table <- data.frame(id=elves$id,ear=character(nrow(elves)), GC_content=numeric(nrow(elves)), stringsAsFactors = FALSE)



for(i in 1:(nrow(elves))){
  
  elf_table[i,]$ear <- earlength(elves$earlength[i])
  elf_table[i,]$GC_content <- GC_counter(elves$dnaseq[i])
  elf_table[i,]$id <- elves$id[i]
  
}

elf_table


str(elf_table)


###writing to CSV
write.csv(elf_table, "grangers_analysis.csv")



is.numeric(elf_table)


##########averaging GC content by ear size category#



##summarise..

avg_GC_by_earsize <- summarise(group_by(elf_table, ear), mean_GC = mean(GC_content, na.rm = TRUE))
avg_GC_by_earsize_df <- data.frame(avg_GC_by_earsize, stringsAsFactors = FALSE)
avg_GC_by_earsize_df
