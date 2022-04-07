# To set a relative current working directory:
if (!require("rstudioapi")) install.packages("rstudioapi")
library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))

.libPaths('../lib')
library(rcrossref, lib.loc='../lib')
library(rtransparent,lib.loc='../lib')
library(oddpub,lib.loc='../lib')
library(metareadr,lib.loc='../lib')
library(stringr, lib.loc='../lib')
library(plyr, lib.loc='../lib')
library(crminer)
library(beepr, lib.loc='../lib')


#defining the rootpath, and filelist
rootpath <- getwd()

pmcidfilename="./output/pmcoalist.csv"

pmcidlist<-read.delim(pmcidfilename, header = TRUE, sep=',')

pmcidlist=pmcidlist$PMCID

pmcnumber<-list()
for (i in pmcidlist){
  go=str_replace(i,'PMC','')
  pmcnumber=c(pmcnumber,go)
  }

count=1

# remember that for this to work you need to have only open access publications from pmc!
for (i in pmcnumber){
  filename=paste0('../publications/',"PMC",i,".xml")
  metareadr::mt_read_pmcoa(pmcid=i, file_name=filename)
  
  count=count+1
  if (count %% 100 == 0){print(paste0(count," have been downloaded"))}
  if(count%%length(pmcnumber) == 0){beep()}
  }

length(pmcnumber)


filepath='./publications/'

# Creates a list called filelist with all the xml in the folder this script is in.
filelist <- list.files(filepath, pattern='*.xml', all.files=FALSE, full.names=FALSE)

# in case some do not get downloaded
downloaded=str_remove(filelist,'PMC')
downloaded=str_remove(downloaded,'.xml')

notdownloaded=setdiff(pmcnumber, downloaded)

# removing the illegal paper with omcid no. 2778622
notdownloaded=notdownloaded[-1]

for (i in notdownloaded){
  #filename=paste0('../PMC',i,".xml")
  filename=paste0('../publications/',"PMC",i,".xml")
  metareadr::mt_read_pmcoa(pmcid=i, file_name=filename)
  
  count=count+1
  if (count %% 100 == 0){print(paste0(count," have been downloaded"))}
  if(count%%length(pmcnumber) == 0){beep()}
}

pmcnumber

count<-0
# You initially run it for data and code and then using rt_all_pmc to 
# get the Conflict of interest, funding and registration data

for (i in filelist){
  count=count+1
  i=paste0('../publications/',i)
  #article <- rt_all_pmc(i, remove_ns = T)
  article <- rt_data_code_pmc(i, remove_ns = T)
  if (!exists("articles.data")) {
    articles.data <- article
  }
  
  else {
    articles.data <- rbind.fill(articles.data, article)
    #articles.data <- bind_rows(articles.data, article)
  }
  if (count %% 100 == 0){print(paste0(count," have been processed"))}

  if (count %% length(filelist) == 0){beep()}
  }; 

#write.csv(articles.data,"../output/resttransparency.csv", row.names = FALSE); beep()
write.csv(articles.data,"../output/codesharing.csv", row.names = FALSE); beep()




