info <- read.table('~/GoogleDrive/LONI/General_Codes/Boris_Shape_Pipe/Vertex_Number_Boris_SSA.txt',header=TRUE)

# Name of metrics
metric.id <- c('thick','LogJacs')

# set path to shape output
path = './'

# get subject directories from base path
sub.dirs <- dir(path)

# initialize data storage list
dat <- list()
for(k in 1:nrow(info)){
  
  dat[[k]] <- list(VOI = info$VOI[[k]], 
                   LUT = info$LUT[[k]],
                   thick = matrix(NA,nrow=length(sub.dirs), ncol = info$Number[k] ),
                   LogJacs = matrix(NA,nrow=length(sub.dirs), ncol = info$Number[k]) )
  
  # add subject_id as rownames
  rownames(dat[[k]]$thick) <- sub.dirs
  rownames(dat[[k]]$LogJacs) <- sub.dirs
  
}

# loop over subjects
for(i in 1:length(sub.dirs)){
  
  # get subject-specific shape files '.raw'
  fid <- dir(paste(path,sub.dirs[i],sep=''), pattern = '.raw')
  fid.full <- dir(paste(path,sub.dirs[i],sep=''), pattern = '.raw',full.names = TRUE)
  
  # loop over metrics
  for(ii in 1:length(metric.id)){
    
    # loop over VOIs
    for(iii in 1:length(info$LUT)){
      
      # dynamically identify file
      tmp.fid <- grep(grep(metric.id[ii],grep(info$LUT[iii],fid,value=TRUE),value=TRUE),fid.full,value=TRUE)
      
      # specify reading options of file
      to.read <- file(tmp.fid,'rb')
      
      # verify length of file
      test <- readBin(to.read, numeric(), n=999999, size=4)
      
      if(length(test)==0){
        next
      }
      
      # read binary file with set parameters into matrix              
      dat[[iii]][[metric.id[[ii]]]][sub.dirs[i],] <- test
      
      close(to.read)
      
    } # iii, VOI
    
  } # ii, metric
  
} # i, subject

#save(dat,file='...Rdata')




