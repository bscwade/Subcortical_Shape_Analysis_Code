# Function to take output of vertex-wise shape regression and return vtu files for paraview. 
# Input: stats.path = path/to/surface/stats; out.dir = name of desired output directory on desktop, no '/'; pattern = identifier for stat, i.e. 't-value' or 'coef'. 
# Note: creates both raw and thresholded versions of each surface
# Note: pattern3 doesn't create separate sub directory

write2vtu <- function(stats.path, out.dir, pattern, pattern2, pattern3){
  
  ### For testing
  #stats.path <- '~/Desktop/temporary/'
  #out.dir <- 'lasso'
  #pattern <- 'mean'
  #pattern2 <- 'responder_all'
  #pattern3 <- 'TP3'
  ###  
  
  # check to see if pattern 2 was supplied
  if(missing(pattern2)){
    
    # if it wasn't AND no out.directory exists, create it
    if(dir.exists(out.dir) == FALSE){
      dir.create(sprintf('~/Desktop/%s',out.dir))
    }
  
    # if pattern2 is specified...
  }else{
    
    # if pattern2 is given, we require a subdirectory so reset name of out.directory   
    # first check if base dir exists
    if(dir.exists(out.dir)==FALSE){
      dir.create(sprintf('~/Desktop/%s',out.dir))
    }
    
    # now rename to match subdir
    out.dir <- sprintf('%s/%s',out.dir,pattern2)
    
    # if subdirectory doesn't exist, create it 
    if(dir.exists(sprintf('~/Desktop/%s',out.dir)) == FALSE){
      
      dir.create(sprintf('~/Desktop/%s',out.dir))
      
    }                 
    
  }  
  
  stats.files <- dir(stats.path, pattern = pattern)
  stats.fid <- dir(stats.path, pattern = pattern, full.names = TRUE)
  fdr.fid <- dir(stats.path, pattern = 'FDR', full.names = TRUE)
  
  if(!missing(pattern2)){    
    stats.files <- grep(pattern2,stats.files,value=TRUE)
    stats.fid <- grep(pattern2,stats.fid,value=TRUE)    
    fdr.fid <- grep(pattern2, fdr.fid,value=TRUE)
  }
  
  if(!missing(pattern3)){
    stats.files <- grep(pattern3,stats.files,value=TRUE)
    stats.fid <- grep(pattern3,stats.fid,value=TRUE)    
    fdr.fid <- grep(pattern3, fdr.fid,value=TRUE)    
  }
  
  # Full list of potential vois
  vois <- c('Left_Accumbens','Right_Accumbens','Left_Pallidum','Right_Pallidum','Left_Putamen','Right_Putamen',
            'Left_Thalamus','Right_Thalamus','Left_Hippocampus','Right_Hippocampus','Left_Amygdala','Right_Amygdala',
            'Left_Caudate','Right_Caudate')  
  
  # iterate over stats files
  for(i in 1:length(stats.fid)){
    
    if(missing(pattern2)){
      
      MID <- ifelse(sum(grep('LogJacs',stats.fid[i])),'LogJacs','thick')
      
    }else{
      
      MID <- ifelse(sum(grep('LogJacs',stats.fid[i])),sprintf('LogJacs-%s',pattern2),sprintf('thick-%s',pattern2))
      
    }
    
    if(!missing(pattern3)){
      
      MID <- ifelse(sum(grep('LogJacs',stats.fid[i])),sprintf('LogJacs-%s-%s',pattern2,pattern3),sprintf('thick-%s-%s',pattern2,pattern3))
      
    }
        
    
    # get current voi  
    voi <- vois[sapply(vois, function(x) length(grep(x,stats.fid[i])))>0]
    print(voi)
    
    # get voi-specific surface file template
    surface.file <- dir('~/GoogleDrive/LONI/General_Codes/Boris_Shape_Pipe/Surfaces/', pattern = voi, full.names = TRUE)
    surface.data <- system(sprintf('cat %s',surface.file),intern=TRUE)    
    
    # get data to write
    write <- as.numeric(unlist(read.table(stats.fid[i])))
    
    # get fdr q-values for thresholding
    fdr <- as.numeric(unlist(read.table(fdr.fid[i])))
    
    # make parallel thresholded surface
    write.thresh <- write
    write.thresh[which(fdr > 0.05)] <- 0
    
    # get range of lines to write to
    line.start <- max(grep('JD_Thresh',surface.data)) + 1 # get index to begin filling
    line.end <- max(grep('</DataArray>',surface.data)) - 1 # get index to stop filling
    
    # rename value within file
    surface.data[min(grep('JD_Thresh',surface.data))] <- gsub('JD_Thresh',pattern,surface.data[min(grep('JD_Thresh',surface.data))])
    surface.data[max(grep('JD_Thresh',surface.data))] <- gsub('JD_Thresh',pattern,surface.data[max(grep('JD_Thresh',surface.data))])
    
    ## write data to file ##          
    # write raw value
    raw.write <- surface.data
    raw.write[line.start:line.end] <- write
    write.table(raw.write, file = sprintf('~/Desktop/%s/%s-%s-nothresh-%s.vtu',out.dir,voi,MID,pattern), quote=FALSE, row.names=FALSE, col.names=FALSE)
    
    # write thresholded value
    thresh.write <- surface.data
    thresh.write[line.start:line.end] <- write.thresh
    write.table(thresh.write, file = sprintf('~/Desktop/%s/%s-%s-thresh-%s.vtu',out.dir,voi,MID,pattern), quote=FALSE, row.names=FALSE, col.names=FALSE)
    
  } # iterate over files   
  
} # function end
