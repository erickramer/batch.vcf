library("dplyr")
library("data.table")
library("VariantAnnotation")

shard.vcf = function(vcf.file, n=1e5, directory=NULL){ 
  
  write.file = function(f, file.list=c()){
  
    tmpdir = paste(directory, 
                   "/",
                   "part",
                   floor(length(file.list) / 100), 
                   sep="")
    if(!file.exists(tmpdir)) dir.create(tmpdir)
    
    lines = scanTabix(f)[[1]]
    
    if(length(lines) > 0){
      
      output.file = paste(tempfile(tmpdir=tmpdir), 
                          ".vcf.gz", 
                          sep="")
            
      o = pipe(paste("bgzip -c >", output.file, sep=""), open="w")
      writeLines(header, o)
      writeLines(lines, o)
      close(o)
      
      rm(lines)
      
      write.file(f, file.list=c(file.list, output.file))
    } else file.list
    
  }
  
  index.file = function(filename){
    system(paste("tabix", filename))
    filename
  }
  
  if(!file.exists(paste(vcf.file, "tbi", sep=""))) index.file(vcf.file)
  
  # find directory for vcf file
  directory = if(is.null(directory)) gsub(".vcf.gz$", "", vcf.file) else
    directory
  if(!file.exists(directory)) dir.create(directory)
  
  # grab header
  header = headerTabix(vcf.file)$header
  
  # split file into many different files
  f = TabixFile(vcf.file, yieldSize=n)
  open(f)
  files = write.file(f) # recursively split file
  close(f)
  
  sapply(files, index.file)
}
