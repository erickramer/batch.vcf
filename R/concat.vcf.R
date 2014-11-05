concat.vcf <- function(vcf1, vcf2, vcf3=NULL, tmpdir=tempdir()){
  if(is.null(vcf1)) return(vcf2)
  if(is.null(vcf2)) return(vcf1)
  
  vcf3 = if(is.null(vcf3)) temp.file(tmpdir=tmpdir) else
    vcf3
  
  system(paste("bcftools concat",
         "-o",
         vcf3,
         "-O z",
         vcf1,
         vcf2))
  
  return(vcf3)
}