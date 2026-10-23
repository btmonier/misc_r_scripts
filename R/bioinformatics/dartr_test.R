#------------------------------------------------------------------------------
# Title:   DArTseq Conversion Test
# Author:  Brandon Monier
#
# Read DArTseq reports with dartR and convert them to GDS and PLINK formats.
#------------------------------------------------------------------------------

## DartSeq to VCF ----

library(SNPRelate)
library(dartR)

silicoPath <- "path/to/Report_DS21-6352_SilicoDArT_1.csv"
mappingPath <- "path/to/Report_DS21-6352_SNP_mapping_2.csv"
snp2Path <- "path/to/Report_DS21-6352_SNP_2.csv"


silicoDartGl <- dartR::gl.read.dart(silicoPath)
mappingDartGl <- dartR::gl.read.dart(mappingPath)
snp2DartGl <- dartR::gl.read.dart(snp2Path)


gl2gds(mappingDartGl, "mapping_gl.gds", outpath = here::here())
gl2gds(snp2DartGl, "snp2_gl.gds", outpath = here::here())
gl2plink(mappingDartGl, "mapping_plink.csv", outpath = here::here())

mappingDartGds <- SNPRelate::snpgdsOpen("mapping_gl.gds")
