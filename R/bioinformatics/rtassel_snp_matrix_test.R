
library(magrittr)
library(rJava)
library(rTASSEL)

params <- list(
    nCol = 20,
    nRow = 10,
    nChr = 5
)


supByteMatBuilder <- J("net/maizegenetics/util/SuperByteMatrixBuilder")
taxaListBuilder   <- J("net/maizegenetics/taxa/TaxaListBuilder")
taxon             <- J("net/maizegenetics/taxa/Taxon")

nucConst <- J("net/maizegenetics/dna/snp/NucleotideAlignmentConstants")
gtUtils  <- J("net/maizegenetics/dna/snp/GenotypeTableUtils")



## Random genotype matrix ----
datum <- c("AA", "AG", "TT", "TA", "TC", "GG", "CC", "GC")
snpRMat <- matrix(
    data = sample(datum, params$nRow * params$nCol, replace = TRUE),
    nrow = params$nRow,
    ncol = params$nCol
)

genoCallBuilder <- J("net/maizegenetics/dna/snp/genotypecall/GenotypeCallTableBuilder")
genotypes <- genoCallBuilder$getUnphasedNucleotideGenotypeBuilder(
    params$nRow %>% as.integer(),
    params$nCol %>% as.integer()
)

for (i in seq_len(params$nRow)) {
    for (j in seq_len(params$nCol)) {
        snpVal <- snpRMat[i, j] %>% strsplit("") %>% unlist()
        byteVal <- gtUtils$getDiploidValue(
            nucConst$getNucleotideDiploidByte(snpVal[1]) %>% .jbyte(),
            nucConst$getNucleotideDiploidByte(snpVal[2]) %>% .jbyte()
        )
        genotypes$setBase(
            as.integer(i - 1),
            as.integer(j - 1),
            byteVal %>% .jbyte()
        )
    }
}



## Random taxa IDs ----
taxa <- paste0("Sample_", LETTERS[seq_len(params$nRow)])
tlb  <- .jnew("net/maizegenetics/taxa/TaxaListBuilder")

for (i in taxa) {
    tlb$add(.jnew("net/maizegenetics/taxa/Taxon", i))
}



## Random position list
chroms <- paste0(
    "Chr",
    rep(
        x = seq_len(params$nChr),
        ceiling(params$nCol / params$nChr)
    )
) %>%
    .[order(.)] %>%
    .[seq_len(params$nCol)]

positions <- sample(1:100000, params$nCol) %>% .[order(.)]



plb <- .jnew("net/maizegenetics/dna/map/PositionListBuilder")
for (i in seq_len(params$nCol)) {
    plb$add(
        J(
            "net/maizegenetics/dna/map/Position", "of",
            chroms[i],
            as.integer(positions[i])
        )
    )
}



gtBuilder <- J("net/maizegenetics/dna/snp/GenotypeTableBuilder")
myGt <- gtBuilder$getInstance(
    genotypes$build(),
    plb$build(),
    tlb$build()
)

rTASSEL:::.tasselObjectConstructor(myGt)








