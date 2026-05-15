#------------------------------------------------------------------------------
# Title:  GWAS Example Plots with rTASSEL
# Author: Brandon Monier (brandon.monier@sdstate.edu)
# Date:   05.15.26
#
# Self-contained end-to-end GWAS demo using the maize diversity panel (MDP)
# data shipped with rTASSEL. The script:
#
#   1. Loads HapMap-format genotypes + a multi-trait phenotype file
#   2. Joins them into a single TasselGenotypePhenotype object
#   3. Computes a centered-IBS kinship matrix
#   4. Runs PCA on the genotype panel
#   5. Fits two association models on every numeric trait at once:
#        - GLM   : naive single-marker linear model with Q-covariates
#        - MLM   : Q + K mixed model (kinship as a random effect)
#   6. Renders a 2x2 figure of the canonical GWAS visuals:
#        PCA           |  QQ (MLM)
#        Manhattan GLM |  Manhattan MLM (faceted by trait)
#------------------------------------------------------------------------------

# Preamble ----

## Load packages
suppressPackageStartupMessages({
    require(rTASSEL)
    require(ggplot2)
    require(patchwork)
})

## Start TASSEL's Java logger (console-only when filePath = NULL)
rTASSEL::startLogger(filePath = NULL, verbose = FALSE)


# Data ----

## All example inputs ship inside the package's extdata directory
ext        <- system.file("extdata", package = "rTASSEL")
geno_path  <- file.path(ext, "mdp_genotype.hmp.txt")
pheno_path <- file.path(ext, "mdp_phenotype.txt")

## Read genotype and phenotype tables separately ...
tas_geno  <- readGenotypeTableFromPath(path = geno_path)
tas_pheno <- readPhenotypeFromPath(path = pheno_path)

## ... and join them into a single object usable by assocModelFitter().
## The phenotype file already declares its columns with TASSEL data types
## (factor = location, data = EarHT/dpoll/EarDia, covariate = Q1/Q2/Q3),
## so a simple "data ~ factor + covariate" formula will pick them up.
tas_geno_pheno <- readGenotypePhenotype(
    genoPathOrObj    = tas_geno,
    phenoPathDFOrObj = tas_pheno
)


# Models ----

## Centered-IBS kinship - the K matrix used in the Q + K mixed model
kin <- kinshipMatrix(
    tasObj = tas_geno,
    method = "Centered_IBS"
)

## PCA of the genotype panel for the population-structure visualization
pca_res <- pca(
    tasObj             = tas_geno,
    useCovariance      = TRUE,
    nComponents        = 5,
    reportEigenvalues  = TRUE,
    reportEigenvectors = TRUE
)

## GLM (naive): trait ~ factors + Q covariates + each marker
glm_res <- assocModelFitter(
    tasObj     = tas_geno_pheno,
    formula    = . ~ .,
    fitMarkers = TRUE,
    kinship    = NULL
)

## MLM (Q + K): same fixed effects, kinship as a random effect
mlm_res <- assocModelFitter(
    tasObj     = tas_geno_pheno,
    formula    = . ~ .,
    fitMarkers = TRUE,
    kinship    = kin
)


# Plots ----

## Suggestive GWAS threshold (-log10 p); Bonferroni-style for the MDP panel
## (~3000 markers -> 0.05/3000 ~= 1.7e-5, so -log10 ~= 4.78). Round to 5.
sig_threshold <- 3

## Two-tone chromosome palette (TASSEL-blue defaults flipped for contrast)
chrom_colors <- c("#3e619b", "#91baff")

p_pca <- plotPCA(
    pcaObj     = pca_res,
    x          = 1,
    y          = 2,
    pointColor = "#3e619b"
) + ggtitle("PCA of MDP genotype panel")

p_qq_mlm <- plotQQ(
    assocRes = mlm_res,
    overlay  = TRUE
) + ggtitle("QQ plot - MLM (Q + K)")

p_man_glm <- plotManhattan(
    assocRes  = glm_res,
    threshold = sig_threshold,
    colors    = chrom_colors
) + ggtitle("Manhattan - GLM (Q covariates)")

p_man_mlm <- plotManhattan(
    assocRes  = mlm_res,
    threshold = sig_threshold,
    colors    = chrom_colors
)


# Layout ----

combined <- (p_pca | p_qq_mlm) / (p_man_glm | p_man_mlm) +
    patchwork::plot_layout(heights = c(1, 1.6)) +
    patchwork::plot_annotation(
        title    = "rTASSEL GWAS example - Maize diversity panel",
        subtitle = "GLM vs. MLM (Q + K) on EarHT, dpoll, and EarDia"
    )


# Output ----

## Display interactively
print(combined)

## Save to disk (uncomment to write)
# ggsave(
#     filename = "rtassel_gwas_example.png",
#     plot     = combined,
#     width    = 16,
#     height   = 10,
#     dpi      = 150,
#     bg       = "white"
# )
