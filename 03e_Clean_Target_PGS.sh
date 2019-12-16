# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   1) QC #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## E) Filter 2: filter out, leaving just the Target sample
plink --bfile Datasets/GADD_D_a2 --keep Datasets/GADD_Target_Keep.txt --make-bed --out Datasets/GADD_E_Target
# F) Create PCA for later
plink --bfile Datasets/GADD_E_Target --pca header --out Datasets/GADD_G_PCA



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   2) Prune for Top Hits #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

awk -F "\t" '{ if ($9 < 1e-8) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/GADD_E_SNPS.txt



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   3) Construct PGS #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

plink --bfile Datasets/GADD_E_Target --score Datasets/GADD_E_SNPS.txt 1 4 7 --out Datasets/GADD_H_PGS



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   4) Import into R / Validate #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #