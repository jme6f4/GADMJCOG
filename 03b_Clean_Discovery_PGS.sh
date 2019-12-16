# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   1) QC #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## A) QC1: prune files based on missingness, maf, and hwe
plink --bfile Datasets/Source/chrALL.anno.rsid --geno 0.05 --maf .01 --hwe 0.001 --make-bed --out Datasets/GADD_A_qc
## B) QC2: prune for LD -- identify those makers in ld & le 
plink --bfile Datasets/GADD_A_qc --indep-pairwise 50 5 0.2 --make-bed --out Datasets/GADD_B_ld
## C) QC3: prune for LD -- pull out the LE set of markers from the previous step.
plink --bfile Datasets/GADD_B_ld --extract Datasets/GADD_B_ld.prune.in --make-bed --out Datasets/GADD_C_le
## D) Filter 1: fix a2-alleles that don't match discovery GWAS [vcf --real-ref-alleles]
plink --bfile Datasets/GADD_C_le --a2-allele Datasets/Source/GWAS_EA3.txt 4 1 -make-bed --out Datasets/GADD_D_a2
## E) Filter 2: filter out, leaving just the discovery sample
plink --bfile Datasets/GADD_D_a2 --keep Datasets/GADD_Discovery_Keep.txt --make-bed --out Datasets/GADD_E_Discovery
# F) Create PCA for later
plink --bfile Datasets/GADD_E_Discovery --pca header --out Datasets/GADD_F_PCA
# ## check Id by state
# ## filter by ancestry
# ## check ICC/proband-sibling order -- look up bw/wi estimate/formula


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   2) Prune for Top Hits #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

awk -F "\t" '{ if ($9 < 1e-55) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e55.txt
awk -F "\t" '{ if ($9 < 1e-50) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e50.txt
awk -F "\t" '{ if ($9 < 1e-45) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e45.txt
awk -F "\t" '{ if ($9 < 1e-40) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e40.txt
awk -F "\t" '{ if ($9 < 1e-35) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e35.txt
awk -F "\t" '{ if ($9 < 1e-30) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e30.txt
awk -F "\t" '{ if ($9 < 1e-25) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e25.txt
awk -F "\t" '{ if ($9 < 1e-20) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e20.txt
awk -F "\t" '{ if ($9 < 1e-15) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e15.txt
awk -F "\t" '{ if ($9 < 1e-10) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e10.txt
awk -F "\t" '{ if ($9 < 1e-9) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e09.txt
awk -F "\t" '{ if ($9 < 1e-8) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e08.txt
awk -F "\t" '{ if ($9 < 1e-7) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e07.txt
awk -F "\t" '{ if ($9 < 1e-6) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e06.txt
awk -F "\t" '{ if ($9 < 1e-5) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e05.txt
awk -F "\t" '{ if ($9 < 1e-4) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e04.txt
awk -F "\t" '{ if ($9 < 1e-3) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e03.txt
awk -F "\t" '{ if ($9 < 1e-2) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e02.txt
awk -F "\t" '{ if ($9 < 1e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_1e01.txt
awk -F "\t" '{ if ($9 < 2e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_2e01.txt
awk -F "\t" '{ if ($9 < 3e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_3e01.txt
awk -F "\t" '{ if ($9 < 4e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_4e01.txt
awk -F "\t" '{ if ($9 < 5e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_5e01.txt
awk -F "\t" '{ if ($9 < 6e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_6e01.txt
awk -F "\t" '{ if ($9 < 7e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_7e01.txt
awk -F "\t" '{ if ($9 < 8e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_8e01.txt
awk -F "\t" '{ if ($9 < 9e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_9e01.txt
awk -F "\t" '{ if ($9 < 10e-1) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_10e1.txt
awk -F "\t" '{ if ($9 < 1.00) { print } }' Datasets/Source/GWAS_EA3.txt > Datasets/hits_100.txt



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   3) Construct PGS #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e55.txt 1 4 7 --out Datasets/score_1e55
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e50.txt 1 4 7 --out Datasets/score_1e50
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e45.txt 1 4 7 --out Datasets/score_1e45
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e40.txt 1 4 7 --out Datasets/score_1e40
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e35.txt 1 4 7 --out Datasets/score_1e35
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e30.txt 1 4 7 --out Datasets/score_1e30
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e25.txt 1 4 7 --out Datasets/score_1e25
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e20.txt 1 4 7 --out Datasets/score_1e20
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e15.txt 1 4 7 --out Datasets/score_1e15
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e10.txt 1 4 7 --out Datasets/score_1e10
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e09.txt 1 4 7 --out Datasets/score_1e09
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e08.txt 1 4 7 --out Datasets/score_1e08
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e07.txt 1 4 7 --out Datasets/score_1e07
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e06.txt 1 4 7 --out Datasets/score_1e06
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e05.txt 1 4 7 --out Datasets/score_1e05
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e04.txt 1 4 7 --out Datasets/score_1e04
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e03.txt 1 4 7 --out Datasets/score_1e03
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e02.txt 1 4 7 --out Datasets/score_1e02
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_1e01.txt 1 4 7 --out Datasets/score_1e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_2e01.txt 1 4 7 --out Datasets/score_2e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_3e01.txt 1 4 7 --out Datasets/score_3e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_4e01.txt 1 4 7 --out Datasets/score_4e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_5e01.txt 1 4 7 --out Datasets/score_5e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_6e01.txt 1 4 7 --out Datasets/score_6e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_7e01.txt 1 4 7 --out Datasets/score_7e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_8e01.txt 1 4 7 --out Datasets/score_8e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_9e01.txt 1 4 7 --out Datasets/score_9e01
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_10e1.txt 1 4 7 --out Datasets/score_10e1
plink --bfile Datasets/GADD_E_Discovery --score Datasets/hits_100.txt 1 4 7 --out Datasets/score_100



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   4) Import into R / Validate #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #