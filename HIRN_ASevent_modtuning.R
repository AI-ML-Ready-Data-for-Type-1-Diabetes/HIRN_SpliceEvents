library(magrittr)

## Hyperparameter search (using tidymodels) ---------------------------------------------------


# Skipped Exon (SE) -------------------------------------------------------

se_data <- readRDS(file = "file/path/to/se_data.rds")
se_metadata <- readRDS(file = "file/path/to/se_metadata.rds")

# merge data and metadata and convert to more compact dataframe
se_traintest <- dplyr::left_join(se_metadata, se_data, by = "MergeID") %>%
  dplyr::mutate(IJC_TRAIN             = purrr::pmap(list(IJC_TRAIN_1, IJC_TRAIN_2, IJC_TRAIN_3, IJC_TRAIN_4,
                                                         IJC_TRAIN_5, IJC_TRAIN_6, IJC_TRAIN_7, IJC_TRAIN_8,
                                                         IJC_TRAIN_9, IJC_TRAIN_10, IJC_TRAIN_11, IJC_TRAIN_12,
                                                         IJC_TRAIN_13, IJC_TRAIN_14, IJC_TRAIN_15, IJC_TRAIN_16,
                                                         IJC_TRAIN_17, IJC_TRAIN_18, IJC_TRAIN_19, IJC_TRAIN_20,
                                                         IJC_TRAIN_21, IJC_TRAIN_22, IJC_TRAIN_23, IJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                IJC_TEST             = purrr::pmap(list(IJC_TEST_1, IJC_TEST_2, IJC_TEST_3, IJC_TEST_4,
                                                        IJC_TEST_5, IJC_TEST_6, IJC_TEST_7, IJC_TEST_8,
                                                        IJC_TEST_9, IJC_TEST_10, IJC_TEST_11, IJC_TEST_12,
                                                        IJC_TEST_13, IJC_TEST_14, IJC_TEST_15, IJC_TEST_16,
                                                        IJC_TEST_17, IJC_TEST_18, IJC_TEST_19, IJC_TEST_20,
                                                        IJC_TEST_21, IJC_TEST_22, IJC_TEST_23, IJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                IJC_TEST_Imputed             = purrr::pmap(list(IJC_TEST_imputed_1, IJC_TEST_imputed_2, IJC_TEST_imputed_3, IJC_TEST_imputed_4,
                                                                IJC_TEST_imputed_5, IJC_TEST_imputed_6, IJC_TEST_imputed_7, IJC_TEST_imputed_8,
                                                                IJC_TEST_imputed_9, IJC_TEST_imputed_10, IJC_TEST_imputed_11, IJC_TEST_imputed_12,
                                                                IJC_TEST_imputed_13, IJC_TEST_imputed_14, IJC_TEST_imputed_15, IJC_TEST_imputed_16,
                                                                IJC_TEST_imputed_17, IJC_TEST_imputed_18, IJC_TEST_imputed_19, IJC_TEST_imputed_20,
                                                                IJC_TEST_imputed_21, IJC_TEST_imputed_22, IJC_TEST_imputed_23, IJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                SJC_TRAIN             = purrr::pmap(list(SJC_TRAIN_1, SJC_TRAIN_2, SJC_TRAIN_3, SJC_TRAIN_4,
                                                         SJC_TRAIN_5, SJC_TRAIN_6, SJC_TRAIN_7, SJC_TRAIN_8,
                                                         SJC_TRAIN_9, SJC_TRAIN_10, SJC_TRAIN_11, SJC_TRAIN_12,
                                                         SJC_TRAIN_13, SJC_TRAIN_14, SJC_TRAIN_15, SJC_TRAIN_16,
                                                         SJC_TRAIN_17, SJC_TRAIN_18, SJC_TRAIN_19, SJC_TRAIN_20,
                                                         SJC_TRAIN_21, SJC_TRAIN_22, SJC_TRAIN_23, SJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                SJC_TEST             = purrr::pmap(list(SJC_TEST_1, SJC_TEST_2, SJC_TEST_3, SJC_TEST_4,
                                                        SJC_TEST_5, SJC_TEST_6, SJC_TEST_7, SJC_TEST_8,
                                                        SJC_TEST_9, SJC_TEST_10, SJC_TEST_11, SJC_TEST_12,
                                                        SJC_TEST_13, SJC_TEST_14, SJC_TEST_15, SJC_TEST_16,
                                                        SJC_TEST_17, SJC_TEST_18, SJC_TEST_19, SJC_TEST_20,
                                                        SJC_TEST_21, SJC_TEST_22, SJC_TEST_23, SJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                SJC_TEST_Imputed             = purrr::pmap(list(SJC_TEST_imputed_1, SJC_TEST_imputed_2, SJC_TEST_imputed_3, SJC_TEST_imputed_4,
                                                                SJC_TEST_imputed_5, SJC_TEST_imputed_6, SJC_TEST_imputed_7, SJC_TEST_imputed_8,
                                                                SJC_TEST_imputed_9, SJC_TEST_imputed_10, SJC_TEST_imputed_11, SJC_TEST_imputed_12,
                                                                SJC_TEST_imputed_13, SJC_TEST_imputed_14, SJC_TEST_imputed_15, SJC_TEST_imputed_16,
                                                                SJC_TEST_imputed_17, SJC_TEST_imputed_18, SJC_TEST_imputed_19, SJC_TEST_imputed_20,
                                                                SJC_TEST_imputed_21, SJC_TEST_imputed_22, SJC_TEST_imputed_23, SJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                IncLevel_TRAIN             = purrr::pmap(list(IncLevel_TRAIN_1, IncLevel_TRAIN_2, IncLevel_TRAIN_3, IncLevel_TRAIN_4,
                                                              IncLevel_TRAIN_5, IncLevel_TRAIN_6, IncLevel_TRAIN_7, IncLevel_TRAIN_8,
                                                              IncLevel_TRAIN_9, IncLevel_TRAIN_10, IncLevel_TRAIN_11, IncLevel_TRAIN_12,
                                                              IncLevel_TRAIN_13, IncLevel_TRAIN_14, IncLevel_TRAIN_15, IncLevel_TRAIN_16,
                                                              IncLevel_TRAIN_17, IncLevel_TRAIN_18, IncLevel_TRAIN_19, IncLevel_TRAIN_20,
                                                              IncLevel_TRAIN_21, IncLevel_TRAIN_22, IncLevel_TRAIN_23, IncLevel_TRAIN_24, MergeID),
                                                         function(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24, mergeid){
                                                           temp <- c(s1, s2, s3, s4, s5, s6,
                                                                     s7, s8, s9, s10, s11, s12,
                                                                     s13, s14, s15, s16, s17, s18,
                                                                     s19, s20, s21, s22, s23, s24)
                                                           attr(temp, "name") <- mergeid
                                                           temp
                                                         }),
                IncLevel_TEST             = purrr::pmap(list(IncLevel_TEST_1, IncLevel_TEST_2, IncLevel_TEST_3, IncLevel_TEST_4,
                                                             IncLevel_TEST_5, IncLevel_TEST_6, IncLevel_TEST_7, IncLevel_TEST_8,
                                                             IncLevel_TEST_9, IncLevel_TEST_10, IncLevel_TEST_11, IncLevel_TEST_12,
                                                             IncLevel_TEST_13, IncLevel_TEST_14, IncLevel_TEST_15, IncLevel_TEST_16,
                                                             IncLevel_TEST_17, IncLevel_TEST_18, IncLevel_TEST_19, IncLevel_TEST_20,
                                                             IncLevel_TEST_21, IncLevel_TEST_22, IncLevel_TEST_23, IncLevel_TEST_24, MergeID),
                                                        function(s1, s2, s3, s4, s5, s6,
                                                                 s7, s8, s9, s10, s11, s12,
                                                                 s13, s14, s15, s16, s17, s18,
                                                                 s19, s20, s21, s22, s23, s24, mergeid){
                                                          temp <- c(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24)
                                                          attr(temp, "name") <- mergeid
                                                          temp
                                                        }),
                IncLevel_TEST_Imputed             = purrr::pmap(list(IncLevel_TEST_imputed_1, IncLevel_TEST_imputed_2, IncLevel_TEST_imputed_3, IncLevel_TEST_imputed_4,
                                                                     IncLevel_TEST_imputed_5, IncLevel_TEST_imputed_6, IncLevel_TEST_imputed_7, IncLevel_TEST_imputed_8,
                                                                     IncLevel_TEST_imputed_9, IncLevel_TEST_imputed_10, IncLevel_TEST_imputed_11, IncLevel_TEST_imputed_12,
                                                                     IncLevel_TEST_imputed_13, IncLevel_TEST_imputed_14, IncLevel_TEST_imputed_15, IncLevel_TEST_imputed_16,
                                                                     IncLevel_TEST_imputed_17, IncLevel_TEST_imputed_18, IncLevel_TEST_imputed_19, IncLevel_TEST_imputed_20,
                                                                     IncLevel_TEST_imputed_21, IncLevel_TEST_imputed_22, IncLevel_TEST_imputed_23, IncLevel_TEST_imputed_24, MergeID),
                                                                function(s1, s2, s3, s4, s5, s6,
                                                                         s7, s8, s9, s10, s11, s12,
                                                                         s13, s14, s15, s16, s17, s18,
                                                                         s19, s20, s21, s22, s23, s24, mergeid){
                                                                  temp <- c(s1, s2, s3, s4, s5, s6,
                                                                            s7, s8, s9, s10, s11, s12,
                                                                            s13, s14, s15, s16, s17, s18,
                                                                            s19, s20, s21, s22, s23, s24)
                                                                  attr(temp, "name") <- mergeid
                                                                  temp
                                                                }),
                Status_TRAIN             = purrr::pmap(list(Status_TRAIN_1, Status_TRAIN_2, Status_TRAIN_3, Status_TRAIN_4,
                                                            Status_TRAIN_5, Status_TRAIN_6, Status_TRAIN_7, Status_TRAIN_8,
                                                            Status_TRAIN_9, Status_TRAIN_10, Status_TRAIN_11, Status_TRAIN_12,
                                                            Status_TRAIN_13, Status_TRAIN_14, Status_TRAIN_15, Status_TRAIN_16,
                                                            Status_TRAIN_17, Status_TRAIN_18, Status_TRAIN_19, Status_TRAIN_20,
                                                            Status_TRAIN_21, Status_TRAIN_22, Status_TRAIN_23, Status_TRAIN_24, MergeID),
                                                       function(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24, mergeid){
                                                         temp <- c(s1, s2, s3, s4, s5, s6,
                                                                   s7, s8, s9, s10, s11, s12,
                                                                   s13, s14, s15, s16, s17, s18,
                                                                   s19, s20, s21, s22, s23, s24)
                                                         attr(temp, "name") <- mergeid
                                                         temp
                                                       }),
                Status_TEST             = purrr::pmap(list(Status_TEST_1, Status_TEST_2, Status_TEST_3, Status_TEST_4,
                                                           Status_TEST_5, Status_TEST_6, Status_TEST_7, Status_TEST_8,
                                                           Status_TEST_9, Status_TEST_10, Status_TEST_11, Status_TEST_12,
                                                           Status_TEST_13, Status_TEST_14, Status_TEST_15, Status_TEST_16,
                                                           Status_TEST_17, Status_TEST_18, Status_TEST_19, Status_TEST_20,
                                                           Status_TEST_21, Status_TEST_22, Status_TEST_23, Status_TEST_24, MergeID),
                                                      function(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24, mergeid){
                                                        temp <- c(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24)
                                                        attr(temp, "name") <- mergeid
                                                        temp
                                                      })) %>%
  dplyr::select(-(Status_TRAIN_1:SJC_TEST_imputed_24)) %>%
  dplyr::select(MergeID, GeneID, geneSymbol, chr, strand, exonStart_0base, exonEnd, upstreamES,
                upstreamEE, downstreamES, downstreamEE, IncFormLen,
                SkipFormLen, ENSEMBL, GENENAME, IJC_TRAIN,
                SJC_TRAIN, IncLevel_TRAIN, Status_TRAIN, PValue_TRAIN,
                FDR_TRAIN, Filter.Count_TRAIN, IJC_TEST, SJC_TEST,
                IncLevel_TEST, IJC_TEST_Imputed, SJC_TEST_Imputed, IncLevel_TEST_Imputed,
                Status_TEST, PValue_TEST, FDR_TEST, Filter.Count_TEST)

rm(se_data, se_metadata)
gc()

# Hyperparameter (FDR thresh and ntree) search
fdr_thresh <- c(0.05, 0.01, 0.005, 0.001) # Define FDR thresholds to consider
ntrees     <- c(100, 200, 300, 400, 500) # Define number of trees for random forest to consider
paramgrid <- tibble::tibble(trees = ntrees)

set.seed(1) # Set seed for analysis
allres_se <- vector("list", length = length(fdr_thresh)) # Define list to store results
pb <- txtProgressBar(min = 0, max = length(fdr_thresh), initial = 0, style = 3)
for(i in 1:length(fdr_thresh)){
  setTxtProgressBar(pb, i)
  temp_train <- se_traintest %>%
    dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= fdr_thresh[i], Filter.Count_TRAIN == 0) %>%
    .$IncLevel_TRAIN %>%
    data.frame() %>%
    setNames(lapply(., function(x){attr(x, "name")})) %>%
    dplyr::mutate(Status = factor(se_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

  temp_mod <- parsnip::rand_forest(trees = tune::tune()) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("classification")

  temp_folds <- rsample::vfold_cv(data    = temp_train,
                                  v       = 3,
                                  repeats = 25,
                                  strata  = Status)

  allres_se[[i]] <- workflows::workflow() %>%
    workflows::add_model(temp_mod) %>%
    workflows::add_formula(Status ~ .) %>%
    tune::tune_grid(resamples = temp_folds,
                    grid      = paramgrid) %>%
    workflowsets::collect_metrics() %>%
    dplyr::mutate(FDR = fdr_thresh[i]) %>%
    dplyr::select(FDR, trees, .metric, .estimator, mean, n, std_err, .config)

  rm(temp_train, temp_mod, temp_folds)
}
close(pb)
rm(pb, i, paramgrid, fdr_thresh, ntrees)

# Extract the AUC (denoted here as ROC) results from the stored result set. Compress into a dataframe.
allres_se_ROC <- Reduce("rbind", allres_se)
saveRDS(allres_se_ROC, file = "file/path/to/save/se_auctune_tidy.rds")

temp <- allres_se_ROC %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(FDR, trees, mean) %>%
  dplyr::rename(AUC = mean) %>%
  dplyr::mutate(FDR = factor(FDR, levels = c(0.001, 0.005, 0.01, 0.05)),
                trees = factor(trees, levels = c(100, 200, 300, 400, 500)))

# se heatmap
ggplot(temp, aes(trees, FDR)) +
  geom_tile(aes(fill = AUC)) + scale_fill_distiller(palette = "YlOrRd", direction = -1) +
  ggtitle("SE")
rm(temp, allres_se, allres_se_ROC, se_traintest)

# Retained Intron (RI) -------------------------------------------------------

ri_data <- readRDS(file = "file/path/to/ri_data.rds")
ri_metadata <- readRDS(file = "file/path/to/ri_metadata.rds")

ri_traintest <- dplyr::left_join(ri_metadata, ri_data, by = "MergeID") %>%
  dplyr::mutate(IJC_TRAIN             = purrr::pmap(list(IJC_TRAIN_1, IJC_TRAIN_2, IJC_TRAIN_3, IJC_TRAIN_4,
                                                         IJC_TRAIN_5, IJC_TRAIN_6, IJC_TRAIN_7, IJC_TRAIN_8,
                                                         IJC_TRAIN_9, IJC_TRAIN_10, IJC_TRAIN_11, IJC_TRAIN_12,
                                                         IJC_TRAIN_13, IJC_TRAIN_14, IJC_TRAIN_15, IJC_TRAIN_16,
                                                         IJC_TRAIN_17, IJC_TRAIN_18, IJC_TRAIN_19, IJC_TRAIN_20,
                                                         IJC_TRAIN_21, IJC_TRAIN_22, IJC_TRAIN_23, IJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                IJC_TEST             = purrr::pmap(list(IJC_TEST_1, IJC_TEST_2, IJC_TEST_3, IJC_TEST_4,
                                                        IJC_TEST_5, IJC_TEST_6, IJC_TEST_7, IJC_TEST_8,
                                                        IJC_TEST_9, IJC_TEST_10, IJC_TEST_11, IJC_TEST_12,
                                                        IJC_TEST_13, IJC_TEST_14, IJC_TEST_15, IJC_TEST_16,
                                                        IJC_TEST_17, IJC_TEST_18, IJC_TEST_19, IJC_TEST_20,
                                                        IJC_TEST_21, IJC_TEST_22, IJC_TEST_23, IJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                IJC_TEST_Imputed             = purrr::pmap(list(IJC_TEST_imputed_1, IJC_TEST_imputed_2, IJC_TEST_imputed_3, IJC_TEST_imputed_4,
                                                                IJC_TEST_imputed_5, IJC_TEST_imputed_6, IJC_TEST_imputed_7, IJC_TEST_imputed_8,
                                                                IJC_TEST_imputed_9, IJC_TEST_imputed_10, IJC_TEST_imputed_11, IJC_TEST_imputed_12,
                                                                IJC_TEST_imputed_13, IJC_TEST_imputed_14, IJC_TEST_imputed_15, IJC_TEST_imputed_16,
                                                                IJC_TEST_imputed_17, IJC_TEST_imputed_18, IJC_TEST_imputed_19, IJC_TEST_imputed_20,
                                                                IJC_TEST_imputed_21, IJC_TEST_imputed_22, IJC_TEST_imputed_23, IJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                SJC_TRAIN             = purrr::pmap(list(SJC_TRAIN_1, SJC_TRAIN_2, SJC_TRAIN_3, SJC_TRAIN_4,
                                                         SJC_TRAIN_5, SJC_TRAIN_6, SJC_TRAIN_7, SJC_TRAIN_8,
                                                         SJC_TRAIN_9, SJC_TRAIN_10, SJC_TRAIN_11, SJC_TRAIN_12,
                                                         SJC_TRAIN_13, SJC_TRAIN_14, SJC_TRAIN_15, SJC_TRAIN_16,
                                                         SJC_TRAIN_17, SJC_TRAIN_18, SJC_TRAIN_19, SJC_TRAIN_20,
                                                         SJC_TRAIN_21, SJC_TRAIN_22, SJC_TRAIN_23, SJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                SJC_TEST             = purrr::pmap(list(SJC_TEST_1, SJC_TEST_2, SJC_TEST_3, SJC_TEST_4,
                                                        SJC_TEST_5, SJC_TEST_6, SJC_TEST_7, SJC_TEST_8,
                                                        SJC_TEST_9, SJC_TEST_10, SJC_TEST_11, SJC_TEST_12,
                                                        SJC_TEST_13, SJC_TEST_14, SJC_TEST_15, SJC_TEST_16,
                                                        SJC_TEST_17, SJC_TEST_18, SJC_TEST_19, SJC_TEST_20,
                                                        SJC_TEST_21, SJC_TEST_22, SJC_TEST_23, SJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                SJC_TEST_Imputed             = purrr::pmap(list(SJC_TEST_imputed_1, SJC_TEST_imputed_2, SJC_TEST_imputed_3, SJC_TEST_imputed_4,
                                                                SJC_TEST_imputed_5, SJC_TEST_imputed_6, SJC_TEST_imputed_7, SJC_TEST_imputed_8,
                                                                SJC_TEST_imputed_9, SJC_TEST_imputed_10, SJC_TEST_imputed_11, SJC_TEST_imputed_12,
                                                                SJC_TEST_imputed_13, SJC_TEST_imputed_14, SJC_TEST_imputed_15, SJC_TEST_imputed_16,
                                                                SJC_TEST_imputed_17, SJC_TEST_imputed_18, SJC_TEST_imputed_19, SJC_TEST_imputed_20,
                                                                SJC_TEST_imputed_21, SJC_TEST_imputed_22, SJC_TEST_imputed_23, SJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                IncLevel_TRAIN             = purrr::pmap(list(IncLevel_TRAIN_1, IncLevel_TRAIN_2, IncLevel_TRAIN_3, IncLevel_TRAIN_4,
                                                              IncLevel_TRAIN_5, IncLevel_TRAIN_6, IncLevel_TRAIN_7, IncLevel_TRAIN_8,
                                                              IncLevel_TRAIN_9, IncLevel_TRAIN_10, IncLevel_TRAIN_11, IncLevel_TRAIN_12,
                                                              IncLevel_TRAIN_13, IncLevel_TRAIN_14, IncLevel_TRAIN_15, IncLevel_TRAIN_16,
                                                              IncLevel_TRAIN_17, IncLevel_TRAIN_18, IncLevel_TRAIN_19, IncLevel_TRAIN_20,
                                                              IncLevel_TRAIN_21, IncLevel_TRAIN_22, IncLevel_TRAIN_23, IncLevel_TRAIN_24, MergeID),
                                                         function(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24, mergeid){
                                                           temp <- c(s1, s2, s3, s4, s5, s6,
                                                                     s7, s8, s9, s10, s11, s12,
                                                                     s13, s14, s15, s16, s17, s18,
                                                                     s19, s20, s21, s22, s23, s24)
                                                           attr(temp, "name") <- mergeid
                                                           temp
                                                         }),
                IncLevel_TEST             = purrr::pmap(list(IncLevel_TEST_1, IncLevel_TEST_2, IncLevel_TEST_3, IncLevel_TEST_4,
                                                             IncLevel_TEST_5, IncLevel_TEST_6, IncLevel_TEST_7, IncLevel_TEST_8,
                                                             IncLevel_TEST_9, IncLevel_TEST_10, IncLevel_TEST_11, IncLevel_TEST_12,
                                                             IncLevel_TEST_13, IncLevel_TEST_14, IncLevel_TEST_15, IncLevel_TEST_16,
                                                             IncLevel_TEST_17, IncLevel_TEST_18, IncLevel_TEST_19, IncLevel_TEST_20,
                                                             IncLevel_TEST_21, IncLevel_TEST_22, IncLevel_TEST_23, IncLevel_TEST_24, MergeID),
                                                        function(s1, s2, s3, s4, s5, s6,
                                                                 s7, s8, s9, s10, s11, s12,
                                                                 s13, s14, s15, s16, s17, s18,
                                                                 s19, s20, s21, s22, s23, s24, mergeid){
                                                          temp <- c(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24)
                                                          attr(temp, "name") <- mergeid
                                                          temp
                                                        }),
                IncLevel_TEST_Imputed             = purrr::pmap(list(IncLevel_TEST_imputed_1, IncLevel_TEST_imputed_2, IncLevel_TEST_imputed_3, IncLevel_TEST_imputed_4,
                                                                     IncLevel_TEST_imputed_5, IncLevel_TEST_imputed_6, IncLevel_TEST_imputed_7, IncLevel_TEST_imputed_8,
                                                                     IncLevel_TEST_imputed_9, IncLevel_TEST_imputed_10, IncLevel_TEST_imputed_11, IncLevel_TEST_imputed_12,
                                                                     IncLevel_TEST_imputed_13, IncLevel_TEST_imputed_14, IncLevel_TEST_imputed_15, IncLevel_TEST_imputed_16,
                                                                     IncLevel_TEST_imputed_17, IncLevel_TEST_imputed_18, IncLevel_TEST_imputed_19, IncLevel_TEST_imputed_20,
                                                                     IncLevel_TEST_imputed_21, IncLevel_TEST_imputed_22, IncLevel_TEST_imputed_23, IncLevel_TEST_imputed_24, MergeID),
                                                                function(s1, s2, s3, s4, s5, s6,
                                                                         s7, s8, s9, s10, s11, s12,
                                                                         s13, s14, s15, s16, s17, s18,
                                                                         s19, s20, s21, s22, s23, s24, mergeid){
                                                                  temp <- c(s1, s2, s3, s4, s5, s6,
                                                                            s7, s8, s9, s10, s11, s12,
                                                                            s13, s14, s15, s16, s17, s18,
                                                                            s19, s20, s21, s22, s23, s24)
                                                                  attr(temp, "name") <- mergeid
                                                                  temp
                                                                }),
                Status_TRAIN             = purrr::pmap(list(Status_TRAIN_1, Status_TRAIN_2, Status_TRAIN_3, Status_TRAIN_4,
                                                            Status_TRAIN_5, Status_TRAIN_6, Status_TRAIN_7, Status_TRAIN_8,
                                                            Status_TRAIN_9, Status_TRAIN_10, Status_TRAIN_11, Status_TRAIN_12,
                                                            Status_TRAIN_13, Status_TRAIN_14, Status_TRAIN_15, Status_TRAIN_16,
                                                            Status_TRAIN_17, Status_TRAIN_18, Status_TRAIN_19, Status_TRAIN_20,
                                                            Status_TRAIN_21, Status_TRAIN_22, Status_TRAIN_23, Status_TRAIN_24, MergeID),
                                                       function(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24, mergeid){
                                                         temp <- c(s1, s2, s3, s4, s5, s6,
                                                                   s7, s8, s9, s10, s11, s12,
                                                                   s13, s14, s15, s16, s17, s18,
                                                                   s19, s20, s21, s22, s23, s24)
                                                         attr(temp, "name") <- mergeid
                                                         temp
                                                       }),
                Status_TEST             = purrr::pmap(list(Status_TEST_1, Status_TEST_2, Status_TEST_3, Status_TEST_4,
                                                           Status_TEST_5, Status_TEST_6, Status_TEST_7, Status_TEST_8,
                                                           Status_TEST_9, Status_TEST_10, Status_TEST_11, Status_TEST_12,
                                                           Status_TEST_13, Status_TEST_14, Status_TEST_15, Status_TEST_16,
                                                           Status_TEST_17, Status_TEST_18, Status_TEST_19, Status_TEST_20,
                                                           Status_TEST_21, Status_TEST_22, Status_TEST_23, Status_TEST_24, MergeID),
                                                      function(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24, mergeid){
                                                        temp <- c(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24)
                                                        attr(temp, "name") <- mergeid
                                                        temp
                                                      })) %>%
  dplyr::select(-(Status_TRAIN_1:SJC_TEST_imputed_24)) %>%
  dplyr::select(MergeID, GeneID, geneSymbol, chr, strand, riExonStart_0base, riExonEnd, upstreamES, upstreamEE,
                downstreamES, downstreamEE, IncFormLen, SkipFormLen, ENSEMBL, GENENAME,
                IJC_TRAIN, SJC_TRAIN, IncLevel_TRAIN, Status_TRAIN, PValue_TRAIN, FDR_TRAIN, Filter.Count_TRAIN,
                IJC_TEST, SJC_TEST, IncLevel_TEST, IJC_TEST_Imputed, SJC_TEST_Imputed, IncLevel_TEST_Imputed, Status_TEST, PValue_TEST, FDR_TEST, Filter.Count_TEST)

rm(ri_data, ri_metadata)
gc()

# Hyperparameter (FDR thresh and ntree) search
fdr_thresh <- c(0.05, 0.01, 0.005, 0.001) # Define FDR thresholds to consider
ntrees     <- c(100, 200, 300, 400, 500) # Define number of trees for random forest to consider
paramgrid <- tibble::tibble(trees = ntrees)

set.seed(1) # Set seed for analysis
allres_ri <- vector("list", length = length(fdr_thresh)) # Define list to store results
pb <- txtProgressBar(min = 0, max = length(fdr_thresh), initial = 0, style = 3)
for(i in 1:length(fdr_thresh)){
  setTxtProgressBar(pb, i)
  temp_train <- ri_traintest %>%
    dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= fdr_thresh[i], Filter.Count_TRAIN == 0) %>%
    .$IncLevel_TRAIN %>%
    data.frame() %>%
    setNames(lapply(., function(x){attr(x, "name")})) %>%
    dplyr::mutate(Status = factor(ri_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

  temp_mod <- parsnip::rand_forest(trees = tune::tune()) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("classification")

  temp_folds <- rsample::vfold_cv(data    = temp_train,
                                  v       = 3,
                                  repeats = 25,
                                  strata  = Status)

  allres_ri[[i]] <- workflows::workflow() %>%
    workflows::add_model(temp_mod) %>%
    workflows::add_formula(Status ~ .) %>%
    tune::tune_grid(resamples = temp_folds,
                    grid      = paramgrid) %>%
    workflowsets::collect_metrics() %>%
    dplyr::mutate(FDR = fdr_thresh[i]) %>%
    dplyr::select(FDR, trees, .metric, .estimator, mean, n, std_err, .config)

  rm(temp_train, temp_mod, temp_folds)
}
close(pb)
rm(pb, i, paramgrid, fdr_thresh, ntrees)

# Extract the AUC (denoted here as ROC) results from the stored result set. Compress into a dataframe.
allres_ri_ROC <- Reduce("rbind", allres_ri)
saveRDS(allres_ri_ROC, file = "file/path/to/save/ri_auctune_tidy.rds")

temp <- allres_ri_ROC %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(FDR, trees, mean) %>%
  dplyr::rename(AUC = mean) %>%
  dplyr::mutate(FDR = factor(FDR, levels = c(0.001, 0.005, 0.01, 0.05)),
                trees = factor(trees, levels = c(100, 200, 300, 400, 500)))

# ri heatmap
ggplot(temp, aes(trees, FDR)) +
  geom_tile(aes(fill = AUC)) + scale_fill_distiller(palette = "YlOrRd", direction = -1) +
  ggtitle("RI")
rm(temp, allres_ri, allres_ri_ROC, ri_traintest)

# Alternative 5' splice site (A5SS) -------------------------------------------------------

a5ss_data <- readRDS(file = "file/path/to/a5ss_data.rds")
a5ss_metadata <- readRDS(file = "file/path/to/a5ss_metadata.rds")

a5ss_traintest <- dplyr::left_join(a5ss_metadata, a5ss_data, by = "MergeID") %>%
  dplyr::mutate(IJC_TRAIN             = purrr::pmap(list(IJC_TRAIN_1, IJC_TRAIN_2, IJC_TRAIN_3, IJC_TRAIN_4,
                                                         IJC_TRAIN_5, IJC_TRAIN_6, IJC_TRAIN_7, IJC_TRAIN_8,
                                                         IJC_TRAIN_9, IJC_TRAIN_10, IJC_TRAIN_11, IJC_TRAIN_12,
                                                         IJC_TRAIN_13, IJC_TRAIN_14, IJC_TRAIN_15, IJC_TRAIN_16,
                                                         IJC_TRAIN_17, IJC_TRAIN_18, IJC_TRAIN_19, IJC_TRAIN_20,
                                                         IJC_TRAIN_21, IJC_TRAIN_22, IJC_TRAIN_23, IJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                IJC_TEST             = purrr::pmap(list(IJC_TEST_1, IJC_TEST_2, IJC_TEST_3, IJC_TEST_4,
                                                        IJC_TEST_5, IJC_TEST_6, IJC_TEST_7, IJC_TEST_8,
                                                        IJC_TEST_9, IJC_TEST_10, IJC_TEST_11, IJC_TEST_12,
                                                        IJC_TEST_13, IJC_TEST_14, IJC_TEST_15, IJC_TEST_16,
                                                        IJC_TEST_17, IJC_TEST_18, IJC_TEST_19, IJC_TEST_20,
                                                        IJC_TEST_21, IJC_TEST_22, IJC_TEST_23, IJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                IJC_TEST_Imputed             = purrr::pmap(list(IJC_TEST_imputed_1, IJC_TEST_imputed_2, IJC_TEST_imputed_3, IJC_TEST_imputed_4,
                                                                IJC_TEST_imputed_5, IJC_TEST_imputed_6, IJC_TEST_imputed_7, IJC_TEST_imputed_8,
                                                                IJC_TEST_imputed_9, IJC_TEST_imputed_10, IJC_TEST_imputed_11, IJC_TEST_imputed_12,
                                                                IJC_TEST_imputed_13, IJC_TEST_imputed_14, IJC_TEST_imputed_15, IJC_TEST_imputed_16,
                                                                IJC_TEST_imputed_17, IJC_TEST_imputed_18, IJC_TEST_imputed_19, IJC_TEST_imputed_20,
                                                                IJC_TEST_imputed_21, IJC_TEST_imputed_22, IJC_TEST_imputed_23, IJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                SJC_TRAIN             = purrr::pmap(list(SJC_TRAIN_1, SJC_TRAIN_2, SJC_TRAIN_3, SJC_TRAIN_4,
                                                         SJC_TRAIN_5, SJC_TRAIN_6, SJC_TRAIN_7, SJC_TRAIN_8,
                                                         SJC_TRAIN_9, SJC_TRAIN_10, SJC_TRAIN_11, SJC_TRAIN_12,
                                                         SJC_TRAIN_13, SJC_TRAIN_14, SJC_TRAIN_15, SJC_TRAIN_16,
                                                         SJC_TRAIN_17, SJC_TRAIN_18, SJC_TRAIN_19, SJC_TRAIN_20,
                                                         SJC_TRAIN_21, SJC_TRAIN_22, SJC_TRAIN_23, SJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                SJC_TEST             = purrr::pmap(list(SJC_TEST_1, SJC_TEST_2, SJC_TEST_3, SJC_TEST_4,
                                                        SJC_TEST_5, SJC_TEST_6, SJC_TEST_7, SJC_TEST_8,
                                                        SJC_TEST_9, SJC_TEST_10, SJC_TEST_11, SJC_TEST_12,
                                                        SJC_TEST_13, SJC_TEST_14, SJC_TEST_15, SJC_TEST_16,
                                                        SJC_TEST_17, SJC_TEST_18, SJC_TEST_19, SJC_TEST_20,
                                                        SJC_TEST_21, SJC_TEST_22, SJC_TEST_23, SJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                SJC_TEST_Imputed             = purrr::pmap(list(SJC_TEST_imputed_1, SJC_TEST_imputed_2, SJC_TEST_imputed_3, SJC_TEST_imputed_4,
                                                                SJC_TEST_imputed_5, SJC_TEST_imputed_6, SJC_TEST_imputed_7, SJC_TEST_imputed_8,
                                                                SJC_TEST_imputed_9, SJC_TEST_imputed_10, SJC_TEST_imputed_11, SJC_TEST_imputed_12,
                                                                SJC_TEST_imputed_13, SJC_TEST_imputed_14, SJC_TEST_imputed_15, SJC_TEST_imputed_16,
                                                                SJC_TEST_imputed_17, SJC_TEST_imputed_18, SJC_TEST_imputed_19, SJC_TEST_imputed_20,
                                                                SJC_TEST_imputed_21, SJC_TEST_imputed_22, SJC_TEST_imputed_23, SJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                IncLevel_TRAIN             = purrr::pmap(list(IncLevel_TRAIN_1, IncLevel_TRAIN_2, IncLevel_TRAIN_3, IncLevel_TRAIN_4,
                                                              IncLevel_TRAIN_5, IncLevel_TRAIN_6, IncLevel_TRAIN_7, IncLevel_TRAIN_8,
                                                              IncLevel_TRAIN_9, IncLevel_TRAIN_10, IncLevel_TRAIN_11, IncLevel_TRAIN_12,
                                                              IncLevel_TRAIN_13, IncLevel_TRAIN_14, IncLevel_TRAIN_15, IncLevel_TRAIN_16,
                                                              IncLevel_TRAIN_17, IncLevel_TRAIN_18, IncLevel_TRAIN_19, IncLevel_TRAIN_20,
                                                              IncLevel_TRAIN_21, IncLevel_TRAIN_22, IncLevel_TRAIN_23, IncLevel_TRAIN_24, MergeID),
                                                         function(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24, mergeid){
                                                           temp <- c(s1, s2, s3, s4, s5, s6,
                                                                     s7, s8, s9, s10, s11, s12,
                                                                     s13, s14, s15, s16, s17, s18,
                                                                     s19, s20, s21, s22, s23, s24)
                                                           attr(temp, "name") <- mergeid
                                                           temp
                                                         }),
                IncLevel_TEST             = purrr::pmap(list(IncLevel_TEST_1, IncLevel_TEST_2, IncLevel_TEST_3, IncLevel_TEST_4,
                                                             IncLevel_TEST_5, IncLevel_TEST_6, IncLevel_TEST_7, IncLevel_TEST_8,
                                                             IncLevel_TEST_9, IncLevel_TEST_10, IncLevel_TEST_11, IncLevel_TEST_12,
                                                             IncLevel_TEST_13, IncLevel_TEST_14, IncLevel_TEST_15, IncLevel_TEST_16,
                                                             IncLevel_TEST_17, IncLevel_TEST_18, IncLevel_TEST_19, IncLevel_TEST_20,
                                                             IncLevel_TEST_21, IncLevel_TEST_22, IncLevel_TEST_23, IncLevel_TEST_24, MergeID),
                                                        function(s1, s2, s3, s4, s5, s6,
                                                                 s7, s8, s9, s10, s11, s12,
                                                                 s13, s14, s15, s16, s17, s18,
                                                                 s19, s20, s21, s22, s23, s24, mergeid){
                                                          temp <- c(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24)
                                                          attr(temp, "name") <- mergeid
                                                          temp
                                                        }),
                IncLevel_TEST_Imputed             = purrr::pmap(list(IncLevel_TEST_imputed_1, IncLevel_TEST_imputed_2, IncLevel_TEST_imputed_3, IncLevel_TEST_imputed_4,
                                                                     IncLevel_TEST_imputed_5, IncLevel_TEST_imputed_6, IncLevel_TEST_imputed_7, IncLevel_TEST_imputed_8,
                                                                     IncLevel_TEST_imputed_9, IncLevel_TEST_imputed_10, IncLevel_TEST_imputed_11, IncLevel_TEST_imputed_12,
                                                                     IncLevel_TEST_imputed_13, IncLevel_TEST_imputed_14, IncLevel_TEST_imputed_15, IncLevel_TEST_imputed_16,
                                                                     IncLevel_TEST_imputed_17, IncLevel_TEST_imputed_18, IncLevel_TEST_imputed_19, IncLevel_TEST_imputed_20,
                                                                     IncLevel_TEST_imputed_21, IncLevel_TEST_imputed_22, IncLevel_TEST_imputed_23, IncLevel_TEST_imputed_24, MergeID),
                                                                function(s1, s2, s3, s4, s5, s6,
                                                                         s7, s8, s9, s10, s11, s12,
                                                                         s13, s14, s15, s16, s17, s18,
                                                                         s19, s20, s21, s22, s23, s24, mergeid){
                                                                  temp <- c(s1, s2, s3, s4, s5, s6,
                                                                            s7, s8, s9, s10, s11, s12,
                                                                            s13, s14, s15, s16, s17, s18,
                                                                            s19, s20, s21, s22, s23, s24)
                                                                  attr(temp, "name") <- mergeid
                                                                  temp
                                                                }),
                Status_TRAIN             = purrr::pmap(list(Status_TRAIN_1, Status_TRAIN_2, Status_TRAIN_3, Status_TRAIN_4,
                                                            Status_TRAIN_5, Status_TRAIN_6, Status_TRAIN_7, Status_TRAIN_8,
                                                            Status_TRAIN_9, Status_TRAIN_10, Status_TRAIN_11, Status_TRAIN_12,
                                                            Status_TRAIN_13, Status_TRAIN_14, Status_TRAIN_15, Status_TRAIN_16,
                                                            Status_TRAIN_17, Status_TRAIN_18, Status_TRAIN_19, Status_TRAIN_20,
                                                            Status_TRAIN_21, Status_TRAIN_22, Status_TRAIN_23, Status_TRAIN_24, MergeID),
                                                       function(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24, mergeid){
                                                         temp <- c(s1, s2, s3, s4, s5, s6,
                                                                   s7, s8, s9, s10, s11, s12,
                                                                   s13, s14, s15, s16, s17, s18,
                                                                   s19, s20, s21, s22, s23, s24)
                                                         attr(temp, "name") <- mergeid
                                                         temp
                                                       }),
                Status_TEST             = purrr::pmap(list(Status_TEST_1, Status_TEST_2, Status_TEST_3, Status_TEST_4,
                                                           Status_TEST_5, Status_TEST_6, Status_TEST_7, Status_TEST_8,
                                                           Status_TEST_9, Status_TEST_10, Status_TEST_11, Status_TEST_12,
                                                           Status_TEST_13, Status_TEST_14, Status_TEST_15, Status_TEST_16,
                                                           Status_TEST_17, Status_TEST_18, Status_TEST_19, Status_TEST_20,
                                                           Status_TEST_21, Status_TEST_22, Status_TEST_23, Status_TEST_24, MergeID),
                                                      function(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24, mergeid){
                                                        temp <- c(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24)
                                                        attr(temp, "name") <- mergeid
                                                        temp
                                                      })) %>%
  dplyr::select(-(Status_TRAIN_1:SJC_TEST_imputed_24)) %>%
  dplyr::select(MergeID, GeneID, geneSymbol, chr, strand, longExonStart_0base, longExonEnd, shortES, shortEE,
                flankingES, flankingEE, IncFormLen, SkipFormLen, ENSEMBL, GENENAME,
                IJC_TRAIN, SJC_TRAIN, IncLevel_TRAIN, Status_TRAIN, PValue_TRAIN, FDR_TRAIN, Filter.Count_TRAIN,
                IJC_TEST, SJC_TEST, IncLevel_TEST, IJC_TEST_Imputed, SJC_TEST_Imputed, IncLevel_TEST_Imputed, Status_TEST, PValue_TEST, FDR_TEST, Filter.Count_TEST)

rm(a5ss_data, a5ss_metadata)
gc()

# Hyperparameter (FDR thresh and ntree) search
fdr_thresh <- c(0.05, 0.01, 0.005, 0.001) # Define FDR thresholds to consider
ntrees     <- c(100, 200, 300, 400, 500) # Define number of trees for random forest to consider
paramgrid <- tibble::tibble(trees = ntrees)

set.seed(1) # Set seed for analysis
allres_a5ss <- vector("list", length = length(fdr_thresh)) # Define list to store results
pb <- txtProgressBar(min = 0, max = length(fdr_thresh), initial = 0, style = 3)
for(i in 1:length(fdr_thresh)){
  setTxtProgressBar(pb, i)
  temp_train <- a5ss_traintest %>%
    dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= fdr_thresh[i], Filter.Count_TRAIN == 0) %>%
    .$IncLevel_TRAIN %>%
    data.frame() %>%
    setNames(lapply(., function(x){attr(x, "name")})) %>%
    dplyr::mutate(Status = factor(a5ss_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

  temp_mod <- parsnip::rand_forest(trees = tune::tune()) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("classification")

  temp_folds <- rsample::vfold_cv(data    = temp_train,
                                  v       = 3,
                                  repeats = 25,
                                  strata  = Status)

  allres_a5ss[[i]] <- workflows::workflow() %>%
    workflows::add_model(temp_mod) %>%
    workflows::add_formula(Status ~ .) %>%
    tune::tune_grid(resamples = temp_folds,
                    grid      = paramgrid) %>%
    workflowsets::collect_metrics() %>%
    dplyr::mutate(FDR = fdr_thresh[i]) %>%
    dplyr::select(FDR, trees, .metric, .estimator, mean, n, std_err, .config)

  rm(temp_train, temp_mod, temp_folds)
}
close(pb)
rm(pb, i, paramgrid, fdr_thresh, ntrees)

# Extract the AUC (denoted here as ROC) results from the stored result set. Compress into a dataframe.
allres_a5ss_ROC <- Reduce("rbind", allres_a5ss)
saveRDS(allres_a5ss_ROC, file = "file/path/to/save/a5ss_auctune_tidy.rds")

temp <- allres_a5ss_ROC %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(FDR, trees, mean) %>%
  dplyr::rename(AUC = mean) %>%
  dplyr::mutate(FDR = factor(FDR, levels = c(0.001, 0.005, 0.01, 0.05)),
                trees = factor(trees, levels = c(100, 200, 300, 400, 500)))

# a5ss heatmap
ggplot(temp, aes(trees, FDR)) +
  geom_tile(aes(fill = AUC)) + scale_fill_distiller(palette = "YlOrRd", direction = -1) +
  ggtitle("A5SS")
rm(temp, allres_a5ss, allres_a5ss_ROC, a5ss_traintest)

# Alternative 3' splice site (A3SS) -------------------------------------------------------

a3ss_data <- readRDS(file = "file/path/to/a3ss_data.rds")
a3ss_metadata <- readRDS(file = "file/path/to/a3ss_metadata.rds")

a3ss_traintest <- dplyr::left_join(a3ss_metadata, a3ss_data, by = "MergeID") %>%
  dplyr::mutate(IJC_TRAIN             = purrr::pmap(list(IJC_TRAIN_1, IJC_TRAIN_2, IJC_TRAIN_3, IJC_TRAIN_4,
                                                         IJC_TRAIN_5, IJC_TRAIN_6, IJC_TRAIN_7, IJC_TRAIN_8,
                                                         IJC_TRAIN_9, IJC_TRAIN_10, IJC_TRAIN_11, IJC_TRAIN_12,
                                                         IJC_TRAIN_13, IJC_TRAIN_14, IJC_TRAIN_15, IJC_TRAIN_16,
                                                         IJC_TRAIN_17, IJC_TRAIN_18, IJC_TRAIN_19, IJC_TRAIN_20,
                                                         IJC_TRAIN_21, IJC_TRAIN_22, IJC_TRAIN_23, IJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                IJC_TEST             = purrr::pmap(list(IJC_TEST_1, IJC_TEST_2, IJC_TEST_3, IJC_TEST_4,
                                                        IJC_TEST_5, IJC_TEST_6, IJC_TEST_7, IJC_TEST_8,
                                                        IJC_TEST_9, IJC_TEST_10, IJC_TEST_11, IJC_TEST_12,
                                                        IJC_TEST_13, IJC_TEST_14, IJC_TEST_15, IJC_TEST_16,
                                                        IJC_TEST_17, IJC_TEST_18, IJC_TEST_19, IJC_TEST_20,
                                                        IJC_TEST_21, IJC_TEST_22, IJC_TEST_23, IJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                IJC_TEST_Imputed             = purrr::pmap(list(IJC_TEST_imputed_1, IJC_TEST_imputed_2, IJC_TEST_imputed_3, IJC_TEST_imputed_4,
                                                                IJC_TEST_imputed_5, IJC_TEST_imputed_6, IJC_TEST_imputed_7, IJC_TEST_imputed_8,
                                                                IJC_TEST_imputed_9, IJC_TEST_imputed_10, IJC_TEST_imputed_11, IJC_TEST_imputed_12,
                                                                IJC_TEST_imputed_13, IJC_TEST_imputed_14, IJC_TEST_imputed_15, IJC_TEST_imputed_16,
                                                                IJC_TEST_imputed_17, IJC_TEST_imputed_18, IJC_TEST_imputed_19, IJC_TEST_imputed_20,
                                                                IJC_TEST_imputed_21, IJC_TEST_imputed_22, IJC_TEST_imputed_23, IJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                SJC_TRAIN             = purrr::pmap(list(SJC_TRAIN_1, SJC_TRAIN_2, SJC_TRAIN_3, SJC_TRAIN_4,
                                                         SJC_TRAIN_5, SJC_TRAIN_6, SJC_TRAIN_7, SJC_TRAIN_8,
                                                         SJC_TRAIN_9, SJC_TRAIN_10, SJC_TRAIN_11, SJC_TRAIN_12,
                                                         SJC_TRAIN_13, SJC_TRAIN_14, SJC_TRAIN_15, SJC_TRAIN_16,
                                                         SJC_TRAIN_17, SJC_TRAIN_18, SJC_TRAIN_19, SJC_TRAIN_20,
                                                         SJC_TRAIN_21, SJC_TRAIN_22, SJC_TRAIN_23, SJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                SJC_TEST             = purrr::pmap(list(SJC_TEST_1, SJC_TEST_2, SJC_TEST_3, SJC_TEST_4,
                                                        SJC_TEST_5, SJC_TEST_6, SJC_TEST_7, SJC_TEST_8,
                                                        SJC_TEST_9, SJC_TEST_10, SJC_TEST_11, SJC_TEST_12,
                                                        SJC_TEST_13, SJC_TEST_14, SJC_TEST_15, SJC_TEST_16,
                                                        SJC_TEST_17, SJC_TEST_18, SJC_TEST_19, SJC_TEST_20,
                                                        SJC_TEST_21, SJC_TEST_22, SJC_TEST_23, SJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                SJC_TEST_Imputed             = purrr::pmap(list(SJC_TEST_imputed_1, SJC_TEST_imputed_2, SJC_TEST_imputed_3, SJC_TEST_imputed_4,
                                                                SJC_TEST_imputed_5, SJC_TEST_imputed_6, SJC_TEST_imputed_7, SJC_TEST_imputed_8,
                                                                SJC_TEST_imputed_9, SJC_TEST_imputed_10, SJC_TEST_imputed_11, SJC_TEST_imputed_12,
                                                                SJC_TEST_imputed_13, SJC_TEST_imputed_14, SJC_TEST_imputed_15, SJC_TEST_imputed_16,
                                                                SJC_TEST_imputed_17, SJC_TEST_imputed_18, SJC_TEST_imputed_19, SJC_TEST_imputed_20,
                                                                SJC_TEST_imputed_21, SJC_TEST_imputed_22, SJC_TEST_imputed_23, SJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                IncLevel_TRAIN             = purrr::pmap(list(IncLevel_TRAIN_1, IncLevel_TRAIN_2, IncLevel_TRAIN_3, IncLevel_TRAIN_4,
                                                              IncLevel_TRAIN_5, IncLevel_TRAIN_6, IncLevel_TRAIN_7, IncLevel_TRAIN_8,
                                                              IncLevel_TRAIN_9, IncLevel_TRAIN_10, IncLevel_TRAIN_11, IncLevel_TRAIN_12,
                                                              IncLevel_TRAIN_13, IncLevel_TRAIN_14, IncLevel_TRAIN_15, IncLevel_TRAIN_16,
                                                              IncLevel_TRAIN_17, IncLevel_TRAIN_18, IncLevel_TRAIN_19, IncLevel_TRAIN_20,
                                                              IncLevel_TRAIN_21, IncLevel_TRAIN_22, IncLevel_TRAIN_23, IncLevel_TRAIN_24, MergeID),
                                                         function(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24, mergeid){
                                                           temp <- c(s1, s2, s3, s4, s5, s6,
                                                                     s7, s8, s9, s10, s11, s12,
                                                                     s13, s14, s15, s16, s17, s18,
                                                                     s19, s20, s21, s22, s23, s24)
                                                           attr(temp, "name") <- mergeid
                                                           temp
                                                         }),
                IncLevel_TEST             = purrr::pmap(list(IncLevel_TEST_1, IncLevel_TEST_2, IncLevel_TEST_3, IncLevel_TEST_4,
                                                             IncLevel_TEST_5, IncLevel_TEST_6, IncLevel_TEST_7, IncLevel_TEST_8,
                                                             IncLevel_TEST_9, IncLevel_TEST_10, IncLevel_TEST_11, IncLevel_TEST_12,
                                                             IncLevel_TEST_13, IncLevel_TEST_14, IncLevel_TEST_15, IncLevel_TEST_16,
                                                             IncLevel_TEST_17, IncLevel_TEST_18, IncLevel_TEST_19, IncLevel_TEST_20,
                                                             IncLevel_TEST_21, IncLevel_TEST_22, IncLevel_TEST_23, IncLevel_TEST_24, MergeID),
                                                        function(s1, s2, s3, s4, s5, s6,
                                                                 s7, s8, s9, s10, s11, s12,
                                                                 s13, s14, s15, s16, s17, s18,
                                                                 s19, s20, s21, s22, s23, s24, mergeid){
                                                          temp <- c(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24)
                                                          attr(temp, "name") <- mergeid
                                                          temp
                                                        }),
                IncLevel_TEST_Imputed             = purrr::pmap(list(IncLevel_TEST_imputed_1, IncLevel_TEST_imputed_2, IncLevel_TEST_imputed_3, IncLevel_TEST_imputed_4,
                                                                     IncLevel_TEST_imputed_5, IncLevel_TEST_imputed_6, IncLevel_TEST_imputed_7, IncLevel_TEST_imputed_8,
                                                                     IncLevel_TEST_imputed_9, IncLevel_TEST_imputed_10, IncLevel_TEST_imputed_11, IncLevel_TEST_imputed_12,
                                                                     IncLevel_TEST_imputed_13, IncLevel_TEST_imputed_14, IncLevel_TEST_imputed_15, IncLevel_TEST_imputed_16,
                                                                     IncLevel_TEST_imputed_17, IncLevel_TEST_imputed_18, IncLevel_TEST_imputed_19, IncLevel_TEST_imputed_20,
                                                                     IncLevel_TEST_imputed_21, IncLevel_TEST_imputed_22, IncLevel_TEST_imputed_23, IncLevel_TEST_imputed_24, MergeID),
                                                                function(s1, s2, s3, s4, s5, s6,
                                                                         s7, s8, s9, s10, s11, s12,
                                                                         s13, s14, s15, s16, s17, s18,
                                                                         s19, s20, s21, s22, s23, s24, mergeid){
                                                                  temp <- c(s1, s2, s3, s4, s5, s6,
                                                                            s7, s8, s9, s10, s11, s12,
                                                                            s13, s14, s15, s16, s17, s18,
                                                                            s19, s20, s21, s22, s23, s24)
                                                                  attr(temp, "name") <- mergeid
                                                                  temp
                                                                }),
                Status_TRAIN             = purrr::pmap(list(Status_TRAIN_1, Status_TRAIN_2, Status_TRAIN_3, Status_TRAIN_4,
                                                            Status_TRAIN_5, Status_TRAIN_6, Status_TRAIN_7, Status_TRAIN_8,
                                                            Status_TRAIN_9, Status_TRAIN_10, Status_TRAIN_11, Status_TRAIN_12,
                                                            Status_TRAIN_13, Status_TRAIN_14, Status_TRAIN_15, Status_TRAIN_16,
                                                            Status_TRAIN_17, Status_TRAIN_18, Status_TRAIN_19, Status_TRAIN_20,
                                                            Status_TRAIN_21, Status_TRAIN_22, Status_TRAIN_23, Status_TRAIN_24, MergeID),
                                                       function(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24, mergeid){
                                                         temp <- c(s1, s2, s3, s4, s5, s6,
                                                                   s7, s8, s9, s10, s11, s12,
                                                                   s13, s14, s15, s16, s17, s18,
                                                                   s19, s20, s21, s22, s23, s24)
                                                         attr(temp, "name") <- mergeid
                                                         temp
                                                       }),
                Status_TEST             = purrr::pmap(list(Status_TEST_1, Status_TEST_2, Status_TEST_3, Status_TEST_4,
                                                           Status_TEST_5, Status_TEST_6, Status_TEST_7, Status_TEST_8,
                                                           Status_TEST_9, Status_TEST_10, Status_TEST_11, Status_TEST_12,
                                                           Status_TEST_13, Status_TEST_14, Status_TEST_15, Status_TEST_16,
                                                           Status_TEST_17, Status_TEST_18, Status_TEST_19, Status_TEST_20,
                                                           Status_TEST_21, Status_TEST_22, Status_TEST_23, Status_TEST_24, MergeID),
                                                      function(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24, mergeid){
                                                        temp <- c(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24)
                                                        attr(temp, "name") <- mergeid
                                                        temp
                                                      })) %>%
  dplyr::select(-(Status_TRAIN_1:SJC_TEST_imputed_24)) %>%
  dplyr::select(MergeID, GeneID, geneSymbol, chr, strand, longExonStart_0base, longExonEnd, shortES, shortEE,
                flankingES, flankingEE, IncFormLen, SkipFormLen, ENSEMBL, GENENAME,
                IJC_TRAIN, SJC_TRAIN, IncLevel_TRAIN, Status_TRAIN, PValue_TRAIN, FDR_TRAIN, Filter.Count_TRAIN,
                IJC_TEST, SJC_TEST, IncLevel_TEST, IJC_TEST_Imputed, SJC_TEST_Imputed, IncLevel_TEST_Imputed, Status_TEST, PValue_TEST, FDR_TEST, Filter.Count_TEST)

rm(a3ss_data, a3ss_metadata)
gc()

# Hyperparameter (FDR thresh and ntree) search
fdr_thresh <- c(0.05, 0.01, 0.005, 0.001) # Define FDR thresholds to consider
ntrees     <- c(100, 200, 300, 400, 500) # Define number of trees for random forest to consider
paramgrid <- tibble::tibble(trees = ntrees)

set.seed(1) # Set seed for analysis
allres_a3ss <- vector("list", length = length(fdr_thresh)) # Define list to store results
pb <- txtProgressBar(min = 0, max = length(fdr_thresh), initial = 0, style = 3)
for(i in 1:length(fdr_thresh)){
  setTxtProgressBar(pb, i)
  temp_train <- a3ss_traintest %>%
    dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= fdr_thresh[i], Filter.Count_TRAIN == 0) %>%
    .$IncLevel_TRAIN %>%
    data.frame() %>%
    setNames(lapply(., function(x){attr(x, "name")})) %>%
    dplyr::mutate(Status = factor(a3ss_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

  temp_mod <- parsnip::rand_forest(trees = tune::tune()) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("classification")

  temp_folds <- rsample::vfold_cv(data    = temp_train,
                                  v       = 3,
                                  repeats = 25,
                                  strata  = Status)

  allres_a3ss[[i]] <- workflows::workflow() %>%
    workflows::add_model(temp_mod) %>%
    workflows::add_formula(Status ~ .) %>%
    tune::tune_grid(resamples = temp_folds,
                    grid      = paramgrid) %>%
    workflowsets::collect_metrics() %>%
    dplyr::mutate(FDR = fdr_thresh[i]) %>%
    dplyr::select(FDR, trees, .metric, .estimator, mean, n, std_err, .config)

  rm(temp_train, temp_mod, temp_folds)
}
close(pb)
rm(pb, i, paramgrid, fdr_thresh, ntrees)

# Extract the AUC (denoted here as ROC) results from the stored result set. Compress into a dataframe.
allres_a3ss_ROC <- Reduce("rbind", allres_a3ss)
saveRDS(allres_a3ss_ROC, file = "file/path/to/save/a3ss_auctune_tidy.rds")

temp <- allres_a3ss_ROC %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(FDR, trees, mean) %>%
  dplyr::rename(AUC = mean) %>%
  dplyr::mutate(FDR = factor(FDR, levels = c(0.001, 0.005, 0.01, 0.05)),
                trees = factor(trees, levels = c(100, 200, 300, 400, 500)))

# a3ss heatmap
ggplot(temp, aes(trees, FDR)) +
  geom_tile(aes(fill = AUC)) + scale_fill_distiller(palette = "YlOrRd", direction = -1) +
  ggtitle("A3SS")
rm(temp, allres_a3ss, allres_a3ss_ROC, a3ss_traintest)

# Mutually exclusive exons (MXE) -------------------------------------------------------

mxe_data <- readRDS(file = "file/path/to/mxe_data.rds")
mxe_metadata <- readRDS(file = "file/path/to/mxe_metadata.rds")

mxe_traintest <- dplyr::left_join(mxe_metadata, mxe_data, by = "MergeID") %>%
  dplyr::mutate(IJC_TRAIN             = purrr::pmap(list(IJC_TRAIN_1, IJC_TRAIN_2, IJC_TRAIN_3, IJC_TRAIN_4,
                                                         IJC_TRAIN_5, IJC_TRAIN_6, IJC_TRAIN_7, IJC_TRAIN_8,
                                                         IJC_TRAIN_9, IJC_TRAIN_10, IJC_TRAIN_11, IJC_TRAIN_12,
                                                         IJC_TRAIN_13, IJC_TRAIN_14, IJC_TRAIN_15, IJC_TRAIN_16,
                                                         IJC_TRAIN_17, IJC_TRAIN_18, IJC_TRAIN_19, IJC_TRAIN_20,
                                                         IJC_TRAIN_21, IJC_TRAIN_22, IJC_TRAIN_23, IJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                IJC_TEST             = purrr::pmap(list(IJC_TEST_1, IJC_TEST_2, IJC_TEST_3, IJC_TEST_4,
                                                        IJC_TEST_5, IJC_TEST_6, IJC_TEST_7, IJC_TEST_8,
                                                        IJC_TEST_9, IJC_TEST_10, IJC_TEST_11, IJC_TEST_12,
                                                        IJC_TEST_13, IJC_TEST_14, IJC_TEST_15, IJC_TEST_16,
                                                        IJC_TEST_17, IJC_TEST_18, IJC_TEST_19, IJC_TEST_20,
                                                        IJC_TEST_21, IJC_TEST_22, IJC_TEST_23, IJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                IJC_TEST_Imputed             = purrr::pmap(list(IJC_TEST_imputed_1, IJC_TEST_imputed_2, IJC_TEST_imputed_3, IJC_TEST_imputed_4,
                                                                IJC_TEST_imputed_5, IJC_TEST_imputed_6, IJC_TEST_imputed_7, IJC_TEST_imputed_8,
                                                                IJC_TEST_imputed_9, IJC_TEST_imputed_10, IJC_TEST_imputed_11, IJC_TEST_imputed_12,
                                                                IJC_TEST_imputed_13, IJC_TEST_imputed_14, IJC_TEST_imputed_15, IJC_TEST_imputed_16,
                                                                IJC_TEST_imputed_17, IJC_TEST_imputed_18, IJC_TEST_imputed_19, IJC_TEST_imputed_20,
                                                                IJC_TEST_imputed_21, IJC_TEST_imputed_22, IJC_TEST_imputed_23, IJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                SJC_TRAIN             = purrr::pmap(list(SJC_TRAIN_1, SJC_TRAIN_2, SJC_TRAIN_3, SJC_TRAIN_4,
                                                         SJC_TRAIN_5, SJC_TRAIN_6, SJC_TRAIN_7, SJC_TRAIN_8,
                                                         SJC_TRAIN_9, SJC_TRAIN_10, SJC_TRAIN_11, SJC_TRAIN_12,
                                                         SJC_TRAIN_13, SJC_TRAIN_14, SJC_TRAIN_15, SJC_TRAIN_16,
                                                         SJC_TRAIN_17, SJC_TRAIN_18, SJC_TRAIN_19, SJC_TRAIN_20,
                                                         SJC_TRAIN_21, SJC_TRAIN_22, SJC_TRAIN_23, SJC_TRAIN_24, MergeID),
                                                    function(s1, s2, s3, s4, s5, s6,
                                                             s7, s8, s9, s10, s11, s12,
                                                             s13, s14, s15, s16, s17, s18,
                                                             s19, s20, s21, s22, s23, s24, mergeid){
                                                      temp <- c(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24)
                                                      attr(temp, "name") <- mergeid
                                                      temp
                                                    }),
                SJC_TEST             = purrr::pmap(list(SJC_TEST_1, SJC_TEST_2, SJC_TEST_3, SJC_TEST_4,
                                                        SJC_TEST_5, SJC_TEST_6, SJC_TEST_7, SJC_TEST_8,
                                                        SJC_TEST_9, SJC_TEST_10, SJC_TEST_11, SJC_TEST_12,
                                                        SJC_TEST_13, SJC_TEST_14, SJC_TEST_15, SJC_TEST_16,
                                                        SJC_TEST_17, SJC_TEST_18, SJC_TEST_19, SJC_TEST_20,
                                                        SJC_TEST_21, SJC_TEST_22, SJC_TEST_23, SJC_TEST_24, MergeID),
                                                   function(s1, s2, s3, s4, s5, s6,
                                                            s7, s8, s9, s10, s11, s12,
                                                            s13, s14, s15, s16, s17, s18,
                                                            s19, s20, s21, s22, s23, s24, mergeid){
                                                     temp <- c(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24)
                                                     attr(temp, "name") <- mergeid
                                                     temp
                                                   }),
                SJC_TEST_Imputed             = purrr::pmap(list(SJC_TEST_imputed_1, SJC_TEST_imputed_2, SJC_TEST_imputed_3, SJC_TEST_imputed_4,
                                                                SJC_TEST_imputed_5, SJC_TEST_imputed_6, SJC_TEST_imputed_7, SJC_TEST_imputed_8,
                                                                SJC_TEST_imputed_9, SJC_TEST_imputed_10, SJC_TEST_imputed_11, SJC_TEST_imputed_12,
                                                                SJC_TEST_imputed_13, SJC_TEST_imputed_14, SJC_TEST_imputed_15, SJC_TEST_imputed_16,
                                                                SJC_TEST_imputed_17, SJC_TEST_imputed_18, SJC_TEST_imputed_19, SJC_TEST_imputed_20,
                                                                SJC_TEST_imputed_21, SJC_TEST_imputed_22, SJC_TEST_imputed_23, SJC_TEST_imputed_24, MergeID),
                                                           function(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24, mergeid){
                                                             temp <- c(s1, s2, s3, s4, s5, s6,
                                                                       s7, s8, s9, s10, s11, s12,
                                                                       s13, s14, s15, s16, s17, s18,
                                                                       s19, s20, s21, s22, s23, s24)
                                                             attr(temp, "name") <- mergeid
                                                             temp
                                                           }),
                IncLevel_TRAIN             = purrr::pmap(list(IncLevel_TRAIN_1, IncLevel_TRAIN_2, IncLevel_TRAIN_3, IncLevel_TRAIN_4,
                                                              IncLevel_TRAIN_5, IncLevel_TRAIN_6, IncLevel_TRAIN_7, IncLevel_TRAIN_8,
                                                              IncLevel_TRAIN_9, IncLevel_TRAIN_10, IncLevel_TRAIN_11, IncLevel_TRAIN_12,
                                                              IncLevel_TRAIN_13, IncLevel_TRAIN_14, IncLevel_TRAIN_15, IncLevel_TRAIN_16,
                                                              IncLevel_TRAIN_17, IncLevel_TRAIN_18, IncLevel_TRAIN_19, IncLevel_TRAIN_20,
                                                              IncLevel_TRAIN_21, IncLevel_TRAIN_22, IncLevel_TRAIN_23, IncLevel_TRAIN_24, MergeID),
                                                         function(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24, mergeid){
                                                           temp <- c(s1, s2, s3, s4, s5, s6,
                                                                     s7, s8, s9, s10, s11, s12,
                                                                     s13, s14, s15, s16, s17, s18,
                                                                     s19, s20, s21, s22, s23, s24)
                                                           attr(temp, "name") <- mergeid
                                                           temp
                                                         }),
                IncLevel_TEST             = purrr::pmap(list(IncLevel_TEST_1, IncLevel_TEST_2, IncLevel_TEST_3, IncLevel_TEST_4,
                                                             IncLevel_TEST_5, IncLevel_TEST_6, IncLevel_TEST_7, IncLevel_TEST_8,
                                                             IncLevel_TEST_9, IncLevel_TEST_10, IncLevel_TEST_11, IncLevel_TEST_12,
                                                             IncLevel_TEST_13, IncLevel_TEST_14, IncLevel_TEST_15, IncLevel_TEST_16,
                                                             IncLevel_TEST_17, IncLevel_TEST_18, IncLevel_TEST_19, IncLevel_TEST_20,
                                                             IncLevel_TEST_21, IncLevel_TEST_22, IncLevel_TEST_23, IncLevel_TEST_24, MergeID),
                                                        function(s1, s2, s3, s4, s5, s6,
                                                                 s7, s8, s9, s10, s11, s12,
                                                                 s13, s14, s15, s16, s17, s18,
                                                                 s19, s20, s21, s22, s23, s24, mergeid){
                                                          temp <- c(s1, s2, s3, s4, s5, s6,
                                                                    s7, s8, s9, s10, s11, s12,
                                                                    s13, s14, s15, s16, s17, s18,
                                                                    s19, s20, s21, s22, s23, s24)
                                                          attr(temp, "name") <- mergeid
                                                          temp
                                                        }),
                IncLevel_TEST_Imputed             = purrr::pmap(list(IncLevel_TEST_imputed_1, IncLevel_TEST_imputed_2, IncLevel_TEST_imputed_3, IncLevel_TEST_imputed_4,
                                                                     IncLevel_TEST_imputed_5, IncLevel_TEST_imputed_6, IncLevel_TEST_imputed_7, IncLevel_TEST_imputed_8,
                                                                     IncLevel_TEST_imputed_9, IncLevel_TEST_imputed_10, IncLevel_TEST_imputed_11, IncLevel_TEST_imputed_12,
                                                                     IncLevel_TEST_imputed_13, IncLevel_TEST_imputed_14, IncLevel_TEST_imputed_15, IncLevel_TEST_imputed_16,
                                                                     IncLevel_TEST_imputed_17, IncLevel_TEST_imputed_18, IncLevel_TEST_imputed_19, IncLevel_TEST_imputed_20,
                                                                     IncLevel_TEST_imputed_21, IncLevel_TEST_imputed_22, IncLevel_TEST_imputed_23, IncLevel_TEST_imputed_24, MergeID),
                                                                function(s1, s2, s3, s4, s5, s6,
                                                                         s7, s8, s9, s10, s11, s12,
                                                                         s13, s14, s15, s16, s17, s18,
                                                                         s19, s20, s21, s22, s23, s24, mergeid){
                                                                  temp <- c(s1, s2, s3, s4, s5, s6,
                                                                            s7, s8, s9, s10, s11, s12,
                                                                            s13, s14, s15, s16, s17, s18,
                                                                            s19, s20, s21, s22, s23, s24)
                                                                  attr(temp, "name") <- mergeid
                                                                  temp
                                                                }),
                Status_TRAIN             = purrr::pmap(list(Status_TRAIN_1, Status_TRAIN_2, Status_TRAIN_3, Status_TRAIN_4,
                                                            Status_TRAIN_5, Status_TRAIN_6, Status_TRAIN_7, Status_TRAIN_8,
                                                            Status_TRAIN_9, Status_TRAIN_10, Status_TRAIN_11, Status_TRAIN_12,
                                                            Status_TRAIN_13, Status_TRAIN_14, Status_TRAIN_15, Status_TRAIN_16,
                                                            Status_TRAIN_17, Status_TRAIN_18, Status_TRAIN_19, Status_TRAIN_20,
                                                            Status_TRAIN_21, Status_TRAIN_22, Status_TRAIN_23, Status_TRAIN_24, MergeID),
                                                       function(s1, s2, s3, s4, s5, s6,
                                                                s7, s8, s9, s10, s11, s12,
                                                                s13, s14, s15, s16, s17, s18,
                                                                s19, s20, s21, s22, s23, s24, mergeid){
                                                         temp <- c(s1, s2, s3, s4, s5, s6,
                                                                   s7, s8, s9, s10, s11, s12,
                                                                   s13, s14, s15, s16, s17, s18,
                                                                   s19, s20, s21, s22, s23, s24)
                                                         attr(temp, "name") <- mergeid
                                                         temp
                                                       }),
                Status_TEST             = purrr::pmap(list(Status_TEST_1, Status_TEST_2, Status_TEST_3, Status_TEST_4,
                                                           Status_TEST_5, Status_TEST_6, Status_TEST_7, Status_TEST_8,
                                                           Status_TEST_9, Status_TEST_10, Status_TEST_11, Status_TEST_12,
                                                           Status_TEST_13, Status_TEST_14, Status_TEST_15, Status_TEST_16,
                                                           Status_TEST_17, Status_TEST_18, Status_TEST_19, Status_TEST_20,
                                                           Status_TEST_21, Status_TEST_22, Status_TEST_23, Status_TEST_24, MergeID),
                                                      function(s1, s2, s3, s4, s5, s6,
                                                               s7, s8, s9, s10, s11, s12,
                                                               s13, s14, s15, s16, s17, s18,
                                                               s19, s20, s21, s22, s23, s24, mergeid){
                                                        temp <- c(s1, s2, s3, s4, s5, s6,
                                                                  s7, s8, s9, s10, s11, s12,
                                                                  s13, s14, s15, s16, s17, s18,
                                                                  s19, s20, s21, s22, s23, s24)
                                                        attr(temp, "name") <- mergeid
                                                        temp
                                                      })) %>%
  dplyr::select(-(Status_TRAIN_1:SJC_TEST_imputed_24)) %>%
  dplyr::select(MergeID, GeneID, geneSymbol, chr, strand, X1stExonStart_0base, X2ndExonEnd,
                X2ndExonStart_0base, X1stExonEnd, upstreamES, upstreamEE,
                downstreamES, downstreamEE, IncFormLen, SkipFormLen, ENSEMBL, GENENAME,
                IJC_TRAIN, SJC_TRAIN, IncLevel_TRAIN, Status_TRAIN, PValue_TRAIN, FDR_TRAIN, Filter.Count_TRAIN,
                IJC_TEST, SJC_TEST, IncLevel_TEST, IJC_TEST_Imputed, SJC_TEST_Imputed, IncLevel_TEST_Imputed, Status_TEST, PValue_TEST, FDR_TEST, Filter.Count_TEST)

rm(mxe_data, mxe_metadata)
gc()

# Hyperparameter (FDR thresh and ntree) search
fdr_thresh <- c(0.05, 0.01, 0.005, 0.001) # Define FDR thresholds to consider
ntrees     <- c(100, 200, 300, 400, 500) # Define number of trees for random forest to consider
paramgrid <- tibble::tibble(trees = ntrees)

# Received these warnings/errors for this analysis:
# ! Fold3, Repeat17: internal: No event observations were detected in `truth` with event level 'Case'.
# ! Fold2, Repeat08: internal: No control observations were detected in `truth` with control level 'Control'.
# ! Fold3, Repeat18: internal: No event observations were detected in `truth` with event level 'Case'.
set.seed(1) # Set seed for analysis
allres_mxe <- vector("list", length = length(fdr_thresh)) # Define list to store results
pb <- txtProgressBar(min = 0, max = length(fdr_thresh), initial = 0, style = 3)
for(i in 1:length(fdr_thresh)){
  setTxtProgressBar(pb, i)
  temp_train <- mxe_traintest %>%
    dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= fdr_thresh[i], Filter.Count_TRAIN == 0) %>%
    .$IncLevel_TRAIN %>%
    data.frame() %>%
    setNames(lapply(., function(x){attr(x, "name")})) %>%
    dplyr::mutate(Status = factor(mxe_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

  temp_mod <- parsnip::rand_forest(trees = tune::tune()) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("classification")

  temp_folds <- rsample::vfold_cv(data    = temp_train,
                                  v       = 3,
                                  repeats = 25,
                                  strata  = Status)

  allres_mxe[[i]] <- workflows::workflow() %>%
    workflows::add_model(temp_mod) %>%
    workflows::add_formula(Status ~ .) %>%
    tune::tune_grid(resamples = temp_folds,
                    grid      = paramgrid) %>%
    workflowsets::collect_metrics() %>%
    dplyr::mutate(FDR = fdr_thresh[i]) %>%
    dplyr::select(FDR, trees, .metric, .estimator, mean, n, std_err, .config)

  rm(temp_train, temp_mod, temp_folds)
}
close(pb)
rm(pb, i, paramgrid, fdr_thresh, ntrees)

# Extract the AUC (denoted here as ROC) results from the stored result set. Compress into a dataframe.
allres_mxe_ROC <- Reduce("rbind", allres_mxe)
saveRDS(allres_mxe_ROC, file = "file/path/to/save/mxe_auctune_tidy.rds")

temp <- allres_mxe_ROC %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(FDR, trees, mean) %>%
  dplyr::rename(AUC = mean) %>%
  dplyr::mutate(FDR = factor(FDR, levels = c(0.001, 0.005, 0.01, 0.05)),
                trees = factor(trees, levels = c(100, 200, 300, 400, 500)))

# mxe heatmap
ggplot(temp, aes(trees, FDR)) +
  geom_tile(aes(fill = AUC)) + scale_fill_distiller(palette = "YlOrRd", direction = -1) +
  ggtitle("MXE")
rm(temp, allres_mxe, allres_mxe_ROC, mxe_traintest)


# -------------------------------------------------------------------------
