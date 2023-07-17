library(magrittr)

## Final model fitting (using tidymodels) -----------------------------------------------------

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

se_train <- se_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(se_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

se_test <- se_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(se_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_se <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_se), initial = 0, style = 3)
for(i in 1:length(allres_se)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = se_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(se_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, se_test) %>%
    dplyr::bind_cols(predict(train_fit, se_test, type = "prob")) %>%
    dplyr::bind_cols(se_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_se[[i]] <- list(train_perf = train_perf,
                         test_perf  = test_perf,
                         var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_se, file = "file/path/to/save/se_tidymodels_finalfit.rds")
rm(list = ls())


# Retained Intron (RI) ----------------------------------------------------

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

ri_train <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

ri_test <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_ri <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_ri), initial = 0, style = 3)
for(i in 1:length(allres_ri)){
  setTxtProgressBar(pb, i)

   temp_folds <- rsample::vfold_cv(data    = ri_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(ri_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, ri_test) %>%
    dplyr::bind_cols(predict(train_fit, ri_test, type = "prob")) %>%
    dplyr::bind_cols(ri_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_ri[[i]] <- list(train_perf = train_perf,
                         test_perf  = test_perf,
                         var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_ri, file = "file/path/to/save/ri_tidymodels_finalfit.rds")
rm(list = ls())

# Alternative 5' splice site (A5SS) -------------------------------------------------------------------------

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

a5ss_train <- a5ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(a5ss_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

a5ss_test <- a5ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(a5ss_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_a5ss <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_a5ss), initial = 0, style = 3)
for(i in 1:length(allres_a5ss)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = a5ss_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(a5ss_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, a5ss_test) %>%
    dplyr::bind_cols(predict(train_fit, a5ss_test, type = "prob")) %>%
    dplyr::bind_cols(a5ss_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_a5ss[[i]] <- list(train_perf = train_perf,
                           test_perf  = test_perf,
                           var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_a5ss, file = "file/path/to/save/a5ss_tidymodels_finalfit.rds")
rm(list = ls())

# Alternative 3' splice site (A3SS) -------------------------------------------------------------------------

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

a3ss_train <- a3ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(a3ss_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

a3ss_test <- a3ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(a3ss_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_a3ss <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_a3ss), initial = 0, style = 3)
for(i in 1:length(allres_a3ss)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = a3ss_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(a3ss_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, a3ss_test) %>%
    dplyr::bind_cols(predict(train_fit, a3ss_test, type = "prob")) %>%
    dplyr::bind_cols(a3ss_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_a3ss[[i]] <- list(train_perf = train_perf,
                           test_perf  = test_perf,
                           var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_a3ss, file = "file/path/to/save/a3ss_tidymodels_finalfit.rds")
rm(list = ls())

# Mutually exclusive exons (MXE) -------------------------------------------------------------------------

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

mxe_train <- mxe_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(mxe_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

mxe_test <- mxe_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(mxe_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_mxe <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_mxe), initial = 0, style = 3)
for(i in 1:length(allres_mxe)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = mxe_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(mxe_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, mxe_test) %>%
    dplyr::bind_cols(predict(train_fit, mxe_test, type = "prob")) %>%
    dplyr::bind_cols(mxe_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_mxe[[i]] <- list(train_perf = train_perf,
                          test_perf  = test_perf,
                          var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_mxe, file = "file/path/to/save/mxe_tidymodels_finalfit.rds")
rm(list = ls())


# Retained Intron (RI) + Skipped Exon (SE) -------------------------------------------------------------------------

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

ri_train <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

ri_test <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

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

se_train <- se_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

se_test <- se_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

rise_train <- cbind(ri_train, se_train)
rise_test  <- cbind(ri_test, se_test)

rm(ri_train, ri_test, se_train, se_test)

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_rise <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_rise), initial = 0, style = 3)
for(i in 1:length(allres_rise)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = rise_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(rise_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, rise_test) %>%
    dplyr::bind_cols(predict(train_fit, rise_test, type = "prob")) %>%
    dplyr::bind_cols(rise_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_rise[[i]] <- list(train_perf = train_perf,
                           test_perf  = test_perf,
                           var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_rise, file = "file/path/to/save/rise_tidymodels_finalfit.rds")
rm(list = ls())

# Retained Intron (RI) + Alternative 5' splice site (A5SS) -------------------------------------------------------------------------

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

ri_train <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

ri_test <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

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

a5ss_train <- a5ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

a5ss_test <- a5ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

ria5ss_train <- cbind(ri_train, a5ss_train)
ria5ss_test  <- cbind(ri_test, a5ss_test)

rm(ri_train, ri_test, a5ss_train, a5ss_test)

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_ria5ss <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_ria5ss), initial = 0, style = 3)
for(i in 1:length(allres_ria5ss)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = ria5ss_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(ria5ss_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, ria5ss_test) %>%
    dplyr::bind_cols(predict(train_fit, ria5ss_test, type = "prob")) %>%
    dplyr::bind_cols(ria5ss_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_ria5ss[[i]] <- list(train_perf = train_perf,
                             test_perf  = test_perf,
                             var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_ria5ss, file = "file/path/to/save/ria5ss_tidymodels_finalfit.rds")
rm(list = ls())

# Retained Intron (RI) + Alternative 3' splice site (A3SS) -------------------------------------------------------------------------

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

ri_train <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

ri_test <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

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

a3ss_train <- a3ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

a3ss_test <- a3ss_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

ria3ss_train <- cbind(ri_train, a3ss_train)
ria3ss_test  <- cbind(ri_test, a3ss_test)

rm(ri_train, ri_test, a3ss_train, a3ss_test)

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_ria3ss <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_ria3ss), initial = 0, style = 3)
for(i in 1:length(allres_ria3ss)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = ria3ss_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(ria3ss_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, ria3ss_test) %>%
    dplyr::bind_cols(predict(train_fit, ria3ss_test, type = "prob")) %>%
    dplyr::bind_cols(ria3ss_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_ria3ss[[i]] <- list(train_perf = train_perf,
                             test_perf  = test_perf,
                             var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_ria3ss, file = "file/path/to/save/ria3ss_tidymodels_finalfit.rds")
rm(list = ls())


# Retained Intron (RI) + Mutually exclusive exons (MXE) -------------------------------------------------------------------------

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

ri_train <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TRAIN[[1]], levels = c("Case", "Control")))

ri_test <- ri_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")})) %>%
  dplyr::mutate(Status = factor(ri_traintest$Status_TEST[[1]], levels = c("Case", "Control")))

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

mxe_train <- mxe_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TRAIN %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

mxe_test <- mxe_traintest %>%
  dplyr::filter(!is.na(FDR_TRAIN), FDR_TRAIN <= 0.001, Filter.Count_TRAIN == 0) %>%
  .$IncLevel_TEST_Imputed %>%
  data.frame() %>%
  setNames(lapply(., function(x){attr(x, "name")}))

rimxe_train <- cbind(ri_train, mxe_train)
rimxe_test  <- cbind(ri_test, mxe_test)

rm(ri_train, ri_test, mxe_train, mxe_test)

# Final Model Specification
final_mod <- parsnip::rand_forest(trees = 300) %>%
  parsnip::set_engine("ranger", num.threads = 15, importance = "impurity") %>%
  parsnip::set_mode("classification")

# Metrics to use for evaluating test data performance
test_metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

set.seed(1)
allres_rimxe <- vector("list", length = 100)
pb <- txtProgressBar(min = 0, max = length(allres_rimxe), initial = 0, style = 3)
for(i in 1:length(allres_rimxe)){
  setTxtProgressBar(pb, i)

  temp_folds <- rsample::vfold_cv(data    = rimxe_train,
                                  v       = 3,
                                  repeats = 100,
                                  strata  = Status)

  train_wf <- workflows::workflow() %>%
    workflows::add_model(final_mod) %>%
    workflows::add_formula(Status ~ .)

  train_perf <- train_wf %>%
    tune::fit_resamples(temp_folds) %>%
    tune::collect_metrics()

  train_fit <- train_wf %>%
    parsnip::fit(rimxe_train)

  var_importance <- train_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi(scale = TRUE)

  test_perf <- predict(train_fit, rimxe_test) %>%
    dplyr::bind_cols(predict(train_fit, rimxe_test, type = "prob")) %>%
    dplyr::bind_cols(rimxe_test %>% dplyr::select(Status)) %>%
    test_metrics(truth = Status, .pred_Case, estimate = .pred_class)

  allres_rimxe[[i]] <- list(train_perf = train_perf,
                            test_perf  = test_perf,
                            var_imp    = var_importance)
  rm(temp_folds, train_wf, train_perf, train_fit, var_importance, test_perf)
}
close(pb)
rm(pb, i, final_mod, test_metrics)

saveRDS(allres_rimxe, file = "file/path/to/save/rimxe_tidymodels_finalfit.rds")
rm(list = ls())

# -------------------------------------------------------------------------
