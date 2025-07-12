# unit testing for computing area under the curve

# single dyad for testing
one_rhyme <- NurseryRhymes_Prepped[NurseryRhymes_Prepped$Event_ID == "LittleLamb",]
one_rhyme <- droplevels(one_rhyme)


test_that("auc formats properly", {
  # computed previously
  expected_auc_df <- data.frame(
    Event_ID = "LittleLamb", Exchanges = 50,
    AUC_emo_anger_raw = 1.485925, AUC_emo_anger_scaled100 = 1.485925
  )
  # compare to computed df
  expect_equal(compute_auc(one_rhyme), expected_auc_df)
})

# remove all but first three exchanges
one_rhyme_small <- one_rhyme[one_rhyme$Exchange_Count < 3, ]

test_that("auc fills with na when under 3 exchanges", {
  expected_small_auc_df <- data.frame(
    Event_ID = "LittleLamb", Exchanges = 2,
    AUC_emo_anger_raw = as.double(NA), AUC_emo_anger_scaled100 = as.double(NA)
  )
  # compare to computed df
  expect_equal(compute_auc(one_rhyme_small), expected_small_auc_df)

})
