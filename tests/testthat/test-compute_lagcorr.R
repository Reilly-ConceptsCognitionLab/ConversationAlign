# unit testing for computing correlations

# prepare data for lagged corr testing
# should give corr of 1 when lagged by 2
input <- data.frame(
  Event_ID = "corr_test",
  Participant_ID = rep(c("P1", "P2"), 50),
  Exchange_Count = rep(1:50, each = 2),
  emo_anger = c(rbind(1:50, c(0, 0, 1:48))) # interleave two sequences, one lagged by 2
)

test_that("computed pearson correlation is correct", {
  # lagged by two should be equal to 1
  p_output <- compute_lagcorr(input, corr_type = "Pearson")
  expect_equal(p_output$TurnCorr_Lag2[1], 1)

})


test_that("computed spearman correlation is correct", {
  # lagged by two should be equal to 1
  p_output <- compute_lagcorr(input, corr_type = "Spearman")
  expect_equal(p_output$TurnCorr_Lag2[1], 1)
})
