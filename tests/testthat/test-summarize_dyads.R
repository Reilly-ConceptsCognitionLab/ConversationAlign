# tests for summarize_dyads larger function

# data to input
one_rhyme <- ConversationAlign::NurseryRhymes_Prepped[ConversationAlign::NurseryRhymes_Prepped$Event_ID == "LittleLamb",]
one_rhyme <- droplevels(one_rhyme)

test_that("required column names are present", {
  # compute summarize dyads on single nursery rhyme
  output <- ConversationAlign::summarize_dyads(one_rhyme)
  # define list of required names and evaluate
  col_vec <- c("Event_ID", "Participant_ID", "Dimension", "Dimension_Mean", "AUC_raw", "AUC_scaled100", "Talked_First")
  expect_in(col_vec, colnames(output))
})
