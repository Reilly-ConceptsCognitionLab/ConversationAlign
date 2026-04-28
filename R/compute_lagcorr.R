#'
#' computes lagged correlations alignment measure across partners within each conversation
#' @name compute_lagcorr_noYRMisc
#' @returns
#' internal function to summarize_dyads that produces a dataframe with lagged correlations across turns (-2,0,2 as default) for each dimension of interest.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom stats cor.test
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect contains
#' @importFrom tidyselect ends_with
#' @keywords internal
#' @noRd

compute_lagcorr <- function(df_prep, lags = c(-2, 0, 2), corr_type = "Pearson") {
  # Validate correlation type
  if (!corr_type %in% c("Pearson", "Spearman")) {
    stop("corr_type must be either 'Pearson' or 'Spearman'")
  }
  corr_type <- tolower(corr_type)

  # Select alignment variables
  align_var <- grep("^(emo_|lex_|sem_|phon_)", colnames(df_prep), value = TRUE, ignore.case = TRUE)
  # Remove empty levels of all factors
  df_prep <- droplevels(df_prep)

  # Add participant pair variable
  df_prep <- df_prep %>%
    dplyr::group_by(Event_ID) %>%
    dplyr::mutate(Participant_Pair = paste(sort(unique(Participant_ID)), collapse = "---")) %>%
    dplyr::ungroup()

  # Define lag correlation function
  lag_corr_dyads <- function(df_prep, corr_type) {
    align_var <- colnames(df_prep[, which(colnames(df_prep) %in% align_var)])
    df_list <- split(df_prep, f = df_prep$Event_ID)

    # Iterate over each split data frame
    output_df_list <- lapply(df_list, function(df) {
      # Establish participant names and S1/S2 keys according to who speaks first
      participantvec <- unique(df$Participant_ID)
      names(participantvec) <- c("S1", "S2")
      df$Participant_ID <- gsub(participantvec[1], names(participantvec)[1], df$Participant_ID)
      df$Participant_ID <- gsub(participantvec[2], names(participantvec)[2], df$Participant_ID)

      # Create wide data frame with aggregated scores
      df_wide <- df %>%
        dplyr::group_by(Event_ID, Exchange_Count, Participant_ID, .add = FALSE) %>%
        dplyr::summarise(dplyr::across(tidyselect::contains(align_var), ~ mean(.x, na.rm = TRUE)),
                         .groups = "drop") %>%
        tidyr::pivot_wider(names_from = tidyselect::contains("Participant_ID"),
                           values_from = align_var)

      # Handle single alignment variable case
      if (length(align_var) == 1) {
        colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))] <-
          paste(align_var[1], colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))], sep = "_")
      }

      df_wide <- df_wide %>%
        dplyr::select(Event_ID, Exchange_Count, tidyselect::contains(align_var))

      # Remove rows with NA values
      rows_with_na_ind <- apply(df_wide[, which(colnames(df_wide) %in% paste(align_var, "S1", sep = "_") |
                                                  colnames(df_wide) %in% paste(align_var, "S2", sep = "_"))], 1,
                                function(x) any(is.na(x) == TRUE & is.nan(x) == FALSE))
      if (any(rows_with_na_ind)) {
        df_wide <- df_wide[!rows_with_na_ind, ]
      }

      # Interpolate missing values
      interp_df <- df_wide %>%
        dplyr::mutate(dplyr::across(tidyselect::contains(align_var),
                                    ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
        tidyr::fill(names(df_wide[, which(colnames(df_wide) %in% paste(align_var, "S1", sep = "_") |
                                            colnames(df_wide) %in% paste(align_var, "S2", sep = "_"))]),
                    .direction = "updown")

      # Prepare x and y variables
      x_vars <- interp_df %>%
        dplyr::select(c('Event_ID') | c(tidyselect::ends_with("_S1"))) %>%
        dplyr::rename_with(~stringr::str_replace(., "_S1", ""),
                           tidyselect::ends_with("_S1"))
      y_vars <- interp_df %>%
        dplyr::select(c('Event_ID') | c(tidyselect::ends_with("_S2"))) %>%
        dplyr::rename_with(~stringr::str_replace(., "_S2", ""),
                           tidyselect::ends_with("_S2"))

      # Modified function to calculate lagged correlation (NO YRmisc dependency)
      calculate_lag_corr <- function(x_vars, y_vars, dim, align_var, corr_type) {
        dim_x_vars <- x_vars[[dim]]
        dim_y_vars <- y_vars[[dim]]

        # For Spearman, convert to ranks
        if (corr_type == "Spearman") {
          dim_x_vars <- rank(dim_x_vars, na.last = "keep")
          dim_y_vars <- rank(dim_y_vars, na.last = "keep")
        }

        # Directly compute correlations for each requested lag
        cor_vals <- sapply(lags, function(L) {
          n <- length(dim_x_vars)
          if (L == 0) {
            cor(dim_x_vars, dim_y_vars, use = "complete.obs", method = corr_type)
          } else if (L < 0) {
            L <- abs(L)
            # y lags behind x: correlate x[t] with y[t-L]  (L>0)
            if (L >= n) return(NA)
            x_sel <- dim_x_vars[(L+1):n]
            y_sel <- dim_y_vars[1:(n-L)]
            cor(x_sel, y_sel, use = "complete.obs", method = corr_type)
          } else {  # L < 0
            # y leads x: correlate x[t] with y[t+|L|]
            #k <- abs(L)
            k <- L
            if (k >= n) return(NA)
            x_sel <- dim_x_vars[1:(n-k)]
            y_sel <- dim_y_vars[(k+1):n]
            cor(x_sel, y_sel, use = "complete.obs", method = corr_type)
          }
        })

        # Create column names as in the original output
        selectNames <- sapply(lags, function(x) {
          if(x < 0) paste0("TurnCorr_Lead", abs(x))
          else if(x > 0) paste0("TurnCorr_Lag", x)
          else "TurnCorr_Immediate"
        })

        # Build a one-row data frame with the selected names
        lo_full <- as.data.frame(as.list(setNames(cor_vals, selectNames)),
                                 stringsAsFactors = FALSE)
        lo_full %>% dplyr::select(tidyselect::all_of(selectNames))
      }

      # Calculate correlations for each dimension
      dyad_dim_list <- lapply(align_var, function(dim) {
        lag_results_df <- calculate_lag_corr(x_vars, y_vars, dim, align_var, corr_type)
        colnames(lag_results_df) <- paste(colnames(lag_results_df), dim, sep = "-")
        if (match(dim, align_var) == 1) {
          lag_results_df$Participant_ID <- paste(participantvec, collapse = "---")
        }
        lag_results_df
      })

      # Combine results
      dyad_covar <- dplyr::bind_cols(dyad_dim_list, Event_ID = unique(df$Event_ID))
      dyad_covar$Talked_First <- participantvec[1]
      dyad_covar
    })

    dplyr::bind_rows(output_df_list)
  }

  # Compute and process correlations
  covar_df <- lag_corr_dyads(df_prep = df_prep, corr_type = corr_type)

  covar_df <- covar_df %>% dplyr::select(!c(Participant_ID)) %>%
    tidyr::pivot_longer(cols = tidyselect::contains("TurnCorr"),
                        names_pattern = "(.*)-(.*)",
                        names_to = c(".value", "Dimension"))

  row.names(covar_df) <- NULL
  return(covar_df)
}
