#' Calculate and Allocate Cohort Achievements
#'
#' @description
#' Reads cohort data, vote data, and achievement definitions; calculates eligibility,
#' then allocates a fixed number of achievements per person based on priority and
#' exclusivity rules. Outputs include a full eligibility list and a final allocation.
#'
#' @param trait_map_path Path to `trait_map.csv`.
#' @param vote_map_path Path to `vote_map.csv`.
#' @param data_path Path to `group_dataset.csv`.
#' @param achievements_path Path to `achievements.csv`.
#' @param voting_data_path Path to `voting_data.csv`.
#' @param output_dir Directory where output CSVs should be written. Defaults to "achievements".
#' @param max_awards Integer count of achievements to allocate per person. Defaults to 10.
#' @param write_qa Logical; when TRUE, write QA summary CSVs to output_dir.
#' @param soft_fail Logical; when TRUE, missing inputs/columns emit warnings and
#'   return empty outputs instead of stopping.
#'
#' @return A list with `all_eligible` and `awarded` data frames.
#' @export
#'
#' @examples
#' \dontrun{
#' inputs_dir <- system.file("achievement_inputs", package = "TraitGraph")
#' TG_achievements(
#'   trait_map_path = file.path(inputs_dir, "trait_map.csv"),
#'   vote_map_path = file.path(inputs_dir, "vote_map.csv"),
#'   data_path = file.path(inputs_dir, "group_dataset.csv"),
#'   achievements_path = file.path(inputs_dir, "achievements.csv"),
#'   voting_data_path = file.path(inputs_dir, "voting_data.csv"),
#'   output_dir = "achievements",
#'   max_awards = 10
#' )
#' }
TG_achievements <- function(
  trait_map_path,
  vote_map_path,
  data_path,
  achievements_path,
  voting_data_path,
  output_dir = "achievements",
  max_awards = 10,
  write_qa = TRUE,
  soft_fail = TRUE
) {
  start_time <- Sys.time()
  summary_warnings <- character()
  issues <- data.frame(source = character(), issue = character(), stringsAsFactors = FALSE)

  rel_path <- function(path) {
    if (is.null(path) || !nzchar(path)) return(path)
    wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    p <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (startsWith(p, paste0(wd, "/")) || p == wd) {
      return(sub(paste0("^", gsub("([.\\+\\*\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", wd), "/?"), "", p))
    }
    path
  }

  add_issue <- function(source, issue) {
    issues <<- rbind(issues, data.frame(source = rel_path(source), issue = issue, stringsAsFactors = FALSE))
  }

  write_issue_log <- function() {
    if (nrow(issues) == 0) return(invisible(NULL))
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    issues_path <- file.path(output_dir, "achievements_input_issues.csv")
    utils::write.csv(issues, issues_path, row.names = FALSE)
    warning(paste("Achievements warnings: missing inputs detected. See", rel_path(issues_path)))
  }

  safe_read_csv <- function(path) {
    if (!file.exists(path)) {
      if (!soft_fail) {
        stop(paste("Missing input file:", path))
      }
      add_issue(path, "file_missing")
      return(NULL)
    }
    tryCatch(
      read.csv(path, stringsAsFactors = FALSE),
      error = function(e) {
        if (!soft_fail) {
          stop(paste("Failed to read input file:", path, "-", e$message))
        }
        add_issue(path, paste0("read_error:", e$message))
        NULL
      }
    )
  }
  # ---- Inputs ----
  achievements <- safe_read_csv(achievements_path)
  people <- safe_read_csv(data_path)
  trait_map <- safe_read_csv(trait_map_path)
  vote_map <- safe_read_csv(vote_map_path)
  voting_data <- safe_read_csv(voting_data_path)

  # ---- Output Folder ----
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # ---- Validation ----
  if (is.null(achievements) || is.null(people) || is.null(trait_map) ||
      is.null(vote_map) || is.null(voting_data)) {
    if (soft_fail) {
      write_issue_log()
      return(list(all_eligible = data.frame(), awarded = data.frame()))
    }
  }
  required_ach_cols <- c("id", "Title", "desirability_rank", "exclusive_group")
  missing_ach_cols <- setdiff(required_ach_cols, names(achievements))
  if (length(missing_ach_cols) > 0) {
    if (!soft_fail) {
      stop(paste("Missing achievement columns:", paste(missing_ach_cols, collapse = ", ")))
    }
    add_issue(achievements_path, paste0("missing_cols:", paste(missing_ach_cols, collapse = ",")))
    write_issue_log()
    return(list(all_eligible = data.frame(), awarded = data.frame()))
  }

  required_people_cols <- c("name", "user_id", "start_time", "completion_time", "total_duration_sec")
  missing_people_cols <- setdiff(required_people_cols, names(people))
  if (length(missing_people_cols) > 0) {
    if (!soft_fail) {
      stop(paste("Missing participant columns:", paste(missing_people_cols, collapse = ", ")))
    }
    add_issue(data_path, paste0("missing_cols:", paste(missing_people_cols, collapse = ",")))
    essential_people_cols <- c("name", "user_id")
    missing_essential <- setdiff(essential_people_cols, names(people))
    if (length(missing_essential) > 0) {
      write_issue_log()
      return(list(all_eligible = data.frame(), awarded = data.frame()))
    }
    for (col in setdiff(required_people_cols, names(people))) {
      people[[col]] <- NA
    }
  }

  required_vote_cols <- c("voter_name", "item_id", "voted_for")
  missing_vote_cols <- setdiff(required_vote_cols, names(voting_data))
  if (length(missing_vote_cols) > 0) {
    if (!soft_fail) {
      stop(paste("voting_data.csv missing columns:", paste(missing_vote_cols, collapse = ", ")))
    }
    add_issue(voting_data_path, paste0("missing_cols:", paste(missing_vote_cols, collapse = ",")))
    write_issue_log()
    return(list(all_eligible = data.frame(), awarded = data.frame()))
  }

  # ---- Helpers ----
  achievements$id <- as.character(achievements$id)
  ach_map <- setNames(achievements$id, achievements$Title)
  missing_awards <- character()

  add_eligible <- function(df, person_name, user_id, title, shared_person_name = NA, shared_person_id = NA) {
    if (is.null(title) || is.na(title) || !nzchar(title)) return(df)
    ach_id <- unname(ach_map[title])
    if (length(ach_id) == 0 || is.na(ach_id)) {
      missing_awards <<- unique(c(missing_awards, title))
      return(df)
    }
    df[nrow(df) + 1, ] <- list(
      person_name = person_name,
      user_id = user_id,
      achievement_id = ach_id,
      Title = title,
      shared_person_name = shared_person_name,
      shared_person_id = shared_person_id
    )
    df
  }

  # Track all achievements that should be handled
  handled_titles <- c(
    "Soulmate","Nucleus","Connector","The Devil You Know","Nemesis",
    "Attack of the clones","Apex Sentinel","Apex Rogue","The Double Agent","Besties","Black Sheep","Bullseye","Apex Strategist","Apex Straight-Shooter",
    "Apex Visionary","Apex Realist","First!","Full Send","Apex Risk-Taker","Apex Regulator","Mr. Popular","Enigma","Apex Connector","Apex Observer",
    "Apex Star","Apex Stewart","The Specialist","Apex Provocateur","Apex Guardian","Vote-Magnet","Paradox Engine",
    "Always the bridesmaid","Bronze is the New Gold","Photo Finish","Apex Peacemaker","Double Jinx",
    "Apex Challenger","Low Profile","Triplets!","Plot Armor",
    "Apex Empath","Apex Anchor","Apex Planner","Apex Improviser","The Double Agent","Speedrunner","Contrarian","Inclusive","Kingmaker",
    "Last-Minute Larry","Ride-or-Die","You get a car, and you get a car!","Caboose","Ugly Duckling","Default Settings",
    "Stormtrooper","The Thinker","Early Bird","I win","Jinx","Keener",
    "Night Owl","Third Wheel","Threes all the way down","Weekend Warrior","Mystery Gift",
    "Consolation Prize","Lunch Break","Middle Child","Participation Trophy","Prime Time","Completionist",
    "Remarkably Unremarkable","Twins!","Extreme Ends"
  )

  missing_titles <- setdiff(achievements$Title, handled_titles)
  extra_titles <- setdiff(handled_titles, achievements$Title)
  if (length(missing_titles) > 0) {
    msg <- paste("Achievement titles missing from handled list:", paste(missing_titles, collapse = ", "))
    summary_warnings <<- c(summary_warnings, msg)
    warning(msg)
  }
  if (length(extra_titles) > 0) {
    msg <- paste("Handled titles not found in achievements.csv:", paste(extra_titles, collapse = ", "))
    summary_warnings <<- c(summary_warnings, msg)
    warning(msg)
  }

  eligible <- data.frame(
    person_name = character(),
    user_id = character(),
    achievement_id = character(),
    Title = character(),
    shared_person_name = character(),
    shared_person_id = character(),
    stringsAsFactors = FALSE
  )

  # ---- Column Maps ----
  trait_columns <- trait_map$Column
  trait_columns <- trait_columns[trait_columns != "" & trait_columns != "NA"]

  factor_cols <- c("HEX_H","HEX_E","HEX_X","HEX_A","HEX_C","HEX_O")
  subtrait_cols <- trait_map$Column[trait_map$Type == "subtrait"]
  subtrait_cols <- subtrait_cols[subtrait_cols %in% names(people)]
  missing_trait_cols <- setdiff(unique(c(factor_cols, trait_columns)), names(people))
  if (length(missing_trait_cols) > 0) {
    add_issue(data_path, paste0("missing_trait_cols:", paste(missing_trait_cols, collapse = ",")))
    if (!soft_fail) {
      stop(paste("Missing trait columns in data:", paste(missing_trait_cols, collapse = ", ")))
    }
  }

  if (!("extreme_response_ratio" %in% names(people))) {
    extreme_cols <- if (length(subtrait_cols) > 0) subtrait_cols else factor_cols
    if (length(extreme_cols) > 0 && all(extreme_cols %in% names(people))) {
      extreme_mat <- data.frame(lapply(people[, extreme_cols, drop = FALSE], as.numeric))
      people$extreme_response_ratio <- apply(extreme_mat, 1, function(r) {
        r <- r[!is.na(r)]
        if (length(r) == 0) return(NA_real_)
        mean(r <= 10 | r >= 90)
      })
    }
  }

  # ---- Preprocessing ----
  people$user_id <- as.character(people$user_id)

  parse_time <- function(x) {
    t <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "")
    if (all(is.na(t))) t <- as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = "")
    t
  }

  people$completion_time <- parse_time(people$completion_time)
  people$start_time <- parse_time(people$start_time)

  people$completion_duration_sec <- as.numeric(difftime(people$completion_time, people$start_time, units = "secs"))
  people$total_duration_sec <- as.numeric(people$total_duration_sec)

  for (col in factor_cols) {
    if (col %in% names(people)) people[[col]] <- as.numeric(people[[col]])
  }

  for (col in c("DT", "DT_N", "DT_M", "DT_P", "DT_S")) {
    if (col %in% names(people)) people[[col]] <- as.numeric(people[[col]])
  }

  # ---- Similarity (Subtraits + DT subtraits only) ----
  sim_cols <- c(subtrait_cols, "DT_N", "DT_M", "DT_P", "DT_S")
  sim_cols <- sim_cols[sim_cols %in% names(people)]

  sim_mat <- NULL
  if (length(sim_cols) > 1) {
    sim_data <- as.matrix(people[, sim_cols])
    sim_mat <- cor(t(sim_data), use = "pairwise.complete.obs")
    rownames(sim_mat) <- people$name
    colnames(sim_mat) <- people$name
  }

  # ---- Vote Mapping (Question -> Trait Column) ----
  resolve_trait_column <- function(trait_name) {
    if (trait_name %in% trait_columns) return(trait_name)
    if (startsWith(trait_name, "Direct_")) {
      stripped <- sub("^Direct_", "", trait_name)
      if (stripped %in% trait_columns) return(stripped)
    }
    if (trait_name %in% c("HEX_Hon", "Direct_HEX_Hon")) return("HEX_H_S")
    if (trait_name %in% c("HEX_Hum", "Direct_HEX_Hum")) return("HEX_H_M")
    return(NA)
  }

  vote_map$Trait_Column <- vapply(vote_map$Trait_Name, resolve_trait_column, character(1))

  # ---- Raw Voting Data ----
  if ("voter_name" %in% names(voting_data)) {
    voters_col <- "voter_name"
  } else if ("\ufeffvoter_name" %in% names(voting_data)) {
    voters_col <- "\ufeffvoter_name"
  } else {
    stop("voting_data.csv missing voter_name column")
  }

  if (!all(c("item_id", "voted_for") %in% names(voting_data))) {
    stop("voting_data.csv must include item_id and voted_for columns")
  }

  vote_records <- data.frame(
    voter_name = character(),
    voter_id = character(),
    item_id = character(),
    target_name = character(),
    stringsAsFactors = FALSE
  )

  name_to_id <- setNames(people$user_id, people$name)

  for (i in seq_len(nrow(voting_data))) {
    voter <- voting_data[[voters_col]][i]
    item <- voting_data$item_id[i]
    voted_for <- voting_data$voted_for[i]

    if (is.na(voter) || is.na(item) || is.na(voted_for)) next

    targets <- unlist(strsplit(voted_for, ","))
    targets <- trimws(targets)
    targets <- targets[targets != ""]

    for (t in targets) {
      if (!(t %in% people$name)) next
      if (t == voter) next  # no self-votes

      vote_records[nrow(vote_records) + 1, ] <- list(
        voter_name = voter,
        voter_id = name_to_id[[voter]],
        item_id = item,
        target_name = t
      )
    }
  }

  # ---- Vote Aggregates ----
  vote_counts_total <- table(vote_records$target_name)
  unique_voters_per_target <- tapply(vote_records$voter_name, vote_records$target_name, function(x) length(unique(x)))

  # Per-question counts and ranks
  question_ranks <- list()
  for (q in unique(vote_records$item_id)) {
    subset <- vote_records[vote_records$item_id == q, ]
    if (nrow(subset) == 0) next

    counts <- table(subset$target_name)
    if (length(counts) == 0) next

    unique_counts <- sort(unique(as.numeric(counts)), decreasing = TRUE)
    ranks <- sapply(as.numeric(counts), function(ct) which(unique_counts == ct)[1])

    question_ranks[[q]] <- data.frame(
      target_name = as.character(names(counts)),
      count = as.numeric(counts),
      rank = as.integer(ranks),
      stringsAsFactors = FALSE
    )
  }

  # Rank tallies per person
  rank_tally <- data.frame(
    target_name = people$name,
    rank1 = 0,
    rank2 = 0,
    rank3 = 0,
    rank4 = 0,
    stringsAsFactors = FALSE
  )

  for (q in names(question_ranks)) {
    qr <- question_ranks[[q]]
    for (i in seq_len(nrow(qr))) {
      idx <- which(rank_tally$target_name == qr$target_name[i])
      r <- qr$rank[i]
      if (r == 1) rank_tally$rank1[idx] <- rank_tally$rank1[idx] + 1
      if (r == 2) rank_tally$rank2[idx] <- rank_tally$rank2[idx] + 1
      if (r == 3) rank_tally$rank3[idx] <- rank_tally$rank3[idx] + 1
      if (r == 4) rank_tally$rank4[idx] <- rank_tally$rank4[idx] + 1
    }
  }

  # Ties for first place
  first_place_ties <- rep(FALSE, nrow(people))
  for (q in names(question_ranks)) {
    qr <- question_ranks[[q]]
    top <- qr[qr$rank == 1, ]
    if (nrow(top) > 1) {
      for (t in top$target_name) {
        idx <- which(people$name == t)
        if (length(idx) == 1) first_place_ties[idx] <- TRUE
      }
    }
  }

  # Vote correctness for accuracy (mapped questions only)
  correct_counts <- rep(0, nrow(people))
  total_counts <- rep(0, nrow(people))

  for (i in seq_len(nrow(vote_map))) {
    q <- vote_map$Vote_Column[i]
    trait_col <- vote_map$Trait_Column[i]

    if (is.na(trait_col) || !(q %in% unique(vote_records$item_id)) || !(trait_col %in% names(people))) next

    trait_vals <- people[[trait_col]]
    if (all(is.na(trait_vals))) next

    max_val <- max(trait_vals, na.rm = TRUE)
    winners <- people$name[trait_vals == max_val]

    subset <- vote_records[vote_records$item_id == q, ]
    if (nrow(subset) == 0) next

    for (r in seq_len(nrow(subset))) {
      voter_idx <- which(people$name == subset$voter_name[r])
      if (length(voter_idx) == 0) next

      total_counts[voter_idx] <- total_counts[voter_idx] + 1
      if (subset$target_name[r] %in% winners) correct_counts[voter_idx] <- correct_counts[voter_idx] + 1
    }
  }

  accuracy <- ifelse(total_counts > 0, correct_counts / total_counts, NA)

  # ---- Eligibility ----
  # Survey Completion
  if (all(c("completion_time", "completion_duration_sec") %in% names(people))) {
    if (all(is.na(people$completion_time)) || all(is.na(people$completion_duration_sec))) {
      add_issue(data_path, "missing_values:completion_time_or_duration_all_na")
    } else {
    open_time <- min(people$completion_time, na.rm = TRUE)
    close_time <- max(people$completion_time, na.rm = TRUE)

    idx_first <- which.min(people$completion_time)
    idx_last <- which.max(people$completion_time)
    idx_fast <- which.min(people$completion_duration_sec)
    idx_slow <- which.max(people$completion_duration_sec)

    eligible <- add_eligible(eligible, people$name[idx_first], people$user_id[idx_first], "First!")
    eligible <- add_eligible(eligible, people$name[idx_last], people$user_id[idx_last], "Caboose")
    eligible <- add_eligible(eligible, people$name[idx_fast], people$user_id[idx_fast], "Speedrunner")
    eligible <- add_eligible(eligible, people$name[idx_slow], people$user_id[idx_slow], "The Thinker")

    keener <- which(people$completion_time <= (open_time + 24 * 3600))
    for (idx in keener) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Keener")

    last_minute <- which(people$completion_time >= (close_time - 24 * 3600))
    for (idx in last_minute) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Last-Minute Larry")

    hours <- as.numeric(format(people$completion_time, "%H"))
    for (idx in which(hours >= 4 & hours < 8)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Early Bird")
    for (idx in which((hours >= 23) | (hours < 4))) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Night Owl")
    for (idx in which(hours >= 9 & hours < 17)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Prime Time")
    for (idx in which(hours >= 11 & hours < 13)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Lunch Break")

    weekday <- as.POSIXlt(people$completion_time)$wday
    for (idx in which(weekday %in% c(0, 6))) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Weekend Warrior")

    for (idx in seq_len(nrow(people))) {
      eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Mystery Gift")
      eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Participation Trophy")
    }
    }
  }

  # Threes all the way down (requires neutral_response_ratio)
  if ("neutral_response_ratio" %in% names(people)) {
    people$neutral_response_ratio <- as.numeric(people$neutral_response_ratio)
    max_neutral <- max(people$neutral_response_ratio, na.rm = TRUE)
    for (idx in which(people$neutral_response_ratio == max_neutral)) {
      eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Threes all the way down")
    }
  }

  # Extreme Ends (requires extreme_response_ratio)
  if ("extreme_response_ratio" %in% names(people)) {
    people$extreme_response_ratio <- as.numeric(people$extreme_response_ratio)
    max_extreme <- max(people$extreme_response_ratio, na.rm = TRUE)
    for (idx in which(people$extreme_response_ratio == max_extreme)) {
      eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Extreme Ends")
    }
  }

  # Low Profile
  if (all(factor_cols %in% names(people))) {
    low_profile <- which(rowSums(people[, factor_cols]) < (40 * length(factor_cols)))
    for (idx in low_profile) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Low Profile")
  }

  # Trait Results
  if (all(factor_cols %in% names(people))) {
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_H)], people$user_id[which.max(people$HEX_H)], "Apex Sentinel")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_H)], people$user_id[which.min(people$HEX_H)], "Apex Rogue")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_E)], people$user_id[which.max(people$HEX_E)], "Apex Empath")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_E)], people$user_id[which.min(people$HEX_E)], "Apex Anchor")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_X)], people$user_id[which.max(people$HEX_X)], "Apex Connector")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_X)], people$user_id[which.min(people$HEX_X)], "Apex Observer")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_A)], people$user_id[which.max(people$HEX_A)], "Apex Peacemaker")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_A)], people$user_id[which.min(people$HEX_A)], "Apex Challenger")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_C)], people$user_id[which.max(people$HEX_C)], "Apex Planner")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_C)], people$user_id[which.min(people$HEX_C)], "Apex Improviser")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_O)], people$user_id[which.max(people$HEX_O)], "Apex Visionary")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_O)], people$user_id[which.min(people$HEX_O)], "Apex Realist")

    mid_dist <- apply(people[, factor_cols], 1, function(r) sum(abs(r - 50)))
    eligible <- add_eligible(eligible, people$name[which.max(mid_dist)], people$user_id[which.max(mid_dist)], "Full Send")

    milq <- apply(people[, factor_cols], 1, function(r) all(r >= 25 & r <= 75))
    for (idx in which(milq)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Default Settings")

    spec <- apply(people[, factor_cols], 1, function(r) any(r > 80 | r < 10))
    for (idx in which(spec)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "The Specialist")

    # Single-Stat Build removed from achievements.csv
  }

  # Trait combo: The Double Agent (high Honesty-Humility + high Machiavellianism)
  if (all(c("HEX_H", "DT_M") %in% names(people))) {
    for (idx in seq_len(nrow(people))) {
      if (is.na(people$HEX_H[idx]) || is.na(people$DT_M[idx])) next
      if (people$HEX_H[idx] >= 75 && people$DT_M[idx] >= 75) {
        eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "The Double Agent")
      }
    }
  }

  # Walking Contradiction (subtraits)
  if (length(subtrait_cols) > 0) {
    factor_facets <- list(
      H = c("HEX_H_S", "HEX_H_F", "HEX_H_G", "HEX_H_M"),
      E = c("HEX_E_F", "HEX_E_A", "HEX_E_D", "HEX_E_S"),
      X = c("HEX_X_SE", "HEX_X_SB", "HEX_X_S", "HEX_X_L"),
      A = c("HEX_A_FO", "HEX_A_G", "HEX_A_FL", "HEX_A_P"),
      C = c("HEX_C_O", "HEX_C_D", "HEX_C_PE", "HEX_C_PR"),
      O = c("HEX_O_A", "HEX_O_I", "HEX_O_C", "HEX_O_U")
    )

    for (i in seq_len(nrow(people))) {
      has_contra <- FALSE
      for (f in names(factor_facets)) {
        cols <- factor_facets[[f]]
        cols <- cols[cols %in% names(people)]
        if (length(cols) < 2) next
        vals <- as.numeric(people[i, cols])
        if (max(vals, na.rm = TRUE) > 75 && min(vals, na.rm = TRUE) < 25) {
          has_contra <- TRUE
          break
        }
      }
      if (has_contra) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Paradox Engine")
    }
  }

  # Dark Traits
  if (all(c("DT_N","DT_M","DT_P","DT_S") %in% names(people))) {
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_N)], people$user_id[which.max(people$DT_N)], "Apex Star")
    eligible <- add_eligible(eligible, people$name[which.min(people$DT_N)], people$user_id[which.min(people$DT_N)], "Apex Stewart")
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_M)], people$user_id[which.max(people$DT_M)], "Apex Strategist")
    eligible <- add_eligible(eligible, people$name[which.min(people$DT_M)], people$user_id[which.min(people$DT_M)], "Apex Straight-Shooter")
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_P)], people$user_id[which.max(people$DT_P)], "Apex Risk-Taker")
    eligible <- add_eligible(eligible, people$name[which.min(people$DT_P)], people$user_id[which.min(people$DT_P)], "Apex Regulator")
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_S)], people$user_id[which.max(people$DT_S)], "Apex Provocateur")
    eligible <- add_eligible(eligible, people$name[which.min(people$DT_S)], people$user_id[which.min(people$DT_S)], "Apex Guardian")
  }

  if ("DT" %in% names(people)) {
    eligible <- add_eligible(eligible, people$name[which.max(people$DT)], people$user_id[which.max(people$DT)], "The Devil You Know")
  }

  # Similarity
  if (!is.null(sim_mat)) {
    avg_sim <- apply(sim_mat, 1, function(r) mean(r[!is.na(r) & r < 1]))

    idx_max <- which.max(avg_sim)
    idx_min <- which.min(avg_sim)
    idx_zero <- which.min(abs(avg_sim - 0))

    eligible <- add_eligible(eligible, names(avg_sim)[idx_max], people$user_id[idx_max], "Nucleus")
    eligible <- add_eligible(eligible, names(avg_sim)[idx_min], people$user_id[idx_min], "Black Sheep")
    eligible <- add_eligible(eligible, names(avg_sim)[idx_zero], people$user_id[idx_zero], "Ugly Duckling")

    sim_vals <- sim_mat
    sim_vals[upper.tri(sim_vals, diag = TRUE)] <- NA
    min_idx <- which(sim_vals == min(sim_vals, na.rm = TRUE), arr.ind = TRUE)[1, ]
    max_idx <- which(sim_vals == max(sim_vals, na.rm = TRUE), arr.ind = TRUE)[1, ]

    eligible <- add_eligible(
      eligible,
      rownames(sim_mat)[min_idx[1]],
      people$user_id[min_idx[1]],
      "Nemesis",
      shared_person_name = colnames(sim_mat)[min_idx[2]],
      shared_person_id = people$user_id[min_idx[2]]
    )
    eligible <- add_eligible(
      eligible,
      colnames(sim_mat)[min_idx[2]],
      people$user_id[min_idx[2]],
      "Nemesis",
      shared_person_name = rownames(sim_mat)[min_idx[1]],
      shared_person_id = people$user_id[min_idx[1]]
    )
    eligible <- add_eligible(
      eligible,
      rownames(sim_mat)[max_idx[1]],
      people$user_id[max_idx[1]],
      "Soulmate",
      shared_person_name = colnames(sim_mat)[max_idx[2]],
      shared_person_id = people$user_id[max_idx[2]]
    )
    eligible <- add_eligible(
      eligible,
      colnames(sim_mat)[max_idx[2]],
      people$user_id[max_idx[2]],
      "Soulmate",
      shared_person_name = rownames(sim_mat)[max_idx[1]],
      shared_person_id = people$user_id[max_idx[1]]
    )

    best_match <- sapply(seq_len(nrow(sim_mat)), function(i) {
      sims <- sim_mat[i, ]
      sims[i] <- -Inf
      which.max(sims)
    })

    inbound <- table(best_match)

    for (i in seq_len(nrow(sim_mat))) {
      cnt <- ifelse(as.character(i) %in% names(inbound), inbound[[as.character(i)]], 0)
      if (cnt == 0) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Enigma")
      if (cnt == 1) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Twins!")
      if (cnt == 2) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Triplets!")
      if (cnt >= 3) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Attack of the clones")
    }

    max_inbound <- max(inbound)
    for (idx in as.integer(names(inbound)[inbound == max_inbound])) {
      eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Connector")
    }

    for (i in seq_len(nrow(sim_mat))) {
      a <- i
      b <- best_match[i]
      if (best_match[b] == a) {
        eligible <- add_eligible(
          eligible,
          people$name[a],
          people$user_id[a],
          "Besties",
          shared_person_name = people$name[b],
          shared_person_id = people$user_id[b]
        )
      }
    }

    mutual_pairs <- list()
    for (i in seq_len(nrow(sim_mat))) {
      a <- i
      b <- best_match[i]
      if (best_match[b] == a) mutual_pairs[[length(mutual_pairs) + 1]] <- c(a, b)
    }

    for (i in seq_len(nrow(sim_mat))) {
      b <- best_match[i]
      is_third <- FALSE
      pair_names <- c()
      for (pair in mutual_pairs) {
        if (b %in% pair && !(i %in% pair)) {
          is_third <- TRUE
          pair_names <- people$name[pair]
          break
        }
      }
      if (is_third) {
        eligible <- add_eligible(
          eligible,
          people$name[i],
          people$user_id[i],
          "Third Wheel",
          shared_person_name = paste(pair_names, collapse = ", "),
          shared_person_id = paste(people$user_id[match(pair_names, people$name)], collapse = ", ")
        )
      }
    }
  }

  # ---- Voting Results ----
  if (nrow(vote_records) > 0) {
    # Vote-Magnet
    if (length(vote_counts_total) > 0) {
      max_total <- max(vote_counts_total)
      for (name in names(vote_counts_total)[vote_counts_total == max_total]) {
        idx <- which(people$name == name)
        if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Vote-Magnet")
      }
    }

    # Mr. Popular / Bridesmaid / Bronze / I win / Better than nothing
    max_r1 <- max(rank_tally$rank1)
    if (max_r1 > 0) {
      for (name in rank_tally$target_name[rank_tally$rank1 == max_r1]) {
        idx <- which(people$name == name)
        if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Mr. Popular")
      }
    }

    max_r2 <- max(rank_tally$rank2)
    if (max_r2 > 0) {
      for (name in rank_tally$target_name[rank_tally$rank2 == max_r2]) {
        idx <- which(people$name == name)
        if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Always the bridesmaid")
      }
    }

    max_r3 <- max(rank_tally$rank3)
    if (max_r3 > 0) {
      for (name in rank_tally$target_name[rank_tally$rank3 == max_r3]) {
        idx <- which(people$name == name)
        if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Bronze is the New Gold")
      }
    }

    for (name in rank_tally$target_name[rank_tally$rank1 > 0 & rank_tally$rank2 > 0 & rank_tally$rank3 > 0]) {
      idx <- which(people$name == name)
      if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Completionist")
    }

    for (name in rank_tally$target_name[rank_tally$rank1 > 0]) {
      idx <- which(people$name == name)
      if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "I win")
    }

    for (name in rank_tally$target_name[rank_tally$rank4 > 0]) {
      idx <- which(people$name == name)
      if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Consolation Prize")
    }

    # Cut it in half (tied for first place)
    for (idx in which(first_place_ties)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Photo Finish")

    # Kingmaker / Contrarian / Bullseye / Stormtrooper
    if (any(total_counts > 0)) {
      max_correct <- max(correct_counts)
      min_correct <- min(correct_counts[total_counts > 0])
      for (idx in which(correct_counts == max_correct & total_counts > 0)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Kingmaker")
      for (idx in which(correct_counts == min_correct & total_counts > 0)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Contrarian")

      max_acc <- max(accuracy, na.rm = TRUE)
      min_acc <- min(accuracy, na.rm = TRUE)
      for (idx in which(accuracy == max_acc)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Bullseye")
      for (idx in which(accuracy == min_acc)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Stormtrooper")
    }

    # You get a car, and you get a car! (voted for everyone)
    cohort_size <- nrow(people)
    for (i in seq_len(nrow(people))) {
      voted_targets <- unique(vote_records$target_name[vote_records$voter_name == people$name[i]])
      voted_targets <- voted_targets[voted_targets != people$name[i]]
      if (length(voted_targets) >= (cohort_size - 1)) {
        eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "You get a car, and you get a car!")
      }
    }

    # Plot Armor (most unique voters)
    if (length(unique_voters_per_target) > 0) {
      max_unique <- max(unique_voters_per_target)
      for (name in names(unique_voters_per_target)[unique_voters_per_target == max_unique]) {
        idx <- which(people$name == name)
        if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Plot Armor")
      }
    }

    # Jinx (two-way voting)
    edges <- unique(vote_records[, c("voter_name", "target_name")])
    edge_set <- paste(edges$voter_name, edges$target_name, sep = "->")

    jinx_pairs <- rep(FALSE, nrow(people))
    jinx_partners <- vector("list", nrow(people))
    for (i in seq_len(nrow(people))) {
      for (j in seq_len(nrow(people))) {
        if (i == j) next
        a <- people$name[i]
        b <- people$name[j]
        if (paste(a, b, sep = "->") %in% edge_set && paste(b, a, sep = "->") %in% edge_set) {
          jinx_pairs[i] <- TRUE
          jinx_pairs[j] <- TRUE
          jinx_partners[[i]] <- unique(c(jinx_partners[[i]], b))
        }
      }
    }
    for (idx in which(jinx_pairs)) {
      partners <- jinx_partners[[idx]]
      eligible <- add_eligible(
        eligible,
        people$name[idx],
        people$user_id[idx],
        "Jinx",
        shared_person_name = paste(partners, collapse = ", "),
        shared_person_id = paste(people$user_id[match(partners, people$name)], collapse = ", ")
      )
    }

    # Double Jinx (three-way voting cycle)
    double_jinx <- rep(FALSE, nrow(people))
    double_jinx_partners <- vector("list", nrow(people))
    for (a in seq_len(nrow(people))) {
      for (b in seq_len(nrow(people))) {
        if (a == b) next
        for (c in seq_len(nrow(people))) {
          if (c == a || c == b) next
          va <- people$name[a]
          vb <- people$name[b]
          vc <- people$name[c]
          if (paste(va, vb, sep = "->") %in% edge_set && paste(vb, vc, sep = "->") %in% edge_set && paste(vc, va, sep = "->") %in% edge_set) {
            double_jinx[a] <- TRUE
            double_jinx[b] <- TRUE
            double_jinx[c] <- TRUE
            double_jinx_partners[[a]] <- unique(c(double_jinx_partners[[a]], vb, vc))
            double_jinx_partners[[b]] <- unique(c(double_jinx_partners[[b]], va, vc))
            double_jinx_partners[[c]] <- unique(c(double_jinx_partners[[c]], va, vb))
          }
        }
      }
    }
    for (idx in which(double_jinx)) {
      partners <- double_jinx_partners[[idx]]
      eligible <- add_eligible(
        eligible,
        people$name[idx],
        people$user_id[idx],
        "Double Jinx",
        shared_person_name = paste(partners, collapse = ", "),
        shared_person_id = paste(people$user_id[match(partners, people$name)], collapse = ", ")
      )
    }

    # Ride-or-Die
    for (i in seq_len(nrow(people))) {
      targets <- vote_records$target_name[vote_records$voter_name == people$name[i]]
      if (length(targets) == 0) next
      max_same <- max(table(targets))
      if (max_same > 6) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Ride-or-Die")
    }

    # Inclusive (voted for someone with exactly one vote total)
    if (length(vote_counts_total) > 0) {
      rare_targets <- names(vote_counts_total)[vote_counts_total == 1]
      for (i in seq_len(nrow(people))) {
        targets <- unique(vote_records$target_name[vote_records$voter_name == people$name[i]])
        if (any(targets %in% rare_targets)) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Inclusive")
      }
    }
  }

  # ---- Backup Achievements ----
  if (all(factor_cols %in% names(people))) {
    remarkable <- apply(people[, factor_cols], 1, function(r) all(r >= 40 & r <= 60))
    for (idx in which(remarkable)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Remarkably Unremarkable")

    is_extreme <- rep(FALSE, nrow(people))
    for (col in factor_cols) {
      max_val <- max(people[[col]], na.rm = TRUE)
      min_val <- min(people[[col]], na.rm = TRUE)
      is_extreme <- is_extreme | (people[[col]] == max_val) | (people[[col]] == min_val)
    }
    for (idx in which(!is_extreme)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Middle Child")
  }

  # ---- All Eligible Output ----
  eligible <- unique(eligible)

  eligible <- merge(
    eligible,
    achievements[, c(
      "id",
      "Title",
      "Image",
      "color_hex",
      "Description",
      "expected_rarity",
      "desirability_rank",
      "exclusive_group",
      "type",
      "subtype",
      "framing",
      "shared_with",
      "mirror_award",
      "shared_role"
    )],
    by.x = c("achievement_id", "Title"),
    by.y = c("id", "Title"),
    all.x = TRUE
  )

  if ("mirror_award" %in% names(eligible)) {
    eligible$mirror_award <- tolower(eligible$mirror_award) == "true"
  }
  if ("shared_with" %in% names(eligible)) {
    eligible$shared_with <- tolower(eligible$shared_with) == "true"
  }

  render_description <- function(desc, person, shared) {
    desc <- ifelse(is.na(desc), "", desc)
    shared <- ifelse(is.na(shared), "", shared)
    desc <- gsub("{person_name}", person, desc, fixed = TRUE)
    gsub("{shared_person_name}", shared, desc, fixed = TRUE)
  }
  eligible$Description <- mapply(
    render_description,
    eligible$Description,
    eligible$person_name,
    eligible$shared_person_name,
    USE.NAMES = FALSE
  )

  # ---- Allocation ----
  # Rules:
  # - Primary types: Voting Results, Similarity, Trait Results (diversity-rotated)
  # - Secondary: Survey Completion
  # - Tertiary: Backup
  # - Seed with top-ranked Voting Results and Similarity (shared allowed)
  # - Seed achievements do not count toward type representation
  # - Maintain positive:negative >= 2:1 with max 3 negatives (unless sentiment relaxed)
  # - Waterfall: relax sentiment, relax diversity, allow secondary, relax exclusivity, allow tertiary
  select_awards <- function(df) {
    df$desirability_rank <- suppressWarnings(as.integer(df$desirability_rank))
    df <- df[order(df$desirability_rank), ]
    if ("shared_with" %in% names(df)) {
      df$shared_with <- tolower(df$shared_with) == "true"
    }
    df$framing[is.na(df$framing) | df$framing == ""] <- "Positive"

    primary_types <- c("Voting Results", "Similarity", "Trait Results")
    secondary_types <- c("Survey Completion")
    tertiary_types <- c("Backup")
    rep_counts <- setNames(rep(0, length(primary_types)), primary_types)
    max_negatives <- 3

    picked <- df[0, ]
    used_groups <- c()
    pos_count <- 0
    neg_count <- 0
    stage <- character()

    can_pick <- function(row, relax_sentiment = FALSE, relax_exclusive = FALSE) {
      if (!relax_exclusive && !is.na(row$exclusive_group) && row$exclusive_group != "" && row$exclusive_group %in% used_groups) return(FALSE)
      if (nrow(picked) > 0 && row$Title %in% picked$Title) return(FALSE)
      if (row$framing == "Negative") {
        if (neg_count >= max_negatives) return(FALSE)
        if (!relax_sentiment && pos_count < 2 * (neg_count + 1)) return(FALSE)
      }
      TRUE
    }

    add_pick <- function(row, is_seed = FALSE, label = "primary") {
      picked <<- rbind(picked, row)
      stage <<- c(stage, label)
      if (row$framing == "Negative") {
        neg_count <<- neg_count + 1
      } else {
        pos_count <<- pos_count + 1
      }
      if (!is.na(row$exclusive_group) && row$exclusive_group != "") {
        used_groups <<- unique(c(used_groups, row$exclusive_group))
      }
      if (!is_seed && row$type %in% names(rep_counts)) {
        rep_counts[[row$type]] <<- rep_counts[[row$type]] + 1
      }
    }

    # Seeds
    for (t in c("Voting Results", "Similarity")) {
      candidates <- df[df$type == t, ]
      for (i in seq_len(nrow(candidates))) {
        row <- candidates[i, ]
        if (!can_pick(row, relax_sentiment = FALSE, relax_exclusive = FALSE)) next
        add_pick(row, is_seed = TRUE, label = "seed")
        break
      }
    }

    fill_once <- function(relax_sentiment = FALSE, relax_diversity = FALSE, allow_secondary = FALSE, allow_tertiary = FALSE, relax_exclusive = FALSE, label = "primary") {
      types_allowed <- primary_types
      if (allow_secondary) types_allowed <- c(types_allowed, secondary_types)
      if (allow_tertiary) types_allowed <- c(types_allowed, tertiary_types)

      pool <- df[df$type %in% types_allowed, ]
      if (nrow(pool) == 0) return(FALSE)

      pool <- pool[sapply(seq_len(nrow(pool)), function(i) can_pick(pool[i, ], relax_sentiment, relax_exclusive)), , drop = FALSE]
      if (nrow(pool) == 0) return(FALSE)

      if (!relax_diversity) {
        primary_pool <- pool[pool$type %in% primary_types, , drop = FALSE]
        if (nrow(primary_pool) > 0) {
          types_with_candidates <- unique(primary_pool$type)
          min_count <- min(rep_counts[types_with_candidates])
          candidate_types <- names(rep_counts)[rep_counts == min_count & names(rep_counts) %in% types_with_candidates]
          cands <- primary_pool[primary_pool$type %in% candidate_types, , drop = FALSE]
          row <- cands[order(cands$desirability_rank)[1], ]
        } else {
          row <- pool[order(pool$desirability_rank)[1], ]
        }
      } else {
        row <- pool[order(pool$desirability_rank)[1], ]
      }

      add_pick(row, is_seed = FALSE, label = label)
      TRUE
    }

    while (fill_once(FALSE, FALSE, FALSE, FALSE, FALSE, "primary")) {}
    while (fill_once(TRUE, FALSE, FALSE, FALSE, FALSE, "relax_sentiment")) {}
    while (fill_once(TRUE, TRUE, FALSE, FALSE, FALSE, "relax_diversity")) {}
    while (fill_once(TRUE, TRUE, TRUE, FALSE, FALSE, "secondary")) {}
    while (fill_once(TRUE, TRUE, TRUE, FALSE, TRUE, "relax_exclusive")) {}
    while (fill_once(TRUE, TRUE, TRUE, TRUE, TRUE, "tertiary")) {}

    picked$selection_stage <- stage
    picked$selection_order <- seq_len(nrow(picked))
    picked
  }

  apply_mirrors <- function(final, eligible, max_awards) {
    if (!("mirrored" %in% names(final))) final$mirrored <- FALSE
    align_cols <- function(row, template) {
      missing <- setdiff(names(template), names(row))
      for (m in missing) row[[m]] <- NA
      row <- row[, names(template), drop = FALSE]
      row
    }
    changed <- TRUE
    while (changed) {
      changed <- FALSE
      for (i in seq_len(nrow(final))) {
        row <- final[i, ]
        if (!isTRUE(row$mirror_award)) next
        if (is.na(row$shared_person_name) || row$shared_person_name == "") next

        targets <- trimws(unlist(strsplit(row$shared_person_name, ",")))
        targets <- targets[targets != ""]
        for (t in targets) {
          if (t == row$person_name) next
          if (any(final$person_name == t & final$Title == row$Title)) next

          cand <- eligible[eligible$person_name == t & eligible$Title == row$Title, ]
          if (nrow(cand) == 0) next

          add <- cand[1, ]
          add$mirrored <- TRUE
          add$shared_person_name <- row$person_name
          add$shared_person_id <- row$user_id
          add <- align_cols(add, final)
          final <- rbind(final, add)
          changed <- TRUE
        }
      }
    }

    final_out <- data.frame()
    for (person in unique(final$person_name)) {
      rows <- final[final$person_name == person, ]
      if (nrow(rows) > max_awards) {
        if (sum(rows$mirrored) >= max_awards) {
          # Keep all mirrored awards even if they exceed quota.
          rows <- rows
        } else {
          ranks <- suppressWarnings(as.integer(rows$desirability_rank))
          ranks[is.na(ranks)] <- 999
          drop_order <- order(rows$mirrored, -ranks)
          keep <- rep(TRUE, nrow(rows))
          for (idx in drop_order) {
            if (sum(keep) <= max_awards) break
            keep[idx] <- FALSE
          }
          rows <- rows[keep, ]
        }
      }
      final_out <- rbind(final_out, rows)
    }
    final_out
  }

  ordered_eligible <- do.call(
    rbind,
    lapply(unique(eligible$person_name), function(person) {
      dfp <- eligible[eligible$person_name == person, ]
      picked <- select_awards(dfp)
      remaining <- dfp[!(dfp$Title %in% picked$Title), , drop = FALSE]
      remaining$desirability_rank <- suppressWarnings(as.integer(remaining$desirability_rank))
      remaining <- remaining[order(remaining$desirability_rank), ]
      if (nrow(remaining) > 0) {
        remaining$selection_stage <- "unpicked"
        remaining$selection_order <- seq_len(nrow(remaining)) + nrow(picked)
        picked <- rbind(picked, remaining)
      }
      picked
    })
  )

  ordered_eligible <- ordered_eligible[order(ordered_eligible$person_name, ordered_eligible$selection_order), ]
  all_eligible_path <- file.path(output_dir, "all_eligible_achievements.csv")
  write.csv(ordered_eligible, all_eligible_path, row.names = FALSE)

  final <- ordered_eligible[ordered_eligible$selection_order <= max_awards, , drop = FALSE]
  for (person in unique(ordered_eligible$person_name)) {
    count <- sum(final$person_name == person)
    if (count < max_awards) {
      msg <- paste("quota short:", person, "-", count, "of", max_awards)
      summary_warnings <<- c(summary_warnings, msg)
      warning(paste("Could not fill quota for", person, "- allocated", count, "of", max_awards))
    }
  }

  final <- apply_mirrors(final, eligible, max_awards)

  final$Description <- mapply(
    render_description,
    final$Description,
    final$person_name,
    final$shared_person_name,
    USE.NAMES = FALSE
  )

  build_image_file <- function(img) {
    img <- ifelse(is.na(img), "", img)
    img <- trimws(img)
    if (img == "") return("")
    if (grepl("\\\\.(svg|png|jpg|jpeg)$", img, ignore.case = TRUE)) return(img)
    paste0(img, ".svg")
  }

  awarded_json <- lapply(split(final, final$person_name), function(df) {
    list(
      person_name = df$person_name[1],
      person_id = df$user_id[1],
      achievements = lapply(seq_len(nrow(df)), function(i) {
        list(
          achievement_id = df$achievement_id[i],
          title = df$Title[i],
          description = df$Description[i],
          image = df$Image[i],
          image_file = build_image_file(df$Image[i]),
          color_hex = if ("color_hex" %in% names(df)) df$color_hex[i] else "",
          type = df$type[i],
          subtype = df$subtype[i],
          framing = df$framing[i],
          exclusive_group = df$exclusive_group[i],
          shared_person_name = df$shared_person_name[i],
          shared_person_id = df$shared_person_id[i],
          desirability_rank = df$desirability_rank[i]
        )
      })
    )
  })

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Missing package: jsonlite. Please install it to write awarded achievements JSON.")
  }

  awarded_path <- file.path(output_dir, "awarded_achievements.json")
  jsonlite::write_json(awarded_json, awarded_path, pretty = TRUE, auto_unbox = TRUE)

  # HTML generation removed (assets are now passed forward for external rendering).

  # ---- QA Summaries ----
  if (isTRUE(write_qa)) {
    qa_counts <- aggregate(
      achievement_id ~ person_name,
      data = eligible,
      FUN = length
    )
    names(qa_counts)[2] <- "eligible_total"

    qa_type <- as.data.frame(table(eligible$person_name, eligible$type), stringsAsFactors = FALSE)
    names(qa_type) <- c("person_name", "type", "eligible_count")

    eligible_type_w <- reshape(qa_type, idvar = "person_name", timevar = "type", direction = "wide")
    names(eligible_type_w) <- gsub("^eligible_count\\.", "eligible_type.", names(eligible_type_w))

    stage_counts <- as.data.frame(table(ordered_eligible$person_name, ordered_eligible$selection_stage), stringsAsFactors = FALSE)
    names(stage_counts) <- c("person_name", "selection_stage", "stage_count")
    stage_w <- reshape(stage_counts, idvar = "person_name", timevar = "selection_stage", direction = "wide")
    names(stage_w) <- gsub("^stage_count\\.", "eligible_stage.", names(stage_w))

    # Ensure all expected stage columns exist
    expected_stages <- c("seed", "primary", "relax_sentiment", "relax_diversity", "secondary", "relax_exclusive", "tertiary")
    for (st in expected_stages) {
      col <- paste0("eligible_stage.", st)
      if (!(col %in% names(stage_w))) stage_w[[col]] <- 0
    }

    stage_w$possible_before_relaxation <- stage_w$eligible_stage.seed + stage_w$eligible_stage.primary
    stage_w$additional_before_relax_sentiment <- stage_w$eligible_stage.relax_sentiment
    stage_w$additional_before_relax_diversity <- stage_w$eligible_stage.relax_diversity
    stage_w$additional_before_relax_exclusivity <- stage_w$eligible_stage.secondary
    stage_w$additional_after_relax_exclusivity <- stage_w$eligible_stage.relax_exclusive
    stage_w$additional_tertiary <- stage_w$eligible_stage.tertiary

    qa_dashboard <- Reduce(function(x, y) merge(x, y, by = "person_name", all = TRUE), list(
      qa_counts,
      eligible_type_w,
      stage_w
    ))
    primary_cols <- c(
      "person_name",
      "eligible_total",
      "possible_before_relaxation",
      "additional_before_relax_sentiment",
      "additional_before_relax_diversity",
      "additional_before_relax_exclusivity",
      "additional_after_relax_exclusivity",
      "additional_tertiary"
    )
    primary_cols <- primary_cols[primary_cols %in% names(qa_dashboard)]
    eligible_cols <- grep("^eligible_", names(qa_dashboard), value = TRUE)
    other_cols <- setdiff(names(qa_dashboard), c(primary_cols, eligible_cols))
    qa_dashboard <- qa_dashboard[, c(primary_cols, other_cols, eligible_cols), drop = FALSE]
    write.csv(qa_dashboard, file.path(output_dir, "qa_dashboard.csv"), row.names = FALSE)
  }

  if (length(missing_awards) > 0) {
    msg <- paste("Skipped awards not found in achievements.csv:", paste(sort(unique(missing_awards)), collapse = ", "))
    summary_warnings <<- c(summary_warnings, msg)
    warning(msg)
  }

  write_issue_log()

  # ---- Summary Output ----
  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  quota_max_data <- NA_integer_
  quota_ideal_possible <- NA_integer_
  if (nrow(ordered_eligible) > 0) {
    totals <- tapply(ordered_eligible$selection_order, ordered_eligible$person_name, max, na.rm = TRUE)
    quota_max_data <- as.integer(min(totals))
    base_totals <- tapply(ordered_eligible$selection_stage %in% c("seed", "primary"), ordered_eligible$person_name, sum, na.rm = TRUE)
    quota_ideal_possible <- as.integer(min(base_totals))
  }

  stage_counts <- as.data.frame(table(final$person_name, final$selection_stage), stringsAsFactors = FALSE)
  names(stage_counts) <- c("person_name", "selection_stage", "count")
  stage_w <- reshape(stage_counts, idvar = "person_name", timevar = "selection_stage", direction = "wide")
  names(stage_w) <- gsub("^count\\.", "", names(stage_w))
  stage_w[is.na(stage_w)] <- 0
  if (!("seed" %in% names(stage_w))) stage_w$seed <- 0
  if (!("primary" %in% names(stage_w))) stage_w$primary <- 0
  if (!("relax_sentiment" %in% names(stage_w))) stage_w$relax_sentiment <- 0
  if (!("relax_diversity" %in% names(stage_w))) stage_w$relax_diversity <- 0
  if (!("secondary" %in% names(stage_w))) stage_w$secondary <- 0
  if (!("relax_exclusive" %in% names(stage_w))) stage_w$relax_exclusive <- 0
  if (!("tertiary" %in% names(stage_w))) stage_w$tertiary <- 0

  stage_w$generated <- stage_w$seed + stage_w$primary + stage_w$relax_sentiment +
    stage_w$relax_diversity + stage_w$secondary + stage_w$relax_exclusive + stage_w$tertiary
  stage_w$base <- stage_w$seed + stage_w$primary
  stage_w$sent <- stage_w$relax_sentiment
  stage_w$div <- stage_w$relax_diversity
  stage_w$excl <- stage_w$secondary + stage_w$relax_exclusive
  stage_w$ter <- stage_w$tertiary

  agg_ideal <- sum(stage_w$base, na.rm = TRUE)
  agg_sent <- sum(stage_w$sent, na.rm = TRUE)
  agg_div <- sum(stage_w$div, na.rm = TRUE)
  agg_excl <- sum(stage_w$excl, na.rm = TRUE)
  agg_ter <- sum(stage_w$ter, na.rm = TRUE)

  cat("ACHIEVEMENTS SUMMARY\n")
  cat(sprintf("Run duration: %.2fs\n", duration))
  cat(sprintf("Quota target: %d\n", max_awards))
  if (!is.na(quota_ideal_possible)) cat(sprintf("Quota ideal possible: %d\n", quota_ideal_possible))
  if (!is.na(quota_max_data)) cat(sprintf("Quota maximum possible: %d\n", quota_max_data))
  if (length(summary_warnings) > 0) {
    cat(sprintf("Warnings: %d (%s)\n", length(summary_warnings), paste(summary_warnings, collapse = "; ")))
  } else {
    cat("Warnings: 0\n")
  }
  cat("\nAggregate\n")
  cat(sprintf("- Ideal achievements used: %d\n", agg_ideal))
  cat(sprintf("- Relax sentiment used: %d\n", agg_sent))
  cat(sprintf("- Relax diversity used: %d\n", agg_div))
  cat(sprintf("- Relax exclusivity used: %d\n", agg_excl))
  cat(sprintf("- Tertiary used: %d\n", agg_ter))
  cat("\nPer-person (generated | base | +sent | +div | +excl | +tertiary)\n")
  stage_w <- stage_w[order(stage_w$person_name), ]
  for (i in seq_len(nrow(stage_w))) {
    cat(sprintf(
      "%s  %d | %d | %d | %d | %d | %d\n",
      stage_w$person_name[i],
      stage_w$generated[i],
      stage_w$base[i],
      stage_w$sent[i],
      stage_w$div[i],
      stage_w$excl[i],
      stage_w$ter[i]
    ))
  }

  #list(all_eligible = eligible, awarded = final)
}
