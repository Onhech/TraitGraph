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
#' @param max_awards Integer count of achievements to allocate per person. Defaults to 3.
#'
#' @return A list with `all_eligible` and `awarded` data frames.
#' @export
#'
#' @examples
#' \dontrun{
#' TG_achievements(
#'   trait_map_path = "achievement_inputs/trait_map.csv",
#'   vote_map_path = "achievement_inputs/vote_map.csv",
#'   data_path = "achievement_inputs/group_dataset.csv",
#'   achievements_path = "achievement_inputs/achievements.csv",
#'   voting_data_path = "achievement_inputs/voting_data.csv",
#'   output_dir = "achievements",
#'   max_awards = 3
#' )
#' }
TG_achievements <- function(
  trait_map_path,
  vote_map_path,
  data_path,
  achievements_path,
  voting_data_path,
  output_dir = "achievements",
  max_awards = 3
) {
  # ---- Inputs ----
  achievements <- read.csv(achievements_path, stringsAsFactors = FALSE)
  people <- read.csv(data_path, stringsAsFactors = FALSE)
  trait_map <- read.csv(trait_map_path, stringsAsFactors = FALSE)
  vote_map <- read.csv(vote_map_path, stringsAsFactors = FALSE)
  voting_data <- read.csv(voting_data_path, stringsAsFactors = FALSE)

  # ---- Output Folder ----
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # ---- Validation ----
  required_ach_cols <- c("id", "Title", "desirability_rank", "exclusive_group")
  missing_ach_cols <- setdiff(required_ach_cols, names(achievements))
  if (length(missing_ach_cols) > 0) {
    stop(paste("Missing achievement columns:", paste(missing_ach_cols, collapse = ", ")))
  }

  required_people_cols <- c("name", "user_id", "start_time", "completion_time", "total_duration_sec")
  missing_people_cols <- setdiff(required_people_cols, names(people))
  if (length(missing_people_cols) > 0) {
    stop(paste("Missing participant columns:", paste(missing_people_cols, collapse = ", ")))
  }

  # ---- Helpers ----
  achievements$id <- as.character(achievements$id)
  ach_map <- setNames(achievements$id, achievements$Title)

  add_eligible <- function(df, person_name, user_id, title) {
    df[nrow(df) + 1, ] <- list(
      person_name = person_name,
      user_id = user_id,
      achievement_id = ach_map[[title]],
      Title = title
    )
    df
  }

  # Track all achievements that should be handled
  handled_titles <- c(
    "Personality Quintuplets","Soulmate","The Epicenter","Connector","Devil","Nemesis",
    "Personality Quadruplets","Angel","Besties","Black Sheep","Bullseye","Chessmaster",
    "Explorer","First!","Full Send","Ice Cube","Mr. Popular","One of One","Sparkplug",
    "The Main Character","The Specialist","Troll Mode","Vote-Magnet","Walking Contradiction",
    "Always the bridesmaid","Bronze is the New Gold","Cut it in half","Diplomat","Double Jinx",
    "Firestarter","Low Profile","Personality Triplets","Plot Armor","Right Down the Middle",
    "Rock","Speedrunner","The Planner","Contrarian","Inclusive","Kingmaker",
    "Last-Minute Larry","Ride-or-Die","You get a car!","Caboose","Default Settings",
    "Milquetoast","Stormtrooper","The Thinker","Early Bird","I win","Jinx","Keener",
    "Night Owl","Third Wheel","Threes all the way down","Weekend Warrior","Mystery Gift",
    "Better than nothing","Lunch Break","Middle Child","Participation Trophy","Prime Time",
    "Remarkably Unremarkable"
  )

  missing_titles <- setdiff(achievements$Title, handled_titles)
  extra_titles <- setdiff(handled_titles, achievements$Title)
  if (length(missing_titles) > 0) {
    stop(paste("Achievement titles missing from handled list:", paste(missing_titles, collapse = ", ")))
  }
  if (length(extra_titles) > 0) {
    stop(paste("Handled titles not found in achievements.csv:", paste(extra_titles, collapse = ", ")))
  }

  eligible <- data.frame(
    person_name = character(),
    user_id = character(),
    achievement_id = character(),
    Title = character(),
    stringsAsFactors = FALSE
  )

  # ---- Column Maps ----
  trait_columns <- trait_map$Column
  trait_columns <- trait_columns[trait_columns != "" & trait_columns != "NA"]

  factor_cols <- c("HEX_H","HEX_E","HEX_X","HEX_A","HEX_C","HEX_O")
  subtrait_cols <- trait_map$Column[trait_map$Type == "subtrait"]
  subtrait_cols <- subtrait_cols[subtrait_cols %in% names(people)]

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
      if (t == voter) next # no self-votes

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

  # Threes all the way down (requires neutral_response_ratio)
  if ("neutral_response_ratio" %in% names(people)) {
    people$neutral_response_ratio <- as.numeric(people$neutral_response_ratio)
    max_neutral <- max(people$neutral_response_ratio, na.rm = TRUE)
    for (idx in which(people$neutral_response_ratio == max_neutral)) {
      eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Threes all the way down")
    }
  }

  # Low Profile
  if (all(factor_cols %in% names(people))) {
    low_profile <- which(rowSums(people[, factor_cols]) < (40 * length(factor_cols)))
    for (idx in low_profile) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Low Profile")
  }

  # Trait Results
  if (all(factor_cols %in% names(people))) {
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_H)], people$user_id[which.max(people$HEX_H)], "Angel")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_E)], people$user_id[which.min(people$HEX_E)], "Rock")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_X)], people$user_id[which.max(people$HEX_X)], "Sparkplug")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_A)], people$user_id[which.max(people$HEX_A)], "Diplomat")
    eligible <- add_eligible(eligible, people$name[which.min(people$HEX_A)], people$user_id[which.min(people$HEX_A)], "Firestarter")
    eligible <- add_eligible(eligible, people$name[which.max(people$HEX_O)], people$user_id[which.max(people$HEX_O)], "Explorer")

    high_c <- which(people$HEX_C >= 75)
    for (idx in high_c) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "The Planner")

    mid_dist <- apply(people[, factor_cols], 1, function(r) sum(abs(r - 50)))
    eligible <- add_eligible(eligible, people$name[which.min(mid_dist)], people$user_id[which.min(mid_dist)], "Right Down the Middle")
    eligible <- add_eligible(eligible, people$name[which.max(mid_dist)], people$user_id[which.max(mid_dist)], "Full Send")

    milq <- apply(people[, factor_cols], 1, function(r) all(r >= 25 & r <= 75))
    for (idx in which(milq)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Milquetoast")

    spec <- apply(people[, factor_cols], 1, function(r) any(r > 80 | r < 10))
    for (idx in which(spec)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "The Specialist")
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
      if (has_contra) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Walking Contradiction")
    }
  }

  # Dark Traits
  if (all(c("DT_N","DT_M","DT_P","DT_S") %in% names(people))) {
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_N)], people$user_id[which.max(people$DT_N)], "The Main Character")
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_M)], people$user_id[which.max(people$DT_M)], "Chessmaster")
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_P)], people$user_id[which.max(people$DT_P)], "Ice Cube")
    eligible <- add_eligible(eligible, people$name[which.max(people$DT_S)], people$user_id[which.max(people$DT_S)], "Troll Mode")
  }

  if ("DT" %in% names(people)) {
    eligible <- add_eligible(eligible, people$name[which.max(people$DT)], people$user_id[which.max(people$DT)], "Devil")
  }

  # Similarity
  if (!is.null(sim_mat)) {
    avg_sim <- apply(sim_mat, 1, function(r) mean(r[!is.na(r) & r < 1]))

    idx_max <- which.max(avg_sim)
    idx_min <- which.min(avg_sim)
    idx_zero <- which.min(abs(avg_sim - 0))

    eligible <- add_eligible(eligible, names(avg_sim)[idx_max], people$user_id[idx_max], "The Epicenter")
    eligible <- add_eligible(eligible, names(avg_sim)[idx_min], people$user_id[idx_min], "Black Sheep")
    eligible <- add_eligible(eligible, names(avg_sim)[idx_zero], people$user_id[idx_zero], "Default Settings")

    sim_vals <- sim_mat
    sim_vals[upper.tri(sim_vals, diag = TRUE)] <- NA
    min_idx <- which(sim_vals == min(sim_vals, na.rm = TRUE), arr.ind = TRUE)[1, ]
    max_idx <- which(sim_vals == max(sim_vals, na.rm = TRUE), arr.ind = TRUE)[1, ]

    eligible <- add_eligible(eligible, rownames(sim_mat)[min_idx[1]], people$user_id[min_idx[1]], "Nemesis")
    eligible <- add_eligible(eligible, colnames(sim_mat)[min_idx[2]], people$user_id[min_idx[2]], "Nemesis")
    eligible <- add_eligible(eligible, rownames(sim_mat)[max_idx[1]], people$user_id[max_idx[1]], "Soulmate")
    eligible <- add_eligible(eligible, colnames(sim_mat)[max_idx[2]], people$user_id[max_idx[2]], "Soulmate")

    best_match <- sapply(seq_len(nrow(sim_mat)), function(i) {
      sims <- sim_mat[i, ]
      sims[i] <- -Inf
      which.max(sims)
    })

    inbound <- table(best_match)

    for (i in seq_len(nrow(sim_mat))) {
      cnt <- ifelse(as.character(i) %in% names(inbound), inbound[[as.character(i)]], 0)
      if (cnt == 0) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "One of One")
      if (cnt == 2) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Personality Triplets")
      if (cnt == 3) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Personality Quadruplets")
      if (cnt >= 4) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Personality Quintuplets")
    }

    max_inbound <- max(inbound)
    for (idx in as.integer(names(inbound)[inbound == max_inbound])) {
      eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Connector")
    }

    for (i in seq_len(nrow(sim_mat))) {
      a <- i
      b <- best_match[i]
      if (best_match[b] == a) eligible <- add_eligible(eligible, people$name[a], people$user_id[a], "Besties")
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
      for (pair in mutual_pairs) {
        if (b %in% pair && !(i %in% pair)) { is_third <- TRUE; break }
      }
      if (is_third) eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "Third Wheel")
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

    for (name in rank_tally$target_name[rank_tally$rank1 > 0]) {
      idx <- which(people$name == name)
      if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "I win")
    }

    for (name in rank_tally$target_name[rank_tally$rank4 > 0]) {
      idx <- which(people$name == name)
      if (length(idx) == 1) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Better than nothing")
    }

    # Cut it in half (tied for first place)
    for (idx in which(first_place_ties)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Cut it in half")

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

    # You get a car! (voted for everyone)
    cohort_size <- nrow(people)
    for (i in seq_len(nrow(people))) {
      voted_targets <- unique(vote_records$target_name[vote_records$voter_name == people$name[i]])
      voted_targets <- voted_targets[voted_targets != people$name[i]]
      if (length(voted_targets) >= (cohort_size - 1)) {
        eligible <- add_eligible(eligible, people$name[i], people$user_id[i], "You get a car!")
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
    for (i in seq_len(nrow(people))) {
      for (j in seq_len(nrow(people))) {
        if (i == j) next
        a <- people$name[i]
        b <- people$name[j]
        if (paste(a, b, sep = "->") %in% edge_set && paste(b, a, sep = "->") %in% edge_set) {
          jinx_pairs[i] <- TRUE
          jinx_pairs[j] <- TRUE
        }
      }
    }
    for (idx in which(jinx_pairs)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Jinx")

    # Double Jinx (three-way voting cycle)
    double_jinx <- rep(FALSE, nrow(people))
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
          }
        }
      }
    }
    for (idx in which(double_jinx)) eligible <- add_eligible(eligible, people$name[idx], people$user_id[idx], "Double Jinx")

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
    achievements[, c("id", "Title", "Image", "Description", "expected_rarity", "desirability_rank", "exclusive_group")],
    by.x = c("achievement_id", "Title"),
    by.y = c("id", "Title"),
    all.x = TRUE
  )

  all_eligible_path <- file.path(output_dir, "all_eligible_achievements.csv")
  write.csv(eligible, all_eligible_path, row.names = FALSE)

  # ---- Allocation ----
  allocate_for_person <- function(df, max_awards) {
    df <- df[order(as.integer(df$desirability_rank)), ]
    picked <- data.frame()
    used_groups <- c()
    for (i in seq_len(nrow(df))) {
      grp <- df$exclusive_group[i]
      if (!(grp %in% used_groups)) {
        picked <- rbind(picked, df[i, ])
        used_groups <- c(used_groups, grp)
      }
      if (nrow(picked) >= max_awards) break
    }
    picked
  }

  final <- data.frame()
  for (person in unique(eligible$person_name)) {
    dfp <- eligible[eligible$person_name == person, ]
    picked <- allocate_for_person(dfp, max_awards)
    final <- rbind(final, picked)
  }

  awarded_path <- file.path(output_dir, "awarded_achievements.csv")
  write.csv(final, awarded_path, row.names = FALSE)

  list(all_eligible = eligible, awarded = final)
}
