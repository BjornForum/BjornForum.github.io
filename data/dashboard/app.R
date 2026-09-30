# Dashboard: Top civil servants, governments, ministers and political appointees
# in Norway. Bjørn Mo Forum, https://bjornforum.github.io/data/
#
# Runs as an ordinary Shiny app (shiny::runApp()) or in the browser via Shinylive.
# Uses only shiny, bslib and ggplot2 so it loads quickly in the browser.

library(shiny)
library(bslib)
library(ggplot2)

# Tells the loading bar on the dashboard page how far the app has come.
# Only used in the browser (webR); silent when the app runs in ordinary R.
progress_step <- function(step) {
  if (identical(R.version$os, "emscripten")) message("dashboard-progress:", step)
}

# ---------------------------------------------------------------------------
# Data
# On the website, dashboard.qmd embeds the four RDS files next to this app when
# the site is rendered. When run locally (shiny::runApp("data/dashboard")), the
# files are read from the data folder one level up.
# ---------------------------------------------------------------------------
read_data <- function(file) {
  for (f in c(file, file.path("..", file))) if (file.exists(f)) return(as.data.frame(readRDS(f)))
  stop("Could not find ", file)
}

yr <- function(d) as.integer(format(d, "%Y"))

translate <- function(x, map) {
  out <- unname(map[x])
  ifelse(is.na(out), x, out)
}

# One row per person per year they were in office on 1 January
expand_years <- function(df, censor) {
  stop <- df$stop
  stop[is.na(stop)] <- censor
  y0 <- yr(df$start) + (format(df$start, "%m-%d") != "01-01")
  y1 <- yr(stop)
  n <- pmax(y1 - y0 + 1L, 0L)
  out <- df[rep(seq_len(nrow(df)), n), , drop = FALSE]
  out$year <- unlist(Map(function(a, b) if (b >= a) a:b else integer(0), y0, y1))
  out
}

fmt_date <- function(d) ifelse(is.na(d), "", format(d, "%d %b %Y"))
period <- function(start, stop) ifelse(is.na(stop), paste("since", fmt_date(start)),
                                        paste0(fmt_date(start), " – ", fmt_date(stop)))

load_all <- function() {
  tcs <- read_data("TCS_NOR.RDS")
  mi  <- read_data("Ministers_NOR.RDS")
  pa  <- read_data("POLADV_NOR.RDS")
  go  <- read_data("Governments_NOR.RDS")

  censor <- max(go$stop_date, na.rm = TRUE)   # date the data were last updated

  pos <- ifelse(grepl("^Assisterende", tcs$position_title), "Assistant Permanent Secretary",
         ifelse(grepl("^Departementsr", tcs$position_title), "Permanent Secretary",
                "Director General"))
  edu <- tcs$education_main
  edu[edu == "Business/Management/Finance"] <- "Business and management"
  edu[!edu %in% c("Law", "Economics", "Political science", "Business and management")] <- "Other or unknown"
  lvl <- c("Level-1 (Permanent Secretary)" = "Level 1\nPermanent Secretary",
           "Level-1 (Director General)"    = "Level 1\nDirector General",
           "Level-2 (Director General)"    = "Level 2\nDirector General")
  tcs <- data.frame(
    year      = tcs$year,
    key       = paste0("t", tcs$PersonID),
    name      = trimws(paste(tcs$first_name, ifelse(is.na(tcs$middle_name), "", tcs$middle_name), tcs$last_name)),
    woman     = tcs$gender,
    position  = factor(pos, levels = c("Permanent Secretary", "Assistant Permanent Secretary", "Director General")),
    level     = factor(unname(lvl[tcs$bureaucratic_buffer_i]), levels = unname(lvl)),
    education = factor(edu, levels = c("Law", "Economics", "Political science", "Business and management", "Other or unknown")),
    portfolio = tcs$ministry_portfolio,
    ministry  = tcs$ministry,
    turnover  = tcs$bureaucrat_turnover,
    gov_change = tcs$GOV_turnover,
    spell     = tcs$id_spells,
    born      = suppressWarnings(as.numeric(tcs$year_born)),
    censored  = tcs$censored,
    spell3    = tcs$id_spells3,
    election  = tcs$election_year,
    next_pos  = tcs$next_position_sector,
    pol_before_any = tcs$office_any_before,             pol_career_any = tcs$political_career,
    pol_before_minister = tcs$office_minister_before,   pol_career_minister = tcs$office_minister_career,
    pol_before_parliament = tcs$office_parliament_before, pol_career_parliament = tcs$office_parliament_career,
    pol_before_statesec = tcs$office_statesec_before,   pol_career_statesec = tcs$office_statesec_career,
    pol_before_polrad = tcs$office_polrad_before,       pol_career_polrad = tcs$office_polrad_career,
    pol_before_local = tcs$office_loc_pol_after1970_before, pol_career_local = tcs$office_loc_pol_after1970_career,
    stringsAsFactors = FALSE
  )
  tcs$name <- gsub("\\s+", " ", tcs$name)

  mi <- data.frame(
    key = mi$first_name_last_name_year_born, name = paste(mi$first_name, mi$last_name),
    woman = mi$gender, party = mi$party_long, portfolio = mi$ministry_portfolio,
    ministry = mi$ministry, start = mi$start_date, stop = mi$stop_date,
    spell = paste(mi$first_name_last_name_year_born, mi$spell_number),
    spell_start = mi$start_complete_spell_date, spell_stop = mi$stop_complete_spell_date,
    born = suppressWarnings(as.numeric(mi$year_born)),
    stringsAsFactors = FALSE
  )
  mi <- mi[!duplicated(mi[c("key", "portfolio", "ministry", "start", "stop")]), ]

  pa <- data.frame(
    key = pa$first_name_last_name_year_born, name = pa$full_name, woman = pa$gender,
    group = ifelse(pa$statesec_poladv == "State Secretary", "State secretaries", "Political advisors"),
    title = translate(pa$position_title, c(
      "Statssekretær" = "State secretary", "Konstituert statssekretær" = "Acting state secretary",
      "Statsministersekretær" = "Secretary to the PM", "Stabssjef" = "Chief of staff",
      "Politisk rådgiver" = "Political advisor", "Personlig rådgiver" = "Personal advisor",
      "Personlig sekretær" = "Personal secretary")),
    party = pa$party_long, portfolio = pa$ministry_portfolio,
    ministry = pa$ministry, start = pa$start_date, stop = pa$stop_date,
    born = suppressWarnings(as.numeric(pa$year_born)), cross = pa$party_cross,
    stringsAsFactors = FALSE
  )
  pa$party[is.na(pa$party) | pa$party == "NA"] <- "None/Unknown"

  # Merge back-to-back appointments of the same person in the same role into
  # continuous stints (a new appointment is often registered when the
  # government or minister changes, without a break in service).
  o <- order(pa$key, pa$group, pa$start)
  st <- integer(nrow(pa)); id <- 0L; run_stop <- as.Date(NA); prev <- ""
  for (i in o) {
    kg <- paste(pa$key[i], pa$group[i])
    if (kg != prev || (!is.na(run_stop) && pa$start[i] > run_stop + 1)) {
      id <- id + 1L; run_stop <- pa$stop[i]
    } else {
      run_stop <- if (is.na(run_stop) || is.na(pa$stop[i])) as.Date(NA) else max(run_stop, pa$stop[i])
    }
    st[i] <- id; prev <- kg
  }
  pa$stint <- st
  stint_stop <- tapply(as.numeric(pa$stop), pa$stint, function(x) if (anyNA(x)) NA_real_ else max(x))
  pa$stint_stop <- as.Date(unname(stint_stop[as.character(pa$stint)]), origin = "1970-01-01")

  go <- data.frame(
    name = as.character(go$government_version), start = go$start_date, stop = go$stop_date,
    ongoing = is.na(go$stop_reason), pm = trimws(go$PM), pm_party = go$PM_party,
    parties = gsub(",", ", ", go$parties_long), bloc = go$government_left_right,
    type = factor(go$government_type, levels = c("Single-Party Majority", "Single-Party Minority",
                                                 "Multi-Party Majority", "Multi-Party Minority"),
                  labels = c("Single-party majority", "Single-party minority",
                             "Coalition majority", "Coalition minority")),
    share_women = go$ministers_share_female, avg_age = go$ministers_avg_age,
    parl = go$ministers_share_parliament_before, statesec = go$ministers_share_state_secretary_before,
    poladv = go$ministers_share_political_advisor_before, minister = go$ministers_share_minister_before,
    seats = go$share_of_parliament,
    stringsAsFactors = FALSE
  )
  go <- go[order(go$start), ]

  groups <- c("Top civil servants", "Ministers", "State secretaries", "Political advisors")
  mi_py <- expand_years(mi, censor)
  pa_py <- expand_years(pa, censor)
  people <- rbind(
    data.frame(group = groups[1], year = tcs$year, key = tcs$key, woman = tcs$woman, portfolio = tcs$portfolio,
               age = tcs$year - tcs$born, cross = NA),
    data.frame(group = groups[2], year = mi_py$year, key = mi_py$key, woman = mi_py$woman, portfolio = mi_py$portfolio,
               age = mi_py$year - mi_py$born, cross = NA),
    data.frame(group = pa_py$group, year = pa_py$year, key = pa_py$key, woman = pa_py$woman, portfolio = pa_py$portfolio,
               age = pa_py$year - pa_py$born, cross = pa_py$cross)
  )
  coverage <- lapply(setNames(groups, groups), function(g) {
    y <- people$year[people$group == g]
    seq(min(y), max(y))
  })

  portfolios <- sort(unique(c(tcs$portfolio, mi$portfolio, pa$portfolio)))
  portfolios <- portfolios[!is.na(portfolios)]

  last_full_year <- if (format(censor, "%m-%d") == "12-31") yr(censor) else yr(censor) - 1L
  gov_years <- sort(unique(tcs$year[tcs$gov_change == 1]))

  # ---- turnover: one row per person per year in office on 1 January, with
  #      exit = TRUE if they left the position during that year
  turn <- rbind(
    data.frame(group = groups[1], year = tcs$year, key = tcs$key, portfolio = tcs$portfolio,
               exit = tcs$turnover == 1, level = as.integer(tcs$level)),
    data.frame(group = groups[2], year = mi_py$year, key = mi_py$key, portfolio = mi_py$portfolio,
               exit = !is.na(mi_py$spell_stop) & yr(mi_py$spell_stop) == mi_py$year, level = NA_integer_),
    data.frame(group = pa_py$group, year = pa_py$year, key = pa_py$key, portfolio = pa_py$portfolio,
               exit = !is.na(pa_py$stint_stop) & yr(pa_py$stint_stop) == pa_py$year, level = NA_integer_)
  )
  turn <- turn[turn$group == groups[1] | turn$year <= last_full_year, ]

  # ---- appointments: a person taking up a new position (back-to-back
  #      registrations of the same person in the same position are merged)
  run_ids <- function(k, start, stop) {
    o <- order(k, start); id <- integer(length(k)); cur <- 0L; run_stop <- as.Date(NA); prev <- ""
    for (i in o) {
      if (k[i] != prev || (!is.na(run_stop) && start[i] > run_stop + 1)) { cur <- cur + 1L; run_stop <- stop[i] }
      else run_stop <- if (is.na(run_stop) || is.na(stop[i])) as.Date(NA) else max(run_stop, stop[i])
      id[i] <- cur; prev <- k[i]
    }
    id
  }
  mi_run <- run_ids(paste(mi$key, mi$portfolio), mi$start, mi$stop)
  mi_first <- tapply(as.numeric(mi$start), mi_run, min)
  pa_run <- run_ids(paste(pa$key, pa$group, pa$portfolio), pa$start, pa$stop)
  pa_first <- tapply(as.numeric(pa$start), pa_run, min)
  pa_new <- pa[match(as.integer(names(pa_first)), pa_run), ]
  mi_new <- mi[match(as.integer(names(mi_first)), mi_run), ]
  tcs_o <- tcs[order(tcs$spell3, tcs$year), ]
  tcs_new <- tcs_o[!duplicated(tcs_o$spell3) & tcs_o$year > min(tcs$year), ]
  appt <- rbind(
    data.frame(group = groups[1], year = tcs_new$year - 1L, key = tcs_new$key, portfolio = tcs_new$portfolio),
    data.frame(group = groups[2], year = yr(as.Date(unname(mi_first), origin = "1970-01-01")), key = mi_new$key, portfolio = mi_new$portfolio),
    data.frame(group = pa_new$group, year = yr(as.Date(unname(pa_first), origin = "1970-01-01")), key = pa_new$key, portfolio = pa_new$portfolio)
  )
  appt_cov <- list(
    "Top civil servants" = seq(min(tcs$year), max(tcs$year) - 1L),
    "Ministers" = seq(min(appt$year[appt$group == groups[2]]), last_full_year),
    "State secretaries" = seq(min(appt$year[appt$group == groups[3]]), last_full_year),
    "Political advisors" = seq(min(appt$year[appt$group == groups[4]]), last_full_year)
  )
  appt <- appt[appt$year <= last_full_year, ]

  # ---- next positions of top civil servants who left office
  np <- tcs[tcs$turnover == 1 & !is.na(tcs$next_pos) & tcs$year >= 2011, ]
  np$next_short <- translate(np$next_pos, c(
    "Central Administration (Ministries)" = "Ministry",
    "Central Administration (Ministries: Special Advisor)" = "Ministry: special advisor",
    "Central Administration (Agencies)" = "Government agency",
    "Central Administration (Foreign Service)" = "Foreign service",
    "Local Administration" = "Local government", "State-Owned Enterprises" = "State-owned enterprise",
    "Private Sector" = "Private sector", "Nonprofit Sector" = "Nonprofit sector",
    "International Organization" = "International organization", "Politics" = "Politics",
    "Retirement" = "Retirement", "Dead" = "Died"))

  # ---- tenure: one row per spell, stint or government ----
  y_between <- function(a, b) as.numeric(b - a) / 365.25
  t_start <- tapply(tcs$year, tcs$spell, min)
  t_end   <- tapply(tcs$year, tcs$spell, max)
  t_cens  <- tapply(!is.na(tcs$censored) & tcs$censored != "No", tcs$spell, any)
  ms <- mi[!duplicated(mi$spell), ]
  s_start <- as.Date(tapply(as.numeric(pa$start), pa$stint, min), origin = "1970-01-01")
  s_stop  <- tapply(as.numeric(pa$stop), pa$stint, function(x) if (anyNA(x)) NA_real_ else max(x))
  s_group <- tapply(pa$group, pa$stint, function(x) x[1])
  ten <- rbind(
    data.frame(group = "Top civil servants", id = paste0("t", names(t_start)), entry = as.integer(t_start),
               years = as.numeric(t_end - t_start + 1), complete = !t_cens),
    data.frame(group = "Ministers", id = paste0("m", ms$spell), entry = yr(ms$spell_start),
               years = y_between(ms$spell_start, ms$spell_stop), complete = !is.na(ms$spell_stop)),
    data.frame(group = as.character(s_group), id = paste0("s", names(s_start)), entry = yr(s_start),
               years = (s_stop - as.numeric(s_start)) / 365.25, complete = !is.na(s_stop)),
    data.frame(group = "Governments", id = paste0("g", go$name), entry = yr(go$start),
               years = y_between(go$start, go$stop), complete = !go$ongoing)
  )
  ten_port <- unique(rbind(
    data.frame(id = paste0("t", tcs$spell), portfolio = tcs$portfolio),
    data.frame(id = paste0("m", mi$spell), portfolio = mi$portfolio),
    data.frame(id = paste0("s", pa$stint), portfolio = pa$portfolio)
  ))

  list(tcs = tcs, mi = mi, pa = pa, go = go, people = people, groups = groups,
       coverage = coverage, portfolios = portfolios, censor = censor,
       ten = ten, ten_port = ten_port,
       turn = turn, appt = appt, appt_cov = appt_cov, np = np,
       gov_years = gov_years, last_full_year = last_full_year,
       first_year = min(people$year), last_year = max(people$year))
}

progress_step("data")
dat <- tryCatch(load_all(), error = function(e) e)
ok <- !inherits(dat, "error")

# ---------------------------------------------------------------------------
# Look and feel (colours checked for colour-blind safety in both modes)
# ---------------------------------------------------------------------------
pal <- list(
  light = list(surface = "#ffffff", ink = "#0b0b0b", ink2 = "#52514e", grid = "#e8e7e1",
               axis = "#c3c2b7", other = "#b9b7ae",
               series = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100"),
               text = c("#1f66bf", "#b9461a", "#0b7651", "#8a5c00"), other_text = "#66655f",
               series5 = "#e87ba4", text5 = "#b3336b", change = "#4a3aa7", change_text = "#4a3aa7"),
  dark  = list(surface = "#2a2a2a", ink = "#ffffff", ink2 = "#c3c2b7", grid = "#3a3a37",
               axis = "#4f4e4a", other = "#6e6d67",
               series = c("#3987e5", "#d95926", "#199e70", "#c98500"),
               text = c("#72aef2", "#f08a5d", "#3fc794", "#e3aa3a"), other_text = "#b3b1a9",
               series5 = "#d55181", text5 = "#ef8fb6", change = "#9085e9", change_text = "#aaa2f2")
)

theme_dash <- function(p) {
  theme_minimal(base_size = 12.5) +
    theme(
      plot.background  = element_rect(fill = p$surface, colour = NA),
      panel.background = element_rect(fill = p$surface, colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = p$grid, linewidth = 0.3),
      axis.line.x  = element_line(colour = p$axis, linewidth = 0.3),
      axis.text    = element_text(colour = p$ink2),
      axis.title   = element_text(colour = p$ink2, size = 11),
      legend.position = "none",
      strip.text = element_text(colour = p$ink2, hjust = 0, size = 11, face = "bold"),
      panel.spacing = unit(18, "pt"),
      legend.title = element_blank(),
      legend.text  = element_text(colour = p$ink2, size = 11),
      legend.key.width = unit(14, "pt"),
      legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(4, "pt"),
      plot.margin  = margin(4, 10, 4, 4)
    )
}

pct <- function(x, digits = 0) ifelse(is.na(x), "–", paste0(formatC(100 * x, format = "f", digits = digits), "%"))

# Push end labels apart so they don't overlap
spread <- function(y, gap) {
  o <- order(y); ys <- y[o]
  if (length(ys) > 1) for (i in 2:length(ys)) if (ys[i] - ys[i - 1] < gap) ys[i] <- ys[i - 1] + gap
  y[o] <- ys
  y
}

year_scale <- function(r, extra = 0) {
  b <- pretty(r, 6); b <- b[b >= r[1] & b <= r[2]]
  scale_x_continuous(limits = c(r[1], r[2] + extra * diff(r)), breaks = b,
                     expand = expansion(mult = c(0.01, 0.02)))
}

css <- "
[data-bs-theme=light] { --bs-body-bg:#ffffff; }
[data-bs-theme=dark]  { --bs-body-bg:#222222; --bs-body-color:#ffffff; --bs-secondary-color:#c3c2b7;
                        --bs-border-color:rgba(255,255,255,.12); --bs-tertiary-bg:#2a2a2a; }
[data-bs-theme=dark] .card { --bs-card-bg:#2a2a2a; --bs-card-cap-bg:#2a2a2a; }
[data-bs-theme=light] .card { --bs-card-bg:#ffffff; --bs-card-cap-bg:#ffffff; }
.card { --bs-card-border-color: var(--bs-border-color); box-shadow:none; }
.card, .bslib-value-box { border:1px solid var(--bs-border-color) !important; }
[data-bs-theme=light] { --bs-border-color:#dcdbd4; }
.card-header { font-weight:600; border-bottom:0; padding-bottom:0; }
.dl-head { display:flex; align-items:baseline; flex-wrap:wrap; column-gap:1rem; }
.bslib-card { container-type:inline-size; }
@container (max-width: 480px) { .dl-links .dl-word { display:none; } }
.dl-links { margin-left:auto; font-weight:400; font-size:.75rem; color:var(--bs-secondary-color); white-space:nowrap; }
.dl-links a { color:var(--bs-secondary-color); margin-left:.55rem; text-decoration:underline; text-underline-offset:2px; }
.dl-links a:hover { color:var(--bs-body-color); }
.readout { font-size:.8rem; line-height:1.5; color:var(--bs-secondary-color); min-height:3em; font-variant-numeric:tabular-nums;
           display:flex; flex-direction:column; justify-content:flex-end; }
.readout.lines-3 { min-height:4.5em; }
@media (max-width: 575.98px) { .readout { min-height:4.5em; } .readout.lines-3 { min-height:6em; } }
.readout .recalculating, .readout.recalculating { opacity:1 !important; transition:none !important; }
.card-body .form-group, .card-body .shiny-input-container { margin-bottom:.2rem; }
.card-body .shiny-options-group { font-size:.85rem; }
.card-body .checkbox-inline, .card-body .form-check-inline { margin-right:1rem; }
.note { font-size:.8rem; color:var(--bs-secondary-color); }
.bslib-value-box { min-height:0 !important; }
.bslib-value-box .value-box-area { padding:.7rem 1rem !important; }
.bslib-value-box .value-box-value { font-size:1.6rem; font-weight:600; margin-bottom:0; }
.bslib-value-box .value-box-title { font-size:.85rem; color:var(--bs-secondary-color); margin-bottom:.15rem; }
.nav-underline { margin-bottom:.75rem; }
.office-table table { font-size:.82rem; margin-bottom:0; font-variant-numeric:tabular-nums; }
.office-table { max-height:300px; overflow:auto; }
.office-table td, .office-table th { padding-right:1rem !important; }
.office-table td:last-child { white-space:nowrap; }
.chart-legend { display:flex; flex-wrap:wrap; gap:.2rem 1.1rem; font-size:.82rem; font-weight:500; margin:.15rem 0 .2rem; }
.legend-item { display:inline-flex; align-items:center; gap:.4rem; }
.legend-key { display:inline-block; flex:none; }
.legend-key.line { width:16px; height:3px; border-radius:2px; }
.legend-key.square { width:11px; height:11px; border-radius:2px; }
.legend-key.dot { width:9px; height:9px; border-radius:50%; }
.readout .ro-item { margin-left:.9rem; }
.readout .ro-label { font-weight:600; }
.vb-sub { font-size:.8rem; color:var(--bs-secondary-color); margin-top:.1rem; }
"

# Follow the website's light/dark switch (or the system setting when run on its own)
js <- "
(function () {
  // Find the Quarto page this app is embedded in: walk up through parent frames
  // (the page may itself sit inside another frame, e.g. RStudio's Viewer pane).
  function quartoBody() {
    var w = window;
    try {
      while (w.parent && w.parent !== w) {
        w = w.parent;
        var b = w.document.body;
        if (b && (b.classList.contains('quarto-dark') || b.classList.contains('quarto-light'))) return b;
      }
    } catch (e) {}
    return null;
  }
  function siteMode() {
    var b = quartoBody();
    if (b) return b.classList.contains('quarto-dark') ? 'dark' : 'light';
    return (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : 'light';
  }
  function apply() {
    var m = siteMode();
    document.documentElement.setAttribute('data-bs-theme', m);
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('mode', m);
  }
  apply();
  document.addEventListener('DOMContentLoaded', function () {
    if (window.jQuery) jQuery(document).on('shiny:connected', apply);
  });
  try { var qb = quartoBody(); if (qb) new MutationObserver(apply).observe(qb, {attributes: true, attributeFilter: ['class']}); } catch (e) {}

  // When embedded in the website, resize the embedding frame to fit the
  // current tab, so there is neither empty space nor a scrollbar inside it.
  function contentBottom(el) {
    if (!el) return 0;
    var b = 0;
    for (var i = 0; i < el.children.length; i++) {
      var r = el.children[i].getBoundingClientRect();
      if (r.height > 0) b = Math.max(b, r.bottom);
    }
    return b + (el.scrollTop || 0) + window.scrollY;
  }
  function fitFrame() {
    try {
      var fe = window.frameElement;
      if (!fe) return;
      var box = fe.closest('.shinylive-container') || fe;
      var h = Math.max(contentBottom(document.querySelector('.bslib-sidebar-layout > .main')),
                       contentBottom(document.querySelector('.bslib-sidebar-layout > .sidebar .sidebar-content'))) + 52;
      if (h > 300 && Math.abs(box.getBoundingClientRect().height - h) > 4) box.style.height = Math.ceil(h) + 'px';
    } catch (e) {}
  }
  var fitTimer = null;
  function scheduleFit() { clearTimeout(fitTimer); fitTimer = setTimeout(fitFrame, 150); }
  document.addEventListener('DOMContentLoaded', function () {
    try {
      var main = document.querySelector('.bslib-sidebar-layout > .main');
      var ro = new ResizeObserver(scheduleFit);
      if (main) { ro.observe(main); Array.prototype.forEach.call(main.children, function (c) { ro.observe(c); }); }
    } catch (e) {}
    if (window.jQuery) jQuery(document).on('shiny:value shown.bs.tab', scheduleFit);
    window.addEventListener('resize', scheduleFit);
  });
  try { window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', apply); } catch (e) {}

  // Hover readouts sit above their chart. If a readout grew and shrank with its
  // text, the chart would jump under the pointer, which changes what is hovered
  // and makes it jump again. Each readout therefore keeps the tallest height it
  // has needed at the current width: it can grow once, but never shrinks back.
  function holdReadouts() {
    var els = document.querySelectorAll('.readout');
    for (var i = 0; i < els.length; i++) {
      var el = els[i], h = el.getBoundingClientRect().height;
      if (h > 0 && h > (parseFloat(el.style.minHeight) || 0)) el.style.minHeight = h + 'px';
    }
  }
  var readoutWidth = null;
  function resetReadouts(width) {
    if (width === readoutWidth) return;
    readoutWidth = width;
    var els = document.querySelectorAll('.readout');
    for (var i = 0; i < els.length; i++) els[i].style.minHeight = '';
    holdReadouts();
  }
  document.addEventListener('DOMContentLoaded', function () {
    var main = document.querySelector('.bslib-sidebar-layout > .main');
    try {
      new MutationObserver(holdReadouts).observe(document.body, {childList: true, subtree: true, characterData: true});
      if (main) new ResizeObserver(function () { resetReadouts(Math.round(main.clientWidth)); }).observe(main);
    } catch (e) {}
    if (window.jQuery) jQuery(document).on('shown.bs.tab', function () { setTimeout(holdReadouts, 0); });
  });
})();
"

hover <- function(id) hoverOpts(id, delay = 80, delayType = "throttle", nullOutside = TRUE)

gov_var_choices <- c(
  "Share of women among ministers" = "share_women", "Average age of ministers" = "avg_age",
  "Ministers with parliamentary experience" = "parl", "Ministers who were state secretaries" = "statesec",
  "Ministers who were political advisors" = "poladv", "Ministers with earlier ministerial experience" = "minister",
  "Governing parties' share of parliamentary seats" = "seats")
pol_choices <- c(
  "Any political office" = "any", "Minister" = "minister", "Member of parliament" = "parliament",
  "State secretary" = "statesec", "Political advisor" = "polrad",
  "Elected local politician (observed from 1970)" = "local")
lvl_choices <- c(
  "Number in office on 1 January" = "n", "Share of women" = "women", "Average age" = "age",
  "Share with a law degree" = "law", "Share who had held political office" = "pol",
  "Share leaving office during the year" = "exit")
level_choices <- c(
  "All levels" = "all", "Level 1 (both types)" = "l1", "Level 1: Permanent Secretary" = "1",
  "Level 1: Director General (no Permanent Secretary)" = "2", "Level 2: Director General" = "3")

# Source line printed under every downloaded chart (split over two lines to fit)
data_source <- paste(
  "Data: Forum, B. M. (2026). Bureaucratic turnover under new governments: The moderating effect of bureaucratic and political layering.",
  "Journal of Public Administration Research and Theory, 36(4), 429\u2013446. https://doi.org/10.1093/jopart/muag016",
  sep = "\n")

# Chrome ignores the service worker that runs the in-browser app for links with
# a `download` attribute (Chromium issue 468227), so the attribute is removed.
# The server still sends the file as an attachment with the right file name.
dl_link <- function(id, label) {
  tag <- downloadLink(id, label)
  tag$attribs$download <- NULL
  tag$attribs$title <- paste("Download this chart as", label)
  tag
}

download_links <- function(plot_id) {
  span(class = "dl-links", span(class = "dl-word", "Download"),
       dl_link(paste0(plot_id, "_png"), "PNG"),
       dl_link(paste0(plot_id, "_pdf"), "PDF"))
}

chart_card <- function(title, plot_id, readout_id, height = 300, note = NULL, controls = NULL, readout_lines = 2) {
  card(
    card_header(class = "dl-head", span(title), download_links(plot_id)),
    card_body(
      class = "pt-1", gap = "0.3rem",
      controls,
      uiOutput(paste0(plot_id, "_legend")),
      div(class = paste0("readout lines-", readout_lines), uiOutput(readout_id, inline = TRUE)),
      plotOutput(plot_id, height = height, hover = hover(paste0(plot_id, "_hover")), fill = FALSE),
      if (!is.null(note)) div(class = "note", note)
    )
  )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
if (ok) {
  yr_min <- dat$first_year; yr_max <- dat$last_year
  portfolio_choices <- c("All ministries", dat$portfolios)
} else {
  yr_min <- 1884; yr_max <- 2026; portfolio_choices <- "All ministries"
}

ui <- page_sidebar(
  theme = bs_theme(version = 5, primary = "#2a78d6",
                   base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")),
  fillable = FALSE,
  tags$head(tags$style(HTML(css)), tags$script(HTML(js))),
  sidebar = sidebar(
    width = 250,
    conditionalPanel("input.tab != 'office'",
      sliderInput("years", "Years", min = yr_min, max = yr_max, value = c(yr_min, yr_max), step = 1, sep = "")),
    conditionalPanel("input.tab == 'office'",
      sliderInput("year1", "Year", min = yr_min, max = yr_max, value = 1972, step = 1, sep = "")),
    conditionalPanel("input.tab != 'governments' || input.gov_sub == 'appointees'",
      selectInput("portfolio", "Ministry", choices = portfolio_choices)),
    conditionalPanel("input.tab == 'tcs' || input.tab == 'turnover' || input.tab == 'nextpos'",
      selectInput("level", "Top civil servants: level", choices = level_choices),
      div(class = "note mb-3", "Level 1 is directly below the minister. Level 2 has a Permanent Secretary in between.")),
    div(class = "note", HTML(paste0(
        "Ministries are grouped into stable policy areas across name changes. ",
        "Data: <a href='https://doi.org/10.1093/jopart/muag016' target='_blank'>Forum (2026)</a>. ",
        "Download the datasets on the <a href='https://bjornforum.github.io/data/data.html' target='_top'>Data page</a>.")))
  ),
  if (!ok) div(class = "alert alert-warning",
               "The data could not be loaded. You can download the datasets from the ",
               tags$a("Data page", href = "https://bjornforum.github.io/data/data.html", target = "_top"), "."),
  navset_underline(
    id = "tab",
    nav_panel(
      "Overview", value = "overview",
      layout_columns(
        col_widths = breakpoints(sm = c(6, 6, 6, 6), lg = c(3, 3, 3, 3)), fill = FALSE,
        value_box("Top civil servants", textOutput("vb_tcs"), div(class = "vb-sub", textOutput("vb_tcs_sp", inline = TRUE))),
        value_box("Ministers", textOutput("vb_mi"), div(class = "vb-sub", textOutput("vb_mi_sp", inline = TRUE))),
        value_box("State secretaries and political advisors", textOutput("vb_pa"), div(class = "vb-sub", textOutput("vb_pa_sp", inline = TRUE))),
        value_box("Governments", textOutput("vb_go"), div(class = "vb-sub", textOutput("vb_go_sp", inline = TRUE)))
      ),
      div(class = "note mb-2",
          "Individuals are counted once for the selected years and ministry. Spells are defined differently in each dataset: ",
          "for top civil servants, a spell is a continuous period in a top position (a new spell starts after at least one year out of the data); ",
          "for ministers, a continuous period in cabinet regardless of changes of portfolio; ",
          "for state secretaries and political advisors, each appointment is counted. See the codebook for details."),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(6, 6)),
        chart_card("People in office on 1 January", "p_people", "r_people"),
        chart_card("Share of women (five-year average)", "p_women", "r_women")
      )
    ),
    nav_panel(
      "Top civil servants", value = "tcs",
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(6, 6)),
        chart_card("Top civil servants by position", "p_pos", "r_pos", readout_lines = 3),
        chart_card("Main field of education", "p_edu", "r_edu", readout_lines = 3)
      ),
      chart_card(
        "Share with a political background", "p_pol", "r_pol", height = 260,
        controls = selectInput("pol_office", NULL, width = "340px", choices = pol_choices),
        note = paste("Share of top civil servants in office on 1 January who had held the selected political office before that year,",
                     "and who held it at any point in their career (including after leaving the civil service).",
                     "Political offices are minister, member of parliament, state secretary, political advisor and elected local politician;",
                     "local offices are only observed from 1970.")),
      chart_card(
        "Compare levels", "p_lvl", "r_lvl", height = 260,
        controls = selectInput("lvl_metric", NULL, width = "340px", choices = lvl_choices),
        note = paste("Top civil servants in office on 1 January by level, in the selected years and ministry (the level filter does not apply here).",
                     "Shares and averages are five-year moving averages. Level 1 Directors General are found in ministries without a Permanent Secretary."))
    ),
    nav_panel(
      "Turnover", value = "turnover",
      chart_card("Share leaving office each year", "p_turn", "r_turn", height = 280, readout_lines = 3,
                 controls = checkboxGroupInput("turn_show", NULL, inline = TRUE, choices = c("Top civil servants", "Ministers", "State secretaries", "Political advisors"), selected = c("Top civil servants", "Ministers", "State secretaries", "Political advisors")),
                 note = paste("Share of those in office on 1 January who left the position during the year.",
                              "Top civil servants leave when they are absent from the data for at least one year; ministers when they leave the cabinet;",
                              "state secretaries and political advisors when their continuous period in the role ends (back-to-back appointments are merged).",
                              "The level filter applies to top civil servants only.")),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(6, 6)),
        chart_card("Leaving office in years with and without a change of government", "p_turngrp", "r_turngrp", height = 250,
                   note = "Share of person-years ending with an exit, in years when the Prime Minister's party changed and in other years (stability within governments)."),
        chart_card("Top civil servants: leaving office by level", "p_turnbar", "r_turnbar", height = 250,
                   note = "Descriptive rates across all person-years in the selection, not the model estimates reported in the article. The level filter does not apply here.")
      )
    ),
    nav_panel(
      "Appointments", value = "appointments",
      chart_card("New appointments each year", "p_appt", "r_appt", height = 300,
                 controls = checkboxGroupInput("appt_show", NULL, inline = TRUE, choices = c("Top civil servants", "Ministers", "State secretaries", "Political advisors"), selected = c("Top civil servants", "Ministers", "State secretaries", "Political advisors")),
                 note = paste("A new appointment is a person taking up a position they did not hold the day before: a new post as top civil servant",
                              "(including a move to another ministry or title), a new ministerial portfolio, or a new appointment as state secretary or political advisor",
                              "in a ministry. Back-to-back registrations of the same person in the same position (for example when a government is reshuffled) are not counted.",
                              "Top civil servants are recorded on 1 January, so a person first recorded in a post on 1 January of a year is counted as appointed the year before.")),
      chart_card("Average number of new appointments per year, with and without a change of government", "p_apptgrp", "r_apptgrp", height = 240)
    ),
    nav_panel(
      "Next positions", value = "nextpos",
      div(class = "note mb-2", textOutput("np_intro", inline = TRUE)),
      chart_card("Where top civil servants went next", "p_np", "r_np", height = 300),
      chart_card("Next positions by level", "p_nplvl", "r_nplvl", height = 340,
                 note = "Share of each level's exits going to each type of position."),
      chart_card("Next positions by the kind of year the civil servant left", "p_npgov", "r_npgov", height = 340,
                 controls = selectInput("np_compare", NULL, width = "460px", choices = c(
                   "Change of the Prime Minister's party vs. other years" = "change",
                   "Election year without a change of government vs. other years" = "election",
                   "All three groups" = "all")),
                 note = paste("Share of exits going to each type of position, by the kind of year in which the civil servant left.",
                              "Other years are years with neither an election nor a change of the Prime Minister's party."))
    ),
    nav_panel(
      "Governments", value = "governments",
      navset_pill(
        id = "gov_sub",
        nav_panel(
          "Cabinets", value = "cabinets",
          chart_card("Governments by type and political bloc", "p_gov", "r_gov", height = 230,
                     note = "A new government is counted when the Prime Minister changes, when parties join or leave, and after elections."),
          card(
            card_header(class = "dl-head", span("Cabinet characteristics"), download_links("p_govvar")),
            card_body(
              class = "pt-1",
              selectInput("gov_var", NULL, width = "340px", choices = gov_var_choices),
              div(class = "readout", textOutput("r_govvar", inline = TRUE)),
              plotOutput("p_govvar", height = 260, hover = hover("p_govvar_hover"), fill = FALSE)
            )
          )
        ),
        nav_panel(
          "Political appointees", value = "appointees",
          chart_card(
            "Staff per minister", "p_ratio", "r_ratio", height = 240,
            note = paste("Number of state secretaries, political advisors and top civil servants in office on 1 January,",
                         "divided by the number of ministers in office on the same date (in the selected ministry).",
                         "The positions of political advisor and state secretary were introduced after the Second World War",
                         "(state secretaries in 1947), so there are none before 1945.")),
          layout_columns(
            col_widths = breakpoints(sm = 12, lg = c(6, 6)),
            chart_card("Serving a minister from another party", "p_cross", "r_cross", height = 230,
                       note = paste("Share of state secretaries and political advisors in office on 1 January whose party differs",
                                    "from the party of the minister they serve under. Cross-party appointments occur in coalition governments.")),
            chart_card("Average age on 1 January", "p_age", "r_age", height = 230,
                       note = "Age is calculated as the calendar year minus the year of birth.")
          ),
          chart_card("State secretaries and political advisors by party", "p_party", "r_party", height = 230,
                     note = "Number of different people in each party who held the position in the selected years and ministry.")
        )
      )
    ),
    nav_panel(
      "Tenure", value = "tenure",
      card(
        card_header("Average length of completed spells"),
        card_body(class = "pt-1", uiOutput("t_tenure"),
                  div(class = "note", paste(
                    "Spells that began in the selected years and have ended. Top civil servants: continuous periods in a top position,",
                    "counted in whole years from the 1 January observations (a new spell starts after at least one year out of the data).",
                    "Ministers: continuous periods in cabinet, regardless of changes of portfolio.",
                    "State secretaries and political advisors: continuous periods in the role, with back-to-back appointments merged.",
                    "Governments: from taking office to leaving office, counting a new government when the Prime Minister changes,",
                    "when parties join or leave, and after elections.",
                    "The ministry filter selects spells that included time in that ministry; it does not apply to governments.")))
      ),
      chart_card("Average length of completed spells, by decade the spell began", "p_tenure", "r_tenure", height = 280)
    ),
    nav_panel(
      "Who held office?", value = "office",
      div(class = "note mb-2", textOutput("office_govs", inline = TRUE)),
      card(card_header(textOutput("h_mi", inline = TRUE)), card_body(div(class = "office-table", tableOutput("t_mi")))),
      card(card_header(textOutput("h_pa", inline = TRUE)), card_body(div(class = "office-table", tableOutput("t_pa")))),
      card(card_header(textOutput("h_tcs", inline = TRUE)), card_body(div(class = "office-table", tableOutput("t_tcs"))))
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  req_ok <- function() req(ok)

  exporting <- new.env(); exporting$on <- FALSE   # exported graphs always use the light palette

  # Render a plot in the app and offer it as PNG and PDF. Exported graphs use the
  # light palette and get a title, a legend and a caption describing the selection.
  plot_output <- function(id, title, fn, caption = NULL, width = 9, height = 5.2) {
    output[[id]] <- renderPlot(fn())
    ttl <- function() if (is.function(title)) title() else title
    selection <- function() {
      tab <- if (is.null(input$tab)) "" else input$tab
      y <- yrs()
      x <- c(if (tab == "office") paste0("Norway, 1 January ", input$year1)
             else if (tab == "nextpos") paste0("Norway, exits ", max(y[1], 2011), "\u2013", y[2], " (next positions are recorded from 2011)")
             else paste0("Norway, ", y[1], "\u2013", y[2]),
             if (!(tab == "governments" && identical(input$gov_sub, "cabinets"))) paste("Ministry:", input$portfolio),
             if (tab %in% c("tcs", "turnover", "nextpos") && !is.null(input$level) && input$level != "all")
               paste("Top civil servants:", names(level_choices)[level_choices == input$level]))
      paste(x, collapse = ". ")
    }
    export_plot <- function() {
      exporting$on <- TRUE
      on.exit(exporting$on <- FALSE)
      p <- pal$light
      fn() +
        labs(title = ttl(), caption = paste(c(caption, paste0(selection(), "."), data_source), collapse = "\n")) +
        theme(legend.position = "top", legend.justification = "left", legend.title = element_blank(),
              legend.text = element_text(colour = p$ink2, size = 10),
              plot.title = element_text(face = "bold", size = 13, colour = p$ink, margin = margin(0, 0, 6, 0)),
              plot.title.position = "plot", plot.caption.position = "plot",
              plot.caption = element_text(colour = p$ink2, size = 8, hjust = 0, lineheight = 1.1),
              plot.margin = margin(12, 16, 10, 12))
    }
    fname <- function(ext) {
      slug <- gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(ttl())))
      if (!grepl("^norw", slug)) slug <- paste0("norway-", slug)
      paste0(slug, ".", ext)
    }
    output[[paste0(id, "_png")]] <- downloadHandler(
      filename = function() fname("png"),
      content = function(file) {
        g <- export_plot()
        plotPNG(function() print(g), filename = file, width = width * 200, height = height * 200, res = 200)
      })
    output[[paste0(id, "_pdf")]] <- downloadHandler(
      filename = function() fname("pdf"),
      content = function(file) {
        g <- export_plot()
        grDevices::pdf(file, width = width, height = height, encoding = "WinAnsi.enc")
        on.exit(grDevices::dev.off())
        print(g)
      })
  }
  P <- function() if (exporting$on || identical(input$mode, "light")) pal$light else pal$dark
  yrs <- reactive(input$years)
  all_min <- reactive(is.null(input$portfolio) || input$portfolio == "All ministries")
  in_port <- function(x) if (all_min()) rep(TRUE, length(x)) else !is.na(x) & x == input$portfolio

  # ---- Overview ----------------------------------------------------------
  people_year <- reactive({
    req_ok()
    d <- dat$people[in_port(dat$people$portfolio), ]
    d <- d[!duplicated(d[c("group", "year", "key")]), ]
    grid <- do.call(rbind, lapply(dat$groups, function(g) data.frame(group = g, year = dat$coverage[[g]])))
    k <- paste(d$group, d$year)
    n <- tapply(rep(1L, nrow(d)), k, sum)
    w <- tapply(d$woman == 1, k, sum, na.rm = TRUE)
    gk <- paste(grid$group, grid$year)
    grid$n <- as.integer(ifelse(is.na(n[gk]), 0L, n[gk]))
    grid$women <- ifelse(is.na(w[gk]), 0, w[gk])
    # share of women as a centred five-year moving average (small groups are noisy)
    roll <- function(v) sapply(seq_along(v), function(i) sum(v[max(1, i - 2):min(length(v), i + 2)]))
    grid$share <- NA_real_
    for (g in dat$groups) {
      i <- which(grid$group == g)
      rn <- roll(grid$n[i]); rw <- roll(grid$women[i])
      grid$share[i] <- ifelse(rn > 0, rw / rn, NA)
    }
    grid$group <- factor(grid$group, levels = dat$groups)
    grid[grid$year >= yrs()[1] & grid$year <= yrs()[2], ]
  })

  fmt_n <- function(x) format(x, big.mark = ",")
  in_period <- function(d) {
    s <- as.Date(paste0(yrs()[1], "-01-01")); e <- as.Date(paste0(yrs()[2], "-12-31"))
    d[d$start <= e & (is.na(d$stop) | d$stop >= s) & in_port(d$portfolio), ]
  }
  tcs_box <- reactive({ req_ok(); dat$tcs[in_port(dat$tcs$portfolio) & dat$tcs$year >= yrs()[1] & dat$tcs$year <= yrs()[2], ] })
  mi_box  <- reactive({ req_ok(); in_period(dat$mi) })
  pa_box  <- reactive({ req_ok(); in_period(dat$pa) })
  go_box  <- reactive({
    req_ok()
    s <- as.Date(paste0(yrs()[1], "-01-01")); e <- as.Date(paste0(yrs()[2], "-12-31"))
    dat$go[dat$go$start <= e & (is.na(dat$go$stop) | dat$go$stop >= s), ]
  })
  output$vb_tcs    <- renderText(fmt_n(length(unique(tcs_box()$key))))
  output$vb_tcs_sp <- renderText(paste("individuals ·", fmt_n(length(unique(tcs_box()$spell))), "employment spells"))
  output$vb_mi     <- renderText(fmt_n(length(unique(mi_box()$key))))
  output$vb_mi_sp  <- renderText(paste("individuals ·", fmt_n(length(unique(mi_box()$spell))), "spells in cabinet"))
  output$vb_pa     <- renderText(fmt_n(length(unique(pa_box()$key))))
  output$vb_pa_sp  <- renderText(paste("individuals ·", fmt_n(nrow(pa_box())), "appointments"))
  output$vb_go     <- renderText(fmt_n(nrow(go_box())))
  output$vb_go_sp  <- renderText(paste("governments ·", fmt_n(length(unique(go_box()$pm))), "prime ministers"))

  # ---- legends and readouts ---------------------------------------------
  legend_ui <- function(labels, cols, tcols, shape = "line") {
    shape <- rep_len(shape, length(labels))
    div(class = "chart-legend", lapply(seq_along(labels), function(i) {
      span(class = "legend-item",
           span(class = paste("legend-key", shape[i]), style = paste0("background:", cols[i], ";")),
           span(style = paste0("color:", tcols[i], ";"), labels[i]))
    }))
  }
  readout_ui <- function(year, labels, values, tcols) {
    tagList(tags$b(year), lapply(seq_along(labels), function(i) {
      span(class = "ro-item", span(class = "ro-label", style = paste0("color:", tcols[i], ";"), labels[i]), " ", values[i])
    }))
  }
  hint <- "Hover over the chart to see the numbers for a year."

  # d: columns year, group (factor), value; colours follow the factor levels
  # xr: year range for the x axis (NULL = let each facet choose its own range)
  line_chart <- function(d, labeller, cols, zero = TRUE, xr = yrs()) {
    p <- P()
    ggplot(d, aes(year, value, colour = group)) +
      geom_line(linewidth = 0.65, na.rm = TRUE) +
      scale_colour_manual(values = setNames(cols, levels(d$group)), drop = TRUE) +
      (if (is.null(xr)) scale_x_continuous(breaks = function(l) pretty(l, 5), expand = expansion(mult = c(0.02, 0.02)))
       else year_scale(xr)) +
      scale_y_continuous(labels = labeller, limits = if (zero) c(0, NA) else NULL,
                         expand = expansion(mult = if (zero) c(0, 0.06) else c(0.06, 0.06))) +
      labs(x = NULL, y = NULL) + theme_dash(p)
  }
  pct_axis <- function(x) paste0(round(100 * x), "%")

  plot_output("p_people", "People in office on 1 January", function() { req_ok(); d <- people_year(); d$value <- d$n; line_chart(d, fmt_n, P()$series) })
  plot_output("p_women", "Share of women (five-year average)", function() { req_ok(); d <- people_year(); d$value <- d$share; line_chart(d, pct_axis, P()$series) })
  output$p_people_legend <- renderUI(legend_ui(dat$groups, P()$series, P()$text))
  output$p_women_legend  <- renderUI(legend_ui(dat$groups, P()$series, P()$text))

  ts_readout <- function(h, d, value, fmt) {
    if (is.null(h)) return(hint)
    y <- round(h$x)
    s <- d[d$year == y & !is.na(d[[value]]), ]
    if (!nrow(s)) return(hint)
    readout_ui(y, as.character(s$group), fmt(s[[value]]), P()$text[as.integer(s$group)])
  }
  output$r_people <- renderUI({ req_ok(); ts_readout(input$p_people_hover, people_year(), "n", identity) })
  output$r_women  <- renderUI({ req_ok(); ts_readout(input$p_women_hover, people_year(), "share", pct) })

  # ---- Top civil servants ------------------------------------------------
  lvl_ok <- function(l) {
    v <- if (is.null(input$level)) "all" else input$level
    if (v == "all") rep(TRUE, length(l)) else if (v == "l1") !is.na(l) & l %in% 1:2 else !is.na(l) & l == as.integer(v)
  }
  tcs_all_levels <- reactive({
    req_ok()
    d <- dat$tcs[in_port(dat$tcs$portfolio), ]
    d[d$year >= yrs()[1] & d$year <= yrs()[2], ]
  })
  tcs_sel <- reactive({ d <- tcs_all_levels(); d[lvl_ok(as.integer(d$level)), ] })

  level_names <- c("Level 1: Permanent Secretary", "Level 1: Director General", "Level 2: Director General")
  lvl_cols  <- function() (P()$series[c(1, 4, 3)])
  lvl_tcols <- function() (P()$text[c(1, 4, 3)])
  roll5 <- function(v) sapply(seq_along(v), function(i) sum(v[max(1, i - 2):min(length(v), i + 2)], na.rm = TRUE))

  comp <- function(d, var, share = FALSE) {
    years <- seq(max(yrs()[1], min(dat$tcs$year)), min(yrs()[2], max(dat$tcs$year)))
    lv <- levels(d[[var]])
    grid <- expand.grid(year = years, cat = lv, stringsAsFactors = FALSE)
    n <- table(factor(d$year, levels = years), factor(d[[var]], levels = lv))
    grid$n <- as.vector(n)
    tot <- rowSums(n)
    grid$total <- rep(tot, times = length(lv))
    grid$value <- if (share) ifelse(grid$total > 0, grid$n / grid$total, NA) else grid$n
    grid$cat <- factor(grid$cat, levels = lv)
    grid
  }

  area_chart <- function(g, cols, share) {
    p <- P()
    ggplot(g, aes(year, value, fill = cat)) +
      geom_area(position = position_stack(reverse = TRUE), colour = p$surface, linewidth = 0.25, alpha = 0.92, na.rm = TRUE) +
      scale_fill_manual(values = cols) +
      year_scale(yrs()) +
      scale_y_continuous(labels = if (share) function(x) paste0(round(100 * x), "%") else waiver(),
                         expand = expansion(mult = c(0, 0.04))) +
      labs(x = NULL, y = NULL) + theme_dash(p)
  }

  pos_data <- reactive(comp(tcs_sel(), "position"))
  edu_data <- reactive(comp(tcs_sel(), "education", share = TRUE))

  plot_output("p_pos", "Top civil servants by position", function() {
    p <- P()
    area_chart(pos_data(), setNames(p$series[1:3], levels(dat$tcs$position)), FALSE)
  })
  plot_output("p_edu", "Top civil servants: main field of education", function() {
    p <- P()
    area_chart(edu_data(), setNames(c(p$series, p$other), levels(dat$tcs$education)), TRUE)
  })

  output$p_pos_legend <- renderUI(legend_ui(levels(dat$tcs$position), P()$series[1:3], P()$text[1:3], "square"))
  output$p_edu_legend <- renderUI(legend_ui(levels(dat$tcs$education), c(P()$series, P()$other),
                                            c(P()$text, P()$other_text), "square"))

  comp_readout <- function(h, g, fmt, tcols) {
    if (is.null(h)) return(hint)
    y <- round(h$x)
    s <- g[g$year == y, ]
    if (!nrow(s) || all(is.na(s$value))) return(hint)
    readout_ui(y, as.character(s$cat), fmt(s$value), tcols[as.integer(s$cat)])
  }
  output$r_pos <- renderUI(comp_readout(input$p_pos_hover, pos_data(), identity, P()$text))
  output$r_edu <- renderUI(comp_readout(input$p_edu_hover, edu_data(), pct, c(P()$text, P()$other_text)))

  # ---- political background of top civil servants
  pol_labels <- c("Before that year", "At any point in their career")
  pol_data <- reactive({
    d <- tcs_sel()
    o <- if (is.null(input$pol_office)) "any" else input$pol_office
    years <- seq(max(yrs()[1], min(dat$tcs$year)), min(yrs()[2], max(dat$tcs$year)))
    f <- factor(d$year, levels = years)
    n  <- as.vector(tapply(rep(1L, nrow(d)), f, sum))
    kb <- as.vector(tapply(d[[paste0("pol_before_", o)]], f, sum))
    kc <- as.vector(tapply(d[[paste0("pol_career_", o)]], f, sum))
    n[is.na(n)] <- 0L; kb[is.na(kb)] <- 0; kc[is.na(kc)] <- 0
    data.frame(year = rep(years, 2),
               group = factor(rep(pol_labels, each = length(years)), levels = pol_labels),
               k = c(kb, kc), n = rep(n, 2),
               value = ifelse(rep(n, 2) > 0, c(kb, kc) / rep(n, 2), NA))
  })
  plot_output("p_pol", function() paste0("Top civil servants with a political background (", names(pol_choices)[pol_choices == (if (is.null(input$pol_office)) "any" else input$pol_office)], ")"), function() { req(nrow(pol_data()) > 0); line_chart(pol_data(), pct_axis, P()$series[1:2]) })
  output$p_pol_legend <- renderUI(legend_ui(pol_labels, P()$series[1:2], P()$text[1:2]))
  output$r_pol <- renderUI({
    h <- input$p_pol_hover; d <- pol_data()
    if (is.null(h)) return(hint)
    s <- d[d$year == round(h$x) & d$n > 0, ]
    if (!nrow(s)) return(hint)
    readout_ui(s$year[1], as.character(s$group), paste0(pct(s$value, 1), " (", s$k, " of ", s$n, ")"),
               P()$text[as.integer(s$group)])
  })

  # ---- compare levels (Top civil servants tab)
  lvl_data <- reactive({
    d <- tcs_all_levels()
    m <- if (is.null(input$lvl_metric)) "n" else input$lvl_metric
    years <- seq(max(yrs()[1], min(dat$tcs$year)), min(yrs()[2], max(dat$tcs$year)))
    out <- do.call(rbind, lapply(1:3, function(l) {
      x <- d[as.integer(d$level) == l, ]
      f <- factor(x$year, levels = years)
      n <- as.vector(tapply(rep(1L, nrow(x)), f, sum)); n[is.na(n)] <- 0L
      num <- switch(m,
        n = n,
        women = as.vector(tapply(x$woman == 1, f, sum)),
        age = as.vector(tapply(x$year - x$born, f, sum, na.rm = TRUE)),
        law = as.vector(tapply(x$education == "Law", f, sum)),
        pol = as.vector(tapply(x$pol_before_any == 1, f, sum)),
        exit = as.vector(tapply(x$turnover == 1, f, sum)))
      num[is.na(num)] <- 0
      value <- if (m == "n") ifelse(n > 0, n, NA) else { rn <- roll5(n); ifelse(rn > 0, roll5(num) / rn, NA) }
      data.frame(group = level_names[l], year = years, n = n, value = value)
    }))
    out$group <- factor(out$group, levels = level_names)
    out
  })
  lvl_fmt <- function(x) {
    m <- if (is.null(input$lvl_metric)) "n" else input$lvl_metric
    if (m == "n") formatC(x, format = "d") else if (m == "age") formatC(x, format = "f", digits = 1) else pct(x, 0)
  }
  plot_output("p_lvl", function() paste0("Top civil servants by level: ", names(lvl_choices)[lvl_choices == (if (is.null(input$lvl_metric)) "n" else input$lvl_metric)]), function() {
    d <- lvl_data(); req(nrow(d) > 0)
    m <- if (is.null(input$lvl_metric)) "n" else input$lvl_metric
    line_chart(d, if (m %in% c("n", "age")) function(x) round(x) else pct_axis, lvl_cols(), zero = m != "age")
  })
  output$p_lvl_legend <- renderUI(legend_ui(level_names, lvl_cols(), lvl_tcols()))
  output$r_lvl <- renderUI({
    h <- input$p_lvl_hover; d <- lvl_data()
    if (is.null(h)) return(hint)
    s <- d[d$year == round(h$x) & !is.na(d$value), ]
    if (!nrow(s)) return(hint)
    readout_ui(s$year[1], as.character(s$group), paste0(lvl_fmt(s$value), " (", s$n, " in office)"),
               lvl_tcols()[as.integer(s$group)])
  })

  # ---- Turnover ----------------------------------------------------------
  change_labels <- c("No change", "Prime Minister's party changed")
  change_cols  <- function() (c(P()$other, P()$change))
  change_tcols <- function() (c(P()$other_text, P()$change_text))
  gov_rules <- function(xr) {
    g <- dat$gov_years[dat$gov_years >= xr[1] & dat$gov_years <= xr[2]]
    if (!length(g)) return(NULL)
    geom_vline(xintercept = g, colour = P()$change, alpha = 0.35, linewidth = 0.45)
  }
  # line chart with vertical rules for the years in which the Prime Minister's party changed
  lines_with_changes <- function(d, labeller) {
    p <- P()
    ggplot(d, aes(year, value, colour = group)) +
      gov_rules(yrs()) +
      geom_line(linewidth = 0.6, na.rm = TRUE) +
      scale_colour_manual(values = setNames(p$series, dat$groups), drop = TRUE) +
      year_scale(yrs()) +
      scale_y_continuous(labels = labeller, limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = NULL) + theme_dash(p)
  }
  legend_with_change <- function(show = dat$groups) {
    i <- which(dat$groups %in% show)
    legend_ui(c(dat$groups[i], "Year the Prime Minister's party changed"),
              c(P()$series[i], P()$change), c(P()$text[i], P()$change_text))
  }
  shown <- function(d, show) d[as.character(d$group) %in% show, ]

  turn_sel <- reactive({
    req_ok()
    d <- dat$turn[in_port(dat$turn$portfolio) & dat$turn$year >= yrs()[1] & dat$turn$year <= yrs()[2], ]
    d <- d[d$group != "Top civil servants" | lvl_ok(d$level), ]
    # one row per person and year; an exit in any of their positions counts
    k <- paste(d$group, d$year, d$key)
    ex <- tapply(d$exit, k, any)
    d <- d[!duplicated(k), ]
    d$exit <- unname(ex[paste(d$group, d$year, d$key)])
    d
  })
  turn_year <- reactive({
    d <- turn_sel(); req(nrow(d) > 0)
    kk <- paste(d$group, d$year)
    n <- tapply(rep(1L, nrow(d)), kk, sum); k <- tapply(d$exit, kk, sum)
    x <- unique(d[c("group", "year")])
    x$n <- as.vector(n[paste(x$group, x$year)]); x$k <- as.vector(k[paste(x$group, x$year)])
    x$value <- x$k / x$n
    x$group <- factor(x$group, levels = dat$groups)
    x[order(x$group, x$year), ]
  })
  plot_output("p_turn", "Share leaving office each year", caption = "Vertical lines mark years when the Prime Minister's party changed.", function() { d <- shown(turn_year(), input$turn_show); req(nrow(d) > 0); lines_with_changes(d, pct_axis) })
  output$p_turn_legend <- renderUI(legend_with_change(input$turn_show))
  output$r_turn <- renderUI({
    h <- input$p_turn_hover; d <- shown(turn_year(), input$turn_show)
    if (is.null(h)) return(hint)
    y <- round(h$x); s <- d[d$year == y, ]
    if (!nrow(s)) return(hint)
    tagList(readout_ui(y, as.character(s$group), paste0(pct(s$value, 0), " (", s$k, " of ", s$n, ")"),
                       P()$text[as.integer(s$group)]),
            if (y %in% dat$gov_years) span(class = "ro-item", style = paste0("color:", P()$change_text, ";"), "Prime Minister's party changed"))
  })

  turn_grp <- reactive({
    d <- turn_sel(); req(nrow(d) > 0)
    d$change <- factor(ifelse(d$year %in% dat$gov_years, change_labels[2], change_labels[1]), levels = change_labels)
    a <- aggregate(exit ~ group + change, data = d, FUN = function(x) c(rate = mean(x), n = length(x), k = sum(x)))
    a <- data.frame(group = factor(a$group, levels = dat$groups), change = a$change,
                    rate = a$exit[, "rate"], n = a$exit[, "n"], k = a$exit[, "k"])
    a
  })
  change_bars <- function(a, xvar, lab = function(v) pct(v, 0), ylab = pct_axis) {
    p <- P()
    a$lab <- lab(a$rate)
    ggplot(a, aes(.data[[xvar]], rate, fill = change)) +
      geom_col(position = position_dodge(width = 0.78), width = 0.72, colour = p$surface, linewidth = 0.5) +
      geom_text(aes(label = lab), position = position_dodge(width = 0.78), vjust = -0.5, colour = p$ink2, size = 3.3) +
      scale_fill_manual(values = setNames(change_cols(), change_labels)) +
      scale_x_discrete(labels = function(x) gsub(": ", "\n", gsub(" (secretaries|advisors|civil servants)$", "\n\\1", x))) +
      scale_y_continuous(labels = ylab, expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = NULL) + theme_dash(p) + theme(axis.text.x = element_text(size = 10, lineheight = 0.95))
  }
  plot_output("p_turngrp", "Leaving office in years with and without a change of government", function() change_bars(turn_grp(), "group"))
  output$p_turngrp_legend <- renderUI(legend_ui(change_labels, change_cols(), change_tcols(), "square"))
  bar_readout <- function(h, a, xvar, what) {
    default <- "Hover over a bar to see the numbers behind it."
    if (is.null(h)) return(default)
    i <- round(h$x); lv <- levels(a[[xvar]])
    if (i < 1 || i > length(lv)) return(default)
    ch <- if (h$x < i) change_labels[1] else change_labels[2]
    s <- a[as.integer(a[[xvar]]) == i & a$change == ch, ]
    if (!nrow(s)) return(default)
    paste0(gsub("\n", " ", lv[i]), ", ", tolower(ch), ": ", s$k, " of ", s$n, " ", what, " (", pct(s$rate, 1), ")")
  }
  output$r_turngrp <- renderUI(bar_readout(input$p_turngrp_hover, turn_grp(), "group", "person-years ended with an exit"))

  turn_bar <- reactive({
    d <- tcs_all_levels(); req(nrow(d) > 0)
    d$change <- factor(ifelse(d$gov_change == 1, change_labels[2], change_labels[1]), levels = change_labels)
    a <- aggregate(turnover ~ level + change, data = d, FUN = function(x) c(rate = mean(x), n = length(x), k = sum(x)))
    data.frame(level = a$level, change = a$change, rate = a$turnover[, "rate"], n = a$turnover[, "n"], k = a$turnover[, "k"])
  })
  plot_output("p_turnbar", "Top civil servants: leaving office by level and change of government", function() change_bars(turn_bar(), "level"))
  output$p_turnbar_legend <- renderUI(legend_ui(change_labels, change_cols(), change_tcols(), "square"))
  output$r_turnbar <- renderUI(bar_readout(input$p_turnbar_hover, turn_bar(), "level", "left office"))

  # ---- Appointments -------------------------------------------------------
  appt_year <- reactive({
    req_ok()
    d <- dat$appt[in_port(dat$appt$portfolio) & dat$appt$year >= yrs()[1] & dat$appt$year <= yrs()[2], ]
    out <- do.call(rbind, lapply(dat$groups, function(g) {
      yy <- dat$appt_cov[[g]]; yy <- yy[yy >= yrs()[1] & yy <= yrs()[2]]
      if (!length(yy)) return(NULL)
      n <- as.vector(table(factor(d$year[d$group == g], levels = yy)))
      data.frame(group = g, year = yy, value = n)
    }))
    out$group <- factor(out$group, levels = dat$groups)
    out
  })
  plot_output("p_appt", "New appointments each year", caption = "Vertical lines mark years when the Prime Minister's party changed.", function() { d <- shown(appt_year(), input$appt_show); req(nrow(d) > 0); lines_with_changes(d, function(x) round(x)) })
  output$p_appt_legend <- renderUI(legend_with_change(input$appt_show))
  output$r_appt <- renderUI({
    h <- input$p_appt_hover; d <- shown(appt_year(), input$appt_show)
    if (is.null(h)) return(hint)
    y <- round(h$x); s <- d[d$year == y, ]
    if (!nrow(s)) return(hint)
    tagList(readout_ui(y, as.character(s$group), s$value, P()$text[as.integer(s$group)]),
            if (y %in% dat$gov_years) span(class = "ro-item", style = paste0("color:", P()$change_text, ";"), "Prime Minister's party changed"))
  })
  appt_grp <- reactive({
    d <- appt_year(); req(nrow(d) > 0)
    d$change <- factor(ifelse(d$year %in% dat$gov_years, change_labels[2], change_labels[1]), levels = change_labels)
    a <- aggregate(value ~ group + change, data = d, FUN = function(x) c(m = mean(x), years = length(x), total = sum(x)))
    data.frame(group = factor(a$group, levels = dat$groups), change = a$change,
               rate = a$value[, "m"], n = a$value[, "years"], k = a$value[, "total"])
  })
  plot_output("p_apptgrp", "Average number of new appointments per year, with and without a change of government", function() {
    change_bars(appt_grp(), "group", lab = function(v) formatC(v, format = "f", digits = 1), ylab = function(x) round(x))
  })
  output$p_apptgrp_legend <- renderUI(legend_ui(change_labels, change_cols(), change_tcols(), "square"))
  output$r_apptgrp <- renderUI({
    h <- input$p_apptgrp_hover; a <- appt_grp()
    default <- "Hover over a bar to see the numbers behind it."
    if (is.null(h)) return(default)
    i <- round(h$x); lv <- levels(a$group)
    if (i < 1 || i > length(lv)) return(default)
    ch <- if (h$x < i) change_labels[1] else change_labels[2]
    s <- a[as.integer(a$group) == i & a$change == ch, ]
    if (!nrow(s)) return(default)
    paste0(lv[i], ", ", tolower(ch), ": ", s$k, " appointments over ", s$n, " years (",
           formatC(s$rate, format = "f", digits = 1), " per year)")
  })

  # ---- Next positions -----------------------------------------------------
  np_sel <- reactive({
    req_ok()
    d <- dat$np[in_port(dat$np$portfolio) & dat$np$year >= yrs()[1] & dat$np$year <= yrs()[2], ]
    d[lvl_ok(as.integer(d$level)), ]
  })
  output$np_intro <- renderText(paste0(
    "Next positions are recorded for top civil servants who left office from 2011 onwards; ",
    "they were collected when the data were extended beyond the register of government employees. ",
    "The categories are shortened versions of next_position_sector in the codebook. ",
    nrow(np_sel()), " exits match the current selection. The year is the last year the person was in office on 1 January."))
  np_counts <- reactive({
    d <- np_sel(); req(nrow(d) > 0)
    tab <- sort(table(d$next_short))
    data.frame(sector = factor(names(tab), levels = names(tab)), n = as.vector(tab), share = as.vector(tab) / sum(tab))
  })
  wrap_if_narrow <- function(id) {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    if (!is.null(w) && w < 560) function(x) vapply(x, function(v) paste(strwrap(v, 16), collapse = "\n"), "")
    else waiver()
  }
  np_theme <- function(p) {
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = p$grid, linewidth = 0.3),
          axis.line.x = element_blank(), plot.margin = margin(4, 64, 4, 4))
  }
  plot_output("p_np", "Where top civil servants went next", function() {
    p <- P(); d <- np_counts()
    ggplot(d, aes(n, sector)) +
      geom_col(fill = p$series[1], width = 0.72) +
      geom_text(aes(label = paste0(n, " (", pct(share, 0), ")")), hjust = -0.15, colour = p$ink2, size = 3.3) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
      scale_y_discrete(labels = wrap_if_narrow("p_np")) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) + theme_dash(p) + np_theme(p)
  })
  output$p_np_legend <- renderUI(NULL)
  output$r_np <- renderUI("Number of exits (and share) by type of next position.")

  # share of exits going to each type of position, for groups of exits
  np_split <- function(d, groups, labels) {
    ord <- levels(np_counts()$sector)
    out <- do.call(rbind, lapply(seq_along(labels), function(i) {
      x <- d$next_short[groups == i]
      if (!length(x)) return(NULL)
      tab <- table(factor(x, levels = ord))
      data.frame(group = labels[i], sector = factor(ord, levels = ord), n = as.vector(tab),
                 share = as.vector(tab) / length(x), total = length(x))
    }))
    out$group <- factor(out$group, levels = labels)
    out
  }
  np_split_plot <- function(d, cols, id) {
    p <- P()
    d$group_rev <- factor(as.character(d$group), levels = rev(levels(d$group)))
    ggplot(d, aes(share, sector, fill = group_rev)) +
      geom_col(position = position_dodge(width = 0.84), width = 0.78, colour = p$surface, linewidth = 0.4) +
      geom_text(aes(label = ifelse(n > 0, pct(share, 0), "")), position = position_dodge(width = 0.84),
                hjust = -0.2, colour = p$ink2, size = 2.9) +
      scale_fill_manual(values = setNames(cols, levels(d$group)), breaks = levels(d$group)) +
      scale_x_continuous(labels = pct_axis, expand = expansion(mult = c(0, 0.04))) +
      scale_y_discrete(labels = wrap_if_narrow(id)) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) + theme_dash(p) + np_theme(p)
  }
  split_legend <- function(d, cols, tcols) {
    i <- sort(unique(as.integer(d$group)))
    legend_ui(paste0(levels(d$group)[i], " (", d$total[match(i, as.integer(d$group))], " exits)"), cols[i], tcols[i], "square")
  }

  np_lvl <- reactive({ d <- np_sel(); req(nrow(d) > 0); np_split(d, as.integer(d$level), level_names) })
  plot_output("p_nplvl", "Next positions of top civil servants, by level", function() np_split_plot(np_lvl(), lvl_cols(), "p_nplvl"))
  output$p_nplvl_legend <- renderUI(split_legend(np_lvl(), lvl_cols(), lvl_tcols()))
  output$r_nplvl <- renderUI("")

  np_year_labels <- c("Other years", "Prime Minister's party changed", "Election year, no change of government")
  np_year_cols  <- function() c(P()$other, P()$change, P()$series5)
  np_year_tcols <- function() c(P()$other_text, P()$change_text, P()$text5)
  np_gov <- reactive({
    d <- np_sel(); req(nrow(d) > 0)
    kind <- ifelse(d$gov_change == 1, 2L, ifelse(d$election == 1, 3L, 1L))
    mode <- if (is.null(input$np_compare)) "change" else input$np_compare
    keep <- switch(mode, change = kind %in% 1:2, election = kind %in% c(1, 3), all = rep(TRUE, length(kind)))
    np_split(d[keep, ], kind[keep], np_year_labels)
  })
  plot_output("p_npgov", "Next positions of top civil servants, by the kind of year they left", function() np_split_plot(np_gov(), np_year_cols(), "p_npgov"), height = 7)
  output$p_npgov_legend <- renderUI(split_legend(np_gov(), np_year_cols(), np_year_tcols()))
  output$r_npgov <- renderUI({
    d <- np_sel()
    yc <- sort(unique(d$year[d$gov_change == 1])); ye <- sort(unique(d$year[d$gov_change != 1 & d$election == 1]))
    paste0("Years when the Prime Minister's party changed: ", if (length(yc)) paste(yc, collapse = ", ") else "none",
           ". Election years without a change: ", if (length(ye)) paste(ye, collapse = ", ") else "none", ".")
  })

  # ---- Governments -------------------------------------------------------
  gov_sel <- reactive({
    req_ok()
    g <- dat$go
    s <- as.Date(paste0(yrs()[1], "-01-01")); e <- as.Date(paste0(yrs()[2], "-12-31"))
    g <- g[g$start <= e & (is.na(g$stop) | g$stop >= s), ]
    g$stop[is.na(g$stop)] <- dat$censor
    g$x0 <- pmax(g$start, s); g$x1 <- pmin(g$stop, e)
    g
  })
  date_limits <- reactive(c(as.Date(paste0(yrs()[1], "-01-01")), min(as.Date(paste0(yrs()[2], "-12-31")), dat$censor)))
  date_scale <- function() {
    lim <- date_limits()
    b <- pretty(yr(lim), 6); b <- as.Date(paste0(b, "-01-01")); b <- b[b >= lim[1] & b <= lim[2]]
    scale_x_date(limits = lim, breaks = b, date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.01)))
  }

  plot_output("p_gov", "Norwegian governments by type and political bloc", function() {
    p <- P(); g <- gov_sel()
    req(nrow(g) > 0)
    g$lane <- as.integer(g$type)
    ggplot(g) +
      geom_rect(aes(xmin = x0, xmax = x1, ymin = lane - 0.36, ymax = lane + 0.36, fill = bloc),
                colour = p$surface, linewidth = 0.35) +
      scale_fill_manual(values = c(Left = p$series[2], Right = p$series[1]),
                        labels = c(Left = "Left-led (Labour, Liberals before 1935)", Right = "Right-led")) +
      scale_y_reverse(breaks = 1:4, labels = levels(dat$go$type), expand = expansion(add = 0.3)) +
      date_scale() + labs(x = NULL, y = NULL) + theme_dash(p) +
      theme(panel.grid.major.y = element_blank())
  })

  gov_at <- function(h) {
    if (is.null(h)) return(NULL)
    d <- as.Date(h$x, origin = "1970-01-01")
    g <- gov_sel()
    g[g$start <= d & g$stop >= d, ][1, ]
  }
  gov_label <- function(s) {
    paste0(s$name, " (PM: ", s$pm, ", ", s$pm_party, ")   ·   ", fmt_date(s$start), " – ",
           if (s$ongoing) "in office" else fmt_date(s$stop), "   ·   ", s$type, ": ", s$parties)
  }
  output$p_gov_legend <- renderUI(legend_ui(c("Right-led", "Left-led (Labour; the Liberals before 1935)"),
                                            P()$series[1:2], P()$text[1:2], "square"))
  output$r_gov <- renderUI({
    s <- gov_at(input$p_gov_hover)
    if (is.null(s) || is.na(s$name)) "Hover over the chart to see each government." else gov_label(s)
  })

  gov_is_pct <- reactive(input$gov_var != "avg_age")
  plot_output("p_govvar", function() names(gov_var_choices)[gov_var_choices == input$gov_var], function() {
    p <- P(); g <- gov_sel()
    req(nrow(g) > 0)
    g$value <- g[[input$gov_var]]
    st <- rbind(data.frame(x = g$x0, y = g$value), data.frame(x = tail(g$x1, 1), y = tail(g$value, 1)))
    ggplot(st, aes(x, y)) +
      geom_step(direction = "hv", colour = p$series[1], linewidth = 0.6, na.rm = TRUE) +
      date_scale() +
      scale_y_continuous(labels = if (gov_is_pct()) function(x) paste0(round(100 * x), "%") else waiver(),
                         limits = if (gov_is_pct()) c(0, NA) else NULL, expand = expansion(mult = c(0, 0.06))) +
      labs(x = NULL, y = NULL) + theme_dash(p)
  })
  output$r_govvar <- renderText({
    s <- gov_at(input$p_govvar_hover)
    if (is.null(s) || is.na(s$name)) return("Hover over the chart to see each government.")
    v <- s[[input$gov_var]]
    paste0(s$name, " (", fmt_date(s$start), ")   ·   ",
           if (gov_is_pct()) pct(v) else formatC(v, format = "f", digits = 1))
  })

  # ---- Political appointees (Governments tab) ---------------------------
  appointee_groups <- c("State secretaries", "Political advisors")
  in_years <- function(d) d[d$year >= yrs()[1] & d$year <= yrs()[2], ]
  person_years <- reactive({
    req_ok()
    d <- dat$people[in_port(dat$people$portfolio), ]
    d[!duplicated(d[c("group", "year", "key")]), ]
  })

  # staff per minister
  ratio_data <- reactive({
    d <- people_year()
    mn <- d[d$group == "Ministers", c("year", "n")]
    names(mn)[2] <- "ministers"
    x <- merge(d[d$group != "Ministers", c("group", "year", "n")], mn, by = "year")
    x$value <- ifelse(x$ministers > 0, x$n / x$ministers, NA)
    x$panel <- factor(ifelse(x$group == "Top civil servants", "Top civil servants", "State secretaries and political advisors"),
                      levels = c("State secretaries and political advisors", "Top civil servants"))
    x$group <- factor(as.character(x$group), levels = dat$groups)
    x[order(x$group, x$year), ]
  })
  ratio_labels <- c("State secretaries", "Political advisors", "Top civil servants")
  plot_output("p_ratio", "Staff per minister", function() {
    d <- ratio_data(); req(nrow(d) > 0)
    line_chart(d, function(x) formatC(x, format = "f", digits = 1), P()$series, xr = NULL) +
      facet_wrap(~panel, nrow = 1, scales = "free")
  })
  output$p_ratio_legend <- renderUI(legend_ui(ratio_labels, P()$series[c(3, 4, 1)], P()$text[c(3, 4, 1)]))
  output$r_ratio <- renderUI({
    h <- input$p_ratio_hover; d <- ratio_data()
    if (is.null(h)) return(hint)
    s <- d[d$year == round(h$x) & !is.na(d$value), ]
    if (!nrow(s)) return(hint)
    s <- s[match(ratio_labels, as.character(s$group), nomatch = 0), ]
    readout_ui(s$year[1], as.character(s$group),
               paste0(formatC(s$value, format = "f", digits = 2), " (", s$n, " / ", s$ministers, " ministers)"),
               P()$text[as.integer(s$group)])
  })

  # appointees serving a minister from another party
  cross_data <- reactive({
    d <- in_years(person_years()); d <- d[d$group %in% appointee_groups, ]
    years <- seq(max(yrs()[1], min(dat$coverage[["State secretaries"]])), min(yrs()[2], dat$last_year))
    out <- do.call(rbind, lapply(appointee_groups, function(g) {
      x <- d[d$group == g, ]
      f <- factor(x$year, levels = years)
      n <- as.vector(tapply(rep(1L, nrow(x)), f, sum)); k <- as.vector(tapply(x$cross == 1, f, sum, na.rm = TRUE))
      n[is.na(n)] <- 0L; k[is.na(k)] <- 0L
      data.frame(group = g, year = years, n = n, k = k, value = ifelse(n > 0, k / n, NA))
    }))
    out$group <- factor(out$group, levels = dat$groups)
    out
  })
  plot_output("p_cross", "State secretaries and political advisors serving a minister from another party", function() {
    d <- cross_data(); req(nrow(d) > 0)
    line_chart(d, pct_axis, P()$series, xr = range(d$year))
  })
  output$p_cross_legend <- renderUI(legend_ui(appointee_groups, P()$series[3:4], P()$text[3:4]))
  output$r_cross <- renderUI({
    h <- input$p_cross_hover; d <- cross_data()
    if (is.null(h)) return(hint)
    s <- d[d$year == round(h$x) & d$n > 0, ]
    if (!nrow(s)) return(hint)
    readout_ui(s$year[1], as.character(s$group), paste0(pct(s$value, 0), " (", s$k, " of ", s$n, ")"),
               P()$text[as.integer(s$group)])
  })

  # average age
  age_data <- reactive({
    d <- in_years(person_years()); d <- d[!is.na(d$age), ]
    req(nrow(d) > 0)
    a <- aggregate(age ~ group + year, data = d, FUN = mean)
    grid <- do.call(rbind, lapply(dat$groups, function(g) {
      yy <- dat$coverage[[g]]; yy <- yy[yy >= yrs()[1] & yy <= yrs()[2]]
      if (length(yy)) data.frame(group = g, year = yy) else NULL
    }))
    x <- merge(grid, a, all.x = TRUE)
    names(x)[names(x) == "age"] <- "value"
    x$group <- factor(x$group, levels = dat$groups)
    x[order(x$group, x$year), ]
  })
  plot_output("p_age", "Average age on 1 January", function() line_chart(age_data(), function(x) round(x), P()$series, zero = FALSE))
  output$p_age_legend <- renderUI(legend_ui(dat$groups, P()$series, P()$text))
  output$r_age <- renderUI({
    h <- input$p_age_hover; d <- age_data()
    if (is.null(h)) return(hint)
    s <- d[d$year == round(h$x) & !is.na(d$value), ]
    if (!nrow(s)) return(hint)
    readout_ui(s$year[1], as.character(s$group), formatC(s$value, format = "f", digits = 1), P()$text[as.integer(s$group)])
  })

  # appointees by party
  party_data <- reactive({
    d <- pa_box()
    req(nrow(d) > 0)
    x <- unique(d[c("key", "party", "group")])
    tab <- as.data.frame(table(party = x$party, group = factor(x$group, levels = appointee_groups)), responseName = "n")
    tot <- tapply(tab$n, tab$party, sum)
    tab$party <- factor(tab$party, levels = names(sort(tot)))
    tab[tab$party %in% names(tot)[tot > 0], ]
  })
  plot_output("p_party", "State secretaries and political advisors by party", function() {
    p <- P(); d <- party_data()
    d$group <- factor(as.character(d$group), levels = rev(appointee_groups))  # state secretaries on top
    ggplot(d, aes(n, party, fill = group)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.74, colour = p$surface, linewidth = 0.4) +
      geom_text(aes(label = ifelse(n > 0, n, "")), position = position_dodge(width = 0.8),
                hjust = -0.25, colour = p$ink2, size = 3.3) +
      scale_fill_manual(values = setNames(p$series[3:4], appointee_groups), breaks = appointee_groups) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = NULL) + theme_dash(p) +
      theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = p$grid, linewidth = 0.3),
            axis.line.x = element_blank())
  })
  output$p_party_legend <- renderUI(legend_ui(appointee_groups, P()$series[3:4], P()$text[3:4], "square"))
  output$r_party <- renderUI("")

  # ---- Tenure -------------------------------------------------------------
  ten_groups <- c(dat$groups, "Governments")
  ten_cols  <- function() (c(P()$series, P()$series5))
  ten_tcols <- function() (c(P()$text, P()$text5))
  ten_sel <- reactive({
    req_ok()
    t <- dat$ten[dat$ten$complete & !is.na(dat$ten$years) &
                   dat$ten$entry >= yrs()[1] & dat$ten$entry <= yrs()[2], ]
    if (!all_min()) {
      ids <- dat$ten_port$id[!is.na(dat$ten_port$portfolio) & dat$ten_port$portfolio == input$portfolio]
      t <- t[t$group == "Governments" | t$id %in% ids, ]
    }
    t$group <- factor(t$group, levels = ten_groups)
    t
  })
  output$t_tenure <- renderUI({
    t <- ten_sel(); tc <- ten_tcols()
    rows <- lapply(seq_along(ten_groups), function(i) {
      x <- t$years[as.integer(t$group) == i]
      tags$tr(tags$td(span(style = paste0("color:", tc[i], ";font-weight:600;"), ten_groups[i])),
              tags$td(if (length(x)) formatC(mean(x), format = "f", digits = 1) else "–"),
              tags$td(if (length(x)) formatC(stats::median(x), format = "f", digits = 1) else "–"),
              tags$td(fmt_n(length(x))))
    })
    div(class = "office-table",
        tags$table(class = "table table-sm mb-1",
                   tags$thead(tags$tr(tags$th(""), tags$th("Average (years)"), tags$th("Median (years)"), tags$th("Completed spells"))),
                   tags$tbody(rows)))
  })
  ten_decade <- reactive({
    t <- ten_sel(); req(nrow(t) > 0)
    t$decade <- t$entry %/% 10 * 10
    a <- aggregate(years ~ group + decade, data = t, FUN = function(x) c(m = mean(x), n = length(x)))
    data.frame(group = a$group, decade = a$decade, value = a$years[, "m"], n = a$years[, "n"])
  })
  plot_output("p_tenure", "Average length of completed spells, by decade the spell began", function() {
    p <- P(); d <- ten_decade()
    br <- sort(unique(d$decade)); if (length(br) > 8) br <- br[seq(1, length(br), by = 2)]
    ggplot(d, aes(decade + 5, value, colour = group)) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 2.3, shape = 21, stroke = 0.8, aes(fill = group), colour = p$surface) +
      scale_colour_manual(values = setNames(ten_cols(), ten_groups)) +
      scale_fill_manual(values = setNames(ten_cols(), ten_groups)) +
      scale_x_continuous(breaks = br + 5, labels = paste0(br, "s"), expand = expansion(mult = c(0.03, 0.03))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
      labs(x = NULL, y = "Years") + theme_dash(p)
  })
  output$p_tenure_legend <- renderUI(legend_ui(ten_groups, ten_cols(), ten_tcols()))
  output$r_tenure <- renderUI({
    h <- input$p_tenure_hover; d <- ten_decade()
    if (is.null(h)) return("Hover over the chart to see the numbers for a decade.")
    dec <- floor(h$x / 10) * 10
    s <- d[d$decade == dec, ]
    if (!nrow(s)) return("Hover over the chart to see the numbers for a decade.")
    s <- s[order(s$group), ]
    readout_ui(paste0(dec, "s"), as.character(s$group),
               paste0(formatC(s$value, format = "f", digits = 1), " yrs (", s$n, ")"),
               ten_tcols()[as.integer(s$group)])
  })

  # ---- Who held office? --------------------------------------------------
  y1 <- reactive(input$year1)
  overlaps <- function(start, stop, y) {
    s <- as.Date(paste0(y, "-01-01")); e <- as.Date(paste0(y, "-12-31"))
    start <- as.Date(start)
    start <= e & (is.na(stop) | stop >= s)
  }
  port_label <- reactive(if (all_min()) "all ministries" else input$portfolio)

  output$office_govs <- renderText({
    req_ok()
    g <- dat$go[overlaps(dat$go$start, dat$go$stop, y1()), ]
    if (!nrow(g)) return(paste0("No governments in the data for ", y1(), "."))
    paste0("Governments in office in ", y1(), ": ",
           paste0(g$name, " (", g$pm_party, ")", collapse = ", "), ".")
  })

  mi_year <- reactive({
    req_ok()
    d <- dat$mi[overlaps(dat$mi$start, dat$mi$stop, y1()) & in_port(dat$mi$portfolio), ]
    if (!nrow(d)) return(d)
    # merge rows for the same person and ministry (the data has one row per government version)
    k <- paste(d$key, d$ministry)
    d <- do.call(rbind, lapply(split(d, k), function(s) {
      s$stop <- if (any(is.na(s$stop))) as.Date(NA) else max(s$stop)
      s$start <- min(s$start)
      s[1, ]
    }))
    d[order(d$portfolio, d$start), ]
  })
  output$h_mi <- renderText(paste0("Ministers in ", y1(), " (", nrow(mi_year()), ")"))
  output$t_mi <- renderTable({
    d <- mi_year()
    validate(need(nrow(d) > 0, "No ministers in the data for this selection."))
    data.frame(Name = d$name, Party = d$party, Ministry = d$ministry, `In office` = period(d$start, d$stop),
               check.names = FALSE)
  }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", na = "")

  pa_year <- reactive({
    req_ok()
    d <- dat$pa[overlaps(dat$pa$start, dat$pa$stop, y1()) & in_port(dat$pa$portfolio), ]
    d[order(d$portfolio, d$group != "State secretaries", d$start), ]
  })
  output$h_pa <- renderText(paste0("State secretaries and political advisors in ", y1(), " (", nrow(pa_year()), ")"))
  output$t_pa <- renderTable({
    d <- pa_year()
    validate(need(nrow(d) > 0, "No state secretaries or political advisors for this selection. The positions were introduced after the Second World War (state secretaries in 1947)."))
    data.frame(Name = d$name, Position = d$title, Party = d$party, Ministry = d$ministry,
               `In office` = period(d$start, d$stop), check.names = FALSE)
  }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", na = "")

  tcs_year <- reactive({
    req_ok()
    d <- dat$tcs[dat$tcs$year == y1() & in_port(dat$tcs$portfolio), ]
    d[order(d$portfolio, as.integer(d$position), d$name), ]
  })
  output$h_tcs <- renderText(paste0("Top civil servants on 1 January ", y1(), " (", nrow(tcs_year()), ")"))
  output$t_tcs <- renderTable({
    d <- tcs_year()
    validate(need(nrow(d) > 0, "No top civil servants in the data for this selection (the data run from 1884 to 2025)."))
    data.frame(Name = d$name, Position = as.character(d$position), Ministry = d$ministry,
               Education = as.character(d$education), check.names = FALSE)
  }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", na = "")
}

progress_step("app")
shinyApp(ui, server)
