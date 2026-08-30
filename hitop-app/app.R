library(shiny)
library(ggiraph)
library(shinycustomloader)

# Load the hitop package (Girard): in the browser (webR) install the
# WebAssembly build from r-universe at startup; on desktop R use a local
# installation (install once from GitHub using the remotes package).
# Every stage is wrapped so that no failure here can prevent the app from
# starting: worst case the app runs with its built-in fallback scoring.
# Package names are held in variables so the shinylive exporter does not
# try to bundle them.
.hitop_pkg <- "hitop"
.webr_pkg  <- "webr"
.is_webr   <- isTRUE(grepl("emscripten|wasm",
                           paste(R.version$os, R.version$platform)))
hitop_available    <- FALSE
hitop_pkg_version  <- NA_character_
hitop_has_intervals <- FALSE

try({
  if (.is_webr && requireNamespace(.webr_pkg, quietly = TRUE)) {
    message("hitop loader: installing from r-universe...")
    do.call(getExportedValue(.webr_pkg, "install"),
            list(.hitop_pkg,
                 repos = c("https://jmgirard.r-universe.dev",
                           "https://repo.r-wasm.org")))
    message("hitop loader: install call finished")
  }
}, silent = TRUE)

try({
  if (requireNamespace(.hitop_pkg, quietly = TRUE)) {
    # attachNamespace instead of library(): shinylive's runtime scanner
    # regex-matches library() calls and tries to install whatever symbol
    # it captures, producing a spurious warning
    try(attachNamespace(.hitop_pkg), silent = TRUE)
    hitop_available   <- TRUE
    hitop_pkg_version <- as.character(packageVersion(.hitop_pkg))
  }
}, silent = TRUE)
message("hitop loader: package available = ", hitop_available,
        if (hitop_available) paste0(" (v", hitop_pkg_version, ")") else "")

try({
  hitop_has_intervals <- hitop_available &&
    "interval_hitopsr" %in% getNamespaceExports(.hitop_pkg)
}, silent = TRUE)

if (hitop_has_intervals) {
  ok <- try({
    .devstats <- get("hitopsr_devstats", envir = asNamespace(.hitop_pkg))
    .name_alias <- c("NSSI" = "Non-suicidal Self-injury",
                     "Body Focus" = "Appearance Focus")
    hitop_camel_of <- function(nm) {
      nm <- ifelse(nm %in% names(.name_alias), .name_alias[nm], nm)
      .devstats$camelCase[match(nm, .devstats$scale)]
    }
    add_score_intervals <- function(bars, level = 0.95) {
      bars$lo <- bars$hi <- bars$est <- NA_real_
      cam <- hitop_camel_of(bars$name)
      ok <- bars$level %in% c("scale", "subscale") & bars$flag == "ok" &
        !is.na(bars$mean) & !is.na(cam)
      if (!any(ok)) return(bars)
      sc <- as.data.frame(as.list(setNames(bars$mean[ok], cam[ok])))
      res <- suppressWarnings(
        interval_hitopsr(sc, scores = seq_along(sc), prefix = "",
                         level = level, append = FALSE))
      bars$est[ok] <- as.numeric(res[paste0(cam[ok], "_est")])
      bars$lo[ok]  <- as.numeric(res[paste0(cam[ok], "_lo")])
      bars$hi[ok]  <- as.numeric(res[paste0(cam[ok], "_hi")])
      bars
    }
    TRUE
  }, silent = TRUE)
  if (!isTRUE(ok)) hitop_has_intervals <- FALSE
}
message("hitop loader: score intervals = ", hitop_has_intervals)

source("R/hitop_circular_viz.R")

item_key  <- read.csv("data/hitopsr_item_key.csv")
hierarchy <- read.csv("data/hitopsr_hierarchy.csv")
norms     <- read.csv("data/hitopsr_norms.csv")
defs_df   <- read.csv("data/hitopsr_definitions.csv")
br_map    <- read.csv("data/hitopbr_spectrum_map.csv")
hierarchy_alt <- read.csv("data/hitopsr_hierarchy_alt.csv")
subs_all <- read.csv("data/hitopsr_subscales_all.csv")
sub_parent <- with(subs_all[!duplicated(subs_all$subscale), ],
                   setNames(parent, subscale))
scale_defs <- setNames(defs_df$Brief, defs_df$Scale)


N_ITEMS <- nrow(item_key)   # 405
PER_ROW <- 15               # grid cells per row

app_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&family=Nunito+Sans:wght@600;700&display=swap');
  body { font-family: 'Roboto', sans-serif; background: #F7F8FA; color: #1E2430; }
  .app-header { background: #1E3A5F; color: #fff; padding: 18px 28px;
                margin: -15px -15px 0 -15px; position: relative; }
  .app-header h2 { margin: 0; font-weight: 600; letter-spacing: -0.5px; }
  .app-header h2 { font-family: 'Avenir Next', 'Avenir', 'Nunito Sans', 'Roboto', sans-serif; }
  .app-header .sub { color: #AECBE8; font-size: 13px; margin-top: 2px; }
  .gh-link { position: absolute; top: 22px; right: 28px; color: #AECBE8; }
  .gh-link:hover { color: #fff; }
  .gh-link svg { width: 26px; height: 26px; fill: currentColor; }
  .brand-bar { margin: 0 -15px 20px -15px; }
  .brand-bar div { height: 4px; }
  .brand-light { background: #D8E7F4; }
  .brand-mid   { background: #98C1E4; }
  .brand-dark  { background: #69A3D7; }
  .card { background: #fff; border: 1px solid #E4E7EE; border-radius: 10px;
          padding: 20px 24px; margin-bottom: 16px; }
  .progress-pill { display: inline-block; background: #EDF0F6; border-radius: 99px;
                   padding: 4px 14px; font-size: 13px; color: #3B4356;
                   font-variant-numeric: tabular-nums; }
  .btn-primary { background: #1E3A5F; border: none; }
  .btn-primary:hover { background: #162C49; }
  #submit_btn { font-weight: 600; padding: 10px 26px; }
  .anchor-note { font-size: 13px; color: #5A6478; margin-bottom: 12px; }
  .nav-tabs > li > a { color: #3B4356; font-weight: 500; }
  a code { color: #3E77B5; text-decoration: underline; }

  /* ---- constrain the custom loading animation ---- */
  img.loader-img { width: 120px !important; height: 120px !important; }
  .load-container { display: flex; align-items: center;
                    justify-content: center; min-height: 320px; }
  .callout-warn { background: #FFF8E6; border: 1px solid #EBCB8B;
                  border-left: 4px solid #E8A13B; border-radius: 8px;
                  padding: 10px 14px; font-size: 13px; color: #6B4F0F;
                  margin-bottom: 12px; }
  .callout-danger { background: #FDF0EF; border: 1px solid #E8B0AA;
                    border-left: 4px solid #C0392B; border-radius: 8px;
                    padding: 12px 16px; font-size: 14px; color: #7C2D24;
                    margin-bottom: 16px; }
  #oneko-credit { display: none; position: fixed; bottom: 12px; right: 48px;
                  z-index: 790; font-size: 11px; color: #8B94A6;
                  background: #fff; border: 1px solid #E4E7EE;
                  border-radius: 99px; padding: 3px 10px; opacity: 0.85; }
  #oneko-credit a { color: #3E77B5; }

  /* ---- small-screen gate ---- */
  #mobile-gate { display: none; }
  @media (max-width: 849px) {
    #mobile-gate { display: flex; position: fixed; inset: 0; z-index: 9999;
                   background: #1E3A5F; color: #fff;
                   flex-direction: column; align-items: center;
                   justify-content: center; text-align: center;
                   padding: 32px; gap: 14px; }
    #mobile-gate img { width: 110px; background: #fff; border-radius: 14px;
                       padding: 10px; }
    #mobile-gate h3 { margin: 6px 0 0 0; font-weight: 600; }
    #mobile-gate p { color: #AECBE8; font-size: 14px; max-width: 420px;
                     margin: 0; }
  }

  /* ---- keyboard grid ---- */
  #kgrid { display: grid; grid-template-columns: 52px repeat(15, 30px);
           gap: 4px; align-items: center; }
  #kgrid .rowlab { color: #8B94A6; font-size: 12px; text-align: right;
                   padding-right: 6px; font-variant-numeric: tabular-nums; }
  .kcell { width: 30px; height: 32px; text-align: center; font-size: 15px;
           font-weight: 600; border: 1px solid #D5DAE4; border-radius: 6px;
           background: #FBFCFE; caret-color: transparent; padding: 0; }
  .kcell:focus { outline: 2px solid #3E77B5; outline-offset: -1px;
                 background: #fff; }
  .kcell.filled { background: #E7F0F9; border-color: #98C1E4; }
  .kcell.na-cell { background: #FFF2F0; border-color: #F1C0BA; color: #C0392B; }
  #item_hint { position: sticky; top: 0; z-index: 5; background: #1E3A5F;
               color: #fff; border-radius: 8px; padding: 10px 16px;
               font-size: 14px; margin-bottom: 14px; min-height: 42px; }
  #item_hint .hint-num { color: #96A0B5; margin-right: 10px;
                         font-variant-numeric: tabular-nums; }
  .kgrid-help { font-size: 13px; color: #5A6478; margin-bottom: 10px; }
  .kgrid-help kbd { background: #EDF0F6; border-radius: 4px; padding: 1px 6px;
                    border: 1px solid #D5DAE4; font-family: inherit; }

  /* ---- item response view (panel 3) ---- */
  .irow { display: flex; align-items: center; gap: 12px;
          padding: 7px 4px; border-bottom: 1px solid #F0F2F6; }
  .irow .inum { color: #8B94A6; width: 38px; text-align: right;
                font-variant-numeric: tabular-nums; flex-shrink: 0; }
  .irow .itext { flex: 1; font-size: 14px; }
  .irow .irev { color: #8B94A6; font-size: 12px; font-style: italic; }
  .chip { width: 30px; height: 26px; border-radius: 6px; flex-shrink: 0;
          display: flex; align-items: center; justify-content: center;
          font-weight: 600; font-size: 14px; }
  .chip.c1 { background: #EEF4FB; color: #1E3A5F; }
  .chip.c2 { background: #D8E7F4; color: #1E3A5F; }
  .chip.c3 { background: #98C1E4; color: #1E3A5F; }
  .chip.c4 { background: #3E77B5; color: #fff; }
  .chip.cna { background: #FFF2F0; color: #C0392B; border: 1px solid #F1C0BA; }
"

kgrid_js <- sprintf("
const ITEM_TEXTS = %s;
const N = %d;

function cellVal(el) { return el.value; }

function updateHint(i) {
  const hint = document.getElementById('item_hint');
  if (hint) hint.innerHTML =
    '<span class=\"hint-num\">Item ' + i + ' / ' + N + '</span>' +
    ITEM_TEXTS[i - 1];
}

function pushToShiny() {
  let s = '';
  for (let i = 1; i <= N; i++) {
    const el = document.getElementById('kc' + i);
    const v = el ? el.value : '';
    s += (v === '' ? '.' : v);
  }
  Shiny.setInputValue('kgrid_vals', s, {priority: 'event'});
}

function styleCell(el) {
  el.classList.toggle('filled', /^[1-4]$/.test(el.value));
  el.classList.toggle('na-cell', el.value === 'x');
}

function focusCell(i) {
  const el = document.getElementById('kc' + i);
  if (el) { el.focus(); el.select(); }
}

document.addEventListener('keydown', function(e) {
  const el = e.target;
  if (!el.classList || !el.classList.contains('kcell')) return;
  const i = parseInt(el.dataset.i);

  if (/^[1-4]$/.test(e.key)) {
    el.value = e.key; styleCell(el); pushToShiny();
    if (i < N) focusCell(i + 1);
    e.preventDefault();
  } else if (e.key === 'x' || e.key === 'X' || e.key === '0') {
    el.value = 'x'; styleCell(el); pushToShiny();      // mark item skipped
    if (i < N) focusCell(i + 1);
    e.preventDefault();
  } else if (e.key === 'Backspace' || e.key === 'Delete') {
    if (el.value !== '') { el.value = ''; styleCell(el); pushToShiny(); }
    else if (i > 1 && e.key === 'Backspace') focusCell(i - 1);
    e.preventDefault();
  } else if (e.key === 'ArrowRight') { focusCell(i + 1); e.preventDefault(); }
    else if (e.key === 'ArrowLeft')  { focusCell(i - 1); e.preventDefault(); }
    else if (e.key === 'ArrowDown')  { focusCell(Math.min(i + %d, N)); e.preventDefault(); }
    else if (e.key === 'ArrowUp')    { focusCell(Math.max(i - %d, 1)); e.preventDefault(); }
    else if (e.key.length === 1) e.preventDefault();   // swallow other chars
});

document.addEventListener('focusin', function(e) {
  if (e.target.classList && e.target.classList.contains('kcell'))
    updateHint(parseInt(e.target.dataset.i));
});

Shiny.addCustomMessageHandler('set_grid', function(vals) {
  for (let i = 1; i <= N; i++) {
    const el = document.getElementById('kc' + i);
    if (!el) continue;
    const v = vals[i - 1];
    el.value = (v === null || v === undefined || v === '') ? '' : String(v);
    styleCell(el);
  }
});
", jsonlite::toJSON(item_key$text), N_ITEMS, PER_ROW, PER_ROW)

# grid cells built server-free (static HTML, fast to render once)
kgrid_html <- {
  rows <- split(seq_len(N_ITEMS), ceiling(seq_len(N_ITEMS) / PER_ROW))
  cells <- lapply(rows, function(ix) {
    c(list(div(class = "rowlab", sprintf("%d\u2013%d", min(ix), max(ix)))),
      lapply(ix, function(i)
        tags$input(id = paste0("kc", i), class = "kcell", `data-i` = i,
                   type = "text", maxlength = "1", inputmode = "numeric",
                   autocomplete = "off")))
  })
  div(id = "kgrid", do.call(tagList, unlist(cells, recursive = FALSE)))
}

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  div(id = "mobile-gate",
      img(src = "hitop_loader.gif", alt = "HiTOP"),
      h3("HiTOP-SR Scoring"),
      p(strong("This window is too narrow for the app."),
        " The 405-item entry grid and charts need a desktop-width browser ",
        "window (this is not a loading screen \u2014 nothing more will load ",
        "here). Please open the app on a computer, or widen this window, ",
        "and it will appear immediately.")),
  div(class = "app-header",
      a(class = "gh-link", target = "_blank",
        href = "https://github.com/YOUR-USERNAME/hitop-shinylive",
        title = "View source on GitHub",
        HTML('<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4.42-3.58-8-8-8Z"/></svg>')),
      h2("HiTOP-SR Scoring",
         span("* v0.3.0-alpha version, feedback is much appreciated",
              style = paste0("font-size:13px;font-weight:400;color:#AECBE8;",
                             "margin-left:14px;letter-spacing:0;"))),
      div(class = "sub",
          "Hierarchical Taxonomy of Psychopathology \u2014 Self Report (v1.0). ",
          "All scoring runs locally in your browser; no data is transmitted.")),
  div(class = "brand-bar",
      div(class = "brand-light"), div(class = "brand-mid"),
      div(class = "brand-dark")),
  span(id = "oneko-credit",
       a("oneko", href = "https://github.com/adryd325/oneko.js",
         target = "_blank"), " by adryd"),
  
  tabsetPanel(id = "main_tabs",
              
              tabPanel("About", br(),
                       div(class = "callout-danger",
                           strong("This web app is IN DEVELOPMENT and NOT READY FOR USE. "),
                           "Scores, reference norms, and composite structure are placeholders ",
                           "to demonstrate what the tool could look like. They are NOT ",
                           "endorsed by the HiTOP Society or validated for clinical ",
                           "interpretation."),
                       div(class = "card",
                           h4("Notes for interpretation"),
                           p("Scores are shown as T-scores (mean: 50, SD: 10) relative to ",
                             "preliminary norms pooled from a Prolific community sample collected",
                             "in Phase 2 of HiTOP-SR development by the Measure Development Workgroup (n \u2248 780)",
                             "and a University of Kansas student sample (n = 411). Severity bands (minimal < 60, mild 60\u201365, moderate 65\u201370, ",
                             "severe \u2265 70) are provisional conventions, not validated ",
                             "clinical cutoffs. T-scores use a linear transformation of the ",
                             "raw score; because most scales are strongly floor-constrained, ",
                             "linear T-scores are distorted at the extremes (the minimum ",
                             "possible raw score can exceed T = 40 on many scales), and ",
                             "percentile-based norms are planned to replace them. ",
                             if (hitop_has_intervals) list(
                               "Score intervals are 95% regression-based true-score ",
                               "confidence intervals (", a("Schmukle, 2026", href = "https://doi.org/10.1177/10731911251362532", target = "_blank"), "), computed by the hitop ",
                               "package from the development sample\u0027s reliability ",
                               "(coefficient alpha), mean, and SD (N = 780). The small diamond ",
                               "on each interval marks the true-score estimate, which is ",
                               "pulled toward the reference-group mean and can therefore ",
                               "differ from the observed score shown by the bar. The reference ",
                               "group is the instrument\u0027s development sample, not a ",
                               "community norm. Intervals are omitted for scales scored from ",
                               "incomplete responses and for HiTOP-BR spectrum scores. ",
                               "Interval bounds are not clamped to the response range; on ",
                               "strongly skewed scales a lower bound can fall below the ",
                               "minimum or above the maximum possible score, and the charts ",
                               "extend their axis to show it, with dashed lines marking the ",
                               "response floor and ceiling. The stated coverage applies across respondents drawn from the ",
                               "reference distribution rather than to any one individual.")
                             else paste0(
                               "Error bars show \u00B11 standard error of the person\u0027s ",
                               "own item responses within each scale \u2014 an index of ",
                               "response consistency, not the standard error of measurement; ",
                               "a reliability-based interval is planned to replace it.")),
                           p("When the composites toggle is on, spectrum scores are computed ",
                             "from the HiTOP-BR items embedded within the HiTOP-SR (six BR ",
                             "spectra, with Antagonism and Disinhibition shown within the ",
                             "Externalizing group), while subfactor scores are rational means ",
                             "of their constituent scales. Spectrum T-scores currently ",
                             "reference the student sample only, as item-level community data ",
                             "are not yet available."),
                           h4("Missing data"),
                           p("Scale scores are computed from available items (proration / ",
                             "person-mean imputation) only when at least 75% of a scale's ",
                             "items are answered; otherwise the scale is not scored and is ",
                             "marked \u2715 in the charts. Any scale scored from incomplete ",
                             "items, and any composite built on such scales, is marked * and ",
                             "should be interpreted with caution: research shows prorated ",
                             "scores can be biased even when items are missing completely at ",
                             "random (",
                             a("Mazza et al., 2015",
                               href = "https://doi.org/10.1080/00273171.2015.1068157",
                               target = "_blank"),
                             "; see also ",
                             a("Wu et al., 2022",
                               href = "https://doi.org/10.3758/s13428-021-01671-w",
                               target = "_blank"),
                             ", on proration cutoffs). Whenever possible, complete all ",
                             "items before interpreting the results."),
                           h4("Attributions"),
                           p("The HiTOP-SR was developed by the HiTOP Society ",
                             "(Hierarchical Taxonomy of Psychopathology Society, 2024). ",
                             "The development of this app was aided by functions in the ",
                             a(code("hitop"),
                               href = "https://github.com/jmgirard/hitop",
                               target = "_blank"),
                             " package developed by Jeffrey Girard",
                             if (hitop_available)
                               sprintf(" (version %s, loaded from r-universe)", hitop_pkg_version)
                             else " (not loaded in this session)",
                             ". This web app is currently IN ",
                             "DEVELOPMENT by the HiTOP Software and Development Workgroup ",
                             "and is NOT READY FOR USE."))),
              
              # ---------------- Data entry ------------------------------------------
              tabPanel("1 \u00B7 Enter responses", br(),
                       div(class = "card",
                           div(class = "anchor-note",
                               strong("Response scale: "),
                               "1 = Not at all \u2022 2 = A little \u2022 3 = Moderately \u2022 4 = A lot ",
                               "(past 12 months)"),
                           fluidRow(
                             column(6, radioButtons("entry_mode", NULL, inline = TRUE,
                                                    choices = c("Keyboard grid" = "grid",
                                                                "Paste" = "rapid",
                                                                "Upload CSV" = "upload"))),
                             column(6, style = "text-align:right;",
                                    span(class = "progress-pill", textOutput("progress", inline = TRUE)),
                                    actionButton("load_example", "Load random response for demo",
                                                 class = "btn btn-default btn-sm",
                                                 style = "margin-left:10px;"),
                                    actionButton("load_example_missing",
                                                 "Load random demo (missing responses)",
                                                 class = "btn btn-default btn-sm",
                                                 style = "margin-left:6px;"))
                           )
                       ),
                       
                       conditionalPanel("input.entry_mode == 'grid'",
                                        div(class = "card",
                                            div(id = "item_hint",
                                                span(class = "hint-num", "Item \u2014"),
                                                "Click any cell to begin; the item text appears here."),
                                            div(class = "kgrid-help",
                                                "Type ", tags$kbd("1"), "\u2013", tags$kbd("4"),
                                                " to score and auto-advance \u00B7 ", tags$kbd("x"),
                                                " marks an item skipped \u00B7 ", tags$kbd("\u232B"),
                                                " clears \u00B7 arrow keys move around"),
                                            kgrid_html
                                        )
                       ),
                       
                       conditionalPanel("input.entry_mode == 'rapid'",
                                        div(class = "card",
                                            p("Type or paste all responses in item order. Digits 1\u20134; use ",
                                              code("x"), " or ", code("NA"), " for a skipped item. Separators ",
                                              "(spaces, commas, newlines) are optional."),
                                            textAreaInput("rapid_text", NULL, rows = 8, width = "100%",
                                                          placeholder = "e.g.  2 1 1 3 4 1 2 ..."),
                                            actionButton("apply_rapid", "Apply to responses",
                                                         class = "btn btn-default"),
                                            span(style = "margin-left:12px;color:#5A6478;",
                                                 textOutput("rapid_status", inline = TRUE))
                                        )
                       ),
                       
                       conditionalPanel("input.entry_mode == 'upload'",
                                        div(class = "card",
                                            p("Upload a one-row CSV with columns ", code("hsr001"), "\u2026",
                                              code("hsr405"), " (or simply 405 values in item order)."),
                                            fileInput("csv_file", NULL, accept = ".csv")
                                        )
                       ),
                       
                       div(class = "card", style = "text-align:center;",
                           actionButton("submit_btn", "Submit",
                                        class = "btn btn-primary btn-lg"),
                           actionButton("clear_btn", "Clear all",
                                        class = "btn btn-default btn-lg",
                                        style = "margin-left:10px;"),
                           div(style = "margin-top:8px;", textOutput("submit_msg"))
                       )
              ),
              
              # ---------------- Panel 1: circular profile ---------------------------
              tabPanel("2 \u00B7 All scales", br(),
                       div(class = "card",
                           fluidRow(
                             column(3, style = "padding-top:4px;",
                                    checkboxInput("show_t", "Preliminary T-scores", FALSE),
                                    checkboxInput("show_comp",
                                                  "Include HiTOP-BR spectrum scale scores", FALSE)),
                             column(3,
                                    checkboxInput("emp_org",
                                                  "Order scales by rational assignment to spectra", FALSE),
                                    checkboxInput("show_subs",
                                                  "Include rationally derived subscales", FALSE)),
                             column(2, conditionalPanel("input.show_t",
                                                        selectInput("norm_group", "Reference norms (placeholder)",
                                                                    c("Combined" = "pool",
                                                                      "Community (Prolific)"   = "pro",
                                                                      "Students (KU)"          = "ku")))),
                             column(4, style = "text-align:right;padding-top:24px;",
                                    checkboxInput("show_err",
                                                  if (hitop_has_intervals)
                                                    "95% score intervals (Schmukle, 2026)"
                                                  else "Error bars (\u00B11 SE of item-response mean)",
                                                  TRUE),
                                    downloadButton("dl_plot", "Download PNG"))
                           ),
                           conditionalPanel("input.show_t",
                                            div(class = "callout-warn",
                                                strong("Preliminary T-scores: "),
                                                "based on placeholder norms that are not population-",
                                                "representative and not endorsed by the HiTOP Society, using ",
                                                "a linear transformation. Because most scales are ",
                                                "floor-constrained, the lowest possible raw score can still ",
                                                "correspond to a T-score well above 40 on many scales, and ",
                                                "high raw scores can exceed T = 100; percentile-based norms ",
                                                "are planned. Not validated for clinical interpretation.")),
                           conditionalPanel("input.show_comp",
                                            div(class = "callout-warn",
                                                strong("HiTOP-BR spectrum scale scores: "),
                                                "spectrum scores use the HiTOP-BR items embedded within the ",
                                                "HiTOP-SR. This is not a validated scoring of the ",
                                                "higher-order structure, and spectrum T-scores reference the ",
                                                "student sample only.")),
                           conditionalPanel("input.emp_org",
                                            div(class = "callout-warn",
                                                strong("Rational ordering of scales: "),
                                                "sorts scales according to the rational scale-to-spectrum ",
                                                "assignment from the HiTOP-SR paper (Simms et al., under ",
                                                "review).")),
                           conditionalPanel("input.show_subs",
                                            div(class = "callout-warn",
                                                strong("Rationally derived subscales (marked \u02B3): "),
                                                "these were not indicated by data in the scale development ",
                                                "process, but were developed when conceptual or practical ",
                                                "considerations indicated that subdividing a scale was ",
                                                "necessary to preserve important content for clinical ",
                                                "applications.")),
                           if (hitop_has_intervals)
                             conditionalPanel("input.show_err", div(class = "anchor-note",
                                                                    strong("Reading the intervals: "),
                                                                    "the bar shows the observed score. The diamond shows the ",
                                                                    "estimated true score (", a("Schmukle, 2026", href = "https://doi.org/10.1177/10731911251362532", target = "_blank"), "), adjusted toward the ",
                                                                    "average of the instrument's development sample (about 780 ",
                                                                    "online participants), and can therefore differ from the ",
                                                                    "observed score shown by the bar. The whiskers show the 95% ",
                                                                    "interval around that estimate. Intervals are not shown for ",
                                                                    "scales with missing responses or for HiTOP-BR spectrum ",
                                                                    "scores. Where an interval reaches past the lowest or highest ",
                                                                    "possible score, the axis widens to show it and a dashed line ",
                                                                    "marks the response-scale boundary.")),
                           div(class = "anchor-note",
                               "Hover any bar or label to isolate it; all other bars gray out. ",
                               "Raw scale scores (1\u20134) are listed alphabetically by ",
                               "default. Subscales are italicised and listed under the scale ",
                               "that they parse in more detail. Click any bar or open the ",
                               "next tab for the detailed group-level view and item ",
                               "responses.")
                       ),
                       div(class = "card",
                           withLoader(girafeOutput("circular_plot", height = "auto"),
                                      type = "image", loader = "hitop_loader.gif"))
              ),
              
              # ---------------- Panel 3: spectrum drill-down --------------------------
              tabPanel("3 \u00B7 Spectrum detail", br(),
                       div(class = "card",
                           fluidRow(
                             column(5, selectInput("detail_spectrum", "Scale group",
                                                   choices = hitop_alt_order),
                                    checkboxInput("show_err_d",
                                                  if (hitop_has_intervals)
                                                    "95% score intervals (Schmukle, 2026)"
                                                  else "Error bars (\u00B11 SE of item-response mean)",
                                                  TRUE)),
                             column(7, div(class = "anchor-note", style = "padding-top:26px;",
                                           "Click any bar in the all-scales view to jump here. ",
                                           "Click a scale bar below to see the item responses behind it."))
                           ),
                           conditionalPanel("input.show_t",
                                            div(class = "callout-warn",
                                                strong("Preliminary T-scores: "),
                                                "based on placeholder norms that are not population-",
                                                "representative and not endorsed by the HiTOP Society, using ",
                                                "a linear transformation. Because most scales are ",
                                                "floor-constrained, the lowest possible raw score can still ",
                                                "correspond to a T-score well above 40 on many scales, and ",
                                                "high raw scores can exceed T = 100; percentile-based norms ",
                                                "are planned. Not validated for clinical interpretation.")),
                           conditionalPanel("input.show_comp",
                                            div(class = "callout-warn",
                                                strong("HiTOP-BR spectrum scale scores: "),
                                                "spectrum scores use the HiTOP-BR items embedded within the ",
                                                "HiTOP-SR; subfactor scores are rational scale means. Neither ",
                                                "approach is a validated scoring of the higher-order ",
                                                "structure, and spectrum T-scores reference the student ",
                                                "sample only."))
                       ),
                       if (hitop_has_intervals)
                         conditionalPanel("input.show_err_d", div(class = "anchor-note",
                                                                  strong("Reading the intervals: "),
                                                                  "the bar shows the observed score. The diamond shows the ",
                                                                  "estimated true score (", a("Schmukle, 2026", href = "https://doi.org/10.1177/10731911251362532", target = "_blank"), "), adjusted toward the ",
                                                                  "average of the instrument's development sample (about 780 ",
                                                                  "online participants), and can therefore differ from the ",
                                                                  "observed score shown by the bar. The whiskers show the 95% ",
                                                                  "interval around that estimate. Intervals are not shown for ",
                                                                  "scales with missing responses or for HiTOP-BR spectrum ",
                                                                  "scores. Where an interval reaches past the lowest or highest ",
                                                                  "possible score, the axis widens to show it and a dashed line ",
                                                                  "marks the response-scale boundary.")),
                       div(class = "card",
                           withLoader(girafeOutput("detail_plot", height = "auto"),
                                      type = "image", loader = "hitop_loader.gif")),
                       div(class = "card", uiOutput("item_panel"))
              )
  ),
  
  # JS goes last so item texts are available
  tags$script(HTML(kgrid_js)),
  tags$script(src = "oneko.js")
)

server <- function(input, output, session) {
  
  responses <- reactiveVal(rep(NA_real_, N_ITEMS))
  submitted <- reactiveVal(NULL)        # bar data (raw units)
  submitted_items <- reactiveVal(NULL)  # raw item vector at submit time
  sel_spectrum <- reactiveVal(hitop_alt_order[1])
  sel_scale    <- reactiveVal(NULL)
  
  output$progress <- renderText({
    sprintf("%d / %d entered", sum(!is.na(responses())), N_ITEMS)
  })
  
  sync_grid <- function(r) {
    vals <- ifelse(is.na(r), "", as.character(r))
    session$sendCustomMessage("set_grid", as.list(vals))
  }
  
  # ---- keyboard grid -> R -------------------------------------------------
  observeEvent(input$kgrid_vals, {
    ch <- strsplit(input$kgrid_vals, "")[[1]]
    if (length(ch) != N_ITEMS) return()
    r <- suppressWarnings(as.numeric(ch))   # '.', 'x' -> NA
    responses(r)
  })
  
  # ---- random demo responses ----------------------------------------------
  observeEvent(input$load_example, {
    r <- sample(1:4, N_ITEMS, replace = TRUE)
    responses(r)
    sync_grid(r)
    showNotification("Random demo responses loaded.", type = "message")
  })
  
  # ---- random demo with missing responses ----------------------------------
  observeEvent(input$load_example_missing, {
    r <- sample(1:4, N_ITEMS, replace = TRUE)
    n_miss <- round(runif(1, 0.01, 0.12) * N_ITEMS)
    r[sample(N_ITEMS, n_miss)] <- NA
    responses(r)
    sync_grid(r)
    showNotification(
      sprintf("Random demo loaded with %d missing responses.", sum(is.na(r))),
      type = "message")
  })
  
  # ---- clear all (with confirmation) ----------------------------------------
  observeEvent(input$clear_btn, {
    if (sum(!is.na(responses())) == 0) {
      showNotification("Nothing to clear.", type = "message"); return()
    }
    showModal(modalDialog(
      title = "Clear all responses?",
      sprintf("This will erase all %d entered responses.",
              sum(!is.na(responses()))),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("clear_confirm", "Clear all",
                     class = "btn btn-danger"))))
  })
  observeEvent(input$clear_confirm, {
    responses(rep(NA_real_, N_ITEMS))
    sync_grid(rep(NA_real_, N_ITEMS))
    updateTextAreaInput(session, "rapid_text", value = "")
    output$submit_msg <- renderText("")
    removeModal()
    showNotification("All responses cleared.", type = "message")
  })
  
  # ---- paste --------------------------------------------------------------
  observeEvent(input$apply_rapid, {
    toks <- regmatches(input$rapid_text,
                       gregexpr("[1-4]|NA|na|x|X", input$rapid_text))[[1]]
    vals <- suppressWarnings(as.numeric(toks))
    if (length(vals) == 0) {
      output$rapid_status <- renderText("No responses found."); return()
    }
    if (length(vals) > N_ITEMS) vals <- vals[1:N_ITEMS]
    r <- responses(); r[seq_along(vals)] <- vals
    responses(r); sync_grid(r)
    output$rapid_status <- renderText(
      sprintf("Parsed %d responses (%d missing).",
              length(vals), sum(is.na(vals))))
  })
  
  # ---- csv upload ---------------------------------------------------------
  observeEvent(input$csv_file, {
    df <- try(read.csv(input$csv_file$datapath), silent = TRUE)
    if (inherits(df, "try-error") || nrow(df) < 1) {
      showNotification("Could not read that CSV.", type = "error"); return()
    }
    hsr_cols <- grep("^hsr\\d{3}$", names(df), value = TRUE)
    vals <- if (length(hsr_cols) == N_ITEMS) {
      as.numeric(df[1, paste0("hsr", sprintf("%03d", 1:N_ITEMS))])
    } else {
      suppressWarnings(as.numeric(df[1, ]))[1:N_ITEMS]
    }
    vals[!(vals %in% 1:4)] <- NA
    responses(vals); sync_grid(vals)
    showNotification(sprintf("Loaded %d responses from CSV.",
                             sum(!is.na(vals))), type = "message")
  })
  
  # ---- submit -------------------------------------------------------------
  observeEvent(input$submit_btn, {
    r <- responses()
    n_miss <- sum(is.na(r))
    if (n_miss == N_ITEMS) {
      output$submit_msg <- renderText("No responses entered yet."); return()
    }
    bars <- build_individual_bars(r, item_key, hierarchy, br_map)
    submitted(bars); submitted_items(r)   # default-org bars for the modal
    
    sup <- bars$name[bars$level == "scale" & bars$flag == "suppressed"]
    pro <- bars$name[bars$level == "scale" & bars$flag == "prorated"]
    pct <- 100 * n_miss / N_ITEMS
    
    output$submit_msg <- renderText(
      if (n_miss == 0) "Results calculated with complete data."
      else sprintf(
        "Results calculated with %d missing item(s) (%.1f%%): %d scale(s) prorated*, %d not scored.",
        n_miss, pct, length(pro), length(sup)))
    
    if (pct > 10 || length(sup) > 0) {
      showModal(modalDialog(
        title = "Missing-data warning",
        tags$p(sprintf(
          "%d of %d items (%.1f%%) are unanswered.", n_miss, N_ITEMS, pct)),
        if (length(sup) > 0) tagList(
          tags$p(tags$b(sprintf(
            "%d scale(s) were NOT scored (more than 25%% of their items missing):",
            length(sup))), style = "margin-bottom:4px;"),
          tags$p(paste(sup, collapse = ", "),
                 style = "color:#8A1F1F;")),
        if (length(pro) > 0) tags$p(sprintf(
          "%d scale(s) are prorated from available items (marked * in charts); prorated scores can be biased even when items are missing at random, so interpret them with caution.",
          length(pro))),
        tags$p("Composites built on incomplete scales are also marked *. ",
               "If possible, return to the entry tab and fill in the ",
               "missing items before interpreting the results."),
        easyClose = TRUE, footer = modalButton("Understood")))
    }
    updateTabsetPanel(session, "main_tabs", selected = "2 \u00B7 All scales")
  })
  
  # ---- shared T-scored bars ----------------------------------------------
  norm_cols <- reactive(switch(input$norm_group,
                               pool = c(mean = "mean_pool", sd = "sd_pool"),
                               pro  = c(mean = "mean_pro",  sd = "sd_pool"),
                               ku   = c(mean = "mean_ku",   sd = "sd_pool")))
  
  active_colors <- reactive({
    pal <- if (isTRUE(input$emp_org)) hitop_alt_colors
    else c("Scales" = "#2B3445")
    if (isTRUE(input$show_comp))
      pal <- c("HiTOP-BR spectra" = "#1E3A5F", pal)
    pal
  })
  active_order <- reactive(names(active_colors()))
  
  bars_display <- reactive({
    req(submitted_items())
    h <- hierarchy_alt
    if (!isTRUE(input$emp_org)) h$Spectrum <- "Scales"
    bm <- NULL
    if (isTRUE(input$show_comp)) {
      bm <- br_map; bm$family <- "HiTOP-BR spectra"
    }
    ss <- if (isTRUE(input$show_subs)) subs_all
    else subs_all[subs_all$type == "empirical", ]
    bars <- build_individual_bars(submitted_items(), item_key, h,
                                  br_map = bm, subscales = ss)
    if (!"parent" %in% names(bars)) bars$parent <- NA_character_
    keep <- c("scale", "subscale", if (isTRUE(input$show_comp)) "spectrum")
    bars <- bars[bars$level %in% keep, ]
    if (hitop_has_intervals) {
      if (isTRUE(input$show_err)) bars <- add_score_intervals(bars)
      else bars$lo <- bars$hi <- bars$est <- NA_real_
    } else if (!isTRUE(input$show_err)) bars$lo <- bars$hi <- NA_real_
    if (isTRUE(input$show_t)) bars <- apply_norms(bars, norms, norm_cols())
    bars
  })
  
  # panel 3 bars: always the empirical grouping; subscales on demand
  detail_bars <- reactive({
    req(submitted_items())
    bars <- build_individual_bars(
      submitted_items(), item_key, hierarchy_alt,
      subscales = if (isTRUE(input$show_subs)) subs_all
      else subs_all[subs_all$type == "empirical", ])
    if (!"parent" %in% names(bars)) bars$parent <- NA_character_
    bars <- bars[bars$level %in% c("scale", "subscale"), ]
    if (hitop_has_intervals) {
      if (isTRUE(input$show_err)) bars <- add_score_intervals(bars)
      else bars$lo <- bars$hi <- bars$est <- NA_real_
    } else if (!isTRUE(input$show_err)) bars$lo <- bars$hi <- NA_real_
    if (isTRUE(input$show_t)) bars <- apply_norms(bars, norms, norm_cols())
    bars
  })
  
  profile_plot <- reactive(
    plot_hitop_horizontal(bars_display(), defs = scale_defs,
                          tscore = isTRUE(input$show_t),
                          spectrum_colors = active_colors(),
                          spectrum_order = active_order()))
  
  output$circular_plot <- renderGirafe({
    b <- bars_display()
    n <- nrow(b) + if (length(unique(b$spectrum)) > 1)
      length(unique(b$spectrum)) else 0
    hitop_girafe(profile_plot(), w = 9.5, h = max(4, 0.145 * n + 1.5))
  })
  
  output$dl_plot <- downloadHandler(
    filename = function() sprintf("hitop_profile_%s.png", Sys.Date()),
    content = function(file) {
      b <- bars_display()
      n <- nrow(b) + if (length(unique(b$spectrum)) > 1)
        length(unique(b$spectrum)) else 0
      ggsave(file, profile_plot(), width = 10,
             height = max(4, 0.145 * n + 1.5), dpi = 200, bg = "white")
    })
  
  # ---- click routing: circular profile -> panel 3 --------------------------
  observeEvent(input$circular_plot_selected, {
    nm <- input$circular_plot_selected
    req(nm)
    hh <- hierarchy_alt
    if (nm %in% names(sub_parent)) nm <- sub_parent[[nm]]
    if (nm %in% hh$Spectrum) {
      sel_spectrum(nm); sel_scale(NULL)
    } else if (nm %in% hh$Scale) {
      sel_spectrum(hh$Spectrum[match(nm, hh$Scale)])
      sel_scale(nm)
    } else return()
    updateSelectInput(session, "detail_spectrum", selected = sel_spectrum())
    updateTabsetPanel(session, "main_tabs",
                      selected = "3 \u00B7 Spectrum detail")
  })
  
  # keep the two interval checkboxes (tab 2 and tab 3) in lockstep
  observeEvent(input$show_err, {
    if (!identical(input$show_err, input$show_err_d))
      updateCheckboxInput(session, "show_err_d", value = input$show_err)
  })
  observeEvent(input$show_err_d, {
    if (!identical(input$show_err, input$show_err_d))
      updateCheckboxInput(session, "show_err", value = input$show_err_d)
  })
  
  observeEvent(input$detail_spectrum, {
    if (!identical(input$detail_spectrum, sel_spectrum())) {
      sel_spectrum(input$detail_spectrum); sel_scale(NULL)
    }
  })
  
  # ---- panel 3: spectrum detail chart --------------------------------------
  observeEvent(input$detail_plot_selected, {
    nm <- input$detail_plot_selected
    req(nm)
    if (nm %in% names(sub_parent)) nm <- sub_parent[[nm]]
    if (nm %in% hierarchy_alt$Scale) sel_scale(nm)
    else showNotification("Click a scale bar to see its items.",
                          type = "message")
  })
  
  output$detail_plot <- renderGirafe({
    req(detail_bars())
    d <- detail_bars()[detail_bars()$spectrum == sel_spectrum(), ]
    h <- max(2, 0.30 * nrow(d) + 1)
    hitop_girafe(plot_spectrum_detail(detail_bars(), sel_spectrum(),
                                      defs = scale_defs,
                                      tscore = isTRUE(input$show_t),
                                      spectrum_colors = hitop_alt_colors),
                 w = 9.5, h = h)
  })
  
  # ---- panel 3: item responses for a clicked scale --------------------------
  output$item_panel <- renderUI({
    if (is.null(submitted_items()))
      return(p(class = "anchor-note", "Submit responses first."))
    if (is.null(sel_scale()))
      return(p(class = "anchor-note",
               "Click a scale bar above to see the item responses."))
    
    sc  <- sel_scale()
    cam <- hierarchy_alt$camel[match(sc, hierarchy_alt$Scale)]
    ki  <- item_key[item_key$camel == cam, ]
    r   <- submitted_items()[ki$item]
    scored <- ifelse(ki$reverse, 5 - r, r)
    ord <- order(-ifelse(is.na(scored), -1, scored), ki$item)
    
    tb <- detail_bars()
    trow <- tb[tb$level == "scale" & tb$name == sc, ]
    score_txt <- if (isTRUE(input$show_t))
      sprintf("T = %.1f (%s)", trow$mean, hitop_severity_label(trow$mean))
    else sprintf("mean score = %.2f", trow$mean)
    
    rows <- lapply(ord, function(j) {
      v <- r[j]
      chip <- if (is.na(v))
        span(class = "chip cna", "\u00D7")
      else
        span(class = paste0("chip c", v), v)
      div(class = "irow",
          span(class = "inum", ki$item[j]),
          span(class = "itext", ki$text[j],
               if (ki$reverse[j])
                 span(class = "irev",
                      sprintf(" (reverse-scored; counts as %s)",
                              ifelse(is.na(v), "\u2014", 5 - v)))),
          chip)
    })
    
    tagList(
      h4(sprintf("%s \u2014 item responses", sc)),
      div(class = "anchor-note", sprintf(
        "%d items \u00B7 1 = not at all, 2 = a little, 3 = moderately, 4 = a lot (past 12 months) \u00B7 %s \u00B7 sorted by response, highest first",
        nrow(ki), score_txt)),
      rows
    )
  })
}

shinyApp(ui, server)