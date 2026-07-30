library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)


# wrap a definition for tooltip display; defs is a named vector (name -> text)
tip_def <- function(name, defs) {
  if (is.null(defs)) return("")
  d <- defs[name]
  ifelse(is.na(d), "",
         vapply(d, function(x) paste0("\n\n", paste(strwrap(x, 46),
                                                    collapse = "\n")), character(1)))
}

tip_missing <- function(flag, n_answered, n_total) {
  ifelse(flag == "suppressed",
         sprintf("\nNOT SCORED \u2014 more than 25%% of items missing (%d/%d answered)",
                 n_answered, n_total),
         ifelse(flag == "prorated",
                sprintf("\nprorated from %d/%d items \u2014 interpret with caution",
                        n_answered, n_total),
                ifelse(flag == "partial",
                       "\nincludes incomplete scales \u2014 interpret with caution", "")))
}

hitop_severity_label <- function(t) {
  cut(t, c(-Inf, 60, 65, 70, Inf),
      labels = c("minimal", "mild", "moderate", "severe"))
}

hitop_spectrum_colors <- c(
  "Internalizing"                  = "#3B5BDB",
  "Internalizing-Thought Disorder" = "#7048E8",
  "Thought Disorder"               = "#AE3EC9",
  "Detachment"                     = "#0CA678",
  "Externalizing"                  = "#E8590C",
  "Somatoform"                     = "#E03131"
)
hitop_spectrum_order <- names(hitop_spectrum_colors)
hitop_level_darken   <- c(spectrum = 0, subfactor = 0.25, scale = 0.45,
                          subscale = 0.6)

# Alternative organization (measure paper, Table 1): 12 groups
hitop_alt_colors <- c(
  "Somatoform"                      = "#E03131",
  "Internalizing-Distress"          = "#364FC7",
  "Internalizing-Fear"              = "#1C7ED6",
  "Internalizing-Eating/Body Image" = "#15AABF",
  "Internalizing-Sexual Problems"   = "#7048E8",
  "Thought Disorder"                = "#AE3EC9",
  "Detachment"                      = "#0CA678",
  "Disinhibited Externalizing"      = "#E8590C",
  "Overcontrol"                     = "#F59F00",
  "Antagonistic Externalizing"      = "#C2255C",
  "Antisocial"                      = "#0B7285",
  "Unassigned"                      = "#868E96"
)
hitop_alt_order <- names(hitop_alt_colors)

hitop_severity_bands <- data.frame(
  band  = factor(c("minimal", "mild", "moderate", "severe"),
                 levels = c("severe", "moderate", "mild", "minimal")),
  lo    = c(-Inf, 60, 65, 70),
  hi    = c(60, 65, 70, Inf),
  # shades of gray rendered as black at increasing opacity over white
  alpha = c(minimal = 0.02, mild = 0.10, moderate = 0.20, severe = 0.32)
)

#' Per-participant scores at all three hierarchy levels.
#' scale_scores: 76 columns named `prefix` + camelCase (score_hitopsr output).
#' hierarchy: hitopsr_hierarchy.csv (Spectrum, Subfactor, Scale).
build_hitop_levels <- function(scale_scores, hierarchy, prefix = "hsr_") {
  to_camel <- function(x) {
    x <- gsub("[^A-Za-z0-9 ]", " ", x)
    parts <- strsplit(trimws(gsub("\\s+", " ", x)), " ")
    vapply(parts, function(p) {
      p <- tolower(p)
      if (length(p) > 1)
        p[-1] <- paste0(toupper(substring(p[-1], 1, 1)), substring(p[-1], 2))
      paste(p, collapse = "")
    }, character(1))
  }
  
  hierarchy <- hierarchy |>
    mutate(col = paste0(prefix, to_camel(Scale)),
           Subfactor = ifelse(is.na(Subfactor) | Subfactor == "NA",
                              NA, Subfactor))
  missing <- setdiff(hierarchy$col, names(scale_scores))
  if (length(missing) > 0)
    stop("Scale columns not found: ", paste(missing, collapse = ", "))
  
  scores <- scale_scores |>
    mutate(.pid = row_number()) |>
    select(.pid, all_of(hierarchy$col)) |>
    pivot_longer(-".pid", names_to = "col", values_to = "score") |>
    left_join(hierarchy, by = "col")
  
  bind_rows(
    scores |>
      summarise(score = mean(score, na.rm = TRUE), .by = c(.pid, Spectrum)) |>
      transmute(.pid, level = "spectrum", name = Spectrum,
                spectrum = Spectrum, subfactor = NA_character_, score),
    scores |>
      filter(!is.na(Subfactor)) |>
      summarise(score = mean(score, na.rm = TRUE),
                .by = c(.pid, Spectrum, Subfactor)) |>
      transmute(.pid, level = "subfactor", name = Subfactor,
                spectrum = Spectrum, subfactor = Subfactor, score),
    scores |>
      transmute(.pid, level = "scale", name = Scale,
                spectrum = Spectrum, subfactor = Subfactor, score)
  )
}

#' One row per bar: mean and error bounds in RAW score units.
summarize_hitop <- function(long_scores,
                            error = c("ci95", "sem", "sd", "none")) {
  error <- match.arg(error)
  if (dplyr::n_distinct(long_scores$.pid) < 2 && error != "none")
    error <- "none"
  
  long_scores |>
    summarise(mean = mean(score, na.rm = TRUE),
              sd = sd(score, na.rm = TRUE),
              n = sum(!is.na(score)),
              .by = c(level, name, spectrum, subfactor)) |>
    mutate(err = case_when(error == "ci95" ~ 1.96 * sd / sqrt(n),
                           error == "sem"  ~ sd / sqrt(n),
                           error == "sd"   ~ sd,
                           TRUE            ~ NA_real_),
           lo = mean - err,
           hi = mean + err)
}

#' Convert raw-unit bar data to T-scores using the preliminary norms.
#' norms: hitopsr_norms.csv (level, name, mean_pool, sd_pool, ...).
#' norm_cols lets the app switch reference group later,
#' e.g. c(mean = "mean_pro", sd = "sd_pro").
apply_norms <- function(bar_data, norms,
                        norm_cols = c(mean = "mean_pool", sd = "sd_pool")) {
  nm <- norms |>
    transmute(level, name,
              norm_mean = .data[[norm_cols[["mean"]]]],
              norm_sd   = .data[[norm_cols[["sd"]]]])
  out <- bar_data |> left_join(nm, by = c("level", "name"))
  if (anyNA(out$norm_mean))
    warning("No norms for: ",
            paste(unique(out$name[is.na(out$norm_mean)]), collapse = ", "))
  tt <- function(x, m, s) 50 + 10 * (x - m) / s
  out |>
    mutate(mean = tt(mean, norm_mean, norm_sd),
           lo   = tt(lo,   norm_mean, norm_sd),
           hi   = tt(hi,   norm_mean, norm_sd))
}

#' Circular bar chart. With tscore = TRUE (default), the radial axis is in
#' T units with shaded severity rings; tscore = FALSE plots raw scores.
plot_hitop_circular <- function(bar_data,
                                defs = NULL,
                                tscore = TRUE,
                                t_floor = 30, t_ceil = 85,
                                score_range = c(1, 4),
                                gap = 2,
                                base_size = 11) {
  
  lvl_rank <- c(spectrum = 1, subfactor = 2, scale = 3)
  
  df <- bar_data |>
    mutate(spectrum = factor(spectrum, levels = hitop_spectrum_order),
           lvl_rank = lvl_rank[level]) |>
    arrange(spectrum,
            !is.na(subfactor) | level != "spectrum",
            subfactor, lvl_rank, name) |>
    group_by(spectrum) |> mutate(.within = row_number()) |> ungroup()
  
  spec_sizes <- df |> count(spectrum, name = "n_bars") |>
    mutate(offset = cumsum(dplyr::lag(n_bars + gap, default = 0)))
  df <- df |>
    left_join(spec_sizes |> select(spectrum, offset), by = "spectrum") |>
    mutate(x = .within + offset)
  total_slots <- max(df$x) + gap
  
  df <- df |>
    mutate(fill = colorspace::darken(
      hitop_spectrum_colors[as.character(spectrum)],
      hitop_level_darken[level]))
  
  # radial scale setup
  if (tscore) {
    floor_y <- t_floor
    df <- df |>
      mutate(mean_c = pmin(pmax(mean, floor_y), t_ceil),
             lo_c   = pmin(pmax(lo, floor_y), t_ceil),
             hi_c   = pmin(pmax(hi, floor_y), t_ceil),
             capped = mean > t_ceil)
    ring_breaks <- seq(40, 80, 10)
    axis_top    <- t_ceil
  } else {
    floor_y <- score_range[1]
    df <- df |> mutate(mean_c = mean,
                       lo_c = pmax(lo, floor_y), hi_c = hi, capped = FALSE)
    ring_breaks <- seq(score_range[1], score_range[2], 1)
    axis_top    <- score_range[2]
  }
  label_r <- axis_top + (axis_top - floor_y) * 0.03
  hollow  <- floor_y - (axis_top - floor_y) * 0.75
  
  df <- df |>
    mutate(deg   = 90 - 360 * (x - 0.5) / total_slots,
           hjust = ifelse(deg < -90, 1, 0),
           angle = ifelse(deg < -90, deg + 180, deg),
           lab   = ifelse(level == "spectrum", toupper(name), name),
           face  = case_when(level == "spectrum" ~ "bold",
                             level == "subfactor" ~ "bold.italic",
                             TRUE ~ "plain"),
           # tooltip + hover identity for interactive rendering
           flag = if ("flag" %in% names(bar_data)) flag else "ok",
           n_answered = if ("n_answered" %in% names(bar_data)) n_answered else NA,
           n_total = if ("n_total" %in% names(bar_data)) n_total else NA,
           tip = paste0(
             ifelse(is.na(mean), name,
                    if (tscore) {
                      sprintf("%s\nT = %.1f (%s)\n%s level",
                              name, mean, hitop_severity_label(mean), level)
                    } else {
                      sprintf("%s\nscore = %.2f\n%s level", name, mean, level)
                    }),
             tip_missing(flag, n_answered, n_total),
             tip_def(name, defs)),
           lab = ifelse(flag == "prorated" | flag == "partial",
                        paste0(lab, "*"), lab),
           labcol = ifelse(flag == "suppressed", "grey60", fill))
  
  p <- ggplot(df)
  
  # severity band annuli (T-score mode only), gray shades + top-right legend
  if (tscore) {
    bands <- hitop_severity_bands |>
      mutate(lo = pmax(lo, floor_y), hi = pmin(hi, axis_top))
    p <- p +
      geom_rect(data = bands,
                aes(xmin = 0, xmax = total_slots,
                    ymin = lo, ymax = hi, alpha = band),
                fill = "grey10") +
      scale_alpha_manual(
        values = setNames(bands$alpha, bands$band),
        name = "T-score (based on preliminary norms)",
        labels = c(severe = "severe (T \u2265 70)",
                   moderate = "moderate (65\u201370)",
                   mild = "mild (60\u201365)",
                   minimal = "minimal (< 60)"),
        guide = guide_legend(override.aes = list(fill = "grey10"))
      ) +
      geom_hline(yintercept = c(60, 65, 70),
                 color = "grey55", linewidth = 0.25, linetype = "31")
  }
  show_tag <- any(df$flag %in% c("prorated", "partial"))
  
  out <- p +
    geom_hline(yintercept = ring_breaks, color = "grey85", linewidth = 0.3) +
    geom_rect_interactive(
      data = df |> filter(!is.na(mean)),
      aes(xmin = x - 0.46, xmax = x + 0.46,
          ymin = floor_y, ymax = mean_c, fill = fill,
          data_id = name, tooltip = tip),
      show.legend = FALSE) +
    geom_point_interactive(
      data = df |> filter(is.na(mean)),
      aes(x = x, y = floor_y + (axis_top - floor_y) * 0.04,
          data_id = name, tooltip = tip),
      shape = 4, size = 1.4, stroke = 0.7, color = "grey55") +
    geom_errorbar(aes(x = x, ymin = lo_c, ymax = hi_c),
                  width = 0.38, linewidth = 0.9, color = "white",
                  na.rm = TRUE) +
    geom_errorbar(aes(x = x, ymin = lo_c, ymax = hi_c),
                  width = 0.35, linewidth = 0.3, color = "grey15",
                  na.rm = TRUE) +
    # arrow marker for bars clipped at the ceiling
    geom_point(data = df |> filter(capped),
               aes(x = x, y = axis_top), shape = 17, size = 1.2,
               color = "grey15") +
    geom_text_interactive(
      aes(x = x, y = label_r, label = lab, angle = angle,
          hjust = hjust, fontface = face, color = labcol,
          data_id = name, tooltip = tip),
      size = base_size * 0.18, show.legend = FALSE) +
    annotate("text", x = total_slots, y = ring_breaks, label = ring_breaks,
             size = base_size * 0.22, color = "grey45") +
    scale_fill_identity() + scale_color_identity() +
    scale_x_continuous(limits = c(0, total_slots), expand = c(0, 0)) +
    scale_y_continuous(limits = c(hollow, label_r +
                                    (axis_top - floor_y) * 0.55)) +
    coord_polar(start = 0, clip = "off") +
    theme_void(base_size = base_size) +
    theme(plot.margin = margin(4, 4, 4, 4),
          legend.position = "inside",
          legend.position.inside = c(0.99, 0.99),
          legend.justification = c(1, 1),
          legend.title = element_text(size = base_size * 0.85,
                                      face = "bold", color = "grey25"),
          legend.text = element_text(size = base_size * 0.78,
                                     color = "grey25"),
          legend.key.size = unit(base_size * 1.1, "pt"),
          legend.key = element_rect(color = "grey75", linewidth = 0.3))
  
  if (show_tag) {
    out <- out +
      labs(tag = "* included one or more missing responses") +
      theme(plot.tag.location = "plot",
            plot.tag.position = c(0.985, 0.872),
            plot.tag = element_text(size = base_size * 0.72,
                                    color = "grey30", hjust = 1))
  }
  out
}

# =============================================================================
# Individual-level helpers (clinician single-client workflow)
# =============================================================================

#' Score one client's 405 item responses into bar data with item-level SEMs.
#' resp: numeric vector of length 405 (values 1-4 or NA), administration order.
#' key: hitopsr_item_key.csv (item, text, reverse, camel)
#' hierarchy: hitopsr_hierarchy.csv (Spectrum, Subfactor, Scale, camel)
#' Error bars: scale = SEM across the scale's items; subfactor/spectrum =
#' SEM across constituent scale scores. These reflect internal consistency
#' of the profile, not test-retest precision.
build_individual_bars <- function(resp, key, hierarchy, br_map = NULL,
                                  subscales = NULL,
                                  ci = 1, max_missing = 0.25) {
  stopifnot(length(resp) == nrow(key))
  r <- as.numeric(resp)
  r[key$reverse] <- 5 - r[key$reverse]
  
  per_scale <- lapply(split(r, key$camel), function(x) {
    n_tot <- length(x); x <- x[!is.na(x)]
    c(mean = if (length(x) / n_tot >= 1 - max_missing) mean(x) else NA,
      sem = if (length(x) > 1) sd(x) / sqrt(length(x)) else NA,
      n_answered = length(x), n_total = n_tot)
  })
  sc <- data.frame(camel = names(per_scale),
                   do.call(rbind, per_scale), row.names = NULL)
  sc <- merge(hierarchy, sc, by = "camel")
  sc$Subfactor[sc$Subfactor == "NA" | sc$Subfactor == ""] <- NA
  sc$flag <- ifelse(is.na(sc$mean), "suppressed",
                    ifelse(sc$n_answered < sc$n_total, "prorated", "ok"))
  
  scale_bars <- data.frame(
    level = "scale", name = sc$Scale, spectrum = sc$Spectrum,
    subfactor = sc$Subfactor, mean = sc$mean,
    lo = sc$mean - ci * sc$sem, hi = sc$mean + ci * sc$sem,
    n_answered = sc$n_answered, n_total = sc$n_total, flag = sc$flag
  )
  
  comp <- function(df, level, name, spectrum, subfactor) {
    ok <- !is.na(df$mean)
    m <- if (any(ok)) mean(df$mean[ok]) else NA
    sem <- if (sum(ok) > 1) sd(df$mean[ok]) / sqrt(sum(ok)) else NA
    flag <- if (!any(ok)) "suppressed"
    else if (any(df$flag != "ok")) "partial" else "ok"
    data.frame(level = level, name = name, spectrum = spectrum,
               subfactor = subfactor, mean = m,
               lo = m - ci * sem, hi = m + ci * sem,
               n_answered = sum(df$n_answered),
               n_total = sum(df$n_total), flag = flag)
  }
  
  subf_bars <- do.call(rbind, lapply(
    split(sc[!is.na(sc$Subfactor), ],
          paste(sc$Spectrum, sc$Subfactor)[!is.na(sc$Subfactor)]),
    function(d) comp(d, "subfactor", d$Subfactor[1], d$Spectrum[1],
                     d$Subfactor[1])))
  
  # Spectrum composites: HiTOP-BR item scoring within the SR responses
  # (Miri's proposal). Each BR spectrum is the mean of its BR items; bars
  # display within the rational family given by br_map$family. Falls back
  # to rational scale-mean spectra if no br_map is supplied.
  if (!is.null(br_map)) {
    spec_bars <- do.call(rbind, lapply(split(br_map, br_map$br_spectrum),
                                       function(m) {
                                         x <- r[m$item]; n_tot <- length(x); x <- x[!is.na(x)]
                                         ok <- length(x) / n_tot >= 1 - max_missing
                                         mn <- if (ok) mean(x) else NA
                                         sem <- if (length(x) > 1) sd(x) / sqrt(length(x)) else NA
                                         data.frame(level = "spectrum", name = m$br_spectrum[1],
                                                    spectrum = m$family[1], subfactor = NA_character_,
                                                    mean = mn, lo = mn - ci * sem, hi = mn + ci * sem,
                                                    n_answered = length(x), n_total = n_tot,
                                                    flag = if (!ok) "suppressed"
                                                    else if (length(x) < n_tot) "prorated" else "ok")
                                       }))
  } else {
    spec_bars <- do.call(rbind, lapply(split(sc, sc$Spectrum), function(d)
      comp(d, "spectrum", d$Spectrum[1], d$Spectrum[1], NA_character_)))
  }
  
  out <- rbind(spec_bars, subf_bars, scale_bars)
  
  # optional rational subscales, nested under their parent scales
  if (!is.null(subscales)) {
    sub_bars <- do.call(rbind, lapply(split(subscales, subscales$subscale),
                                      function(m) {
                                        x <- r[m$item]; n_tot <- length(x); x <- x[!is.na(x)]
                                        ok <- length(x) / n_tot >= 1 - max_missing
                                        mn <- if (ok) mean(x) else NA
                                        sem <- if (length(x) > 1) sd(x) / sqrt(length(x)) else NA
                                        prow <- scale_bars[scale_bars$name == m$parent[1], ]
                                        data.frame(level = "subscale", name = m$subscale[1],
                                                   spectrum = prow$spectrum[1], subfactor = prow$subfactor[1],
                                                   mean = mn, lo = mn - ci * sem, hi = mn + ci * sem,
                                                   n_answered = length(x), n_total = n_tot,
                                                   flag = if (!ok) "suppressed"
                                                   else if (length(x) < n_tot) "prorated" else "ok")
                                      }))
    sub_bars$parent <- vapply(split(subscales, subscales$subscale),
                              function(m) m$parent[1], character(1))
    out$parent <- NA_character_
    out <- rbind(out, sub_bars)
  }
  rownames(out) <- NULL
  out
}

#' Wrap a chart as an interactive girafe widget.
#' Hovering any bar (or its label) keeps it at full color and grays out
#' every other bar; a tooltip shows the T-score and severity band.
#' With selection = "single", clicking a bar reports its data_id to Shiny
#' as input$<outputId>_selected.
hitop_girafe <- function(p, w = 10.5, h = 10.5,
                         selection = c("single", "none")) {
  selection <- match.arg(selection)
  girafe(
    ggobj = p, width_svg = w, height_svg = h,
    options = list(
      opts_hover(css = "stroke:#1E3A5F;stroke-width:0.8px;"),
      opts_hover_inv(css = "opacity:0.15;filter:grayscale(85%);"),
      opts_selection(type = selection,
                     css = "stroke:#1E3A5F;stroke-width:1.2px;"),
      opts_tooltip(css = paste0(
        "background:#1E3A5F;color:#fff;padding:8px 12px;",
        "border-radius:6px;font-family:Roboto,sans-serif;font-size:13px;",
        "white-space:pre-line;box-shadow:0 2px 10px rgba(0,0,0,.25);")),
      opts_sizing(rescale = TRUE),
      opts_toolbar(saveaspng = FALSE)
    )
  )
}

#' Horizontal T-score detail chart for one spectrum (panel 3).
#' Shows the spectrum composite, each subfactor block, and every scale,
#' against the same gray severity bands. Bars are interactive: clicking a
#' scale bar reports its name back to Shiny for the item-level view.
plot_spectrum_detail <- function(bars_t, spectrum_name,
                                 defs = NULL,
                                 tscore = TRUE,
                                 t_floor = 30, t_ceil = 85,
                                 score_range = c(1, 4),
                                 base_size = 12,
                                 spectrum_colors = hitop_spectrum_colors) {
  lvl_rank <- c(spectrum = 1, subfactor = 2, scale = 3, subscale = 4)
  base_col <- spectrum_colors[[spectrum_name]]
  
  floor_y <- if (tscore) t_floor else score_range[1]
  bt <- bars_t[bars_t$spectrum == spectrum_name, ]
  ceil_y  <- if (tscore)
    max(t_ceil, 5 * ceiling(max(c(bt$mean, bt$hi), na.rm = TRUE) / 5))
  else score_range[2]
  span    <- ceil_y - floor_y
  breaks  <- if (tscore) seq(40, ceil_y - 5, 10) else seq(score_range[1], score_range[2], 1)
  
  d <- bars_t |>
    filter(spectrum == spectrum_name) |>
    mutate(lvl_rank = lvl_rank[level],
           sortkey = ifelse(level == "subscale", parent, name)) |>
    arrange(!is.na(subfactor) | level != "spectrum",
            subfactor, sortkey, lvl_rank, name) |>
    mutate(
      ypos  = rev(seq_len(n())),
      fill  = colorspace::darken(base_col, hitop_level_darken[level]),
      lab   = case_when(level == "spectrum"  ~ toupper(name),
                        level == "subfactor" ~ name,
                        level == "subscale"  ~ paste0("        ", name),
                        TRUE ~ paste0("    ", name)),
      face  = case_when(level == "spectrum"  ~ "bold",
                        level == "subfactor" ~ "bold.italic",
                        level == "subscale"  ~ "italic",
                        TRUE ~ "plain"),
      mean_c = pmin(pmax(mean, floor_y), ceil_y),
      lo_c   = pmin(pmax(lo, floor_y), ceil_y),
      hi_c   = pmin(pmax(hi, floor_y), ceil_y),
      capped = mean > ceil_y,
      flag = if ("flag" %in% names(bars_t)) flag else "ok",
      n_answered = if ("n_answered" %in% names(bars_t)) n_answered else NA,
      n_total = if ("n_total" %in% names(bars_t)) n_total else NA,
      tip = paste0(
        ifelse(is.na(mean), name,
               if (tscore) {
                 sprintf("%s\nT = %.1f (%s)\n%s level%s%s",
                         name, mean, hitop_severity_label(mean), level,
                         ifelse(level == "subscale", " (rational)", ""),
                         ifelse(level == "scale", "\nclick for item responses", ""))
               } else {
                 sprintf("%s\nscore = %.2f (1\u20134 scale)\n%s level%s%s",
                         name, mean, level,
                         ifelse(level == "subscale", " (rational)", ""),
                         ifelse(level == "scale", "\nclick for item responses", ""))
               }),
        tip_missing(flag, n_answered, n_total),
        tip_def(name, defs)),
      lab = ifelse(flag == "prorated" | flag == "partial",
                   paste0(lab, "*"), lab),
      labcol = ifelse(flag == "suppressed", "grey60", fill)
    )
  
  p <- ggplot(d)
  
  if (tscore) {
    bands <- hitop_severity_bands
    bands$lo <- pmax(bands$lo, floor_y); bands$hi <- pmin(bands$hi, ceil_y)
    p <- p +
      geom_rect(data = bands,
                aes(xmin = lo, xmax = hi,
                    ymin = 0.4, ymax = nrow(d) + 0.6, alpha = band),
                fill = "grey10", show.legend = FALSE) +
      scale_alpha_manual(values = setNames(bands$alpha, bands$band)) +
      geom_vline(xintercept = c(60, 65, 70), color = "grey55",
                 linewidth = 0.25, linetype = "31")
  } else {
    p <- p +
      geom_vline(xintercept = breaks, color = "grey88", linewidth = 0.3)
  }
  
  p +
    geom_rect_interactive(
      data = d |> filter(!is.na(mean)),
      aes(xmin = floor_y, xmax = mean_c,
          ymin = ypos - 0.36, ymax = ypos + 0.36,
          fill = fill, data_id = name, tooltip = tip),
      show.legend = FALSE) +
    geom_point_interactive(
      data = d |> filter(is.na(mean)),
      aes(x = floor_y + span * 0.027, y = ypos,
          data_id = name, tooltip = tip),
      shape = 4, size = 1.8, stroke = 0.8, color = "grey55") +
    geom_errorbarh(aes(xmin = lo_c, xmax = hi_c, y = ypos),
                   height = 0.28, linewidth = 0.9, color = "white",
                   na.rm = TRUE) +
    geom_errorbarh(aes(xmin = lo_c, xmax = hi_c, y = ypos),
                   height = 0.25, linewidth = 0.3, color = "grey15",
                   na.rm = TRUE) +
    geom_point(data = d |> filter(capped),
               aes(x = ceil_y, y = ypos), shape = 17, size = 1.4,
               color = "grey15") +
    geom_text_interactive(
      aes(x = floor_y - span * 0.018, y = ypos, label = lab, fontface = face,
          color = labcol, data_id = name, tooltip = tip),
      hjust = 1, size = base_size * 0.28, show.legend = FALSE) +
    scale_fill_identity() + scale_color_identity() +
    scale_x_continuous(limits = c(floor_y - span * 0.44, ceil_y + span * 0.02),
                       breaks = breaks, sec.axis = dup_axis()) +
    scale_y_continuous(limits = c(0.3, nrow(d) + 0.7), expand = c(0, 0)) +
    theme_minimal(base_size = base_size) +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(color = "grey45"),
          plot.margin = margin(6, 10, 6, 4))
}

#' Horizontal full-profile chart: every spectrum block in one scrollable view.
#' The "standard bar chart" alternative to the circle (Frank & Miri feedback).
#' Works in raw (1-4) or T-score mode; interactive like the other charts.
plot_hitop_horizontal <- function(bars, defs = NULL,
                                  tscore = TRUE,
                                  t_floor = 30, t_ceil = 85,
                                  score_range = c(1, 4),
                                  base_size = 9.5,
                                  spectrum_gap = 1.6,
                                  spectrum_colors = hitop_spectrum_colors,
                                  spectrum_order = names(spectrum_colors)) {
  lvl_rank <- c(spectrum = 1, subfactor = 2, scale = 3, subscale = 4)
  floor_y <- if (tscore) t_floor else score_range[1]
  # widen the axis so no observed score is clipped (feedback: bars were
  # exceeding the axis maximum)
  ceil_y  <- if (tscore)
    max(t_ceil, 5 * ceiling(max(c(bars$mean, bars$hi), na.rm = TRUE) / 5))
  else score_range[2]
  span    <- ceil_y - floor_y
  breaks  <- if (tscore) seq(40, ceil_y - 5, 10) else seq(score_range[1], score_range[2], 1)
  
  d <- bars |>
    mutate(spectrum = factor(spectrum, levels = spectrum_order),
           lvl_rank = lvl_rank[level],
           sortkey = ifelse(level == "subscale", parent, name)) |>
    arrange(spectrum, !is.na(subfactor) | level != "spectrum",
            subfactor, sortkey, lvl_rank, name) |>
    group_by(spectrum) |> mutate(.i = row_number()) |> ungroup()
  
  sizes <- d |> count(spectrum, name = "n") |>
    mutate(off = cumsum(dplyr::lag(n, default = 0)) +
             (dplyr::row_number() - 1) * spectrum_gap)
  d <- d |>
    left_join(sizes |> select(spectrum, off), by = "spectrum") |>
    mutate(row = .i + off, ypos = max(row) + 1 - row,
           fill = colorspace::darken(
             spectrum_colors[as.character(spectrum)],
             hitop_level_darken[level]),
           lab = case_when(level == "spectrum"  ~ toupper(as.character(name)),
                           level == "subfactor" ~ as.character(name),
                           level == "subscale"  ~ paste0("        ", name),
                           TRUE ~ paste0("    ", name)),
           face = case_when(level == "spectrum"  ~ "bold",
                            level == "subfactor" ~ "bold.italic",
                            level == "subscale"  ~ "italic",
                            TRUE ~ "plain"),
           mean_c = pmin(pmax(mean, floor_y), ceil_y),
           lo_c   = pmin(pmax(lo, floor_y), ceil_y),
           hi_c   = pmin(pmax(hi, floor_y), ceil_y),
           capped = mean > ceil_y,
           flag = if ("flag" %in% names(bars)) flag else "ok",
           n_answered = if ("n_answered" %in% names(bars)) n_answered else NA,
           n_total = if ("n_total" %in% names(bars)) n_total else NA,
           tip = paste0(
             ifelse(is.na(mean), as.character(name),
                    if (tscore) {
                      sprintf("%s\nT = %.1f (%s)\n%s level%s%s",
                              name, mean, hitop_severity_label(mean), level,
                              ifelse(level == "subscale", " (rational)", ""),
                              ifelse(level == "scale",
                                     "\nclick for item responses", ""))
                    } else {
                      sprintf("%s\nscore = %.2f (1\u20134 scale)\n%s level%s%s",
                              name, mean, level,
                              ifelse(level == "subscale", " (rational)", ""),
                              ifelse(level == "scale",
                                     "\nclick for item responses", ""))
                    }),
             tip_missing(flag, n_answered, n_total),
             tip_def(name, defs)),
           lab = ifelse(flag == "prorated" | flag == "partial",
                        paste0(lab, "*"), lab),
           labcol = ifelse(flag == "suppressed", "grey60", fill))
  
  ymax <- max(d$ypos) + 0.7
  p <- ggplot(d)
  if (tscore) {
    bands <- hitop_severity_bands
    bands$lo <- pmax(bands$lo, floor_y); bands$hi <- pmin(bands$hi, ceil_y)
    p <- p +
      geom_rect(data = bands,
                aes(xmin = lo, xmax = hi, ymin = 0.3, ymax = ymax,
                    alpha = band),
                fill = "grey10", show.legend = FALSE) +
      scale_alpha_manual(values = setNames(bands$alpha, bands$band)) +
      geom_vline(xintercept = c(60, 65, 70), color = "grey55",
                 linewidth = 0.25, linetype = "31")
  } else {
    p <- p + geom_vline(xintercept = breaks, color = "grey88",
                        linewidth = 0.3)
  }
  
  p +
    geom_rect_interactive(
      data = d |> filter(!is.na(mean)),
      aes(xmin = floor_y, xmax = mean_c,
          ymin = ypos - 0.38, ymax = ypos + 0.38,
          fill = fill, data_id = name, tooltip = tip),
      show.legend = FALSE) +
    geom_point_interactive(
      data = d |> filter(is.na(mean)),
      aes(x = floor_y + span * 0.02, y = ypos,
          data_id = name, tooltip = tip),
      shape = 4, size = 1.5, stroke = 0.7, color = "grey55") +
    geom_errorbarh(aes(xmin = lo_c, xmax = hi_c, y = ypos),
                   height = 0.3, linewidth = 0.8, color = "white",
                   na.rm = TRUE) +
    geom_errorbarh(aes(xmin = lo_c, xmax = hi_c, y = ypos),
                   height = 0.26, linewidth = 0.28, color = "grey15",
                   na.rm = TRUE) +
    geom_point(data = d |> filter(capped),
               aes(x = ceil_y, y = ypos), shape = 17, size = 1.2,
               color = "grey15") +
    geom_text_interactive(
      aes(x = floor_y - span * 0.015, y = ypos, label = lab,
          fontface = face, color = labcol,
          data_id = name, tooltip = tip),
      hjust = 1, size = base_size * 0.24, show.legend = FALSE) +
    scale_fill_identity() + scale_color_identity() +
    scale_x_continuous(limits = c(floor_y - span * 0.42, ceil_y + span * 0.02),
                       breaks = breaks, position = "top",
                       sec.axis = dup_axis()) +
    scale_y_continuous(limits = c(0.3, ymax), expand = c(0, 0)) +
    theme_minimal(base_size = base_size) +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(color = "grey45"),
          plot.margin = margin(6, 10, 6, 4))
}
