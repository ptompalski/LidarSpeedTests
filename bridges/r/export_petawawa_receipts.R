# Export the receipts of a historical Petawawa fresh preparation as JSON.
#
#   Rscript --vanilla bridges/r/export_petawawa_receipts.R \
#       --root <fresh preparation root> --stage full|pilot --output <new file.json>
#
# Base R only (readRDS); no renv, lidR or sf is needed, nothing is recomputed
# and nothing below --root is written. Only plain receipt fields are read:
# S4 objects embedded in receipts (lidR headers) are never touched, and
# machine-specific absolute paths are not exported. The output
# (lidarbench.petawawa-receipts/1) is consumed by
# `lidarbench data register petawawa-fresh-v4 --legacy-export <file>`, which
# cross-checks every MD5 against the bytes it hashes. See design/DATASETS.md.
#
# Exit status: 0 exported; 1 a receipt is missing or unreadable; 2 usage error.
local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script <- gsub("~+~", " ", sub("^--file=", "", file_arg[[1]]), fixed = TRUE)
  source(file.path(dirname(normalizePath(script, winslash = "/")), "bridge.R"), local = TRUE)

  options <- tryCatch(
    bridge_parse_args(commandArgs(trailingOnly = TRUE), c("root", "stage", "output")),
    error = function(e) {
      message("export: ", conditionMessage(e))
      quit(save = "no", status = 2L)
    }
  )
  if (!options$stage %in% c("full", "pilot")) {
    message("export: --stage must be 'full' or 'pilot'")
    quit(save = "no", status = 2L)
  }
  if (file.exists(options$output)) {
    message("export: refusing to overwrite ", options$output)
    quit(save = "no", status = 2L)
  }

  root <- normalizePath(options$root, winslash = "/", mustWork = FALSE)
  stage <- file.path(root, options$stage)
  receipt <- function(...) {
    path <- file.path(...)
    if (!file.exists(path)) stop("missing receipt: ", path)
    readRDS(path)
  }
  optional <- function(...) {
    path <- file.path(...)
    if (file.exists(path)) readRDS(path) else NULL
  }
  chr <- function(x) if (is.null(x)) NULL else as.character(x)
  arr <- function(x) I(as.character(x))
  named <- function(x) if (is.null(x)) NULL else as.list(vapply(x, as.character, character(1)))
  # Only the LAStools build number is kept; banner licensing text is never exported.
  build <- function(x) {
    text <- paste(x, collapse = "\n")
    if (grepl("version [0-9]+", text)) sub("(?s).*?version ([0-9]+).*", "\\1", text, perl = TRUE) else NULL
  }
  tool_versions <- function(versions) {
    lapply(versions, function(v) list(status = as.integer(v$status),
                                       build = build(c(v$stdout, v$stderr))))
  }

  export <- tryCatch({
    fresh <- receipt(root, "fresh-root.rds")
    download <- receipt(root, "download", "download.rds")
    transition <- optional(root, "negative-z-transition.rds")
    stage_identity <- receipt(stage, "stage-identity.rds")
    plan <- receipt(stage, "prepared", "preparation.rds")
    ready <- receipt(stage, "ready.rds")
    parallel <- optional(stage, "parallel-transition.rds")
    config <- fresh$config

    repairs <- lapply(plan$sources$tile, function(tile) {
      r <- receipt(stage, "repaired", paste0(tile, ".laz.rds"))
      actions <- r$actions
      counts <- if (is.data.frame(actions) && nrow(actions)) table(actions$action) else integer()
      list(
        tile = tile,
        archive_member = chr(r$archive_member),
        raw_md5 = chr(r$raw_md5),
        md5 = chr(r$md5),
        policy_version = chr(r$policy$version),
        source_filter = r$source_filter,
        qc_points = r$qc$points,
        qc_area_m2 = r$qc$area_m2,
        actions = as.list(stats::setNames(as.integer(counts), names(counts))),
        negative_source_z = if (is.data.frame(actions) && "source_Z" %in% names(actions))
          sum(actions$source_Z < 0) else NULL
      )
    })

    tiles <- list()
    for (density in plan$densities) {
      folder <- paste0("tile_1000_density_", density)
      for (tile in plan$sources$tile) {
        rel <- file.path("1_org", folder, paste0(tile, ".laz"))
        t <- receipt(stage, "prepared", paste0(rel, ".rds"))
        tiles[[length(tiles) + 1L]] <- list(
          density = density, tile = tile, path = rel,
          laz_md5 = chr(t$md5[1]), lax_md5 = chr(t$md5[2]),
          source_md5 = chr(t$source_md5), seed = t$seed,
          points = t$summary$points,
          index_validation = chr(t$index_validation$status)
        )
      }
    }

    list(
      schema = "lidarbench.petawawa-receipts/1",
      stage = options$stage,
      exported_with = list(R = as.character(getRversion())),
      root = list(
        policy_version = chr(fresh$policy$version),
        archive_url = chr(config$archive_url),
        archive_md5_supplied = chr(config$archive_md5),
        crs = config$crs,
        vertical_epsg = config$vertical_epsg,
        metadata_confirmation = chr(config$metadata_confirmation),
        seed = config$seed,
        executables = named(config$executables),
        tool_license_confirmed = !is.null(config$tool_license) && nzchar(config$tool_license)
      ),
      negative_z_transition = if (is.null(transition)) NULL else list(
        previous_policy = chr(transition$previous$policy$version),
        current_policy = chr(transition$current$policy$version),
        frozen_md5 = arr(transition$frozen_md5)
      ),
      parallel_transition = !is.null(parallel),
      download = list(url = chr(download$url), md5 = chr(download$md5),
                      bytes = download$bytes, authenticity = chr(download$authenticity)),
      stage_identity = list(
        policy_version = chr(stage_identity$policy$version),
        archive_md5 = chr(stage_identity$archive_md5),
        members = arr(stage_identity$members),
        tools_md5 = arr(stage_identity$tools_md5),
        tool_versions = tool_versions(stage_identity$versions),
        implementation_md5 = arr(stage_identity$implementation)
      ),
      preparation = list(
        method = chr(plan$method),
        densities = I(plan$densities),
        seed = plan$config$seed,
        tiles = arr(plan$sources$tile),
        source_md5 = arr(plan$sources$md5),
        source_citation = chr(plan$config$source_citation),
        license = chr(plan$config$license),
        crs_wkt = chr(plan$config$crs),
        repair_provenance = chr(plan$config$repair_provenance),
        parameters = lapply(plan$parameters, as.character),
        tools = lapply(seq_len(nrow(plan$tools)), function(i) {
          list(name = plan$tools$name[i], file = basename(plan$tools$path[i]),
               md5 = plan$tools$md5[i])
        }),
        tool_versions = tool_versions(plan$versions),
        implementation_md5 = arr(plan$implementation),
        fresh_implementation_md5 = arr(plan$fresh$implementation),
        archive_md5 = chr(plan$fresh$archive_md5),
        repairs_md5 = arr(plan$fresh$repairs_md5),
        runtime = list(R = chr(plan$runtime$R), platform = chr(plan$runtime$platform),
                       lidR = chr(plan$runtime$lidR), sf = chr(plan$runtime$sf),
                       native = named(plan$runtime$native))
      ),
      ready = list(status = chr(ready$status), policy_version = chr(ready$policy$version),
                   policy = lapply(ready$policy, as.character),
                   archive_md5 = chr(ready$archive_md5),
                   preparation_md5 = chr(ready$preparation_md5)),
      repairs = repairs,
      tiles = tiles
    )
  }, error = function(e) {
    message("export: ", conditionMessage(e))
    quit(save = "no", status = 1L)
  })
  bridge_write_json(export, options$output)
  message("export: wrote ", options$output)
})
