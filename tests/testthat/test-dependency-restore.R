test_that("the lockfile restores and loads in an isolated temporary library", {
  withr::local_dir(repo)
  fixture <- benchmark_fixture()
  library <- file.path(fixture$test_dir, "library")
  dir.create(library)
  restored <- callr::r(function(project, lockfile, library) {
    renv::restore(project = project, lockfile = lockfile, library = library,
                  strict = TRUE, retry = FALSE, prompt = FALSE)
    .libPaths(c(library, .Library))
    packages <- renv::lockfile_read(lockfile)$Packages
    vapply(names(packages), function(package) {
      requireNamespace(package, quietly = TRUE) &&
        utils::packageVersion(package, lib.loc = c(library, .Library)) ==
          package_version(packages[[package]]$Version)
    }, logical(1))
  }, args = list(project = repo, lockfile = file.path(repo, "renv.lock"),
                 library = library),
  libpath = .libPaths(), show = TRUE, supervise = TRUE)
  expect_true(length(restored) > 0L)
  expect_true(all(restored), info = paste(names(restored)[!restored], collapse = ", "))
  standard <- installed.packages(lib.loc = .Library)
  recommended <- rownames(standard)[which(standard[, "Priority"] == "recommended")]
  expect_true(all(file.exists(file.path(library, names(restored), "DESCRIPTION")) |
                    names(restored) %in% recommended))
})
