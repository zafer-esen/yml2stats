package yml2stats

import plotly.kaleido
import plotly.element._

object Settings {
  // default args
  var inFileName = ""

  var printFairnessWarnings     = true
  var printIndividualStats      = false
  var printCombinedResults      = true
  var printCombinedResultsLatex = false
  var printCombinatorialResults = false

  var printTable5     = false
  var printExtraStats = true

  // if below list is nonempty, they will be used to create a "portfolio" row in
  // the combined results.
  var portfolioToolNames = Seq("Eldarica-array", "Z3", "Eldarica")

  var verbosityLevel = 0 // 0 : quiet, 1 : print warnings, 2 : print information, 3 : print even more information

  // if true, will try to categorize benchmarks based on their directories
  var categorizeBasedOnDirectory = true

  // overrides all other plot settings
  var disableAllPlots = true

  var plotDurations                         = true
  var plotDurationsFile                     = true
  var plotDurationsPdfNamePrefix            = "durations"
  var plotDurationsMarkerSize               = 4
  var plotDurationsMarkerSymbol             = Symbol.Diamond()
  var plotDurationsFileScale                = 1
  var plotDurationsWidth                    = 600
  var plotDurationsHeight                   = 640
  var plotDurationsShowDiagonal             = true
  var plotDurationsShowDiagonalExplanations = true // also requires diagonal to be shown

  var plotCactus          = true
  var plotCactusFile      = true
  var plotCactusPdfName   = "cactus"
  var plotCactusFileScale = 1
  var plotCactusWidth     = 600
  var plotCactusHeight    = 640

  var useResultInsteadOfExpectedInPlots = true
  var produceSinglePlotForAllCategories = false

  var plotSaveDirectory = System.getProperty("user.dir")
  var plotOutputFormat  = kaleido.KaleidoFormat.PDF

  var kaleidoDirectory =
    System.getProperty("user.dir") + "/dependencies/kaleido/kaleido"

  // exclude benchmarks that could not be processed in *any* of the provided files
  var excludeErrors                = false
  var excludeSolverErrors          = false // e.g., do not exclude "Predicate generation failed" kind of errors
  var excludeIncorrect             = false
  var considerSolveErrorsUnknown   = false
  var considerOutOfMemErrorUnknown = false
  var considerKilledAsTimeout      = true

  // will print a list of errors and error messages for each tool
  // requires excludeErrors to be false. does not depend on verbosity.
  var printErrorsForEachTool     = false
  var printUnsoundForEachTool    = false
  var printIncompleteForEachTool = false
  var printComparedRunNames      = false

  var printPerFilenameStats = true

  // merge yml files that were run using the same tool name and tool options.
  var mergeYmlFiles = true
  // todo: if runs contain same benchmarks, use the latest results?
  var ignoreDifferentOptions         = false // merge yml files even if options were different
  var ignoreDifferentOptionsForTools = Nil // merge yml files even if options were different only for these tools (above option needs to be false)
  var ignoreDifferentNotes           = true

  // instead of merging, results will be combined using the following algorithm with the listed priority:
  //   - if a benchmark is ERROR   in any of the combined results, result will be ERROR
  //   - if a benchmark is TIMEOUT in any of the combined results, result will be TIMEOUT
  //   - if a benchmark is UNSAT   in any of the combined results, result will be UNSAT
  //   - if a benchmark is SAT     in all of the combined results, result will be SAT
  var combineResults = false // mergeYmlFiles needs to be false if this is true

  var uniqueBenchmarks = true // take first result if multiple exist in the *same* file

  // all of these extensions will be stripped to obtain the base benchmark names
  // this constitutes the basis filenames for comparisons
  // e.g., "name.c.smt2" and "name.smt2" and "name.c" and "name.smt2.c" will
  // all be considered to be the same benchmark with "name".
  var benchmarkExtensions = List(".smt2", ".c", ".yml", ".i", ".cil")
  // this option toggles stripping of above extensions
  var discardBenchmarkExtensions = true

  val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  // enable dev. assertions
  var assertsOn = true

}
