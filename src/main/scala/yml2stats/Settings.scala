package yml2stats

import plotly.kaleido
import plotly.element._

object Settings {
  // default args
  var inFileName = ""

  var printFairnessWarnings = true
  var printIndividualStats  = true
  var printCombinedResults  = true

  // overrides all other plot settings
  var disableAllPlots            = true

  var plotDurations              = true
  var plotDurationsFile          = true
  var plotDurationsPdfNamePrefix = "durations"
  var plotDurationsMarkerSize    = 4
  var plotDurationsMarkerSymbol  = Symbol.Diamond()
  var plotDurationsFileScale     = 1
  var plotDurationsWidth         = 600
  var plotDurationsHeight        = 640
  var plotDurationsShowDiagonal  = true
  var plotDurationsShowDiagonalExplanations = true // also requires diagonal to be shown

  var plotCactus            = true
  var plotCactusFile         = true
  var plotCactusPdfName     = "cactus"
  var plotCactusFileScale    = 1
  var plotCactusWidth       = 600
  var plotCactusHeight      = 640

  var plotSaveDirectory     = System.getProperty("user.dir")
  var plotOutputFormat      = kaleido.KaleidoFormat.PDF

  var kaleidoDirectory      =
    System.getProperty("user.dir") + "/dependencies/kaleido/kaleido"

  // exclude benchmarks that could not be processed in *any* of the provided files
  var excludeErrors         = false

  // merge yml files that were run using the same tool name and tool options.
  var mergeYmlFiles         = true
  // todo: if runs contain same benchmarks, use the latest results?

  // all of these extensions will be stripped to obtain the base benchmark names
  // this constitutes the basis filenames for comparisons
  // e.g., "name.c.smt2" and "name.smt2" and "name.c" and "name.smt2.c" will
  // all be considered to be the same benchmark with "name".
  var benchmarkExtensions   = List(".smt2", ".c")
  // this option disables stripping of above extensions
  var discardBenchmarkExtensions = true

  val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  // enable dev. assertions
  var assertsOn = true

}
