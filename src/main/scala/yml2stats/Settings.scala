package yml2stats

import yml2stats.Benchmarks.Summary

//import plotly.kaleido
//import plotly.element._

case class VirtualPortfolio(name      : String,
                            tools     : Set[String],
                            encodings : Set[String])

case class CactusPlotDisplayOptions(
  showTitle : Boolean = true,
  showAxes : Boolean = true,
  showLegend : Boolean = true,
  width : Int = 500,
  height : Int = 350
)

object Settings {
  // default args
  var inFileName = ""

  var doPortfolio                   = true
  var stripSafeUnsafeSuffixInTable6 = false
//  val virtualPortfolios = Seq[VirtualPortfolio]()
  val virtualPortfolios = Seq(
    VirtualPortfolio(
      name = "All - None",
      tools = Set("TriCera"),
      encodings = Set(
        "R", "R-opt", "R-tag", "R-opt-tag",
        "RW", "RW-opt", "RW-tag", "RW-opt-tag",
        "RW-fun", "RW-fun-opt", "RW-fun-tag", "RW-fun-tag-opt", "RW-fun-tag-opt-p"
        )
      ),
    VirtualPortfolio(
      name = "All",
      tools = Set("TriCera"),
      encodings = Set(
        "None",
        "R", "R-opt", "R-tag", "R-opt-tag",
        "RW", "RW-opt", "RW-tag", "RW-opt-tag",
        "RW-fun", "RW-fun-opt", "RW-fun-tag", "RW-fun-tag-opt", "RW-fun-tag-opt-p"
        )
      )
    )

  var printFairnessWarnings = false
  var printIndividualStats  = false
  var printCombinedResults  = true
  var printUnsoundRuns      = true
  var printIncompleteRuns   = true
  var printSummaryLatex     = true
  var printDetailedLatex    = true
  var printCombinatorialResults = false

  var doTable5Text = false
  var doTable6Text = false
  var doTable5Tex = false
  var doTable6Tex = false
  var doTable5SimpleText = false
  var doTable5SimpleTex  = false
  var doMatrixText = false
  var doMatrixTex  = false
  var doCactusPdf  = false
  var doCactusPlotly = false

  var verbosityLevel        = 0 // 0 : quiet, 1 : print warnings, 2 : print information

  // overrides all other plot settings
  var disableAllPlots            = false

  var plotDurations              = false
  var plotDurationsFile          = false
  var plotDurationsPdfNamePrefix = "durations"
  var plotDurationsMarkerSize    = 4
  //var plotDurationsMarkerSymbol  = Symbol.Diamond
  var plotDurationsFileScale     = 1
  var plotDurationsWidth         = 600
  var plotDurationsHeight        = 640
  var plotDurationsShowDiagonal  = true
  var plotDurationsShowDiagonalExplanations = true // also requires diagonal to be shown
  var plotDurationsTreatUnknownAsTimeout = true
  var plotDurationsUseResultInsteadOfExpected = true // safe to do when there are no conflicts

  var plotCactusFile = false
  var plotCactusPdfName     = "cactus"
  var plotCactusFileScale    = 1
  var plotCactusWidth       = 600
  var plotCactusHeight      = 640

  var plotCactusByTime = false
  val cactusPlotByTimeFile = "cactus-plot-by-time"
  val useLogarithmicYCactus = true
  val plotMarkerOutlineThickness = 0.5f

  val cactusPlotTotalOptions = CactusPlotDisplayOptions(
    showTitle = false,
    showAxes = true,
    showLegend = true,
    width = 900,
    height = 250
  )

  val cactusPlotSafeOptions = CactusPlotDisplayOptions(
    showTitle = false,
    showAxes = true,
    showLegend = false,
    width = 500,
    height = 250
  )

  val cactusPlotUnsafeOptions = CactusPlotDisplayOptions(
    showTitle = false,
    showAxes = true,
    showLegend = false,
    width = 500,
    height = 250
  )

  var plotSaveDirectory     = System.getProperty("user.dir")
  //var plotOutputFormat      = kaleido.KaleidoFormat.PDF

  var kaleidoDirectory      =
    System.getProperty("user.dir") + "/dependencies/kaleido/kaleido"

  // exclude benchmarks that could not be processed in *any* of the provided files
  var excludeErrors         = false
  var excludeSolverErrors = false // e.g., do not exclude "Predicate generation failed" kind of errors
  var excludeIncorrect      = false
  var considerSolveErrorsUnknown = false
  var considerOutOfMemErrorUnknown = false
  var considerKilledAsTimeout = true

  // merge yml files that were run using the same tool name and tool options.
  var mergeYmlFiles         = false
  // todo: if runs contain same benchmarks, use the latest results?
  var ignoreDifferentOptions = false // merge yml files even if options were different
  var ignoreDifferentOptionsForTools = List("CPAchecker") // merge yml files even if options were different only for these tools (above option needs to be false)
  var ignoreDifferentNotes   = false

  // instead of merging, results will be combined using the following algorithm with the listed priority:
  //   - if a benchmark is ERROR   in any of the combined results, result will be ERROR
  //   - if a benchmark is TIMEOUT in any of the combined results, result will be TIMEOUT
  //   - if a benchmark is UNSAT   in any of the combined results, result will be UNSAT
  //   - if a benchmark is SAT     in all of the combined results, result will be SAT
  var combineResults        = true // mergeYmlFiles needs to be false if this is true

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

  // Create a map from the sort order list for efficient lookups.
  def getCleanEncodingName(encoding: String): String = {
    val prefix = Settings.encodingPrefixes.find(p => encoding.startsWith(p))
    prefix match {
      case Some(p) => encoding.stripPrefix(p)
      case None    => encoding
    }
  }

  def formatNameForPlot(summary : Summary): String = {
    val toolName = summary.toolName
    val cleanEncoding = getCleanEncodingName(summary.notes)

    val toolLatex = Settings.latexToolNameReplacements.getOrElse(toolName, toolName)
    val encodingLatex = Settings.latexEncodingReplacements.getOrElse(cleanEncoding, cleanEncoding)

    val plainTool = toolLatex
      .replace("\\tricera{}", "TriCera")
      .replace("\\seahorn{}", "SeaHorn")
      .replace("\\cpachecker{}", "CPAchecker")
      .replace("\\predator{}", "PredatorHP")

    val plainEncoding = encodingLatex
      .replace("$\\mathit{", "")
      .replace("}$", "")
      .replace("\\text{-}", "-")
      .replaceAll("_\\{([^}]+)\\}", "-$1")

    if (plainEncoding == "None" || plainEncoding.isEmpty)
      s"$plainTool"
    else if (virtualPortfolios.exists(vp => vp.name == plainEncoding))
      s"$plainTool ($plainEncoding)"
    else
      s"$plainTool ($plainEncoding)"

  }

  def getEncodingSortKey(encoding: String): Int = {
    val cleanName = getCleanEncodingName(encoding)
    // Return the defined index, or a large value to push unknown encodings to the end.
    encodingSortMap.getOrElse(cleanName, Int.MaxValue)
  }

  // Prefixes to be stripped from encoding names before LaTeX replacement.
  val encodingPrefixes = List(
    "cpareach-",
    "predatorhpreach-",
    "sea-",
    "tri-"
    )

  // Replacements for tool names in LaTeX output.
  val latexToolNameReplacements = Map(
    "TriCera"    -> "\\tricera{}",
    "SeaHorn"    -> "\\seahorn{}",
    "CPAchecker (reach)" -> "\\cpachecker{}",
    "PredatorHP (reach)" -> "\\predator{}"
    )

  // Replacements for encoding/note strings in LaTeX output.
  // This is applied *after* stripping a prefix from `encodingPrefixes`.
  // The match must be exact.
  val latexEncodingReplacements = Map(
    "None"         -> "None",
    "R"            -> "$\\mathit{R}$",
    "R-opt"        -> "$\\mathit{R_{C}}$",
    "R-tag"        -> "$\\mathit{R_{T}}$",
    "R-tag-opt"    -> "$\\mathit{R_{CT}}$",
    "RW"           -> "$\\mathit{RW}$",
    "RW-opt"       -> "$\\mathit{RW_{C}}$",
    "RW-tag"       -> "$\\mathit{RW_{T}}$",
    "RW-tag-opt"   -> "$\\mathit{RW_{CT}}$",
    "RW-fun"       -> "$\\mathit{RW\\text{-}fun}$",
    "RW-fun-tag"   -> "$\\mathit{RW_{T}\\text{-}fun}$",
    "RW-fun-opt"   -> "$\\mathit{RW_{C}\\text{-}fun}$",
    "RW-fun-tag-opt"   -> "$\\mathit{RW_{CT}\\text{-}fun}$",
    "RW-fun-tag-opt-p" -> "$\\mathit{RW_{CTP}\\text{-}fun}$",
    )

  // Explicit sort order for encodings / notes (after elimination of prefix).
  // Items not in this list will be sorted alphabetically at the end.
  val encodingSortMap = List(
    "None",
    "R",
    "R-opt",
    "R-tag",
    "R-tag-opt",
    "RW",
    "RW-opt",
    "RW-tag",
    "RW-tag-opt",
    "RW-fun",
    "RW-fun-opt",
    "RW-fun-tag",
    "RW-fun-tag-opt",
    "RW-fun-tag-opt-p",
    "RW-fun-tag-opt2"
    ).zipWithIndex.toMap
}