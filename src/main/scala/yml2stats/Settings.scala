package yml2stats

object Settings {
  // default args
  var inFileName = ""

  var printFairnessWarnings = true
  var printIndividualStats  = true
  var printCombinedResults  = true

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

}
