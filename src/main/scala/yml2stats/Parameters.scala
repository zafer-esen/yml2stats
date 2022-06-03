package yml2stats

object Parameters {
  // default args
  var inFileName = ""

  var printFairnessWarnings = true
  var printIndividualStats  = true
  var printCombinedResults  = true

  // exclude benchmarks that could not be processed in *any* of the provided files
  var excludeErrors         = false

  // all of these extensions will be stripped to obtain the base benchmark names
  // this constitutes the basis filenames for comparisons
  // e.g., "name.c.smt2" and "name.smt2" and "name.c" and "name.smt2.c" will
  // all be considered to be the same benchmark with "name".
  var benchmarkExtensions   = List(".smt2", ".c")
  // this option disables stripping of above extensions
  var discardBenchmarkExtensions = true
}
