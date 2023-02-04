package yml2stats
import Benchmarks._
import plotly._
import plotly.element._
import plotly.layout._
import plotly.kaleido.Kaleido._ // adds save(...) method to plotly-scala
import Settings._

object Plotting {

  private class HtmlString(val s: String) {
    def toHtml: String = {
      s.replace(" ", "&nbsp;")
    }
  }
  private implicit def stringToHtmlString(s: String) = new HtmlString(s)

  private def plotToFile(data:   Seq[Trace],
                         name:   String,
                         layout: Layout,
                         width:  Int,
                         height: Int,
                         scale:  Float): Unit = {
    try {
      data.save(plotSaveDirectory,
                name,
                layout,
                plotOutputFormat,
                width,
                height,
                scale,
                kaleidoDirectory)
    } catch {
      case _: Exception =>
        println(
          "Could not save plot to file, is kaleido binary present at " +
            kaleidoDirectory + "?")
    }
  }

  def plotCactus(allToolRuns: Seq[(Summary, RunInfos)],
                 extraTitle:  Option[String]) = {
    val traces = for (toolRuns <- allToolRuns) yield {
      // todo: filter out incorrect runs? maybe in another place
      val sortedDurations =
        (toolRuns._2.satRuns ++ toolRuns._2.unsatRuns).sortBy(_.duration)
      val cumulativeDurations = sortedDurations.indices.map(
        ind => sortedDurations.take(ind + 1).map(_.duration).sum
      )

      Scatter()
        .withX(0 to cumulativeDurations.length)
        .withY(Seq(0.0) ++ cumulativeDurations)
        .withMode(ScatterMode(ScatterMode.Lines))
        .
//        withMarker(Marker().
//          withSymbol(Symbol.Cross()).
//          withSize(4).
//          withLine(Line().withColor(Color.StringColor(getNextColor)))
//        ).
        withName(
          (toolRuns._1.toolName + " (" + toolRuns._1.toolVersion + ")").toHtml)
        .withLine(Line()
          .withColor(Color.StringColor(getNextColor))
          .withShape(LineShape.Linear))
    }

    val layout = Layout(
      title =
        (extraTitle match {
          case Some(s) => s + " - "
          case None    => ""
        }) + "Cactus plot".toHtml,
      legend = Legend()
        .withFont(Font(size = 12))
        .withYanchor(Anchor.Middle)
        .withXanchor(Anchor.Right),
      plotCactusWidth,
      plotCactusHeight,
      hovermode = HoverMode.Closest //,
    ).withYaxis(
        Axis().withTitle("time (seconds)".toHtml).withType(AxisType.Log))
      .withXaxis(Axis()
        .withTitle("number of solved instances".toHtml)
        .withType(AxisType.Log))

    if (Settings.plotCactus) {
      Plotly.plot("plotly.html", traces, layout)
    }
    if (plotCactusFile) {
      plotToFile(traces,
                 plotCactusPdfName,
                 layout,
                 plotCactusWidth,
                 plotCactusHeight,
                 plotCactusFileScale)
    }
  }

  def plotDuratıons(toolRunInfo1: (Summary, RunInfos),
                    toolRunInfo2: (Summary, RunInfos),
                    extraTitle:   Option[String]) = {
    if (assertsOn) {
      // passed run infos should be of same length and cotnain the same (base)-
      //   named benchmarks
      assert(toolRunInfo1._2.runs.length == toolRunInfo2._2.runs.length)
      for (i <- toolRunInfo1._2.runs.indices) {
        assert(
          toolRunInfo1._2.runs(i).bmBaseName ==
            toolRunInfo2._2.runs(i).bmBaseName)
      }
    }

    def getResult(run: RunInfo) = {
      if (Settings.useResultInsteadOfExpectedInPlots)
        run.result
      else run.expected
    }

    def saturateDuration(duration: Double, timeout: Double) =
      if (duration > timeout) timeout else duration

    // todo: do one pass

    val conflictNames = ((toolRunInfo1._2.runs zip toolRunInfo2._2.runs)
      .filter {
        case (run1, run2) =>
          assert(run1.bmBaseName == run2.bmBaseName)
          val (res1, res2) = (getResult(run1), getResult(run2))
          res1 match {
            case True  => res2 == False
            case False => res2 == True
            case _     => false
          }
      })
      .map(_._1.bmBaseName)
    val expSatNames = ((toolRunInfo1._2.runs zip toolRunInfo2._2.runs)
      .filter {
        case (run1, run2) =>
          assert(run1.bmBaseName == run2.bmBaseName)
          val (res1, res2) = (getResult(run1), getResult(run2))
          res1 == True && res2 != False ||
          res2 == True && res1 != False
      })
      .map(_._1.bmBaseName)
    val expUnsatNames = ((toolRunInfo1._2.runs zip toolRunInfo2._2.runs)
      .filter {
        case (run1, run2) =>
          assert(run1.bmBaseName == run2.bmBaseName)
          val (res1, res2) = (getResult(run1), getResult(run2))
          res1 == False && res2 != True ||
          res2 == False && res1 != True
      })
      .map(_._1.bmBaseName)
    val expUnknownNames = ((toolRunInfo1._2.runs zip toolRunInfo2._2.runs)
      .filter {
        case (run1, run2) =>
          assert(run1.bmBaseName == run2.bmBaseName)
          val (res1, res2) = (getResult(run1), getResult(run2))
          res1 == Unknown && res2 != True && res2 != False ||
          res2 == Unknown && res1 != True && res1 != False
      })
      .map(_._1.bmBaseName)
    // todo: handle mismatches if run.result is used?

    val tool1Durations = toolRunInfo1._2.runs
      .map(
        run =>
          run.bmBaseName ->
            saturateDuration(run.duration, toolRunInfo1._1.wallTimeLimit))
      .toMap
    val tool2Durations = toolRunInfo2._2.runs
      .map(
        run =>
          run.bmBaseName ->
            saturateDuration(run.duration, toolRunInfo2._1.wallTimeLimit))
      .toMap

    val tool1ExpSatDurations = for (name <- expSatNames)
      yield tool1Durations(name)
    val tool1ExpUnsatDurations = for (name <- expUnsatNames)
      yield tool1Durations(name)
    val tool1ExpUnknownDurations = for (name <- expUnknownNames)
      yield tool1Durations(name)
    val tool1ConflictDurations = for (name <- conflictNames)
      yield tool1Durations(name)

    val tool2ExpSatDurations = for (name <- expSatNames)
      yield tool2Durations(name)
    val tool2ExpUnsatDurations = for (name <- expUnsatNames)
      yield tool2Durations(name)
    val tool2ExpUnknownDurations = for (name <- expUnknownNames)
      yield tool2Durations(name)
    val tool2ConflictDurations = for (name <- conflictNames)
      yield tool2Durations(name)

    val traces: Seq[Trace] = {
      val trace1 = Scatter()
        .withX(tool1ExpSatDurations)
        .withY(tool2ExpSatDurations)
        .withText(expSatNames)
        .withMode(ScatterMode(ScatterMode.Markers))
        .withMarker(Marker(color = Color.RGB(0, 128, 0), // green
                           symbol = plotDurationsMarkerSymbol,
                           size = plotDurationsMarkerSize))
        .withName((if (Settings.useResultInsteadOfExpectedInPlots) "Sat"
                   else "Expected sat").toHtml)

      val trace2 = Scatter()
        .withX(tool1ExpUnsatDurations)
        .withY(tool2ExpUnsatDurations)
        .withText(expUnsatNames)
        .withMode(ScatterMode(ScatterMode.Markers))
        .withMarker(Marker(color = Color.RGB(255, 0, 0), // red
                           symbol = plotDurationsMarkerSymbol,
                           size = plotDurationsMarkerSize))
        .withName((if (Settings.useResultInsteadOfExpectedInPlots) "UnSat"
                   else "Expected unsat").toHtml)

      val trace3 = Scatter()
        .withX(tool1ExpUnknownDurations)
        .withY(tool2ExpUnknownDurations)
        .withText(expUnknownNames)
        .withMode(ScatterMode(ScatterMode.Markers))
        .withMarker(Marker(color = Color.RGB(255, 255, 0), // yellow
                           symbol = plotDurationsMarkerSymbol,
                           size = plotDurationsMarkerSize))
        .withName("Unknown".toHtml)

      val trace4 = Scatter()
        .withX(tool1ConflictDurations)
        .withY(tool2ConflictDurations)
        .withText(conflictNames)
        .withMode(ScatterMode(ScatterMode.Markers))
        .withMarker(Marker(color = Color.RGB(0, 0, 128), // blue
                           symbol = plotDurationsMarkerSymbol,
                           size = plotDurationsMarkerSize))
        .withName("Conflict".toHtml)
      Seq(trace1, trace2, trace3, trace4)
    }

    val diagonalTrace = Scatter()
      .withX(Seq(0, toolRunInfo1._1.wallTimeLimit))
      .withY(Seq(0, toolRunInfo2._1.wallTimeLimit))
      .withFill(Fill.ToZeroY)
      .withFillcolor(Color.RGBA(204, 204, 255, 0.5))
      .withLine(
        Line().withColor(Color.RGBA(0, 0, 0, 0.0))
      )
      .withName("Show/hide diagonal".toHtml)
      .withShowlegend(false) //.
    // withName("Region where " + toolRunInfo2._1.toolName +
    //   " (" + toolRunInfo2._1.toolVersion + ") is faster")

    val text1 = Scatter()
      .withX(Seq(toolRunInfo1._1.wallTimeLimit / 3))
      .withY(Seq(toolRunInfo2._1.wallTimeLimit * 2 / 3))
      .withText(toolRunInfo1._1.toolName +
        (" (" + toolRunInfo1._1.toolVersion + ")<br>is faster").toHtml)
      .withMode(ScatterMode(ScatterMode.Text))
      .withName("")
      .withShowlegend(false)

    val text2 = Scatter()
      .withX(Seq(toolRunInfo1._1.wallTimeLimit * 2 / 3))
      .withY(Seq(toolRunInfo2._1.wallTimeLimit / 3))
      .withText(toolRunInfo2._1.toolName +
        (" (" + toolRunInfo2._1.toolVersion + ")<br>is faster").toHtml)
      .withMode(ScatterMode(ScatterMode.Text))
      .withName("")
      .withShowlegend(false)

    val layout = Layout(
      title = (extraTitle match {
        case Some(s) => s + " - "
        case None    => ""
      }) + "Benchmark durations by tool (seconds - wall time)".toHtml,
      legend = Legend(
        font = Font(
          size = 10
        ),
        yanchor = Anchor.Middle,
        xanchor = Anchor.Right
      ),
      width = plotDurationsWidth,
      height = plotDurationsHeight,
//      paper_bgcolor = Color.RGB(254, 247, 234),
//      plot_bgcolor = Color.RGB(254, 247, 234),
      hovermode = HoverMode.Closest //,
    ).withXaxis(Axis().withTitle(
        (toolRunInfo1._1.toolName + " (" + toolRunInfo1._1.toolVersion + ")").toHtml))
      .withYaxis(Axis().withTitle(
        (toolRunInfo2._1.toolName + " (" + toolRunInfo2._1.toolVersion + ")").toHtml))

    val data: Seq[Trace] =
      (if (plotDurationsShowDiagonal) Seq(diagonalTrace) else Nil) ++
        traces ++
        (if (plotDurationsShowDiagonal && plotDurationsShowDiagonalExplanations)
           Seq(text1, text2)
         else Nil)

    if (plotDurationsFile) {
      plotToFile(
        data,
        name = plotDurationsPdfNamePrefix + "_" +
          toolRunInfo1._1.toolName + "_" + toolRunInfo2._1.toolName,
        layout,
        plotDurationsWidth,
        plotDurationsHeight,
        plotDurationsFileScale
      )
    }

    if (Settings.plotDurations) {
      Plotly.plot("plotly.html", data, layout) // attaches to div element with id 'plot'
    }
  }

  private val colors =
    Seq( // K Kelly, Color Eng., 3 (6) (1965), colors of max contrast
      //"FFB300", // Vivid Yellow
      "007D34", // Vivid Green
      "C10020", // Vivid Red
      "00538A", // Strong Blue
      "803E75", // Strong Purple
      "FF6800", // Vivid Orange
      //"A6BDD7", // Very Light Blue
      "CEA262", // Grayish Yellow
      "817066", // Medium Gray
      "F6768E", // Strong Purplish Pink
      "FF7A5C", // Strong Yellowish Pink
      "53377A", // Strong Violet
      "FF8E00", // Vivid Orange Yellow
      "B32851", // Strong Purplish Red
      //"F4C800", // Vivid Greenish Yellow
      "7F180D", // Strong Reddish Brown
      "93AA00", // Vivid Yellowish Green
      "593315", // Deep Yellowish Brown
      "F13A13", // Vivid Reddish Orange
      "232C16" // Dark Olive Green
    )
  private var curColor: Int = 0
  private def getNextColor: String = {
    if (curColor >= colors.size - 1)
      curColor = 0
    val res = colors(curColor)
    curColor += 1
    res
  }

}
