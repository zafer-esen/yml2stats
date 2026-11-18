package yml2stats
import org.jfree.chart.block.{BlockContainer, BorderArrangement, ColumnArrangement, FlowArrangement, RectangleConstraint}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.title.{CompositeTitle, LegendTitle}
import org.jfree.chart.ui.{HorizontalAlignment, RectangleInsets, Size2D, VerticalAlignment}
import yml2stats.Benchmarks._

import java.awt.Graphics2D
import java.awt.geom.GeneralPath
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
//import plotly._
//import plotly.element.{Color => PlotlyColor, _}
//import plotly.layout._
//import plotly.kaleido.Kaleido._

import org.jfree.chart.annotations.XYLineAnnotation
import org.jfree.chart.axis.{LogAxis, NumberAxis, ValueAxis}
import org.jfree.chart.plot.ValueMarker
import org.jfree.chart.block.BlockBorder
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.plot.SeriesRenderingOrder
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.RectangleEdge
import org.jfree.chart.util.ExportUtils
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import org.jfree.chart.title.Title
import java.awt.geom.Rectangle2D
import org.jfree.data.Range

import yml2stats.Settings._
import yml2stats.Main.printInfo

import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{BasicStroke, Color, Polygon, Shape, Font => AwtFont}
import java.io.File

object Plotting {

  private class HtmlString (val s : String) {
    def toHtml : String = {
      s.replace(" ", "&nbsp;")
    }
  }
  private implicit def stringToHtmlString(s : String): HtmlString = new HtmlString(s)


//  private def plotToFile(data : Seq[Trace], name : String, layout : Layout,
//                         width : Int, height : Int, scale : Float) : Unit = {
//    val outputDir = new java.io.File(plotSaveDirectory)
//    if (!outputDir.exists()) {
//      println(s"Warning: Save directory does not exist. Attempting to create: ${outputDir.getAbsolutePath}")
//      outputDir.mkdirs()
//    }
//    println(s"Attempting to save plot to directory: ${outputDir.getAbsolutePath}")
//
//    val saveResult : Try[Unit] = data.save(plotSaveDirectory, name, layout, plotOutputFormat,
//                                           width, height, scale, kaleidoDirectory)
//
//    saveResult match {
//      case Success(_) =>
//        println(s"Plot '$name' saved successfully!")
//      case Failure(e) =>
//        println(s"ERROR: Failed to save plot '$name'.")
//        e.printStackTrace()
//    }
//  }
//
//  private case class PlotStyle(color: String, dash: Dash, symbol: Symbol)
//
//  private val plotStyles = Seq(
//    PlotStyle("00538A", Dash.Solid, Symbol.Circle(false, false)),
//    PlotStyle("C10020", Dash.Solid, Symbol.Square(false, false)),
//    PlotStyle("007D34", Dash.Solid, Symbol.Diamond(false, false)),
//    PlotStyle("803E75", Dash.Solid, Symbol.Cross(false, false)),
//    PlotStyle("FF6800", Dash.DashDot, Symbol.Cross(false, true)),
//    PlotStyle("7F180D", Dash.DashDot, Symbol.Circle(false, true)),
//    PlotStyle("F6768E", Dash.DashDot, Symbol.Square(false, true))
//  )
//  private var curStyleIndex : Int = 0
//  private def getNextStyle : PlotStyle = {
//    if (curStyleIndex >= plotStyles.size)
//      curStyleIndex = 0
//    val res = plotStyles(curStyleIndex)
//    curStyleIndex += 1
//    res
//  }

//  def plotCactus(allToolRuns : Seq[(Summary, RunInfos)]) = {
//    curStyleIndex = 0
//    val traces = for (toolRuns <- allToolRuns) yield {
//      val sortedDurations = (toolRuns._2.satRuns ++ toolRuns._2.unsatRuns).
//        sortBy(_.duration)
//      val cumulativeDurations = sortedDurations.indices.map (
//        ind => sortedDurations.take(ind+1).map(_.duration).sum
//      )
//
//      val style = getNextStyle
//
//      Scatter().
//        withX(0 to cumulativeDurations.length).
//        withY(Seq(0.0) ++ cumulativeDurations).
//        withMode(ScatterMode(ScatterMode.Lines)).
//        withName((toolRuns._1.fullToolName + " (" +toolRuns._1.toolVersion + ")").toHtml).
//        withLine(Line().
//          withColor(PlotlyColor.StringColor(style.color)).
//          withShape(LineShape.Linear).
//          withDash(style.dash) // Use dash pattern for accessibility
//        )
//    }
//
//    val layout = Layout(
//      title = "Cactus plot".toHtml,
//      legend = Legend().
//        withFont(Font(size = 12)).
//        withYanchor(Anchor.Middle).
//        withXanchor(Anchor.Right),
//      plotCactusWidth, plotCactusHeight,
//      hovermode = HoverMode.Closest
//    ).withYaxis(Axis().withTitle("time (seconds)".toHtml).withType(AxisType.Log)).
//      withXaxis(Axis().withTitle("number of solved instances".toHtml))
//
//    if (Settings.doCactus) {
//      Plotly.plot("plotly.html", traces, layout)
//    }
//    if (plotCactusFile) {
//      plotToFile(traces, plotCactusPdfName, layout,
//        plotCactusWidth, plotCactusHeight, plotCactusFileScale)
//    }
//  }
//
//  def plotDuratıons(toolRunInfo1    : (Summary, RunInfos),
//                    toolRunInfo2    : (Summary, RunInfos),
//                    expSatNames     : Seq[String],
//                    expUnsatNames   : Seq[String],
//                    expUnknownNames : Seq[String]) = {
//    if (assertsOn) {
//      // passed run infos should be of same length and cotnain the same (base)-
//      //   named benchmarks
//      assert(toolRunInfo1._2.runs.length == toolRunInfo2._2.runs.length)
//      for (i <- toolRunInfo1._2.runs.indices) {
//        assert(toolRunInfo1._2.runs(i).bmBaseName ==
//          toolRunInfo2._2.runs(i).bmBaseName)
//      }
//    }
//
//    val tool1Durations = toolRunInfo1._2.runs.map {
//      run => run.bmBaseName -> (run.result match {
//        case Unknown if plotDurationsTreatUnknownAsTimeout => toolRunInfo1._1.wallTimeLimit
//        case _ => run.duration
//      })}.toMap
//    val tool2Durations = toolRunInfo2._2.runs.map {
//      run => run.bmBaseName -> (run.result match {
//        case Unknown if plotDurationsTreatUnknownAsTimeout => toolRunInfo2._1.wallTimeLimit
//        case _ => run.duration
//      })}.toMap
//
//    val tool1ExpSatDurations = for (name <- expSatNames) yield tool1Durations(name)
//    val tool1ExpUnsatDurations = for (name <- expUnsatNames) yield tool1Durations(name)
//    val tool1ExpUnknownDurations = for (name <- expUnknownNames) yield tool1Durations(name)
//
//    val tool2ExpSatDurations = for (name <- expSatNames) yield tool2Durations(name)
//    val tool2ExpUnsatDurations = for (name <- expUnsatNames) yield tool2Durations(name)
//    val tool2ExpUnknownDurations = for (name <- expUnknownNames) yield tool2Durations(name)
//
//    implicit def hex2int (hex: String): Int = Integer.parseInt(hex, 16)
//    val trace1 = Scatter().
//      withX(tool1ExpSatDurations).
//      withY(tool2ExpSatDurations).
//      withText(expSatNames).
//      withMode(ScatterMode(ScatterMode.Markers)).
//      withMarker(Marker(
//        color = PlotlyColor.RGB(0,128,0), // green
//        symbol = plotDurationsMarkerSymbol,
//        size = plotDurationsMarkerSize)).
//      withName("Expected sat".toHtml)
//
//    val trace2 = Scatter().
//      withX(tool1ExpUnsatDurations).
//      withY(tool2ExpUnsatDurations).
//      withText(expUnsatNames).
//      withMode(ScatterMode(ScatterMode.Markers)).
//      withMarker(Marker(
//        color = PlotlyColor.RGB(255,0,0), // red
//        symbol = plotDurationsMarkerSymbol,
//        size = plotDurationsMarkerSize)).
//      withName("Expected unsat".toHtml)
//
//    val trace3 = Scatter().
//      withX(tool1ExpUnknownDurations).
//      withY(tool2ExpUnknownDurations).
//      withText(expUnknownNames).
//      withMode(ScatterMode(ScatterMode.Markers)).
//      withMarker(Marker(
//        color = PlotlyColor.RGB("80","3E","75"), // purple
//        symbol = plotDurationsMarkerSymbol,
//        size = plotDurationsMarkerSize)).
//      withName("Expected unknown".toHtml)
//
//    val diagonalTrace = Scatter().
//      withX(Seq(0, toolRunInfo1._1.wallTimeLimit)).
//      withY(Seq(0, toolRunInfo2._1.wallTimeLimit)).
//      withFill(Fill.ToZeroY).
//      withFillcolor(PlotlyColor.RGBA(204,204,255,0.5)).
//      withLine(
//        Line().
//          withColor(PlotlyColor.RGBA(0,0,0,0.0))
//      ).withName("Show/hide diagonal".toHtml).withShowlegend(false)//.
//     // withName("Region where " + toolRunInfo2._1.toolName +
//     //   " (" + toolRunInfo2._1.toolVersion + ") is faster")
//
//    val text1 = Scatter().
//      withX(Seq(toolRunInfo1._1.wallTimeLimit/3)).
//      withY(Seq(toolRunInfo2._1.wallTimeLimit*2/3)).
//      withText(toolRunInfo1._1.fullToolName +
//        (" (" + toolRunInfo1._1.toolVersion + ")<br>is faster").toHtml).
//      withMode(ScatterMode(ScatterMode.Text)).withName("").withShowlegend(false)
//
//    val text2 = Scatter().
//      withX(Seq(toolRunInfo1._1.wallTimeLimit*2/3)).
//      withY(Seq(toolRunInfo2._1.wallTimeLimit/3)).
//      withText(toolRunInfo2._1.fullToolName +
//        (" (" + toolRunInfo2._1.toolVersion + ")<br>is faster").toHtml).
//      withMode(ScatterMode(ScatterMode.Text)).withName("").withShowlegend(false)
//
//    val layout = Layout(
//      title = "Benchmark durations by tool (seconds - wall time)".toHtml,
//      legend = Legend(
//        font = Font(
//          size = 10
//        ),
//        yanchor = Anchor.Middle,
//        xanchor = Anchor.Right
//      ),
//      width = plotDurationsWidth,
//      height = plotDurationsHeight,
////      paper_bgcolor = Color.RGB(254, 247, 234),
////      plot_bgcolor = Color.RGB(254, 247, 234),
//      hovermode = HoverMode.Closest//,
//    ).withXaxis(Axis().withTitle(
//      (toolRunInfo1._1.fullToolName + " (" + toolRunInfo1._1.toolVersion + ")").toHtml)).
//      withYaxis(Axis().withTitle(
//        (toolRunInfo2._1.fullToolName + " (" + toolRunInfo2._1.toolVersion + ")").toHtml))
//
//
//    val data =
//      (if(plotDurationsShowDiagonal) Seq(diagonalTrace) else Nil) ++
//        Seq(trace1, trace2, trace3) ++
//        (if (plotDurationsShowDiagonal && plotDurationsShowDiagonalExplanations)
//          Seq(text1, text2) else Nil)
//
//    if (plotDurationsFile) {
//      plotToFile(data,
//        name = plotDurationsPdfNamePrefix + "_" +
//        toolRunInfo1._1.fullToolName + "_" + toolRunInfo2._1.fullToolName,
//        layout, plotDurationsWidth, plotDurationsHeight, plotDurationsFileScale)
//    }
//
//    if (Settings.plotDurations) {
//      Plotly.plot("plotly.html", data, layout) // attaches to div element with id 'plot'
//    }
//  }

//  /**
//   * @param toolRuns A sequence of tool summaries and their corresponding run information.
//   */
//  def plotCactusByTime(toolRuns: Seq[(Summary, RunInfos)]): Unit = {
//    curStyleIndex = 0 // Reset style index before creating traces
//
//    val sortedToolRuns = toolRuns.sortBy { case (summary, _) =>
//      (getEncodingSortKey(summary.notes), getCleanEncodingName(summary.notes), summary.toolName)
//    }
//
//    val traces = for ((summary, runs) <- sortedToolRuns) yield {
//      val seriesName = s"${getCleanEncodingName(summary.notes)} ${summary.toolName}"
//
//      val correctRuns = runs.correctRuns.sortBy(_.duration)
//
//      // X-axis: Number of solved benchmarks (0, 1, 2, 3...)
//      val xData = 0 to correctRuns.length
//      // Y-axis: Time taken for the Nth benchmark (0.0, time1, time2, time3...)
//      val yData = Seq(0.0) ++ correctRuns.map(_.duration)
//      val style = getNextStyle
//
//      Scatter()
//        .withX(xData)
//        .withY(yData)
//        .withName(seriesName.toHtml)
//        .withMode(ScatterMode(ScatterMode.Lines, ScatterMode.Markers))
//        .withLine(Line()
//          .withColor(PlotlyColor.StringColor(style.color))
//          .withDash(style.dash)
//          .withWidth(2)
//        )
//        .withMarker(Marker()
//          .withSymbol(style.symbol)
//          .withSize(6)
//        )
//    }
//
//    val yAxisType = if (useLogarithmicYCactus) AxisType.Log else AxisType.Linear
//
//    val layout = Layout(
//      title = "Cactus Plot: Solved Benchmarks vs. Time".toHtml,
//      width = plotCactusByTimeWidth,
//      height = plotCactusByTimeHeight,
//      legend = Legend(
//        font = Font(size = 12),
//        yanchor = Anchor.Bottom,
//        xanchor = Anchor.Right
//      ),
//      hovermode = HoverMode.Closest
//    ).withXaxis(Axis()
//        .withTitle("Number of Benchmarks Solved Correctly".toHtml)
//        .withGridcolor(PlotlyColor.StringColor("#e5e5e5")) // Lighter grid lines
//    ).withYaxis(Axis()
//        .withTitle("Time (seconds)".toHtml)
//        .withType(yAxisType)
//        .withGridcolor(PlotlyColor.StringColor("#e5e5e5"))
//    )
//
//    println(s"Generating time-based cactus plot to ${cactusPlotByTimeFile}")
//    plotToFile(
//      data = traces,
//      name = cactusPlotByTimeFile,
//      layout = layout,
//      width = plotCactusByTimeWidth,
//      height = plotCactusByTimeHeight,
//      scale = plotCactusByTimeFileScale
//    )
//  }

  private def createCactusDataset(
    toolRuns: Seq[(Summary, RunInfos)],
    getRuns: RunInfos => Seq[RunInfo]
  ): XYSeriesCollection = {
    val dataset = new XYSeriesCollection()
    val sortedToolRuns =
      if (selectedTools.nonEmpty) toolRuns
      else toolRuns.sortBy { case (summary, _) =>
        (getEncodingSortKey(summary.notes), getCleanEncodingName(summary.notes), summary.toolName)
      }

    val floor =
      if (useLogarithmicYCactus)
        logAxisFloor(toolRuns.flatMap(row => getRuns(row._2)).map(_.duration))
      else 0.0

    for ((summary, runInfos) <- sortedToolRuns) {
      val seriesName = formatNameForPlot(summary)
      val series = new XYSeries(seriesName)
      val relevantRuns = getRuns(runInfos).sortBy(_.duration)

      for ((run, index) <- relevantRuns.zipWithIndex) {
        series.add(new BenchmarkItem(index + 1, math.max(run.duration, floor),
                                     run.bmName))
      }
      dataset.addSeries(series)
    }
    dataset
  }

  private def generateAndSaveCactusPlot(
    toolRuns: Seq[(Summary, RunInfos)],
    getRuns: RunInfos => Seq[RunInfo],
    fileNameSuffix: String,
    title: String,
    xAxisLabel: String,
    options: CactusPlotDisplayOptions // <-- New parameter for display options
  ): Unit = {
    printInfo(s"Generating time-based cactus plot for '$fileNameSuffix' benchmarks...")

    val dataset = createCactusDataset(toolRuns, getRuns)

    val chart = ChartFactory.createXYLineChart(
      title,
      xAxisLabel,
      "Time (s)",
      dataset,
      PlotOrientation.VERTICAL,
      true, true, false
    )

    customizeChartAppearance(chart)

    if (!options.showTitle) {
      chart.setTitle(null.asInstanceOf[String])
    }
    if (!options.showAxes) {
      chart.getXYPlot.getDomainAxis.setLabel(null)
      chart.getXYPlot.getRangeAxis.setLabel(null)
    }
    if (!options.showLegend) {
      chart.removeLegend()
    }

    try {
      val plotName = s"${Settings.cactusPlotByTimeFile}-$fileNameSuffix"
      val plotFile = new File(s"$plotSaveDirectory/$plotName")
      printInfo(s"Attempting to save plot to: ${plotFile.getAbsolutePath}")

      writePlot(chart, options.width, options.height, plotFile.getPath)

    } catch {
      case e: Exception =>
        System.err.println(s"ERROR: Failed to save plot as PDF.")
        e.printStackTrace()
    }
  }


// 4. REPLACE your existing `plotCactusByTime` with this version, which now passes the options.
  def plotCactusByTime(toolRuns: Seq[(Summary, RunInfos)]): Unit = {
    generateAndSaveCactusPlot(
      toolRuns,
      _.correctRuns,
      "total-solved",
      "Total Solved",
      "Number of solved benchmarks",
      Settings.cactusPlotTotalOptions
    )

    generateAndSaveCactusPlot(
      toolRuns,
      _.correctSatRuns,
      "safe-solved",
      "S Solved",
      "Number of solved S benchmarks",
      Settings.cactusPlotSafeOptions
    )

    generateAndSaveCactusPlot(
      toolRuns,
      _.correctUnsatRuns,
      "unsafe-solved",
      "U Solved",
      "Number of solved U benchmarks",
      Settings.cactusPlotUnsafeOptions
    )
  }

  /**
   * Writes the cactus plots `NAME-sat.pdf` and `NAME-unsat.pdf`, on a
   * logarithmic axis.
   */
  def plotCactusPerVerdict(toolRuns : Seq[(Summary, RunInfos)],
                           name : String) : Unit =
    for ((verdict, solvedRuns) <- RunInfos.verdicts) {
      val chart = ChartFactory.createXYLineChart(
        "", s"$verdict benchmarks solved", "Duration (s)",
        createCactusDataset(toolRuns, solvedRuns),
        PlotOrientation.VERTICAL, true, false, false)
      customizeChartAppearance(chart)
      chart.setTitle(null.asInstanceOf[String])
      chart.getLegend.setPosition(RectangleEdge.TOP)
      writePlot(chart, 400, 320, s"$name-$verdict")
    }

  // the benchmarks with different verdicts of the two tools
  private[yml2stats] val Inconsistent = "inconsistent"

  /**
   * The points of the scatter plot of two tools by verdict: the benchmarks
   * run by both and solved by at least one, an unsolved one at the time
   * limit.
   */
  private[yml2stats] def scatterPoints(runs : RunInfos, otherRuns : RunInfos,
                                       limit : Double)
      : Map[String, Seq[(String, Double, Double)]] = {
    def solved(r : RunInfos) : Map[String, (String, Double)] =
      (for ((verdict, solvedRuns) <- RunInfos.verdicts; run <- solvedRuns(r))
       yield run.bmBaseName -> ((verdict, run.duration))).toMap
    val (toolSolved, otherSolved) = (solved(runs), solved(otherRuns))
    val benchmarks = runs.runs.map(r => r.bmBaseName -> r.bmName).toMap
    val runByBoth =
      benchmarks.keySet intersect otherRuns.runs.map(_.bmBaseName).toSet
    val points =
      for (name <- (toolSolved.keySet ++ otherSolved.keySet).toSeq.sorted
           if runByBoth(name)) yield {
        val (byTool, byOther) = (toolSolved.get(name), otherSolved.get(name))
        val verdict = (byTool ++ byOther).map(_._1).toSeq.distinct match {
          case Seq(v) => v
          case _ => Inconsistent
        }
        (verdict, (benchmarks(name), byTool.map(_._2).getOrElse(limit),
                   byOther.map(_._2).getOrElse(limit)))
      }
    points.groupBy(_._1).map { case (verdict, ps) => verdict -> ps.map(_._2) }
  }

  /**
   * Writes the scatter plot `NAME.pdf` of the durations of `tool` (x) and
   * `other` (y), with the diagonal and dashed lines at the time limit.
   */
  def plotScatter(tool : (Summary, RunInfos), other : (Summary, RunInfos),
                  name : String) : Unit = {
    val limit = math.max(tool._1.wallTimeLimit, other._1.wallTimeLimit)
    val points = scatterPoints(tool._2, other._2, limit)
    val values = points.values.flatten.flatMap { case (_, x, y) => Seq(x, y) }
    val low = logAxisFloor(values ++ Seq(limit))
    val dataset = new XYSeriesCollection()
    for (verdict <- RunInfos.verdicts.map(_._1) ++
                    Seq(Inconsistent).filter(points.contains)) {
      val verdictPoints = points.getOrElse(verdict, Nil)
      val series = new XYSeries(s"$verdict (${verdictPoints.length})")
      for ((benchmark, x, y) <- verdictPoints)
        series.add(new BenchmarkItem(math.max(x, low), math.max(y, low), benchmark))
      dataset.addSeries(series)
    }
    val chart = ChartFactory.createScatterPlot(
      "", s"${formatNameForPlot(tool._1)} duration (s)",
      s"${formatNameForPlot(other._1)} duration (s)", dataset)
    customizeChartAppearance(chart)
    chart.setTitle(null.asInstanceOf[String])
    chart.getLegend.setPosition(RectangleEdge.TOP)

    val plot = chart.getXYPlot
    val (domainAxis, rangeAxis) =
      (logAxis(plot.getDomainAxis), logAxis(plot.getRangeAxis))
    for (axis <- Seq(domainAxis, rangeAxis)) axis.setRange(low, limit * 1.5)
    plot.setDomainAxis(domainAxis)
    plot.setRangeAxis(rangeAxis)
    val lineColor = new Color(120, 120, 120)
    plot.addAnnotation(new XYLineAnnotation(low, low, limit, limit,
                                            new BasicStroke(0.8f), lineColor))
    val dashed = new BasicStroke(0.8f, BasicStroke.CAP_BUTT,
                                 BasicStroke.JOIN_MITER, 10.0f,
                                 Array(4.0f, 3.0f), 0.0f)
    plot.addDomainMarker(new ValueMarker(limit, lineColor, dashed))
    plot.addRangeMarker(new ValueMarker(limit, lineColor, dashed))

    val renderer = new XYLineAndShapeRenderer(false, true) {
      // opaque in the legend, to be readable
      override def getLegendItem(datasetIndex : Int, series : Int) = {
        val item = super.getLegendItem(datasetIndex, series)
        val color = lookupSeriesPaint(series).asInstanceOf[Color]
        item.setFillPaint(new Color(color.getRed, color.getGreen, color.getBlue))
        item
      }
    }
    // no outlines, as their thin lines make dense regions pale; unsat drawn
    // over sat, inconsistent over both
    val (width, height) = (320, 340)
    val size = scatterMarkerSize.getOrElse(2.5) * documentScale(width)
    val alpha = (scatterMarkerOpacity.getOrElse(0.8) * 255).round.toInt
    val styles = Seq((circle _, new Color(0x2a, 0x78, 0xd6, alpha)),
                     (triangle _, new Color(0xeb, 0x68, 0x34, alpha)),
                     (diamond _, new Color(0x1b, 0xaf, 0x7a, alpha)))
    renderer.setDrawOutlines(false)
    for (((shape, color), i) <- styles.zipWithIndex) {
      renderer.setSeriesShape(i, shape(size))
      renderer.setLegendShape(i, shape(4 * documentScale(width)))
      renderer.setSeriesPaint(i, color)
    }
    plot.setRenderer(renderer)
    plot.setSeriesRenderingOrder(SeriesRenderingOrder.FORWARD)
    writePlot(chart, width, height, name)
  }

  // scatter markers, `size` points wide
  private def circle(size : Double) : Shape =
    new Ellipse2D.Double(-size / 2, -size / 2, size, size)

  private def triangle(size : Double) : Shape = {
    val half = size / 2
    val result = new GeneralPath()
    result.moveTo(0, -half)
    result.lineTo(half, half)
    result.lineTo(-half, half)
    result.closePath()
    result
  }

  // sides of `size`, larger than the circle, so that the rare inconsistent
  // points stand out
  private def diamond(size : Double) : Shape = {
    val half = size / math.sqrt(2)
    val result = new GeneralPath()
    result.moveTo(0, -half)
    result.lineTo(half, 0)
    result.lineTo(0, half)
    result.lineTo(-half, 0)
    result.closePath()
    result
  }

  // the power of ten below the shortest positive duration, where a duration
  // of 0 s, as in logs in whole seconds, is drawn on a log axis
  private def logAxisFloor(durations : Iterable[Double]) : Double = {
    val positive = durations.filter(_ > 0)
    if (positive.isEmpty) 1.0
    else math.pow(10, math.ceil(math.log10(positive.min)) - 1)
  }

  // ticks at integer powers of ten
  private def logAxis(axis : ValueAxis) : LogAxis = {
    val result = new LogAxis(axis.getLabel)
    result.setLabelFont(axis.getLabelFont)
    result.setTickLabelFont(axis.getTickLabelFont)
    result.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    result.setMinorTickMarksVisible(true)
    result
  }

  // NAME.pdf and, with -plot-html, NAME.html
  private def writePlot(chart : JFreeChart, width : Int, defaultHeight : Int,
                        name : String) : Unit = {
    val height = plotAspect.map(a => (width * a).round.toInt)
                           .getOrElse(defaultHeight)
    setDocumentFonts(chart, width)
    val pdf = new File(s"$name.pdf")
    Option(pdf.getAbsoluteFile.getParentFile).foreach(_.mkdirs())
    ExportUtils.writeAsPDF(chart, width, height, pdf)
    printInfo(s"Plot saved to $name.pdf")
    if (plotHtml) {
      HtmlPlots.write(chart, width, height, s"$name.html")
      printInfo(s"Plot saved to $name.html")
    }
  }

  // points of a plot `width` points wide per point of the document, where
  // the plot is plotWidth points wide
  private def documentScale(width : Int) : Double =
    width / plotWidth.getOrElse(width.toDouble)

  // the text at plotFontSize in the document, else at its size
  private def setDocumentFonts(chart : JFreeChart, width : Int) : Unit = {
    val scale = documentScale(width)
    def sized(font : AwtFont) = font.deriveFont(
      (plotFontSize.getOrElse(font.getSize2D.toDouble) * scale).toFloat)
    val plot = chart.getXYPlot
    for (axis <- Seq(plot.getDomainAxis, plot.getRangeAxis)) {
      axis.setLabelFont(sized(axis.getLabelFont))
      axis.setTickLabelFont(sized(axis.getTickLabelFont))
    }
    for (legend <- Option(chart.getLegend)) {
      val font = legend.getItemFont
      legend.setItemFont(sized(font))
      // padding in proportion to the text
      val factor = sized(font).getSize2D / font.getSize2D
      val padding = legend.getItemLabelPadding
      legend.setItemLabelPadding(new RectangleInsets(
        padding.getTop * factor, padding.getLeft * factor,
        padding.getBottom * factor, padding.getRight * factor))
    }
  }

  private def generateStyles(
    numStyles: Int,
    colors: Array[Color],
    strokes: Array[BasicStroke],
    baseShapes: Array[Shape]
  ): Seq[(Color, BasicStroke, Shape, Boolean)] = {

    val filledOptions = Array(true, false)
    val styles = scala.collection.mutable.ArrayBuffer.empty[(Color, BasicStroke, Shape, Boolean)]

    var styleCount = 0
    var strokeIdx = 0
    var colorIdx  = 0
    var shapeIdx  = 0
    var filledIdx = 0

    while (styleCount < numStyles) {

      val stroke = strokes(strokeIdx % strokes.length)
      val color  = colors(colorIdx % colors.length)
      val shape  = baseShapes(shapeIdx % baseShapes.length)
      val filled = filledOptions(filledIdx % filledOptions.length)

      styles += ((color, stroke, shape, filled))
      styleCount += 1

      shapeIdx += 1
      if (shapeIdx % baseShapes.length == 0) filledIdx += 1     // toggle filled/unfilled after all shapes
      colorIdx = (colorIdx + 1) % colors.length                // advance color every step
      strokeIdx = (strokeIdx + 1) % strokes.length            // advance stroke every step (optional)
    }

    styles.toSeq
  }


  /**
   * A shared helper function to apply a consistent, academic style to a JFreeChart chart.
   * @param chart The chart to customize.
   */
  private def customizeChartAppearance(chart: JFreeChart): Unit = {
    val plot = chart.getXYPlot
    // before the labels are copied to log axes
    for (axis <- Seq(plot.getDomainAxis, plot.getRangeAxis))
      axis.setLabel(axisLabelRenames.getOrElse(axis.getLabel, axis.getLabel))
    plot.setBackgroundPaint(Color.WHITE)
    val gridColor = new Color(200, 200, 200)
    plot.setDomainGridlinePaint(gridColor)
    plot.setRangeGridlinePaint(gridColor)

    val titleFont     = new AwtFont("Serif", AwtFont.BOLD, 12)
    val axisLabelFont = new AwtFont("SansSerif", AwtFont.PLAIN, 12)
    val tickLabelFont = new AwtFont("SansSerif", AwtFont.PLAIN, 12)
    val legendFont    = new AwtFont("SansSerif", AwtFont.PLAIN, 10)

    chart.getTitle.setFont(titleFont)
    plot.getDomainAxis.setLabelFont(axisLabelFont)
    plot.getRangeAxis.setLabelFont(axisLabelFont)
    plot.getDomainAxis.setTickLabelFont(tickLabelFont)
    plot.getRangeAxis.setTickLabelFont(tickLabelFont)

    if (useLogarithmicYCactus)
      plot.setRangeAxis(logAxis(plot.getRangeAxis))

    val legend: LegendTitle = chart.getLegend()
    if (legend != null) {
      legend.setPosition(RectangleEdge.RIGHT)
      //legend.setItemFont(legendFont)
      legend.setFrame(BlockBorder.NONE)
    }

    val customRenderer = new XYLineAndShapeRenderer(true, true) {
      override def getItemShapeVisible(series: Int, item: Int): Boolean = {
        val itemCount = getPlot.getDataset.getItemCount(series)
        if (itemCount <= 0) return false

        // For series with few solved instances, show all markers.
        // Otherwise, aim for a target number of markers.
        val targetMarkers = 15
        val dynamicFrequency =
          if (itemCount > targetMarkers)
            Math.ceil(itemCount / targetMarkers).toInt
          else 1

        // Always show the last point
        if (item == itemCount - 1) return true

        item % dynamicFrequency == 0
      }
    }
    plot.setRenderer(customRenderer)

    // --- Define the style components ---
    val colors = Array(
      new Color(0, 114, 178),   // Blue
      new Color(230, 159, 0),   // Orange
      new Color(0, 158, 115),   // Green
      new Color(213, 94, 0),    // Vermillion
      new Color(86, 180, 233),  // Sky Blue
      new Color(204, 121, 167), // Reddish Purple
      //new Color(240, 228, 66),  // Yellow
      new Color(110, 20, 140),  // Custom Purple
      Color.BLACK)

    val lineThickness = 1.5f
    val strokes = Array(
      // Solid line
      new BasicStroke(lineThickness),
      // Dashed line
      //new BasicStroke(lineThickness, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(8.0f, 6.0f), 0.0f),
      // Dotted line
      new BasicStroke(lineThickness, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(2.0f, 5.0f), 0.0f),
      // Dash-dot line
      //new BasicStroke(lineThickness, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(10.0f, 4.0f, 2.0f, 4.0f), 0.0f)
      )

    val shapeSize = 5.0
    val halfSize = shapeSize / 2.0

    val circle = new Ellipse2D.Double(-halfSize, -halfSize, shapeSize, shapeSize)
    val square = new Rectangle2D.Double(-halfSize, -halfSize, shapeSize, shapeSize)
    val h = halfSize.toInt
    val triangleUp = new Polygon(Array(0, h, -h), Array(-h, h, h), 3)
    val triangleDown = new Polygon(Array(0, h, -h), Array(h, -h, -h), 3)
    val diamond = new Polygon(Array(0, h, 0, -h), Array(-h, 0, h, 0), 4)

//    val cross = new GeneralPath()
//    cross.moveTo(-halfSize, 0)
//    cross.lineTo(halfSize, 0)
//    cross.moveTo(0, -halfSize)
//    cross.lineTo(0, halfSize)

    val baseShapes: Array[Shape] = Array(circle, square, triangleUp, triangleDown, diamond)

    val numTraces = plot.getSeriesCount
    val stylesToApply = generateStyles(numTraces, colors, strokes, baseShapes)

    for (((color, stroke, shape, isFilled), i) <- stylesToApply.zipWithIndex) {
      customRenderer.setSeriesPaint(i, color)
      customRenderer.setSeriesStroke(i, stroke)
      customRenderer.setSeriesShape(i, shape)
      customRenderer.setLegendShape(i, shape)

      customRenderer.setDrawOutlines(true)
      customRenderer.setSeriesOutlinePaint(i, color)
      customRenderer.setSeriesOutlineStroke(i, new BasicStroke(Settings.plotMarkerOutlineThickness))

      customRenderer.setUseFillPaint(true)
      customRenderer.setSeriesShapesFilled(i, isFilled)

      if (isFilled) customRenderer.setSeriesFillPaint(i, color)
      else customRenderer.setSeriesFillPaint(i, Color.WHITE)
    }
  }
}
