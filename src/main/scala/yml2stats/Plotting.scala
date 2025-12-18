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

import org.jfree.chart.axis.LogarithmicAxis
import org.jfree.chart.block.BlockBorder
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.RectangleEdge
import org.jfree.chart.util.ExportUtils
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import org.jfree.chart.title.Title
import java.awt.geom.Rectangle2D
import org.jfree.data.Range

import yml2stats.Settings._

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
    val sortedToolRuns = toolRuns.sortBy { case (summary, _) =>
      (getEncodingSortKey(summary.notes), getCleanEncodingName(summary.notes), summary.toolName)
    }

    for ((summary, runInfos) <- sortedToolRuns) {
      val seriesName = formatNameForPlot(summary)
      val series = new XYSeries(seriesName)
      val relevantRuns = getRuns(runInfos).sortBy(_.duration)

      for ((run, index) <- relevantRuns.zipWithIndex) {
        series.add(index + 1, run.duration)
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
    println(s"Generating time-based cactus plot for '$fileNameSuffix' benchmarks...")

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
      val plotFileName = s"${Settings.cactusPlotByTimeFile}-$fileNameSuffix.pdf"
      val pdfFile = new File(s"$plotSaveDirectory/$plotFileName")
      val outputDir = new File(plotSaveDirectory)
      if (!outputDir.exists()) outputDir.mkdirs()
      println(s"Attempting to save plot to: ${pdfFile.getAbsolutePath}")

      ExportUtils.writeAsPDF(chart, options.width, options.height, pdfFile)
      println(s"Plot '$plotFileName' saved successfully!")

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

    if (useLogarithmicYCactus) {
      val logAxis = new LogarithmicAxis(plot.getRangeAxis.getLabel)
      logAxis.setLabelFont(axisLabelFont)
      logAxis.setTickLabelFont(tickLabelFont)
      logAxis.setAllowNegativesFlag(true)
      plot.setRangeAxis(logAxis)
    }

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
