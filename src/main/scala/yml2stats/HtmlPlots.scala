package yml2stats

import org.jfree.chart.JFreeChart
import org.jfree.chart.annotations.XYLineAnnotation
import org.jfree.chart.axis.{LogAxis, ValueAxis}
import org.jfree.chart.plot.ValueMarker
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.{Layer, RectangleEdge}
import org.jfree.data.xy.{XYDataItem, XYSeriesCollection}

import java.awt.geom.{Ellipse2D, PathIterator, Rectangle2D}
import java.awt.{BasicStroke, Color, Paint, Shape, Stroke}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters._

class BenchmarkItem(x : Double, y : Double, val benchmark : String)
  extends XYDataItem(x, y)

/**
 * Interactive versions of the plots in HTML with plotly.js, the benchmark
 * of a point shown on hover.
 */
object HtmlPlots {
  // in pixels
  private val InitialWidth = 800

  private lazy val plotlyJs = resource("plotly.js")
  private lazy val template = resource("plot.html")

  /**
   * Writes the interactive version of `chart`; `width` and `height` are
   * its size in the PDF, in points.
   */
  def write(chart : JFreeChart, width : Int, height : Int,
            fileName : String) : Unit = {
    val page = template
      .replace("@TITLE@", escapeHtml(Paths.get(fileName).getFileName.toString))
      .replace("@WIDTH@", InitialWidth.toString)
      .replace("@HEIGHT@", (InitialWidth * height / width).toString)
      .replace("@FIGURE@", figure(chart, width))
      // last, so that plotly.js is not searched for placeholders
      .replace("@PLOTLY@", plotlyJs)
    Files.write(Paths.get(fileName), page.getBytes(StandardCharsets.UTF_8))
  }

  /** The plotly figure of `chart`, `width` points wide in its PDF. */
  private[yml2stats] def figure(chart : JFreeChart, width : Int) : String = {
    val plot = chart.getXYPlot
    val dataset = plot.getDataset.asInstanceOf[XYSeriesCollection]
    val renderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
    val (xAxis, yAxis) = (plot.getDomainAxis, plot.getRangeAxis)
    // sizes relative to the width as in the PDF
    val scale = InitialWidth.toDouble / width
    val hover = s"%{text}<br>${escapeHtml(label(xAxis))}: %{x}<br>" +
      s"${escapeHtml(label(yAxis))}: %{y}<extra>%{fullData.name}</extra>"

    val traces = for (s <- 0 until dataset.getSeriesCount) yield {
      val series = dataset.getSeries(s)
      val items = (0 until series.getItemCount).map(series.getDataItem)
      val shape = renderer.lookupSeriesShape(s)
      val open = if (renderer.getItemShapeFilled(s, 0)) "" else "-open"
      Map(
        "type" -> "scatter",
        "mode" -> (if (renderer.getItemLineVisible(s, 0)) "lines+markers"
                   else "markers"),
        "name" -> series.getKey.toString,
        "x" -> items.map(_.getXValue), "y" -> items.map(_.getYValue),
        "text" -> items.map {
          case item : BenchmarkItem => item.benchmark
          case _ => ""
        },
        "hovertemplate" -> hover,
        "marker" -> Map(
          "symbol" -> (symbol(shape) + open),
          "color" -> rgba(renderer.lookupSeriesPaint(s)),
          // hidden markers only for the hover
          "size" -> items.indices.map(i =>
            if (renderer.getItemShapeVisible(s, i))
              shape.getBounds2D.getWidth * scale
            else 0.0)),
        "line" -> line(renderer.lookupSeriesPaint(s),
                       renderer.lookupSeriesStroke(s), scale))
    }

    def lineTrace(xs : Seq[Double], ys : Seq[Double], paint : Paint,
                  stroke : Stroke) =
      Map("type" -> "scatter", "mode" -> "lines", "x" -> xs, "y" -> ys,
          "line" -> line(paint, stroke, scale), "showlegend" -> false,
          "hoverinfo" -> "skip")
    val annotations = plot.getAnnotations.asScala.toSeq.collect {
      case a : XYLineAnnotation =>
        lineTrace(Seq(a.getX1, a.getX2), Seq(a.getY1, a.getY2), a.getPaint,
                  a.getStroke)
    }
    def markers(found : java.util.Collection[_]) : Seq[ValueMarker] =
      Option(found).toSeq.flatMap(_.asScala).collect {
        case m : ValueMarker => m
      }
    val domainMarkers = markers(plot.getDomainMarkers(Layer.FOREGROUND)).map {
      m => lineTrace(Seq(m.getValue, m.getValue),
                     Seq(yAxis.getLowerBound, yAxis.getUpperBound),
                     m.getPaint, m.getStroke)
    }
    val rangeMarkers = markers(plot.getRangeMarkers(Layer.FOREGROUND)).map {
      m => lineTrace(Seq(xAxis.getLowerBound, xAxis.getUpperBound),
                     Seq(m.getValue, m.getValue), m.getPaint, m.getStroke)
    }

    val legend = Option(chart.getLegend).map(_.getPosition) match {
      case Some(RectangleEdge.TOP) =>
        Map("orientation" -> "h", "x" -> 0.5, "xanchor" -> "center",
            "y" -> 1.02, "yanchor" -> "bottom")
      case _ => Map()
    }
    val layout = Map(
      "title" -> Map("text" ->
        Option(chart.getTitle).map(_.getText).getOrElse("")),
      "xaxis" -> axis(xAxis, plot.getDomainGridlinePaint),
      "yaxis" -> axis(yAxis, plot.getRangeGridlinePaint),
      "showlegend" -> (chart.getLegend != null), "legend" -> legend,
      "hovermode" -> "closest", "plot_bgcolor" -> "white",
      "margin" -> Map("t" -> 40, "r" -> 20))
    val config = Map(
      "scrollZoom" -> true, "displaylogo" -> false,
      "edits" -> Map("axisTitleText" -> true, "titleText" -> true,
                     "legendPosition" -> true),
      "toImageButtonOptions" -> Map("format" -> "svg"))
    json(Map("data" -> (traces ++ annotations ++ domainMarkers ++ rangeMarkers),
             "layout" -> layout, "config" -> config))
  }

  private def label(axis : ValueAxis) : String =
    Option(axis.getLabel).getOrElse("")

  private def axis(axis : ValueAxis, gridPaint : Paint) : Map[String, Any] = {
    val log = axis.isInstanceOf[LogAxis]
    Map("title" -> Map("text" -> label(axis)),
        "type" -> (if (log) "log" else "linear"),
        "showline" -> true, "mirror" -> true, "ticks" -> "outside",
        "zeroline" -> false, "gridcolor" -> rgba(gridPaint)) ++
      (if (axis.isAutoRange) Map()
       // the range of a log axis in exponents
       else Map("range" -> Seq(axis.getLowerBound, axis.getUpperBound)
                             .map(b => if (log) math.log10(b) else b)))
  }

  private def line(paint : Paint, stroke : Stroke,
                   scale : Double) : Map[String, Any] = stroke match {
    case s : BasicStroke =>
      Map("color" -> rgba(paint), "width" -> s.getLineWidth * scale,
          "dash" -> (Option(s.getDashArray).map(_.head) match {
                       case None => "solid"
                       case Some(dash) if dash <= 2 => "dot"
                       case _ => "dash"
                     }))
    case _ => Map("color" -> rgba(paint))
  }

  private def symbol(shape : Shape) : String = shape match {
    case _ : Ellipse2D => "circle"
    case _ : Rectangle2D => "square"
    case _ =>
      val corners = ArrayBuffer[Double]()
      val it = shape.getPathIterator(null)
      val coords = new Array[Double](6)
      while (!it.isDone) {
        if (Seq(PathIterator.SEG_MOVETO, PathIterator.SEG_LINETO)
              .contains(it.currentSegment(coords)))
          corners += coords(1)
        it.next()
      }
      corners.size match {
        // y grows downwards, so a single top corner is the tip
        case 3 => if (corners.count(_ == corners.min) == 1) "triangle-up"
                  else "triangle-down"
        case 4 => "diamond"
        case _ => "circle"
      }
  }

  private def rgba(paint : Paint) : String = paint match {
    case c : Color =>
      s"rgba(${c.getRed},${c.getGreen},${c.getBlue},${c.getAlpha / 255.0})"
    case _ => "black"
  }

  private def escapeHtml(text : String) : String =
    text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  private[yml2stats] def json(value : Any) : String = value match {
    case s : String =>
      s.flatMap {
        case '"' => "\\\""
        case '\\' => "\\\\"
        // no </script> in the page
        case '<' => "\\u003c"
        case c if c < ' ' => f"\\u${c.toInt}%04x"
        case c => c.toString
      }.mkString("\"", "", "\"")
    case d : Double => d.toString
    case f : Float => f.toString
    case i : Int => i.toString
    case b : Boolean => b.toString
    case m : Map[_, _] =>
      m.map { case (k, v) => json(k.toString) + ":" + json(v) }
       .mkString("{", ",", "}")
    case s : Seq[_] => s.map(json).mkString("[", ",", "]")
  }

  private def resource(name : String) : String = {
    val source = Source.fromResource(name)(scala.io.Codec.UTF8)
    try source.mkString finally source.close()
  }
}
