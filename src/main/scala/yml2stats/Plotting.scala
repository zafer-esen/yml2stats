package yml2stats
import Benchmarks._
import plotly.{element, _}
import plotly.element._
import plotly.layout._

object Plotting {
  def plotRuns(toolRunInfo1 : (Summary, RunInfos), toolRunInfo2 : (Summary, RunInfos)) = {

//    val data = for ((run, ind) <- toolRunInfo._2.runs.zipWithIndex) yield {
//      Map("name" -> run.bmBaseName, "duration (s)" -> run.duration)
//    }
    // todo: wip
    //val x = 1 to toolRunInfo1._2.length

    val bmNames = toolRunInfo1._2.runs.map(_.bmBaseName)
    val x = toolRunInfo1._2.runs.map(_.duration)
    val y = toolRunInfo2._2.runs.map(_.duration)
//    val annotations = (x zip y.zipWithIndex).map {
//      case (x, y) =>
//        Annotation(
//          x = x,
//          y = y._1,
//          text = bmNames(y._2),
//          xanchor = Anchor.Center,
//          yanchor = Anchor.Bottom,
//          showarrow = false
//        )
//    }

    val trace1 = Scatter().
      withX(x).
      withY(y).
      withText(bmNames).
      withMode(ScatterMode(ScatterMode.Markers)).
      withName(toolRunInfo1._1.toolName).
      withMarker(Marker(
        color = Color.RGBA(156, 165, 196, 0.95),
        line = Line(
          color = Color.RGBA(156, 165, 196, 1.0),
          width = 1.0
        ),
        symbol = Symbol.Circle(),
        size = 16))


    val layout = Layout(
      title = "Durations of all benchmarks",
      xaxis = Axis(
        showgrid = false,
        showline = true,
        linecolor = Color.RGB(102, 102, 102),
        titlefont = Font().withColor(Color.RGB(204, 204, 204)),
        tickfont = Font().withColor(color = Color.RGB(102, 102, 102)),
        autotick = false,
        dtick = 10.0,
        ticks = Ticks.Outside,
        tickcolor = Color.RGB(102, 102, 102)
      ),
      margin = Margin(
        l = 140,
        r = 40,
        b = 50,
        t = 80
      ),
      legend = Legend(
        font = Font(
          size = 10
        ),
        yanchor = Anchor.Middle,
        xanchor = Anchor.Right
      ),
      width = 600,
      height = 400,
      paper_bgcolor = Color.RGB(254, 247, 234),
      plot_bgcolor = Color.RGB(254, 247, 234),
      hovermode = HoverMode.Closest//,
      //annotations = annotations
    )

    val data = Seq(trace1)

    Plotly.plot("plotly.html", data, layout)  // attaches to div element with id 'plot'
  }
}

//import vegas._

//
//object Plotting {
//  def plotRuns(toolRunInfo : (Summary, RunInfos)) = {
//
//    val data = for ((run, ind) <- toolRunInfo._2.runs.zipWithIndex) yield {
//      Map("name" -> run.bmBaseName, "duration (s)" -> run.duration)
//    }
//
//
//    val plot = Vegas("Durations (all) " + toolRunInfo._1.toolName).
//      withData(data).
//      encodeX("name", Nom).
//      encodeY("duration (s)", Quant).
//      mark(Line)
//    plot.show
//  }
//}