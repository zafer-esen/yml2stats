package yml2stats

import org.jfree.chart.ChartFactory
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.scalatest.FunSuite

class HtmlPlotsSpec extends FunSuite {
  test("html plot: every point with its benchmark, also after sorting") {
    val series = new XYSeries("sat")
    series.add(new BenchmarkItem(2.0, 5.0, "b"))
    series.add(new BenchmarkItem(1.0, 7.0, "a"))
    val chart = ChartFactory.createScatterPlot(
      "", "x", "y", new XYSeriesCollection(series))
    val figure = HtmlPlots.figure(chart, 400)
    for (part <- Seq("\"x\":[1.0,2.0]", "\"y\":[7.0,5.0]",
                     "\"text\":[\"a\",\"b\"]"))
      assert(figure.contains(part))
  }
}
