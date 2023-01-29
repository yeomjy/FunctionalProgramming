package scalashop

import org.scalameter.*

object VerticalBoxBlurRunner:

    val standardConfig = config(
        Key.exec.minWarmupRuns := 5,
        Key.exec.maxWarmupRuns := 10,
        Key.exec.benchRuns := 10,
        Key.verbose := false
    ) withWarmer (Warmer.Default())

    def main(args: Array[String]): Unit =
        val radius = 3
        val width = 1920
        val height = 1080
        val src = Img(width, height)
        val dst = Img(width, height)
        val seqtime = standardConfig measure
                      {
                          VerticalBoxBlur.blur(src, dst, 0, width, radius)
                      }
        println(s"sequential blur time: $seqtime")

        val numTasks = 32
        val partime = standardConfig measure
                      {
                          VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
                      }
        println(s"fork/join blur time: $partime")
        println(s"speedup: ${seqtime.value / partime.value}")


/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface :

    /** Blurs the columns of the source image `src` into the destination image
     * `dst`, starting with `from` and ending with `end` (non-inclusive).
     *
     * Within each column, `blur` traverses the pixels by going from top to
     * bottom.
     */
    def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {

        val height = src.height

        var idx = from
        while (idx < end)
        {
            var h = 0
            while (h < height)
            {
                dst.update(idx, h, boxBlurKernel(src, idx, h, radius))
                h += 1
            }
            idx += 1
        }
    }

    /** Blurs the columns of the source image in parallel using `numTasks` tasks.
     *
     * Parallelization is done by stripping the source image `src` into
     * `numTasks` separate strips, where each strip is composed of some number of
     * columns.
     */
    def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
        val step = Math.max(src.width / numTasks, 1)
        val r = (0 until src.width by step)
//        val ranges = r zip (step until (src.width + step) by step)
//        val tasks = ranges.filter(t => t._2 <= src.width).map(t => task(blur(src, dst, t._1, t._2, radius)))
        val tasks = r map (i => task(blur(src, dst, i, clamp(i + step, 0, src.width), radius)))
        tasks.foreach(_.join)

    }

