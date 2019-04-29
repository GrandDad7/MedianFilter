import java.awt.{Color, Font, Graphics, Image}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.{JComponent, JFileChooser, JFrame}
import scala.swing.Font


object Exam2 extends App { // App allows the entire code to be ran without a main
  def currentTime: Long = System.currentTimeMillis()

  def deltaTime(t: Long): Long = System.currentTimeMillis() - t

  def wait(t: Int): Unit = Thread.sleep(t)

  def clamp(n: Int, min: Int, max: Int): Int = {
    math.min(max, math.max(n, min))
  }


  def loadImage: BufferedImage = {
    val choose = new JFileChooser
    val filter = new FileNameExtensionFilter("JPG & PNG Images", "jpg", "png")
    choose.setFileFilter(filter)
    val returnVal = choose.showOpenDialog(null)
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      return ImageIO.read(choose.getSelectedFile)
    }
    null
  }


  def resizeImage(img: BufferedImage, newWidth: Int, newHeight: Int): BufferedImage = {
    val tmp = img.getScaledInstance(300, 300, Image.SCALE_SMOOTH)
    val retImg = new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_ARGB)
    val g2d = retImg.createGraphics
    g2d.drawImage(tmp, 0, 0, null)
    g2d.dispose()

    retImg
  }

  def cloneImage(img: BufferedImage): BufferedImage = {
    val image = new BufferedImage(img.getWidth, img.getHeight, img.getType)
    val graphics = image.getGraphics
    graphics.drawImage(image, 0, 0, null)
    graphics.dispose()
    image
  }

  def medianFilter(img: BufferedImage): BufferedImage = {
    medianFilter(img, 6)
  }

  def medianFilter(img: BufferedImage, str: Int): BufferedImage = { // old no parallel median filter
    val square: Int = math.max(str * str, 9)
    val imagePixels = new Array[Color](square)
    var R = new Array[Int](9)
    var G = new Array[Int](9)
    var B = new Array[Int](9)
    val newImg = cloneImage(img)

    for (x: Int <- 0 until img.getWidth()) {
      for (y: Int <- 0 until img.getHeight()) {
        var i = 0
        val a = (str - 1) / 2
        for (w <- -a to a) {
          for (h <- -a to a) {
            imagePixels(i) = new Color(img.getRGB(clamp(w + x, 0, img.getWidth - 1), clamp(h + y, 0, img.getHeight - 1)))
            i += 1
          }
        }
        for (i <- 0 until str) {
          R(i) = imagePixels(i).getRed
          G(i) = imagePixels(i).getGreen
          B(i) = imagePixels(i).getBlue

        }

        R = R.sorted
        G = G.sorted
        B = B.sorted
        newImg.setRGB(x, y, new Color(R((str / 2) + 1), G((str / 2) + 1), B((str / 2) + 1)).getRGB)
      }
    }

    newImg
  }

  def MedianFilterParallel(img: BufferedImage, str: Int): FilteredImage = {
    val start: Long = currentTime
    val newImg = cloneImage(img)

    for (xy: Int <- (0 until (img.getWidth() * img.getHeight())).par) {
      val x: Int = xy % img.getWidth()
      val y: Int = math.floor(xy / img.getWidth()).toInt
      val color = getMedian(img, x, y, str)
      newImg.setRGB(x, y, color.getRGB)
    }


    new FilteredImage(newImg,deltaTime(start))
  }

  def MedianFilterNoParallel(img: BufferedImage, str: Int): FilteredImage = {
    val start: Long = currentTime
    val newImg = cloneImage(img)
    for (xy: Int <- 0 until (img.getWidth() * img.getHeight())) {
      val x: Int = xy % img.getWidth()
      val y: Int = math.floor(xy / img.getWidth()).toInt
      val color = getMedian(img, x, y, str)
      newImg.setRGB(x, y, color.getRGB)
    }

    new FilteredImage(newImg,deltaTime(start))
  }


  def getMedian(img: BufferedImage, x: Int, y: Int, size: Int): Color = {
    var s = size
    if (size % 2 == 0)
      s += 1
    val square: Int = math.max(s * s, 9)
    var R = new Array[Int](square)
    var G = new Array[Int](square)
    var B = new Array[Int](square)
    val imagePixels = new Array[Color](square)

    var ah = 0
    val he = (s - 1) / 2
    for (i: Int <- -he to he) {
      for (j: Int <- -he to he) {
        val color = new Color(img.getRGB(clamp(i + x, 0, img.getWidth - 1), clamp(j + y, 0, img.getHeight - 1)))
        imagePixels(ah) = color
        ah += 1
      }
    }
    for (i <- 0 until square) {
      R(i) = imagePixels(i).getRed
      G(i) = imagePixels(i).getGreen
      B(i) = imagePixels(i).getBlue
    }
    R = R.sorted
    G = G.sorted
    B = B.sorted

    new Color(R((square / 2) + 1), G((square / 2) + 1), B((square / 2) + 1))
  }


  class Window(var images: List[FilteredImage]) extends JComponent {

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      if (images != null) {
        g.drawImage(img, 50, 200, null)
        g.setFont(new Font("Arial", Font.BOLD, 30))
        g.setColor(Color.GREEN)
        g.drawString("Original Image", img.getWidth / 2 - 50, 190)

        for (i: Int <- images.indices) {
          val img: FilteredImage = images(i)
          if (img != null) {
            val x: Int = i % 2
            val y: Int = math.floor(i / 2).toInt
            g.drawImage(img.getImg, x * img.getImg.getWidth + 50 * (x + 1) + img.getImg.getWidth + 50, y * img.getImg.getHeight + 50 * (y + 2), null)
            g.setFont(new Font("Arial", Font.BOLD, 14))
            g.setColor(Color.BLUE)
            g.drawString("This image took " + img.getTime + " milliseconds", x * img.getImg.getWidth + 50 * (x + 1) + img.getImg.getWidth + 50, y * img.getImg.getHeight + 46 * (y + 2))
          }
        }
      }
    }
  }

  class FilteredImage(img: BufferedImage, time: Long) {
    def getImg: BufferedImage = img
    def getTime: Long = time
  }

  var img = loadImage
  if (img != null) {
    img = resizeImage(img, 300, 300)

    val noParaMedFilter = MedianFilterNoParallel(this.img, str = 6)
    val firstNoParaMed: FilteredImage = noParaMedFilter
    System.out.println("The first NoParallelImageMedianFilter Image took " + firstNoParaMed.getTime + " milliseconds to complete.")

    val noParaBigFilter = MedianFilterNoParallel(this.img, str = 9)
    val secondNoParaBig: FilteredImage = noParaBigFilter
    System.out.println("The second NoParallelImageMedianFilter Image took " + secondNoParaBig.getTime + " milliseconds to complete.")

    val yesParaMedFilter = MedianFilterParallel(this.img, str = 6)
    val firstYesParaMed: FilteredImage = yesParaMedFilter
    System.out.println("The first ParallelImageMedianFilter Image took " + firstYesParaMed.getTime + " milliseconds to complete.")

    val yesParaBigFilter = MedianFilterParallel(this.img, str = 9)
    val secondYesParaBig: FilteredImage = yesParaBigFilter
    System.out.println("The second ParallelImageMedianFilter Image took " + secondYesParaBig.getTime + " milliseconds to complete.")

    this.wait(2000) // 2 second wait time to see console

    val photos = List(noParaMedFilter, noParaBigFilter, yesParaMedFilter, yesParaBigFilter)

    val component = new Window(photos)
    val frame: JFrame = new JFrame("Frame")

    frame.add(component)
    frame.setSize(photos.length * img.getWidth() + 100, photos.length * img.getHeight + 50)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  }
}
