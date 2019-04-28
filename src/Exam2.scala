import java.awt.{BorderLayout, Color, Graphics}
import java.awt.image.BufferedImage
import java.io.InputStream

import javax.imageio.ImageIO
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.{JComponent, JFileChooser, JFrame}


object Exam2 extends App {
  //  def main(args: Array[String]) = {
  //  }
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


  def cloneImage(img: BufferedImage): BufferedImage = {
    val image = new BufferedImage(img.getWidth, img.getHeight, img.getType)
    val graphics = image.getGraphics
    graphics.drawImage(image, 0, 0, null)
    graphics.dispose()
    image
  }

  def medianFilter(img: BufferedImage): BufferedImage = {
    medianFilter(img, 3)
  }

  def medianFilter(img: BufferedImage, str: Int): BufferedImage = {
    val square: Int = math.max(str * str, 9)
    val imagePixels = new Array[Color](square)
    var R = new Array[Int](9)
    var G = new Array[Int](9)
    var B = new Array[Int](9)
    var newImg = cloneImage(img)

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

  class Window(var images: List[BufferedImage]) extends JComponent {

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      if (images != null) {
        for (i: Int <- images.indices) {
          val img: BufferedImage = images(i)
          if (img != null) {
            val x: Int = i % (4-1)
            val y: Int = math.floor(i / (4-1)).toInt
            g.drawImage(img, x * img.getWidth + 50, y * img.getHeight() + 50, null)

          }
        }
      }
    }
  }


  val img = loadImage
  if (img != null) {
    val smallFilter = medianFilter(this.img, 3)
    val medFilter = medianFilter(this.img, 6)
    val bigFilter = medianFilter(this.img, 9)

    val photos = List(smallFilter, medFilter, bigFilter)

    var component = new Window(photos)
    val frame: JFrame = new JFrame("Frame")
    frame.add(component)
    frame.setSize(photos.length * img.getWidth() + 100, photos.length * img.getHeight + 100)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)


  }
}


