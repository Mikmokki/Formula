package UIs

import java.io.File
import javax.sound.sampled.{AudioSystem, Clip}

object Functions {
 def playMusic(filePath: String): Clip = {
      val file = new File(filePath)
      val audioIn = AudioSystem.getAudioInputStream(file)
      val clip = AudioSystem.getClip()
      clip.open(audioIn)
      clip.start()
      clip
    }
}
