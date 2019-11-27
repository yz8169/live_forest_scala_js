package tool

/**
  * Created by yz on 2019/5/6
  */
import com.google.inject.{Provider, Provides}
import com.hhandoko.play.pdf.PdfGenerator
import javax.inject.Inject
import play.api.Environment

class PdfGeneratorProvider @Inject()(environment: Environment) extends Provider[PdfGenerator] {

  var pdfGen: PdfGenerator = _
  providePdfGenerator()

  @Provides
  def providePdfGenerator(): PdfGenerator = {
    pdfGen = new PdfGenerator(environment)
    pdfGen.loadLocalFonts(Seq("fonts/simsun.ttf"))
    pdfGen
  }

  override def get(): PdfGenerator = pdfGen
}

