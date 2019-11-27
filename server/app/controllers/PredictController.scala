package controllers

import java.io._
import java.net.URL

import com.google.inject.Provides
import com.hhandoko.play.pdf.PdfGenerator
import dao._
import io.github.cloudify.scala.spdf.{Landscape, Pdf, PdfConfig, Portrait}
import javax.inject.Inject
import models.Tables.MissionRow
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import org.jxls.common.Context
import org.jxls.util.JxlsHelper
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents, RequestHeader}
import tool.{DivData, FormTool, ImageInfo, MyLogger, PdfGeneratorProvider, Tool}
import utils.Utils
import models.Tables._
import net.logstash.logback.marker.LogstashMarker
import org.apache.commons.lang3.StringUtils
import org.slf4j.MarkerFactory
import play.api
import play.api.{Logger, MarkerContext}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import utils.Implicits._
import shared._

/**
  * Created by yz on 2018/12/12
  */
class PredictController @Inject()(cc: ControllerComponents, formTool: FormTool, tool: Tool,
                                  missionDao: MissionDao, extraDataDao: ExtraDataDao,
                                  trashDao: TrashDao, pdfGen: PdfGeneratorProvider) extends
  AbstractController(cc) {

  val logger = MyLogger(tool)(this.getClass())

  def predictBefore = Action { implicit request =>
    Ok(views.html.user.predict())
  }

  def batchPredictBefore = Action { implicit request =>
    Ok(views.html.user.batchPredict())
  }

  def predict = Action.async { implicit request =>
    val data = formTool.predictForm.bindFromRequest().get
    val userId = tool.getUserId
    val tmpDir = tool.createTempDirectory("tmpDir")
    val inputFile = new File(tmpDir, "input.txt")
    val lines = ArrayBuffer(ArrayBuffer("SampleID", "Age", "AST", "ALT", "PLT", "Tyr", "TCA"))
    lines += ArrayBuffer(data.sampleId, data.age, data.ast, data.alt, data.plt, data.tyr, data.tca)
    Utils.lines2File(inputFile, lines.map(_.mkString("\t")))
    Utils.txt2Xlsx(inputFile, new File(tmpDir, "input.xlsx"))
    FileUtils.copyFileToDirectory(new File(Utils.dataDir, "LiveForest_load.RData"), tmpDir)
    val linuxCommand =
      s"""
         |dos2unix *
         |Rscript ${Utils.dosPath2Dos(Utils.rPath)}/predict.R
       """.stripMargin
    val windowsCommand =
      s"""
         |Rscript ${Utils.dosPath2Dos(Utils.rPath)}/predict.R
       """.stripMargin
    val command = if (Utils.isWindows) windowsCommand else linuxCommand
    val startTime = System.currentTimeMillis()
    val execCommand = Utils.callScript(tmpDir, shBuffer = ArrayBuffer(command))
    if (execCommand.isSuccess) {
      val outFile = new File(tmpDir, "out.txt")
      val map = tool.getPredictValue(outFile)
      val resultData = tool.getResultData(outFile)
      val score = Shared.calculateScore(resultData)
      val result = Shared.getResult(score)
      val time = new DateTime()
      val row = MissionRow(0, userId, data.age, data.ast, data.alt, data.plt, data.tyr,
        data.tca, score.toString(), result, time)
      missionDao.insertAndRetunId(row).flatMap { missionId =>
        val extraRow = tool.getEmptyExtraDataRow(missionId, userId).copy(sampleId = data.sampleId, name = data.name,
          checkDate = Utils.dataTime2String(time))
        val trashRow = TrashRow(missionId, userId, true)
        extraDataDao.insertOrUpdate(extraRow).zip(trashDao.insert(trashRow)).map { x =>
          val missionIdDir = tool.getMissionIdDirById(missionId)
          val outFile = new File(tmpDir, "out.txt")
          FileUtils.copyFileToDirectory(outFile, missionIdDir)
          tool.deleteDirectory(tmpDir)
          val json = Json.obj("missionId" -> missionId)
          Ok(json)
        }

      }
    } else {
      tool.deleteDirectory(tmpDir)
      Utils.result2Future(Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr)))
    }
  }

  def batchPredict = Action.async(parse.multipartFormData) { implicit request =>
    val tmpDir = tool.createTempDirectory("tmpDir")
    val inputFile = new File(tmpDir, "input.xlsx")
    val file = request.body.file("file").get
    file.ref.moveTo(inputFile, replace = true)
    val userId = tool.getUserId
    val missionIds = ArrayBuffer[Int]()
    val time = new DateTime()
    FileUtils.copyFileToDirectory(new File(Utils.dataDir, "LiveForest_load.RData"), tmpDir)
    val command =
      s"""
         |Rscript ${(Utils.rPath)}/predict.R
       """.stripMargin
    val execCommand = Utils.callScript(tmpDir, shBuffer = ArrayBuffer(command))
    if (execCommand.isSuccess) {
      val outFile = new File(tmpDir, "out.txt")
      val outLines = Utils.file2Lines(outFile)
      val fileContents = outLines.drop(1).map { line =>
        ArrayBuffer(outLines.head, line)
      }
      val missions = fileContents.map { newLines =>
        val resultData = tool.getResultData(newLines)
        val score = Shared.calculateScore(resultData)
        val result = Shared.getResult(score)
        val orinalData = tool.getOrinalData(newLines)
        MissionRow(0, userId, orinalData.age, orinalData.ast, orinalData.alt, orinalData.plt, orinalData.tyr,
          orinalData.tca, score.toString(), result, time)
      }
      val f = missionDao.insertAndRetunIds(missions).flatMap { missionIds =>
        val etRows = missionIds.zip(fileContents).map { case (missionId, newLines) =>
          val missionIdDir = tool.getMissionIdDirById(missionId)
          val outFile = new File(missionIdDir, "out.txt")
          Utils.lines2File(outFile, newLines)
          val extraData = tool.getExtraData(outFile)
          val checkDate = if (StringUtils.isEmpty(extraData.checkDate)) Utils.dataTime2String(time) else extraData.checkDate
          val orinalData = tool.getOrinalData(newLines)
          val extraRow = tool.getEmptyExtraDataRow(missionId, userId).copy(sampleId = orinalData.sampleId, name = extraData.name,
            unit = extraData.unit, address = extraData.address, sex = extraData.sex, office = extraData.office,
            doctor = extraData.doctor, number = extraData.number, sampleTime = extraData.sampleTime, submitTime = extraData.submitTime,
            sampleType = extraData.sampleType, sampleStatus = extraData.sampleStatus,
            title = extraData.title, reporter = extraData.reporter, checker = extraData.checker, checkDate = checkDate,
            reportDate = extraData.reportDate
          )
          val trashRow = TrashRow(missionId, userId, true)
          (extraRow, trashRow)
        }
        extraDataDao.insertOrUpdates(etRows).map { x =>
          missionIds
        }
      }
      val missionIds = Utils.execFuture(f)
      tool.deleteDirectory(tmpDir)
      missionDao.selectByMissionIds(missionIds).zip(extraDataDao.selectByMissionIds(missionIds)).map { case (x, extraRows) =>
        val missionRows = x
        val extraMap = extraRows.map(x => (x.id, x)).toMap
        val t2s = missionRows.map(x => (x, extraMap.getOrElse(x.id, tool.getEmptyExtraDataRow(x.id, x.userId))))
        val array = Utils.getArrayByT2s(t2s)
        Ok(Json.toJson(array))
      }
    } else {
      tool.deleteDirectory(tmpDir)
      Utils.result2Future(Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr)))
    }

  }

  def downloadExampleFile = Action { implicit request =>
    val data = formTool.fileNameForm.bindFromRequest().get
    val file = new File(Utils.path, s"example/${data.fileName}")
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> s"attachment; filename=${
        file.getName
      }",
      CONTENT_TYPE -> "application/x-download"
    )
  }

  def exportExcel = Action { implicit request =>
    val data = formTool.basicInfoForm.bindFromRequest().get
    val file = new File(Utils.templateDir, s"live_forest_template.xlsx")
    val tmpDir = tool.createTempDirectory("tmpDir")
    val outFile = new File(tmpDir, "live_forest_out.xlsx")
    val is: InputStream = new FileInputStream(file)
    val os: OutputStream = new FileOutputStream(outFile)
    val context = new Context()
    context.putVar("basicInfo", data)
    JxlsHelper.getInstance().processTemplate(is, os, context)
    is.close()
    os.close()
    Ok.sendFile(outFile, onClose = () => {
      tool.deleteDirectory(tmpDir)
    }).withHeaders(
      CONTENT_DISPOSITION -> s"attachment; filename=${
        outFile.getName
      }",
      CONTENT_TYPE -> "application/x-download"
    )
  }

  def pdfTest = Action { implicit request =>
    val tmpDir = tool.createTempDirectory("tmpDir")
    val startTime = System.currentTimeMillis()
    val outFile = new File(tmpDir, "live_boost_out.pdf")
    val pdf = Pdf(new PdfConfig {
      orientation := Portrait
      pageSize := "A4"
      pageSize := "Letter"
      marginTop := "1in"
      marginBottom := "1in"
      marginLeft := "0in"
      marginRight := "0in"
    })
    pdf.run(views.html.user.htmlTest().toString(), outFile)

    Ok("success!")
  }

  def export = Action.async { implicit request =>
    val data = formTool.basicInfoForm.bindFromRequest().get
    val pdfData = formTool.pdfDataForm.bindFromRequest().get
    val missionId = formTool.missionIdForm.bindFromRequest().get.missionId
    val userId = tool.getUserId
    val tmpDir = tool.createTempDirectory("tmpDir")
    val outPdfFile = new File(tmpDir, "live_forest_out.pdf")
    val pdf = Pdf(new PdfConfig {
      orientation := Portrait
      pageSize := "A4"
      pageSize := "Letter"
      marginTop := "0.5in"
      marginBottom := "0in"
      marginLeft := "0.5in"
      marginRight := "0.5in"
    })
    val newConfig = new PdfConfig {
      orientation := Portrait
      pageSize := "A4"
      pageSize := "Letter"
      marginTop := "0.5in"
      marginBottom := "0in"
      marginLeft := "0.5in"
      marginRight := "0.5in"
    }
    logger.debug(newConfig.disableExternalLinks.default)
    missionDao.selectByMissionId(missionId).flatMap { row =>
      val missionIdDir = tool.getMissionIdDirById(missionId)
      val outFile = new File(missionIdDir, "out.txt")
      val pngFile = new File(missionIdDir, "out.png")
      val svgFile = new File(missionIdDir, "out.svg")
      FileUtils.writeStringToFile(svgFile, pdfData.svgStr)
      Utils.svg2png(svgFile)
      val base64 = Utils.getBase64Str(pngFile)
      val logoFile = new File(Utils.dataDir, "logo1.png")
      val logoBase64 = logoFile.base64
      val qrBase64 = new File(Utils.dataDir, "qr_code.jpg").base64
      val fontBase64 = new File(Utils.dataDir, "simsun.ttf").base64
      val imageInfo = Map("barPlot" -> base64, "logo" -> logoBase64, "qrCode" -> qrBase64, "font" -> fontBase64)
      val html = views.html.user.html(data, row, imageInfo, pdfData).toString()
      val htmlFile = new File(tmpDir, "out.html")
      html.toFile(htmlFile)
      pdf.run(htmlFile, outPdfFile)
      val bytes = FileUtils.readFileToByteArray(outPdfFile)
      val extraDataRow = ExtraDataRow(row.id, row.userId, data.sample, data.unit, data.address, data.name, data.sex,
        data.office, data.doctor, data.number, data.sampleTime, data.submitTime, data.sampleType, data.sampleStatus,
        data.title, data.danger, data.reporter, data.checker, data.checkDate, data.reportDate)
      extraDataDao.insertOrUpdate(extraDataRow).map { x =>
        Ok(bytes).as("application/pdf")
      }

    }

  }


  def toHtml = Action { implicit request =>
    Ok(views.html.user.htmlTest())
  }

  def predictResult = Action.async { implicit request =>
    val data = formTool.missionIdForm.bindFromRequest().get
    missionDao.selectByMissionId(data.missionId).zip(extraDataDao.selectByMissionId(data.missionId)).map { case (row, extraRow) =>
      val missionIdDir = tool.getMissionIdDirById(data.missionId)
      val outFile = new File(missionIdDir, "out.txt")
      val resultData = tool.getResultData(outFile)
      val extraDataJson = Utils.getJsonByT(extraRow)
      val missionJson = tool.getMissionJson(row, resultData)
      val resultDataJson = Utils.getMapByT(resultData)
      val json = Json.obj("extraData" -> extraDataJson, "mission" -> missionJson,
      ) ++ Json.toJsObject(resultDataJson)
      Ok(json)
    }

  }

  def fileCheck = Action(parse.multipartFormData) { implicit request =>
    val tmpDir = tool.createTempDirectory("tmpDir")
    val dataFile = new File(tmpDir, "data.xlsx")
    val file = request.body.file("file").get
    file.ref.moveTo(dataFile, replace = true)
    val myMessage = tool.fileCheck(dataFile)
    tool.deleteDirectory(tmpDir)
    Ok(Json.obj("valid" -> myMessage.valid, "message" -> myMessage.message))
  }

}
