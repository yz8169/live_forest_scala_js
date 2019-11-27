package dao

import javax.inject.Inject
import models.Tables.Mission
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.Tables._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by yz on 2019/1/4
  */
class ExtraDataDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def insert(row: ExtraDataRow): Future[Unit] = db.run(ExtraData += row).map(_ => ())

  def insertOrUpdate(row: ExtraDataRow): Future[Unit] = db.run(ExtraData.insertOrUpdate(row)).map(_ => ())

  def selectByMissionIds(missionIds: Seq[Int]): Future[Seq[ExtraDataRow]] = db.run(ExtraData.filter(_.id.inSetBind(missionIds)).result)

  def selectByMissionId(missionId: Int): Future[ExtraDataRow] = db.run(ExtraData.filter(_.id === missionId).result.head)

  def deleteById(id: Int): Future[Unit] = db.run(ExtraData.filter(_.id === id).delete).map(_ => ())

  def deleteByIds(ids: Seq[Int]): Future[Unit] = db.run(ExtraData.filter(_.id.inSetBind(ids)).delete).map(_ => ())

  def selectAll: Future[Seq[ExtraDataRow]] = db.run(ExtraData.result)

  def deleteByUserId(userId: Int): Future[Unit] = db.run(ExtraData.filter(_.userId === userId).delete).map(_ => ())

  def insertOrUpdates(etRows: Seq[(ExtraDataRow, TrashRow)]) = {
    val seqAction = DBIO.sequence {
      val extraRows = etRows.map(_._1)
      val trashRows = etRows.map(_._2)
      extraRows.zip(trashRows).map { case (extraRow, trashRow) =>
        val extraInsert = ExtraData.insertOrUpdate(extraRow)
        val trashInsert = Trash += trashRow
        extraInsert.flatMap(_ => trashInsert)
      }
    }.transactionally
    db.run(seqAction).map(_ => ())
  }






}
