package services.mongodb

import javax.inject.{Inject, Singleton}

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import com.novus.salat.dao.{SalatDAO, ModelCompanion}
import models.{UserSpace, ProjectSpace, UUID}
import play.api.Play._
import services._
import MongoContext.context
import util.Direction._

/**
 * Store Spaces in MongoDB.
 *
 * @author Luigi Marini
 *
 */
@Singleton
class MongoDBSpaceService @Inject() (
  collections: CollectionService,
  files: FileService,
  datasets: DatasetService) extends SpaceService {

  def count(filter: Option[Map[String, String]] = None): Long = {0}
  def list(start: Option[String] = None, limit: Integer = 20,
           order: Option[String] = None, direction: Direction=DESC,
           filter: Option[Map[String, String]] = None): List[ProjectSpace]={List.empty}

  def get(id: UUID): Option[ProjectSpace] = {
    ProjectSpaceDAO.findOneById(new ObjectId(id.stringify))
  }

  def insert(dataset: ProjectSpace): Option[String] = {
    ProjectSpaceDAO.insert(dataset).map(_.toString)
  }

  def update(space: ProjectSpace): Unit = {
    ProjectSpaceDAO.save(space)
  }

  def delete(id: UUID): Unit = {
    ProjectSpaceDAO.removeById(new ObjectId(id.stringify))
  }

  def list(): List[ProjectSpace] = {
    (for (space <- ProjectSpaceDAO.find(MongoDBObject())) yield space).toList
  }

  /**
   * Salat ProjectSpace model companion.
   */
  object ProjectSpaceDAO extends ModelCompanion[ProjectSpace, ObjectId] {
    val dao = current.plugin[MongoSalatPlugin] match {
      case None => throw new RuntimeException("No MongoSalatPlugin");
      case Some(x) => new SalatDAO[ProjectSpace, ObjectId](collection = x.collection("spaces.projects")) {}
    }
  }

  /**
   * Salat UserSpace model companion.
   */
  object UserSpaceDAO extends ModelCompanion[UserSpace, ObjectId] {
    val dao = current.plugin[MongoSalatPlugin] match {
      case None => throw new RuntimeException("No MongoSalatPlugin");
      case Some(x) => new SalatDAO[UserSpace, ObjectId](collection = x.collection("spaces.users")) {}
    }
  }
}

