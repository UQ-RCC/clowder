package services

import play.api.{ Plugin, Logger, Application }
import play.libs.Akka
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._

import models._
import util.SpaceConfig
import util.PPMSUtils
import securesocial.core._ 

case class SyncAgent(id: UUID, var typeOfAgent: String = "cat:syncer", syncService: String = "ppms", serverUrl: Option[URL]) extends Metadata.Agent {
  def operation: String = "Synced"
  def displayName: String = syncService
  def url: Option[URL] = serverUrl
}


/**
 * A service that syncs with data structures in PPMS
 * At the moment, it is a mixed between Puma nad Apiv2
 */
class PPMSSyncService (application: Application) extends Plugin {

  // parameters
  var ppmsUrl: Option[String] = None
  var ppmsPumaApiKey: Option[String] = None
  var ppmsApi2Key: Option[String] = None
  var ppmsGetProjectAction: Option[String] = None
  var ppmsGetProjectMemberAction: Option[String] = None
  var ppmsGetUserAction: Option[String] = None
  var ppmsGetXtraProjectProfileAction: Option[String] = None
  var ppmsProjectProfileTileField: Option[String] = None
  var ppmsDefaultIdProvider: Option[String] = None
  var ppmsDefaultAuthMethod: Option[String] = None
  
  
  // services
  val spaces: SpaceService = DI.injector.getInstance(classOf[SpaceService])
  val users: UserService = DI.injector.getInstance(classOf[UserService])
  val events: EventService = DI.injector.getInstance(classOf[EventService])
  val metadatas: MetadataService = DI.injector.getInstance(classOf[MetadataService])

  
  override def onStart() {
    Logger.debug("Starting ppms sync plugin")
    /*make sure username password is disabled, otherwise turn this off. reason: cannot create username password */
    if ( play.Play.application().configuration().getBoolean("enableUsernamePassword") ) {
      Logger.debug("Make sure to turn off usernamepassword to make this plugin works")
      return
    } 
    var ppmsUrl = play.api.Play.configuration.getString("ppms.url").getOrElse("")
    if (!ppmsUrl.endsWith("/"))
	    ppmsUrl = ppmsUrl + "/"
    this.ppmsPumaApiKey = play.api.Play.configuration.getString("ppms.pumapikey").getOrElse("")
    this.ppmsApi2Key = play.api.Play.configuration.getString("ppms.api2key").getOrElse("")
    this.ppmsGetProjectAction = play.api.Play.configuration.getString("ppms.action.getprojects").getOrElse("getprojects")
    this.ppmsGetProjectMemberAction = play.api.Play.configuration.getString("ppms.action.getprojectmember").getOrElse("getprojectmember")
    this.ppmsGetUserAction = play.api.Play.configuration.getString("ppms.action.getuser").getOrElse("getuser")
    this.ppmsGetXtraProjectProfileAction = play.api.Play.configuration.getString("ppms.action.getextraprojectprofile").getOrElse("")
    this.ppmsStorageField = play.api.Play.configuration.getString("ppms.project.profile.storagefield").getOrElse("RawDataCollection")
    this.ppmsDefaultIdProvider = play.api.Play.configuration.getString("ppms.default.securesocial").getOrElse("cilogon")
    this.ppmsDefaultAuthMethod = play.api.Play.configuration.getString("ppms.default.authmethod").getOrElse("oauth2")
    
    
    /*start timeinterval*/
    val timeInterval = play.Play.application().configuration().getInt("ppms.syncEvery") 
	    Akka.system().scheduler.schedule(0.days, timeInterval.intValue().days){
	      syncProjectsFromPPMS
	  }
  }
  
  override def onStop() {
    Logger.debug("Shutting down ppms sync plugin")
  }

  override lazy val enabled = {
    !application.configuration.getString("ppms.syncservice").filter(_ == "disabled").isDefined
  }

  /**
  * get users from PPMS and sync with users in this project
  */
  private def syncUsersInproject(ppmsProjectId: Integer, space: ProjectSpace) = {
    // go though users in this project 
    val projectMembers = PPMSUtils.getPPMSProjectUsers(ppmsUrl, ppmsPumaApiKey, projId, ppmsGetProjectMemberAction, ppmsGetUserAction)
    projectMembers.foreach{member =>
      users.findByEmail((member \ "email").as[String]) match {
        case Some(anUser) => {
          // make sure this user is in space
          spaces.addUser(anUser.id , Role.Editor, space.id)
        }
        case None => {
          // create new user
          val newUser = new User.ClowderUser(
                              id=UUID.generate,
                              identityId=new IdentityId(None, ppmsDefaultIdProvider),
                              firstName=(member \ "fname").as[String],
                              lastName=(member \ "lname").as[String],
                              fullName=((member \ "fname").as[String] + " "+ (member \ "lname").as[String]),
                              email=(member \ "email").as[String],
                              authMethod=AuthenticationMethod(ppmsDefaultAuthMethod),
                              status=User.UserStatus.Active,
                              termsOfServices=Some(UserTermsOfServices(accepted=false))
                            )
          val addedUser = users.insert(newUser)
          if(addedUser != None) {
            spaces.addUser(addedUser.id , Role.Editor, space.id)
          }
        }
      } // end findByEmail
    }
  }

  /**
    each project profile
    {
    "CoreFacilityRef": 2,
    "ProjectName": "00017 - Saavedra Mella Felipe - Phosphorus-minerals interactions for hydrogeochemical stabilization",
    "Phase": 2,
    "Active": "True",
    "Bcode": "9342909-01-478-21-702805-019806 (FFT - ARCLPKM)",
    "ProjectRef": 27,
    "Affiliation": "IntPrepaid",
    "ProjectType": "CMM-Projects",
    "ProjectGroup": "SMI - MINED LAND REHAB - HUANG GROUP",
    "Descr": ""
    }
    extra profile
    {
        "ProjectRef": 911,
        "PlateformID": 2,
        "ProjectName": "HoangNguyen  - Prototype of Data Repository",
        "Discipline (Physical/Engineering, Bio/Med, Env/Geo Sc)": "Bio/Medical",
        "RawDataCollection": "Q3504"
    }
  */
  private def syncProject(projectInfo: JsValue, extraProfile: JsValue) = {
    val projName = (projectInfo \ "ProjectName").as[String]
    val projId = (projectInfo \ "ProjectRef").as[Integer]
    val projType = (projectInfo \ "ProjectType").as[String]
    val projGroup = (projectInfo \ "ProjectGroup").as[String]
    val projDesc = (projectInfo \ "Descr").as[String]
    val rawDataStorage = (extraProfile \ ppmsStorageField).as[String]
    if (rawDataStorage == None || rawDataStorage.trim().isEmpty()) {
      Logger.debug("Project " + projName + " has no storage defined. Ignore!!!")
      return
    }

    val desc = """Project ID:%d
                    |RDM Collection:%s
                    |ProjectType:%s
                    |ProjectGroup:%s
                    |Desc:%s""" format(projId, rawDataStorage, projType, projGroup, projDesc)
    Logger.debug("Syncing project: name =" + projName + " projectId=" + projectId + " rawdata=" + rawDataStorage)
    // check whether space with given name exists
    // TODO: make sure this is correct
    spaces.listUser(None, false, 0, projName, None, true, None) match {
      case Nil => {
        Logger.debug(">>>No space exists, create a new one")
        //create new space
        val newSpace = ProjectSpace(name = projName, description = desc,
                                    created = new Date, creator = userId, 
                                    homePage = "", logoURL = "", bannerURL = "",
                                    collectionCount = 0, datasetCount = 0, userCount = 0, 
                                    metadata = List.empty,
                                    resourceTimeToLive = SpaceConfig.getTimeToLive(), 
                                    isTimeToLiveEnabled = SpaceConfig.getIsTimeToLiveEnabled(),
                                    status = Space.SpaceStatus.PRIVATE,
                                    affiliatedSpaces = List.empty)
        val addedSpace = spaces.insert(newSpace)
        if(addedSpace == None) {
          Logger.error("Failed to add space")
          return
        }
        events.addObjectEvent(None, addedSpace.id, addedSpace.name, "create_space")
        // metadata
        val creator = Some(SyncAgent(id=UUID.generate, serverUrl=ppmsUrl))
        val spaceMetadata : JsValue = JsObject(
          Seq(
            "projType"    -> JsString(projType),
            "projGroup"   -> JsString(projGroup),
            "projStorage" -> JsString(rawDataStorage),
            "projId"      -> JsNumber(projId)
          )
        )
        addedSpace.metadata = List(new Metadata(content=spaceMetadata, creator=creator, attachedTo=ResourceRef(ResourceRef.space, dataset.id)))
        spaces.update(addedSpace)
        // add admins first
        users.getAdmins().foreach { admin =>
          spaces.addUser(admin.id , Role.Admin, addedSpace.id)
        }
        // go though users in this project 
        syncUsersInproject(projId, addedSpace)
        // metadata
        val clowder_metadata = metadatas.getDefinitions()
        clowder_metadata.foreach { md =>
          val new_metadata = MetadataDefinition(spaceId = Some(addedSpace.id), json = md.json)
          metadatas.addDefinition(new_metadata)
        }
      }
      case _ => {
        Logger.debug("Space exists, update it")
        // go through the metadata of each space to make sure the collection is there
        spaces.foreach{aSpace => 
          aSpace.metadata.foreach{metadata =>
            Logger.debug("Space "+ aSpace.name + " metadata: " + metadata.content)
            if (metadata.content != None && rawDataStorage.equals( ((metadata.content \ "projStorage").as[String])  ) ) {
              // now update the space
              syncUsersInproject(projId, aSpace)
            }
          } // end metadata for each
        } // end spaces for each
      } // end space exists 
    }
  }

  /**
  * sync all projects from PPMS
  */
  private def syncProjectsFromPPMS() = {
    if(ppmsUrl.equals("") || ppmsPumaApiKey.equals("") || ppmsApi2Key.equals("")) {
      Logger.debug("ppms.url or key not provided, ignore")
      return
    }
    // get projects
    val projectsJson = PPMSUtils.getPPMSProjects(ppmsUrl, ppmsPumaApiKey, ppmsGetProjectAction)
    projectsJson.foreach { projectInfo =>
      val projId = (projectInfo \ "ProjectRef").as[Integer]
      val projectXtraProfileArr = PPMSUtils.getPPMSExtraProjectProfile(ppmsUrl, ppmsApi2Key, projId, ppmsGetXtraProjectProfileAction)
      projectXtraProfileArr.foreach(syncProject(projectInfo, _)) 
    }
  } // end syncProjectsFromPPMS


}