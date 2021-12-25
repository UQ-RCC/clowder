package services


import java.net.URL
import java.util.Date

import util.{SpaceConfig, PPMSUtils}
import play.api.Play._
import play.api.{ Plugin, Logger, Application }
import play.libs.Akka
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import api.Permission
import api.Permission.Permission
import models._
import securesocial.core._ 



case class SyncAgent(id: UUID, var typeOfAgent: String = "cat:syncer", syncService: String = "ppms", serverUrl: Option[URL]) extends Agent {
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
  var ppmsUrl: String = ""
  var ppmsPumaApiKey: String = ""
  var ppmsApi2Key: String = ""
  var ppmsGetProjectAction: String = ""
  var ppmsGetProjectMemberAction: String = ""
  var ppmsGetUserAction: String = ""
  var ppmsGetXtraProjectProfileAction: String = ""
  var ppmsProjectProfileTileField: String = ""
  var ppmsDefaultIdProvider: String = ""
  var ppmsDefaultAuthMethod: String = ""
  var ppmsStorageField: String = ""
  var startingProjectId: Int = 0
  
  
  // services
  val spaces: SpaceService = DI.injector.getInstance(classOf[SpaceService])
  val users: UserService = DI.injector.getInstance(classOf[UserService])
  val events: EventService = DI.injector.getInstance(classOf[EventService])
  val metadatas: MetadataService = DI.injector.getInstance(classOf[MetadataService])

  
  override def onStart() {
    Logger.info("Starting ppms sync plugin")
    Logger.info("There are total of:" + spaces.list().length + " spaces")
    /*make sure username password is disabled, otherwise turn this off. reason: cannot create username password */
    // if ( play.Play.application().configuration().getBoolean("enableUsernamePassword") ) {
    //   Logger.info("Make sure to turn off usernamepassword to make this plugin works")
    //   return
    // }

    var ppmsUrl = play.api.Play.configuration.getString("ppms.url").getOrElse("")
    if (!ppmsUrl.endsWith("/"))
	    ppmsUrl = ppmsUrl + "/"
    this.ppmsUrl = ppmsUrl
    this.ppmsPumaApiKey = play.api.Play.configuration.getString("ppms.pumapikey").getOrElse("")
    this.ppmsApi2Key = play.api.Play.configuration.getString("ppms.api2key").getOrElse("")
    this.ppmsGetProjectAction = play.api.Play.configuration.getString("ppms.action.getprojects").getOrElse("getprojects")
    this.ppmsGetProjectMemberAction = play.api.Play.configuration.getString("ppms.action.getprojectmember").getOrElse("getprojectmember")
    this.ppmsGetUserAction = play.api.Play.configuration.getString("ppms.action.getuser").getOrElse("getuser")
    this.ppmsGetXtraProjectProfileAction = play.api.Play.configuration.getString("ppms.action.getextraprojectprofile").getOrElse("")
    this.ppmsStorageField = play.api.Play.configuration.getString("ppms.project.profile.storagefield").getOrElse("RawDataCollection")
    this.ppmsDefaultIdProvider = play.api.Play.configuration.getString("ppms.default.securesocial").getOrElse("cilogon")
    this.ppmsDefaultAuthMethod = play.api.Play.configuration.getString("ppms.default.authmethod").getOrElse("oauth2")
    this.startingProjectId = play.api.Play.configuration.getInt("ppms.startingProjectId").getOrElse(0)
    
    /*start timeinterval*/
    val timeInterval = play.Play.application().configuration().getInt("ppms.syncEvery")
    Logger.info("time interval:" + timeInterval.toString)
	  Akka.system().scheduler.schedule(0.minutes, timeInterval.intValue().minutes){
      Logger.info("Syncing ....")
      if ( getFirstAdmin() != None ) {
        syncProjectsFromPPMS
      }
	  }
  }
  
  override def onStop() {
    Logger.info("Shutting down ppms sync plugin")
  }

  override lazy val enabled = {
    !application.configuration.getString("ppmssyncservice").filter(_ == "disabled").isDefined
  }

  /**
  * get users from PPMS and sync with users in this project
  */
  private def syncUsersInproject(ppmsProjectId: Int, space: ProjectSpace) = {
    // go though users in this project 
    val projectMembers = PPMSUtils.getPPMSProjectUsers(ppmsUrl, ppmsPumaApiKey, ppmsProjectId, ppmsGetProjectMemberAction, ppmsGetUserAction)
    projectMembers.foreach{member =>
      val memberEmail = ((member.get \ "email").as[String])
      users.findByEmail( memberEmail ) match {
        case Some(anUser) => {
          // make sure this user is in space, if not so
          users.getUserRoleInSpace(anUser.id, space.id) match {
            case Some(userRole) => {
              Logger.info("User " + memberEmail + " existing role: " + userRole.name)
              // ignore
            }
            case None => spaces.addUser(anUser.id , Role.Editor, space.id)
          } 
        }
        case None => {
          // create new user
          Logger.info("User " + memberEmail + " does not exist! Create a new one!")
          val newUser = new ClowderUser(
                              id=UUID.generate,
                              identityId=new IdentityId( (member.get \ "email").as[String], ppmsDefaultIdProvider),
                              firstName=(member.get \ "fname").as[String],
                              lastName=(member.get \ "lname").as[String],
                              fullName=((member.get \ "fname").as[String] + " "+ (member.get \ "lname").as[String]),
                              email=Some(memberEmail),
                              authMethod=AuthenticationMethod(ppmsDefaultAuthMethod),
                              status=UserStatus.Active,
                              termsOfServices=Some(UserTermsOfServices(accepted=false))
                            )
          val addedUser = users.insert(newUser)
          if(addedUser != None) {
            Logger.info("User created. Adding to space. Status= " + addedUser.get.status)
            spaces.addUser(addedUser.get.id , Role.Editor, space.id)
          }
        }
      } // end findByEmail
    }
  }

  /**
  * returns the admin user - the first if there are multiple
  */
  private def getFirstAdmin(): Option[User] = {
    val firstAdmin = play.Play.application().configuration().getString("initialAdmins").trim.split("\\s*,\\s*").filter(_ != "").toList.head
    users.findByEmail(firstAdmin) match {
      case Some(admin) => Some(admin)
      case None => None
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
  private def syncProject(projectInfo: JsValue, extraProfile: JsValue): Unit = {
    val projName = (projectInfo \ "ProjectName").as[String]
    val projId = (projectInfo \ "ProjectRef").as[Int]
    val projType = (projectInfo \ "ProjectType").as[String]
    val projGroup = (projectInfo \ "ProjectGroup").as[String]
    val projDesc = (projectInfo \ "Descr").as[String]
    var rawDataStorage = (extraProfile \ ppmsStorageField).as[String]
    Logger.info(">>>Syncing project: " + projName + " id=" + projId.toString)
    if (rawDataStorage == None || rawDataStorage.trim().isEmpty()) {
      Logger.info("Project " + projName + " has no storage defined. Ignore!!!")
      return
    }
    rawDataStorage = rawDataStorage.trim()
    if(rawDataStorage.contains("-")) {
      rawDataStorage = rawDataStorage.split("-").last
    }

    val desc = 
    """Project ID:%d
    RDM Collection:%s
    ProjectType:%s
    ProjectGroup:%s
    Desc:%s""" format(projId, rawDataStorage, projType, projGroup, projDesc)

    Logger.info("Syncing project: name =" + projName + " projectId=" + projId + " rawdata=" + rawDataStorage)
    // allSpaces: List[ProjectSpace]
    // val allSpaces = spaces.list()
    // Logger.info("There are total of:" + allSpaces.length + " spaces")
    // var spaceList = allSpaces.filter(_space => _space.name == projName)
    val allSpaces = spaces.listAccess(0, Set[Permission](Permission.ViewSpace), getFirstAdmin, showAll=true, showPublic=true, onlyTrial=false, showOnlyShared=false)
    Logger.info("There are total of:" + allSpaces.length + " spaces")
    var spaceInDb = None
    spaceList.foreach{aSpace =>
      metadatas.getMetadataByAttachTo(ResourceRef(ResourceRef.space, aSpace.id)).foreach { metadata => 
        Logger.info("Space "+ aSpace.name + " metadata: " + metadata.content)
        if (metadata.content != None && projId == (metadata.content \ "projId").as[Int]  ) {
          spaceInDb = aSpace
        }
      } 
    } // end spaces for each

    // var spaceList = allSpaces.filter(_space => _space.name == projName)
    // Logger.info("Found:" + spaceList.length + " spaces with given name")

    if (spaceInDb == None) {
      Logger.info(">>>>>>> No space exists, create a new one <<<<<<")
      //create new space
      var newSpace = ProjectSpace(name = projName, description = desc,
                                  created = new Date, creator = getFirstAdmin.get.id, 
                                  homePage = List.empty, logoURL = None, bannerURL = None,
                                  collectionCount = 0, datasetCount = 0, userCount = 0, 
                                  metadata = List.empty,
                                  resourceTimeToLive = SpaceConfig.getTimeToLive(), 
                                  isTimeToLiveEnabled = SpaceConfig.getIsTimeToLiveEnabled(),
                                  status = SpaceStatus.PRIVATE.toString,
                                  affiliatedSpaces = List.empty)
      val addedSpace = spaces.insert(newSpace)
      if(addedSpace == None) {
        Logger.error("Failed to add space")
        return
      }
      val addedSpaceUUID = UUIDConversions.stringToUUID(addedSpace.get)
      events.addObjectEvent(getFirstAdmin, addedSpaceUUID, newSpace.name, "create_space")
      // metadata
      val creator = SyncAgent(id=UUID.generate, serverUrl=Some(new URL(ppmsUrl)) )
      val spaceMetadata : JsValue = JsObject(
        Seq(
          "projType"    -> JsString(projType),
          "projGroup"   -> JsString(projGroup),
          "projStorage" -> JsString(rawDataStorage),
          "projId"      -> JsNumber(projId)
        )
      )
      // newSpace.metadata = List(Metadata(content=spaceMetadata, creator=creator, attachedTo=ResourceRef(ResourceRef.space, newSpace.id)))
      // spaces.update(newSpace)
      val newSpaceMetadata = Metadata(content=spaceMetadata, creator=creator, attachedTo=ResourceRef(ResourceRef.space, addedSpaceUUID))
      metadatas.addMetadata(newSpaceMetadata) 
      // add admins first
      users.getAdmins.foreach ( admin => spaces.addUser(admin.id , Role.Admin, newSpace.id) )
      // go though users in this project 
      syncUsersInproject(projId, newSpace)
      // metadata
      val clowder_metadata = metadatas.getDefinitions()
      clowder_metadata.foreach { md =>
        val new_metadata = MetadataDefinition(spaceId = Some(newSpace.id), json = md.json)
        metadatas.addDefinition(new_metadata)
      }
    } else {
      Logger.info(">>>>>>>>>>>>>Space exists, update it<<<<<<<<<<<<<<<")
      metadatas.getMetadataByAttachTo(ResourceRef(ResourceRef.space, spaceInDb.id)).foreach { metadata => 
        Logger.info("Space "+ spaceInDb.name + " metadata: " + metadata.content)
        if (metadata.content != None && rawDataStorage.equals( ((metadata.content \ "projStorage").as[String])  ) ) {
          syncUsersInproject(projId, spaceInDb)
        }
      }
      // go through the metadata of each space to make sure the collection is there
      // spaceList.foreach{aSpace =>
      //   metadatas.getMetadataByAttachTo(ResourceRef(ResourceRef.space, aSpace.id)).foreach { metadata => 
      //     Logger.info("Space "+ aSpace.name + " metadata: " + metadata.content)
      //     if (metadata.content != None && rawDataStorage.equals( ((metadata.content \ "projStorage").as[String])  ) ) {
      //       syncUsersInproject(projId, aSpace)
      //     }
      //   } 
      //   // aSpace.metadata.foreach{metadata =>
      //   //   Logger.info("Space "+ aSpace.name + " metadata: " + metadata.content)
      //   //   if (metadata.content != None && rawDataStorage.equals( ((metadata.content \ "projStorage").as[String])  ) ) {
      //   //     // now update the space
      //   //     syncUsersInproject(projId, aSpace)
      //   //   }
      //   // } // end metadata for each
      // } // end spaces for each
    } // end space exists 
  }

  /**
  * sync all projects from PPMS
  */
  private def syncProjectsFromPPMS(): Unit = {
    Logger.info("Start the syncing process ...")
    if(ppmsUrl.equals("") || ppmsPumaApiKey.equals("") || ppmsApi2Key.equals("")) {
      Logger.info("ppms.url or key not provided, ignore")
      return
    }
    // get projects
    val projectsJsonArr = PPMSUtils.getPPMSProjects(ppmsUrl, ppmsPumaApiKey, ppmsGetProjectAction)
    projectsJsonArr.value.foreach { projectInfo =>
      Logger.info("Syncing project: " + (projectInfo \ "ProjectName").as[String])
      val projId = (projectInfo \ "ProjectRef").as[Int]
      if ( projId >= startingProjectId ) {
        val projectXtraProfileArr = PPMSUtils.getPPMSExtraProjectProfile(ppmsUrl, ppmsApi2Key, projId, ppmsGetXtraProjectProfileAction)
        projectXtraProfileArr.value.foreach(syncProject(projectInfo, _)) 
      }
    }
  } // end syncProjectsFromPPMS


}