package services

import play.api.{Plugin, Logger, Application}
import play.api.Play.current
import java.io.File
import org.apache.commons.io.FileUtils
import models._
import services._

/**
 * File organiser service.
 *
 */
class FileOrganiserService(application: Application) extends Plugin {

  var storageRootDir: Option[String] = None
  var spaceStorageMetadataField: String = ""

  lazy val spaces: SpaceService = DI.injector.getInstance(classOf[SpaceService])
  lazy val metadatas: MetadataService = DI.injector.getInstance(classOf[MetadataService])

  override def onStart() {
    Logger.debug("Starting file organiser plugin")
    val fileSep = System.getProperty("file.separator")
    
		var storageRootDir = play.api.Play.configuration.getString("storageroot.dir").getOrElse("")
		if(!storageRootDir.equals("")){
			if(!storageRootDir.endsWith(fileSep))
				storageRootDir = storageRootDir + fileSep
			this.storageRootDir = Some(storageRootDir)
		}
    this.spaceStorageMetadataField = play.api.Play.configuration.getString("fileorganiser.storagemetadatafield").getOrElse("").trim
	}	
  

  override def onStop() {
    Logger.debug("Shutting down file organiser plugin")
  }

  override lazy val enabled = {
    !application.configuration.getString("fileorganiserservice").filter(_ == "disabled").isDefined
  }

  /**
  * create a filename that does not exists
  */
  def makeFileName(fileName: String, folderPath: String): String = {
    val fileParts = fileName.split("\\.", -1)
    var tentativeFile = folderPath + fileName
    // this does not look functional at all :|
    while (new File(tentativeFile).exists) {
      // no extension
      if(fileParts.size == 1)
        tentativeFile = folderPath + fileName + java.time.LocalDateTime.now
      else 
        tentativeFile = folderPath + fileParts.init + java.time.LocalDateTime.now + "." + fileParts.last
    }
    return tentativeFile
  }

  /**
  * copy fileitem to storage root
  * This one does not delete the 
  */
  def copy(fileItem: FileItem): Option[String] = {
    Logger.debug("Copying file " + fileItem.fileName)
    storageRootDir match {
      case Some(rootDir) => {
        val fileSep = System.getProperty("file.separator")
        // create filePath from spaces, dataset, directories
        val relativeFilePath = fileItem.dataset match {
          // dataset
          case Some(ds) => {
            // space
            ds.spaces match {
              // no folder means no copy
              case Nil => None
              case _ => spaces.get(ds.spaces.head) match {
                // should not happen but just in case
                case None => ""
                // found a space
                case Some(aSpace) => {
                  var spaceStorageName = aSpace.name
                  if (!this.spaceStorageMetadataField.equals("")) {
                    metadatas.getMetadataByAttachTo(ResourceRef(ResourceRef.space, aSpace.id)).foreach { metadata => 
                      Logger.debug("Space "+ aSpace.name + " metadata: " + metadata.content)
                      if (metadata.content != None) {
                        val spaceStorageInMetadata = (metadata.content \ this.spaceStorageMetadataField).as[String]
                        if (spaceStorageInMetadata != None && !spaceStorageInMetadata.equals(""))  
                          spaceStorageName = spaceStorageInMetadata
                      }
                    } 
                  }
                  fileItem.folder match {
                    case Some(folder) => spaceStorageName + fileSep + ds.name + folder.name.replace("/", fileSep) + fileSep
                    case None => spaceStorageName + fileSep + ds.name + fileSep
                  }
                } 
              }
            }
          }
          // no dataset provided
          case None => None
        }
        Logger.debug(s"relativeFilePath=$relativeFilePath")
        if(!relativeFilePath.equals("")){        
          // make sure the file copied to does not exists
          val destFile = makeFileName(fileItem.fileName, rootDir + relativeFilePath + fileSep)
          val copiedFile = new File(destFile)
          copiedFile.getParentFile().mkdirs()
          FileUtils.copyFile(fileItem.fileObject, copiedFile)
          Logger.debug("File copied successfully")
          return Some(destFile)
        } else {
          return None
        } 
      }
      case None => { 
        Logger.warn("storageroot.dir not defined.")
        return None
      }
    }
  }
}

case class FileItem(
  fileObject: File,
  fileId: String,
  fileName: String,
  dataset: Option[Dataset],
  folder: Option[Folder]
)
