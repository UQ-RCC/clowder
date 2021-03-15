package util

import java.util.ArrayList
import play.api.Logger

import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.util.EntityUtils
import org.apache.http.NameValuePair
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.HttpResponse

import play.api.libs.json._

object PPMSUtils {

    /**
    * requestPPMS
    */
    def requestPPMS(url: String, isApi2: Boolean, params: ArrayList[NameValuePair]): Option[HttpResponse] = {
        val httpclient = new DefaultHttpClient()
        var httpresp: Option[HttpResponse] = None  
        if (url == None)
            return httpresp

        val httpPost = new HttpPost(url + (if (isApi2) "api2/" else "pumapi/")) 
        try
        {
            httpPost.setEntity(new UrlEncodedFormEntity(params))
            httpresp = Some(httpclient.execute(httpPost))
        }
        finally
        {
            httpPost.releaseConnection();
        }
        return httpresp
    }

    /**
    * getPPMSProjects 
    */
    def getPPMSProjects(url: String, pumaKey: String, getProjectAction: String): JsArray = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", pumaKey))
        params.add(new BasicNameValuePair("action", getProjectAction))
        params.add(new BasicNameValuePair("format", "json"))
        params.add(new BasicNameValuePair("active", true.toString))  
        val ppmsResponse = requestPPMS(url, false, params)
        ppmsResponse match {
            case Some(resp) => {
                Logger.debug("PPMS get projects response status: " + resp.getStatusLine().toString())
                val status = resp.getStatusLine.getStatusCode()
                if(status < 200 || status >= 300) {
                    Logger.debug("PPMS get project returns unexpected status code:" + status)
                    return Json.arr().as[JsArray]
                }
                // digest the result
                val projectsGetEntity = resp.getEntity()
                if(projectsGetEntity == null) {
                    Logger.debug("PPMS get project returns null")
                    return Json.arr().as[JsArray]
                }
                val projectsGetResult = EntityUtils.toString(projectsGetEntity)
                val projectsList: JsArray = Json.parse(projectsGetResult).as[JsArray]
                Logger.debug("PPMS get projects response status: " + projectsList)
                return projectsList
            }
            case None => {
                Logger.debug("Get request returns null")
                return Json.arr()
            }
        } 
    }

    /**
    sample outputs
    [
        {
            "ProjectRef": 911,
            "PlateformID": 2,
            "ProjectName": "HoangNguyen  - Prototype of Data Repository",
            "Discipline (Physical/Engineering, Bio/Med, Env/Geo Sc)": "Bio/Medical",
            "RawDataCollection": "Q3504"
        }
    ]
    */
    def getPPMSExtraProjectProfile(url: String, api2key: String, projectId: Integer, extraProfileAction: String): JsArray = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", api2key))
        params.add(new BasicNameValuePair("action", extraProfileAction))
        params.add(new BasicNameValuePair("outformat", "json"))
        params.add(new BasicNameValuePair("projectId", projectId.toString))
        
        val ppmsResponse = requestPPMS(url, true, params)
        ppmsResponse match {
            case Some(projectProfileGetResponse) => {
                val status = projectProfileGetResponse.getStatusLine.getStatusCode()
                if(status < 200 || status >= 300) {
                    Logger.debug("Get project profile returns unexpected status code:" + status)
                    return Json.arr().as[JsArray]
                }
                //
                val projectProfileGetEntity = projectProfileGetResponse.getEntity()
                if(projectProfileGetEntity == null) {
                    Logger.debug("Get project profile returns null")
                    return Json.arr().as[JsArray]
                }
                val projectProfileGetResult = EntityUtils.toString(projectProfileGetEntity)
                val projectProfileJson: JsArray = Json.parse(projectProfileGetResult).as[JsArray]
                Logger.debug("Get projects profile result: " + projectProfileJson)
                return projectProfileJson
            }
            case None => {
                Logger.debug("Get request returns null")
                return Json.arr()
            }
        }
    }

    /**
    *
    */
    def getPPMSUser(url: String, pumakey: String, username: String, getUserAction: String): Option[JsValue] = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", pumakey))
        params.add(new BasicNameValuePair("action", getUserAction))
        params.add(new BasicNameValuePair("login", username))
        params.add(new BasicNameValuePair("format", "json"))
        val ppmsResponse = requestPPMS(url, true, params)
        ppmsResponse match {
            case Some(userGetResponse) => {
                Logger.debug("Get user response status: " + userGetResponse.getStatusLine().toString())
                val status = userGetResponse.getStatusLine.getStatusCode()
                if(status < 200 || status >= 300) {
                    Logger.debug("Get user returns unexpected status code:" + status)
                    return None
                }
                val userGetEntity = userGetResponse.getEntity()
                if(userGetEntity == null) {
                    Logger.debug("Get user returns null")
                    return None
                }
                val userGetResult = EntityUtils.toString(userGetEntity)
                val userProfileJson: JsValue = Json.parse(userGetResult)
                Logger.debug("User profile: " + userProfileJson)
                return Some(userProfileJson)
            }
            case None => {
                Logger.debug("Get request returns null")
                return None
            }
        }
    }


    /**
    * returns an array of users info belong to a project
    */
    def getPPMSProjectUsers(url: String, pumakey: String, projectId: Integer, getProjectUsersAction: String, getUserAction: String): Array[Option[JsValue]] = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", pumakey))
        params.add(new BasicNameValuePair("action", getProjectUsersAction))
        params.add(new BasicNameValuePair("projectid", projectId.toString))
        params.add(new BasicNameValuePair("withdeactivated", false.toString))
        val ppmsResponse = requestPPMS(url, true, params)
        ppmsResponse match {
            case Some(projectUsersGetResponse) => {
                Logger.debug("Get project users response status: " + projectUsersGetResponse.getStatusLine().toString())
                val status = projectUsersGetResponse.getStatusLine.getStatusCode()
                if(status < 200 || status >= 300) {
                    Logger.debug("Get project returns unexpected status code:" + status)
                    return Array[Option[JsValue]]()
                }
                val projectUsersGetEntity = projectUsersGetResponse.getEntity()
                if(projectUsersGetEntity == null) {
                    Logger.debug("Get project profile returns null")
                    return Array[Option[JsValue]]()
                }
                val projectUsers = EntityUtils.toString(projectUsersGetEntity).split("\n")
                Logger.debug("Project users:" + projectUsers)
                // var users: Array[JsValue] = Array()
                // projectUsers.foreach{ projectUser =>
                //     getPPMSUser(url, pumakey, projectUser, getUserAction) match {
                //         case Some(ppmsUser) => { users ++ ppmsUser }
                //     }
                // }
                // var users = projectUsers.map ( projectUser => {
                //     getPPMSUser(url, pumakey, projectUser, getUserAction) match {
                //         case Some(ppmsUser) => ppmUser
                //     }
                // })
                var users = projectUsers.map { getPPMSUser(url, pumakey, _, getUserAction) }.filter(_ != None)
                Logger.debug("Project user profiles:" + users) 
                return users
            }
            case None => {
                Logger.debug("Get request returns null")
                return return Array[Option[JsValue]]()
            }
        }
    }


}