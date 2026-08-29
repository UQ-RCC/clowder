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
    * request PPMS
    * Sometimes string returns contains \t which throws error
    */
    def requestPPMS(url: String, isApi2: Boolean, params: ArrayList[NameValuePair]): Option[String] = {
        Logger.debug("\t===>   PPMS request:" + params + "url:" + (url + (if (isApi2) "API2/" else "pumapi/")) )
        val httpclient = new DefaultHttpClient()
        var respString: Option[String] = None
        if (url == None || url.trim.equals(""))
            return respString
        var httpPost = new HttpPost(url + (if (isApi2) "API2/" else "pumapi/"))
        if (isApi2) {
            httpPost.setHeader("Content-Type", "application/x-www-form-urlencoded")
            // httpPost.setHeader("Cookie", "ASPSESSIONIDCWBTTABA=LDLFKLLDFKFIIPKBFDMPDDGK")
        }
        try
        {
            httpPost.setEntity(new UrlEncodedFormEntity(params))
            val httpresp = httpclient.execute(httpPost)
            Logger.debug("PPMS response status: " + httpresp.getStatusLine().toString())
            val status = httpresp.getStatusLine.getStatusCode()
            if(status < 200 || status >= 300) {
                Logger.debug("PPMS get project returns unexpected status code:" + status)
            } else {
                // digest the result
                val projectsGetEntity = httpresp.getEntity()
                if(projectsGetEntity == null)
                    Logger.debug("PPMS get project returns null")
                else
                    respString = Some(EntityUtils.toString(projectsGetEntity).replace('\t', ' '))
            }
        }
        finally
        {
            httpPost.releaseConnection()
        }
        return respString
    }


    

    /**
    * getPPMSProjects 
    */
    def getPPMSProjects(url: String, pumaKey: String, getProjectAction: String, coreid: String): JsArray = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", pumaKey))
        params.add(new BasicNameValuePair("action", getProjectAction))
        params.add(new BasicNameValuePair("coreid", coreid))
        params.add(new BasicNameValuePair("format", "json"))
        params.add(new BasicNameValuePair("active", true.toString))  
        val ppmsResponse = requestPPMS(url, false, params)
        ppmsResponse match {
            case Some(resp) => {
                val projectsList: JsArray = Json.parse(resp).as[JsArray]
                Logger.info("PPMS get projects: got " + projectsList.value.size + " projects for coreid " + coreid)
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
    def getPPMSExtraProjectProfile(url: String, api2key: String, extraProfileAction: String, projectId: Integer, coreid: String): JsArray = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", api2key))
        params.add(new BasicNameValuePair("action", extraProfileAction))
        params.add(new BasicNameValuePair("projectId", projectId.toString))
        params.add(new BasicNameValuePair("coreid", coreid))
        params.add(new BasicNameValuePair("outformat", "json"))
        
        val ppmsResponse = requestPPMS(url, true, params)
        ppmsResponse match {
            case Some(projectProfileGetResponse) => {
                val projectProfileJson: JsArray = Json.parse(projectProfileGetResponse).as[JsArray]
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
        params.add(new BasicNameValuePair("login", username.replaceAll("\\r$", "") ))
        params.add(new BasicNameValuePair("format", "json"))
        val ppmsResponse = requestPPMS(url, false, params)
        ppmsResponse match {
            case Some(userGetResponse) => {
                val userProfileJson: JsValue = Json.parse(userGetResponse)
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
        val ppmsResponse = requestPPMS(url, false, params)
        ppmsResponse match {
            case Some(projectUsersGetResponse) => {
                val projectUsers = projectUsersGetResponse.split("\n")
                Logger.debug("Project users:" + projectUsersGetResponse)
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


    /**
    sample outputs
    [
      {
        "User ID": 1234,
        "User Login": "uq...",
        "User Full Name": "Last First",
        "User ORCID": "0000-0001-2345-6789",
        "User is in an active project?": "Yes"
      }
    ]
    */
    def getPPMSUserids(url: String, api2key: String, useridsAction: String, coreid: String): JsArray = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", api2key))
        params.add(new BasicNameValuePair("action", useridsAction))
        params.add(new BasicNameValuePair("coreid", coreid))
        params.add(new BasicNameValuePair("outformat", "json"))

        val ppmsResponse = requestPPMS(url, true, params)
        ppmsResponse match {
            case Some(useridsGetResponse) => {
                val useridsJson: JsArray = Json.parse(useridsGetResponse).as[JsArray]
                Logger.debug("Get user orcids result: " + useridsJson)
                return useridsJson
            }
            case None => {
                Logger.warn("Get user orcids request returns null")
                return Json.arr()
            }
        }
    }


    def getPPMSReport(url: String, api2key: String, reportAction: String, coreid: String): JsArray = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", api2key))
        params.add(new BasicNameValuePair("action", reportAction))
        params.add(new BasicNameValuePair("coreid", coreid))
        params.add(new BasicNameValuePair("outformat", "json"))

        val ppmsResponse = requestPPMS(url, true, params)
        ppmsResponse match {
            case Some(reportGetResponse) => {
                val reportJson: JsArray = Json.parse(reportGetResponse).as[JsArray]
                Logger.debug("Get " + reportAction + " result: " + reportJson)
                return reportJson
            }
            case None => {
                Logger.warn("Get " + reportAction + " request returns null")
                return Json.arr()
            }
        }
    }

}
