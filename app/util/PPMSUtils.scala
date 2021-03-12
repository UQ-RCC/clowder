package util

import java.util.ArrayList
import play.api.Logger

import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.util.EntityUtils
import org.apache.http.NameValuePair
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.HttpResponse



object PPMSUtils {

    /**
    * httpGet
    */
    def httpGet(url: String, isApi2: Boolean, params: ArrayList): HttpResponse = {
        val httpclient = new DefaultHttpClient()
        val httpGet = new HttpGet(url + (if (isApi2) "api2/" else "pumapi/")) 
        httpGet.setEntity(new UrlEncodedFormEntity(params))
        return httpclient.execute(httpGet)
    }

    /**
    * getPPMSProjects 
    */
    def getPPMSProjects(url: String, pumaKey: String, getProjectAction: String): JsArray = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", pumaKey))
        params.add(new BasicNameValuePair("action", getProjectAction))
        params.add(new BasicNameValuePair("format", "json"))
        params.add(new BasicNameValuePair("active", true))  
        val projectsGetResponse = httpGet(url, false, params)
        
        Logger.debug("PPMS get projects response status: " + projectsGetResponse.getStatusLine().toString())
        val status = projectsGetResponse.getStatusLine.getStatusCode()
        if(status < 200 || status >= 300) {
            Logger.debug("PPMS get project returns unexpected status code:" + status)
            return Json.arr()
        }
        // digest the result
        val projectsGetEntity = projectsGetResponse.getEntity()
        if(projectsGetEntity == null) {
            Logger.debug("PPMS get project returns null")
            return Json.arr()
        }
        val projectsGetResult = EntityUtils.toString(projectsGetEntity)
        val projectsList: JsArray = Json.parse(projectsGetResult).as[JsArray]
        Logger.debug("PPMS get projects response status: " + projectsList)
        return projectsList
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
        params.add(new BasicNameValuePair("projectId", projectId))
        params.setEntity(new UrlEncodedFormEntity(projectProfileGetParams))

        val projectProfileGetResponse = httpGet(url, true, params)
        val status = projectProfileGetResponse.getStatusLine.getStatusCode()
        if(status < 200 || status >= 300) {
            Logger.debug("Get project profile returns unexpected status code:" + status)
            return Json.arr()
        }
        //
        val projectProfileGetEntity = projectProfileGetResponse.getEntity()
        if(projectProfileGetEntity == null) {
            Logger.debug("Get project profile returns null")
            return Json.arr()
        }
        val projectProfileGetResult = EntityUtils.toString(projectProfileGetEntity)
        val projectProfileJson: JsArray = Json.parse(projectProfileGetResult).as[JsArray]
        Logger.debug("Get projects profile result: " + projectProfileJson)
        return projectProfileJson
    }

    /**
    *
    */
    def getPPMSUser(url: String, pumakey: String, username: String, getUserAction: String): Option[JsObject] = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", pumakey))
        params.add(new BasicNameValuePair("action", getUserAction))
        params.add(new BasicNameValuePair("login", username))
        params.add(new BasicNameValuePair("format", "json"))
        val userGetResponse = httpGet(url, false, params)
        
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
        val userProfileJson: JsObject = Json.parse(projectProfileGetResult).as[JsObject]
        Logger.debug("User profile: " + userProfileJson)
        return userProfileJson
    }


    /**
    * returns an array of users info belong to a project
    */
    def getPPMSProjectUsers(url: String, pumakey: String, projectId: Integer, getProjectUsersAction: String, getUserAction: String): Array[JsObject] = {
        val params = new ArrayList[NameValuePair]()
        params.add(new BasicNameValuePair("apikey", pumakey))
        params.add(new BasicNameValuePair("action", getProjectUsersAction))
        params.add(new BasicNameValuePair("projectid", projectId))
        params.add(new BasicNameValuePair("withdeactivated", false))
        val projectUsersGetResponse = httpGet(url, false, params)
        
        Logger.debug("Get project users response status: " + projectUsersGetResponse.getStatusLine().toString())
        val status = projectUsersGetResponse.getStatusLine.getStatusCode()
        if(status < 200 || status >= 300) {
            Logger.debug("Get project returns unexpected status code:" + status)
            return Json.arr()
        }
        val projectUsersGetEntity = projectUsersGetResponse.getEntity()
        if(projectUsersGetEntity == null) {
            Logger.debug("Get project profile returns null")
            return Json.arr()
        }
        val projectUsers = EntityUtils.toString(projectUsersGetEntity).split("\n")
        Logger.debug("Project users:" + projectUsers)
        val users = projectUsers.map(projectUser => getPPMSUser(url, pumakey, projectUser, getUserAction)).filter(_ != None)
        Logger.debug("Project user profiles:" + users)
        return users
    }


}