package io.swagger.client

import java.io.File
import java.net.URLEncoder

import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap

object ScalaJsonUtil {
  def getJsonMapper = {
    val mapper = new ObjectMapper()
    mapper.registerModule(new DefaultScalaModule())
    mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    mapper.setSerializationInclusion(JsonInclude.Include.NON_DEFAULT)
    mapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false)
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY)
    mapper
  }
}

class ApiInvoker(val mapper: ObjectMapper = ScalaJsonUtil.getJsonMapper,
                 httpHeaders: HashMap[String, String] = HashMap(),
                 hostMap: HashMap[String, Client] = HashMap(),
                 asyncHttpClient: Boolean = false,
                 authScheme: String = "",
                 authPreemptive: Boolean = false) {

  var defaultHeaders: HashMap[String, String] = httpHeaders

  def escape(value: Long): String = value.toString

  def escape(value: Double): String = value.toString

  def escape(value: Float): String = value.toString

  def deserialize(json: String, containerType: String, cls: Class[_]) = {
    if (cls == classOf[String]) {
      json match {
        case s: String => {
          if (s.startsWith("\"") && s.endsWith("\"") && s.length > 1) s.substring(1, s.length - 2)
          else s
        }
        case _ => null
      }
    } else {
      containerType.toLowerCase match {
        case "array" => {
          val typeInfo = mapper.getTypeFactory().constructCollectionType(classOf[java.util.List[_]], cls)
          val response = mapper.readValue(json, typeInfo).asInstanceOf[java.util.List[_]]
          response.asScala.toList
        }
        case "list" => {
          val typeInfo = mapper.getTypeFactory().constructCollectionType(classOf[java.util.List[_]], cls)
          val response = mapper.readValue(json, typeInfo).asInstanceOf[java.util.List[_]]
          response.asScala.toList
        }
        case _ => {
          json match {
            case e: String if ("\"\"" == e) => null
            case _ => mapper.readValue(json, cls)
          }
        }
      }
    }
  }

  def invokeApi(host: String, path: String, method: String, queryParams: Map[String, String], body: AnyRef, headerParams: Map[String, String], contentType: String): String = {
    val client = getClient(host)

    val querystring = queryParams.filter(k => k._2 != null).map(k => (escape(k._1) + "=" + escape(k._2))).mkString("?", "&", "")
    val builder = client.resource(host + path + querystring).accept(contentType)
    headerParams.map(p => builder.header(p._1, p._2))
    defaultHeaders.map(p => {
      headerParams.contains(p._1) match {
        case true => // override default with supplied header
        case false => if (p._2 != null) builder.header(p._1, p._2)
      }
    })

    val response: ClientResponse = method match {
      case "GET" => {
        builder.get(classOf[ClientResponse]).asInstanceOf[ClientResponse]
      }
      case "POST" => {
        if (body != null && body.isInstanceOf[File]) {
          val file = body.asInstanceOf[File]
          val form = new FormDataMultiPart()
          form.field("filename", file.getName())
          form.bodyPart(new FileDataBodyPart("file", file, MediaType.MULTIPART_FORM_DATA_TYPE))
          builder.post(classOf[ClientResponse], form)
        }
        else {
          if (body == null) builder.post(classOf[ClientResponse], serialize(body))
          else builder.`type`(contentType).post(classOf[ClientResponse], serialize(body))
        }
      }
      case "PUT" => {
        if (body == null) builder.put(classOf[ClientResponse], null)
        else builder.`type`(contentType).put(classOf[ClientResponse], serialize(body))
      }
      case "DELETE" => {
        builder.delete(classOf[ClientResponse])
      }
      case _ => null
    }
    response.getClientResponseStatus().getStatusCode() match {
      case 204 => ""
      case code: Int if (Range(200, 299).contains(code)) => {
        response.hasEntity() match {
          case true => response.getEntity(classOf[String])
          case false => ""
        }
      }
      case _ => {
        val entity = response.hasEntity() match {
          case true => response.getEntity(classOf[String])
          case false => "no data"
        }
        throw new ApiException(
          response.getClientResponseStatus().getStatusCode(),
          entity)
      }
    }
  }

  def escape(value: String): String = {
    URLEncoder.encode(value, "utf-8").replaceAll("\\+", "%20")
  }

  def serialize(obj: AnyRef): String = {
    if (obj != null) {
      obj match {
        case e: List[_] => mapper.writeValueAsString(obj.asInstanceOf[List[_]].asJava)
        case _ => mapper.writeValueAsString(obj)
      }
    } else null
  }

  def getClient(host: String): Client = {
    hostMap.contains(host) match {
      case true => hostMap(host)
      case false => {
        val client = newClient(host)
        // client.addFilter(new LoggingFilter())
        hostMap += host -> client
        client
      }
    }
  }

  def newClient(host: String): Client = asyncHttpClient match {
    case true => {

      val config: DefaultAhcConfig = new DefaultAhcConfig()
      if (!authScheme.isEmpty) {
        val authSchemeEnum = Realm.AuthScheme.valueOf(authScheme)
        config.getAsyncHttpClientConfigBuilder
          .setRealm(new Realm.RealmBuilder().setScheme(authSchemeEnum)
          .setUsePreemptiveAuth(authPreemptive).build)
      }
      AhcHttpClient.create(config)
    }
    case _ => Client.create()
  }
}

object ApiInvoker extends ApiInvoker(mapper = ScalaJsonUtil.getJsonMapper,
  httpHeaders = HashMap(),
  hostMap = HashMap(),
  asyncHttpClient = false,
  authScheme = "",
  authPreemptive = false)

class ApiException(val code: Int, msg: String) extends RuntimeException(msg)

