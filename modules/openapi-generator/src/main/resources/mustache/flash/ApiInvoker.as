package org.openapitools.common {
import org.openapitools.event.ApiClientEvent;
import org.openapitools.event.Response;

import flash.events.EventDispatcher;
import flash.utils.*;
import mx.rpc.AsyncToken;
import mx.rpc.events.ResultEvent;
import mx.rpc.events.FaultEvent;
import mx.utils.ObjectUtil;
import mx.rpc.http.HTTPService;
import mx.messaging.messages.HTTPRequestMessage;
import mx.messaging.ChannelSet;
import mx.messaging.channels.DirectHTTPChannel;
import asaxb.xml.bind.ASAXBContext;
import asaxb.xml.bind.Unmarshaller;

public class ApiInvoker extends EventDispatcher {

    private static const DELETE_DATA_DUMMY:String = "dummyDataRequiredForDeleteOverride";
    private static const X_HTTP_OVERRIDE_KEY:String = "X-HTTP-Method-Override";
    private static const CONTENT_TYPE_HEADER_KEY:String = "Content-Type";

    public function ApiInvoker(apiUsageCredentials:ApiUserCredentials, eventNotifier:EventDispatcher, useProxy:Boolean = true) {
        _apiUsageCredentials = apiUsageCredentials;
        _useProxyServer = useProxy;
        if (_apiUsageCredentials.hostName != null) {
            _proxyHostName = _apiUsageCredentials.hostName;
        }
        _apiPath = _apiUsageCredentials.apiPath;
        _proxyPath = _apiUsageCredentials.proxyPath;
        _apiProxyServerUrl = _apiUsageCredentials.apiProxyServerUrl;
        _apiEventNotifier = eventNotifier;
    }
    public var _apiEventNotifier:EventDispatcher;
    internal var _apiProxyServerUrl:String = "";
    internal var _useProxyServer:Boolean = true;
    private var _apiUsageCredentials:ApiUserCredentials;
    private var _baseUrl:String = "";
    private var _proxyHostName:String = "";
    private var _apiPath:String = "";
    private var _proxyPath:String = "";

    public function invokeAPI(resourceURL:String, method:String, queryParams:Dictionary, postObject:Object, headerParams:Dictionary):AsyncToken {
        //make the communication
        if (_useProxyServer) {
            resourceURL = _apiProxyServerUrl + resourceURL;
        }
        else {
            resourceURL = "http://" + _proxyHostName + _apiPath + resourceURL;
        }

        var counter:int = 0;
        var symbol:String = "&";
        var paramValue:Object;
        for (var paramName:String in queryParams) {
            paramValue = queryParams[paramName];
            //var key:String = paramName;
            // do stuff
            symbol = "&";
            if (counter == 0) {
                symbol = "?";
            }
            resourceURL = resourceURL + symbol + paramName + "=" + paramValue.toString();
            counter++;

        }
//        trace(resourceURL);
        //create a httpservice and invoke the rest url waiting for response
        var requestHeader:Object = new Object();
        if (headerParams != null) {
            for (var key:String in headerParams) {
                requestHeader[key] = headerParams[key];
            }
        }

        resourceURL = ApiUrlHelper.appendTokenInfo(resourceURL, requestHeader, _apiUsageCredentials);

        var bodyData:String = marshal(postObject).toString();//restRequest.postData;

        return doRestCall(resourceURL, onApiRequestResult, onApiRequestFault, method, bodyData, requestHeader, "application/xml");


    }

    public function marshal(source:Object):Object {
//        trace("marshal got - "  + source)
        if (source is String) {
            return source;
        } else if (source is Array && source.length > 0) {
            var writer:XMLWriter = new XMLWriter();
            var sourceArray:Array = source as Array;
            var arrayEnclosure:String = getArrayEnclosure(sourceArray);
            writer.xml.setName(arrayEnclosure);

            for (var i:int = 0; i < sourceArray.length; i++) {
                var o:Object = sourceArray[i];
                writer.xml.appendChild(marshal(o));
            }
            return writer.xml;
        } else
            return marshalObject(source);
    }

    public function marshalObject(source:Object):XML {
        var writer:XMLWriter = new XMLWriter();
        var objDescriptor:XML = describeType(source);
        var property:XML;
        var propertyType:String;
        var propertyValue:Object;

        var qualifiedClassName:String = objDescriptor.@name;
        qualifiedClassName = qualifiedClassName.replace("::", ".");
        var className:String = qualifiedClassName.substring(qualifiedClassName.lastIndexOf(".") + 1);
        className = className.toLowerCase() + className.substring(1);
        writer.xml.setName(className);

        for each(property in objDescriptor.elements("variable")) {
            propertyValue = source[property.@name];
            if (propertyValue != null) {
                if (ObjectUtil.isSimple(propertyValue)) {
                    writer.addProperty(property.@name, propertyValue.toString());
                }
                else {
                    writer.addProperty(property.@name, marshal(propertyValue).toXMLString());
                }
            }
        }
        for each(property in objDescriptor.elements("accessor")) {
            if (property.@access == "readonly") {
                continue;
            }
            propertyValue = source[property.@name];
            if (source[property.@name] != null) {
                if (ObjectUtil.isSimple(propertyValue)) {
                    writer.addProperty(property.@name, propertyValue.toString());
                }
                else {
                    writer.addProperty(property.@name, marshal(propertyValue).toXMLString());
                }
            }
        }
        return writer.xml;
    }

    public function escapeString(str:String):String {
        return str;
    }

    private function doRestCall(url:String, resultFunction:Function, faultFunction:Function = null,
                                restMethod:String = "GET",
                                bodyData:Object = null, headers:Object = null, contentType:String = "application/xml"):AsyncToken {
        var httpService:HTTPService = new HTTPService();

        if (headers == null) {
            headers = new Object();
        }
        httpService.method = restMethod;

        if (restMethod.toUpperCase() != HTTPRequestMessage.GET_METHOD) {
            //httpService.method = HTTPRequestMessage.POST_METHOD; - not required as we're using the proxy
            if (bodyData == null) {
                bodyData = new Object();
            }

            if (restMethod == HTTPRequestMessage.DELETE_METHOD) {
                headers[X_HTTP_OVERRIDE_KEY] = HTTPRequestMessage.DELETE_METHOD;
                bodyData = DELETE_DATA_DUMMY;
            }
            else if (restMethod == HTTPRequestMessage.PUT_METHOD) {
                headers[X_HTTP_OVERRIDE_KEY] = HTTPRequestMessage.PUT_METHOD;
            }
            else {
                headers[CONTENT_TYPE_HEADER_KEY] = contentType;
            }
        }
        else {
            //if the request type is GET and content type is xml then the Flex HTTPService converts it to a POST ... yeah
            contentType = null;
        }

        httpService.url = url;
        httpService.contentType = contentType;
        httpService.resultFormat = "e4x";
        httpService.headers = headers;
        httpService.addEventListener(ResultEvent.RESULT, resultFunction);
        if (faultFunction != null) {
            httpService.addEventListener(FaultEvent.FAULT, faultFunction);
        }
        if (_useProxyServer) {
            httpService.useProxy = true;

            var channelSet:ChannelSet = new ChannelSet();
            var httpChannel:DirectHTTPChannel = new DirectHTTPChannel("");
            httpChannel.uri = ApiUrlHelper.getProxyUrl(_proxyHostName, _proxyPath);
            channelSet.addChannel(httpChannel);
            httpService.channelSet = channelSet;
        }

        return httpService.send(bodyData);
    }

    private function onApiRequestResult(event:ResultEvent):void {
        
        // I put this in comments. Here, it's trying to parse XML and it fails if it's not XML.
        // Therefore, it's better to have the raw result that we can parse as we want. 
        // TODO: Create different parser (JSON, XML, etc.)
        /*
        var completionListener:Function = event.token.completionListener;
        var result:Object = event.result;
        var resultType:Class = event.token.returnType;
        var resultObject:Object;
        if (resultType != null) {
            var context:ASAXBContext = ASAXBContext.newInstance(resultType);
            var unmarshaller:Unmarshaller = context.createUnmarshaller();
            var resultXML:XML = new XML(event.result);
            try {
                resultObject = unmarshaller.unmarshal(resultXML);
            }
            catch (error:TypeError) {
                var errorResponse:Response = new Response(false, null, "Could not unmarshall response");
                if (_apiEventNotifier != null) { //dispatch event via assigned dispatcher
                    var failureEvent:ApiClientEvent = new ApiClientEvent(event.token.completionEventType);
                    failureEvent.response = errorResponse;
                    _apiEventNotifier.dispatchEvent(failureEvent);
                }
            }

            if (resultObject is ListWrapper) {
                resultObject = ListWrapper(resultObject).getList();
            }
        }

        var response:Response = new Response(true, resultObject);
        */

        var response:Response = new Response(true, event.result);
        response.requestId = event.token.requestId;
        var successEventType:String = event.token.completionEventType != null ? event.token.completionEventType : ApiClientEvent.SUCCESS_EVENT;

        if (_apiEventNotifier != null) { //dispatch event via assigned dispatcher
            var successEvent:ApiClientEvent = new ApiClientEvent(successEventType);
            successEvent.response = response;
            _apiEventNotifier.dispatchEvent(successEvent);
        }
    }

    private function onApiRequestFault(event:FaultEvent):void {
        var completionListener:Function = event.token.completionListener;
        if (completionListener != null) {
            completionListener.call(null, new Response(false, null, event.fault.faultString));
        }

        var failureEventType:String = event.token.completionEventType != null ? event.token.completionEventType : ApiClientEvent.FAILURE_EVENT;

        if (_apiEventNotifier != null) { //dispatch event via assigned dispatcher
            var failureEvent:ApiClientEvent = new ApiClientEvent(failureEventType);
            failureEvent.response = new Response(false, null, event.fault.faultString);
            _apiEventNotifier.dispatchEvent(failureEvent);
        }
    }

    private function getArrayEnclosure(arr:Array):String {
        if (arr != null && arr.length > 0) {
            var className:String = flash.utils.getQualifiedClassName(arr[0])
            if (className.indexOf("::") > 0)
                className = className.substr(className.indexOf("::") + 2, className.length)

            return className.substring(0, 1).toLowerCase() + className.substring(1, className.length) + "s";
        } else
            return "";
    }


}
}
