#include "Service.h"
#include "Arduino.h"

void Tiny::Service::begin(std::string url){
    http.begin(String(url.c_str()), test_root_ca); //HTTPS connection
}

void Tiny::Service::addHeader(std::string key, std::string value){
    http.addHeader(String(key.c_str()), String(value.c_str()));
}

void Tiny::Service::addQueryParam(std::string key, std::string value){
    queryParams[key] = value;
}

int Tiny::Service::sendRequest(std::string url, const char * type, uint8_t * payload, size_t size){
    this->url = url;
    prepareRequest();

    begin(this->url);

    int httpCode = http.sendRequest(type, payload, size);
    return httpCode;
}

void Tiny::Service::prepareRequest(){
    if (!queryParams.empty()){
        addQueryParamsToUrl();
    }
}

void Tiny::Service::addQueryParamsToUrl(){
    this->url += "?";
    for (const auto& kv : queryParams) {
        this->url += kv.first + "=" + kv.second + "&";
    }

    // Remove last '&' char from url
    this->url.pop_back();
}


String Tiny::Service::getResponseBody(){
    String respBody = http.getString();
    http.end();
    return respBody;
}

