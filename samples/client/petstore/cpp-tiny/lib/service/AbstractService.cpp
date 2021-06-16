#include "AbstractService.h"
#include "Arduino.h"


void Tiny::AbstractService::addHeader(std::string key, std::string value){
    http.addHeader(String(key.c_str()), String(value.c_str()));
}

void Tiny::AbstractService::addQueryParam(std::string key, std::string value){
    queryParams[key] = value;
}

int Tiny::AbstractService::sendRequest(const char * type, uint8_t * payload, size_t size){
    int httpCode = http.sendRequest(type, payload, size);
    return httpCode;
}

String Tiny::AbstractService::getResponseBody(){
    String respBody = http.getString();
    http.end();
    return respBody;
}

void Tiny::AbstractService::begin(std::string url){
    http.begin(String(url.c_str()), test_root_ca); //HTTPS connection
}
