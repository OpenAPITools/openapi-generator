#include "AbstractService.h"
#include "Arduino.h"


void Tiny::AbstractService::addHeader(std::string key, std::string value){
    http.addHeader(String(key.c_str()), String(value.c_str()));
}

void Tiny::AbstractService::addQueryParam(std::string key, std::string value){
    queryParams[key] = value;
}

int Tiny::AbstractService::sendRequest(const char * type, Stream * stream, size_t size){
    int httpCode = http.sendRequest(type, stream, size);
    return httpCode;
}
void Tiny::AbstractService::begin(std::string url){
    http.begin(String(url.c_str()), test_root_ca); //HTTPS connection
}
