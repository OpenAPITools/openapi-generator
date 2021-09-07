#include "AbstractService.h"
#include "Arduino.h"



void Tiny::AbstractService::begin(std::string url){
    http.begin(String(url.c_str()), test_root_ca); //HTTPS connection
}
