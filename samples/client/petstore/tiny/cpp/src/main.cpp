#include "PetApi.h"
const char* ssid = "NOKIA-CE21"; //Enter SSID
const char* password = "R4f6U4pEeS"; //Enter Password


void setup(){
    //Initialize serial and wait for port to open:
    Serial.begin(9600);
    delay(100);
    
    Serial.print("Attempting to connect to SSID: ");
    Serial.println(ssid);
    WiFi.begin(ssid, password);

    // attempt to connect to Wifi network:
    while (WiFi.status() != WL_CONNECTED) {
        Serial.print(".");
        // wait 1 second for re-trying
        delay(1000);
    }

    Serial.print("Connected to ");
    Serial.println(ssid);

    //Print LAN IP.  
    Serial.print("IP address set: ");
    Serial.println(WiFi.localIP());

    Tiny::PetApi petapi;
    auto resp = petapi.getPetById(1);
    Serial.println(resp.code);
    Serial.println(resp.obj.toJson().dump().c_str());
}

void loop(){}

#else
int main(){
    return 1;
}
#endif