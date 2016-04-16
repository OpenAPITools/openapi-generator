
#include "SWGApiResponse.h"

#include "SWGHelpers.h"

#include <QJsonDocument>
#include <QJsonArray>
#include <QObject>
#include <QDebug>

namespace Swagger {


SWGApiResponse::SWGApiResponse(QString* json) {
    init();
    this->fromJson(*json);
}

SWGApiResponse::SWGApiResponse() {
    init();
}

SWGApiResponse::~SWGApiResponse() {
    this->cleanup();
}

void
SWGApiResponse::init() {
    code = 0;
type = new QString("");
message = new QString("");
}

void
SWGApiResponse::cleanup() {
    
if(type != NULL) {
        delete type;
    }
if(message != NULL) {
        delete message;
    }
}

SWGApiResponse*
SWGApiResponse::fromJson(QString &json) {
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonObject jsonObject = doc.object();
    this->fromJsonObject(jsonObject);
    return this;
}

void
SWGApiResponse::fromJsonObject(QJsonObject &pJson) {
    setValue(&code, pJson["code"], "qint32", "");
setValue(&type, pJson["type"], "QString", "QString");
setValue(&message, pJson["message"], "QString", "QString");
}

QString
SWGApiResponse::asJson ()
{
    QJsonObject* obj = this->asJsonObject();
    
    QJsonDocument doc(*obj);
    QByteArray bytes = doc.toJson();
    return QString(bytes);
}

QJsonObject*
SWGApiResponse::asJsonObject() {
    QJsonObject* obj = new QJsonObject();
    obj->insert("code", QJsonValue(code));

    
    toJsonValue(QString("type"), type, obj, QString("QString"));
    
        

    
    toJsonValue(QString("message"), message, obj, QString("QString"));
    
        

    return obj;
}

qint32
SWGApiResponse::getCode() {
    return code;
}
void
SWGApiResponse::setCode(qint32 code) {
    this->code = code;
}

QString*
SWGApiResponse::getType() {
    return type;
}
void
SWGApiResponse::setType(QString* type) {
    this->type = type;
}

QString*
SWGApiResponse::getMessage() {
    return message;
}
void
SWGApiResponse::setMessage(QString* message) {
    this->message = message;
}



} /* namespace Swagger */

