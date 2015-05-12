
#include "SWGTag.h"

#include "SWGHelpers.h"

#include <QJsonDocument>
#include <QJsonArray>
#include <QObject>
#include <QDebug>

namespace Swagger {


SWGTag::SWGTag(QString* json) {
    init();
    this->fromJson(*json);
}

SWGTag::SWGTag() {
    init();
}

SWGTag::~SWGTag() {
    this->cleanup();
}

void
SWGTag::init() {
    id = 0L;
    name = new QString("");
    
}

void
SWGTag::cleanup() {
    
    if(name != NULL) {
        delete name;
    }
    
}

SWGTag*
SWGTag::fromJson(QString &json) {
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonObject jsonObject = doc.object();
    this->fromJsonObject(jsonObject);
    return this;
}

void
SWGTag::fromJsonObject(QJsonObject &pJson) {
    setValue(&id, pJson["id"], "qint64", "");
    setValue(&name, pJson["name"], "QString", "QString");
    
}

QString
SWGTag::asJson ()
{
    QJsonObject* obj = this->asJsonObject();
    
    QJsonDocument doc(*obj);
    QByteArray bytes = doc.toJson();
    return QString(bytes);
}

QJsonObject*
SWGTag::asJsonObject() {
    QJsonObject* obj = new QJsonObject();
    obj->insert("id", QJsonValue(id));
    
    
    toJsonValue(QString("name"), name, obj, QString("QString"));
    
    
    
    

    return obj;
}

qint64
SWGTag::getId() {
    return id;
}
void
SWGTag::setId(qint64 id) {
    this->id = id;
}

QString*
SWGTag::getName() {
    return name;
}
void
SWGTag::setName(QString* name) {
    this->name = name;
}



} /* namespace Swagger */

