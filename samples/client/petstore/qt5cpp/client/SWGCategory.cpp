
#include "SWGCategory.h"

#include "SWGHelpers.h"

#include <QJsonDocument>
#include <QJsonArray>
#include <QObject>
#include <QDebug>

namespace Swagger {


SWGCategory::SWGCategory(QString* json) {
    init();
    this->fromJson(*json);
}

SWGCategory::SWGCategory() {
    init();
}

SWGCategory::~SWGCategory() {
    this->cleanup();
}

void
SWGCategory::init() {
    id = 0L;
    name = new QString("");
    
}

void
SWGCategory::cleanup() {
    
    if(name != NULL) {
        delete name;
    }
    
}

SWGCategory*
SWGCategory::fromJson(QString &json) {
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonObject jsonObject = doc.object();
    this->fromJsonObject(jsonObject);
    return this;
}

void
SWGCategory::fromJsonObject(QJsonObject &pJson) {
    setValue(&id, pJson["id"], "qint64", "");
    setValue(&name, pJson["name"], "QString", "QString");
    
}

QString
SWGCategory::asJson ()
{
    QJsonObject* obj = this->asJsonObject();
    
    QJsonDocument doc(*obj);
    QByteArray bytes = doc.toJson();
    return QString(bytes);
}

QJsonObject*
SWGCategory::asJsonObject() {
    QJsonObject* obj = new QJsonObject();
    obj->insert("id", QJsonValue(id));
    
    
    toJsonValue(QString("name"), name, obj, QString("QString"));
    
    
    
    

    return obj;
}

qint64
SWGCategory::getId() {
    return id;
}
void
SWGCategory::setId(qint64 id) {
    this->id = id;
}

QString*
SWGCategory::getName() {
    return name;
}
void
SWGCategory::setName(QString* name) {
    this->name = name;
}



} /* namespace Swagger */

