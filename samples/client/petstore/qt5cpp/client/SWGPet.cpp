
#include "SWGPet.h"

#include "SWGHelpers.h"

#include <QJsonDocument>
#include <QJsonArray>
#include <QObject>
#include <QDebug>

namespace Swagger {


SWGPet::SWGPet(QString* json) {
    init();
    this->fromJson(*json);
}

SWGPet::SWGPet() {
    init();
}

SWGPet::~SWGPet() {
    this->cleanup();
}

void
SWGPet::init() {
    id = 0L;
    category = new SWGCategory();
    name = new QString("");
    photoUrls = new QList<QString*>();
    tags = new QList<SWGTag*>();
    status = new QString("");
    
}

void
SWGPet::cleanup() {
    
    if(category != NULL) {
        delete category;
    }
    if(name != NULL) {
        delete name;
    }
    if(photoUrls != NULL) {
        QList<QString*>* arr = photoUrls;
        foreach(QString* o, *arr) {
            delete o;
        }
        delete photoUrls;
    }
    if(tags != NULL) {
        QList<SWGTag*>* arr = tags;
        foreach(SWGTag* o, *arr) {
            delete o;
        }
        delete tags;
    }
    if(status != NULL) {
        delete status;
    }
    
}

SWGPet*
SWGPet::fromJson(QString &json) {
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonObject jsonObject = doc.object();
    this->fromJsonObject(jsonObject);
    return this;
}

void
SWGPet::fromJsonObject(QJsonObject &pJson) {
    setValue(&id, pJson["id"], "qint64", "");
    setValue(&category, pJson["category"], "SWGCategory", "SWGCategory");
    setValue(&name, pJson["name"], "QString", "QString");
    setValue(&photoUrls, pJson["photoUrls"], "QList", "QString");
    setValue(&tags, pJson["tags"], "QList", "SWGTag");
    setValue(&status, pJson["status"], "QString", "QString");
    
}

QString
SWGPet::asJson ()
{
    QJsonObject* obj = this->asJsonObject();
    
    QJsonDocument doc(*obj);
    QByteArray bytes = doc.toJson();
    return QString(bytes);
}

QJsonObject*
SWGPet::asJsonObject() {
    QJsonObject* obj = new QJsonObject();
    obj->insert("id", QJsonValue(id));
    
    
    toJsonValue(QString("category"), category, obj, QString("SWGCategory"));
    
    
    
    
    
    toJsonValue(QString("name"), name, obj, QString("QString"));
    
    
    
    
    
    QList<QString*>* photoUrlsList = photoUrls;
    QJsonArray photoUrlsJsonArray;
    toJsonArray((QList<void*>*)photoUrls, &photoUrlsJsonArray, "photoUrls", "QString");

    obj->insert("photoUrls", photoUrlsJsonArray);
    
    
    
    
    QList<SWGTag*>* tagsList = tags;
    QJsonArray tagsJsonArray;
    toJsonArray((QList<void*>*)tags, &tagsJsonArray, "tags", "SWGTag");

    obj->insert("tags", tagsJsonArray);
    
    
    
    
    toJsonValue(QString("status"), status, obj, QString("QString"));
    
    
    
    

    return obj;
}

qint64
SWGPet::getId() {
    return id;
}
void
SWGPet::setId(qint64 id) {
    this->id = id;
}

SWGCategory*
SWGPet::getCategory() {
    return category;
}
void
SWGPet::setCategory(SWGCategory* category) {
    this->category = category;
}

QString*
SWGPet::getName() {
    return name;
}
void
SWGPet::setName(QString* name) {
    this->name = name;
}

QList<QString*>*
SWGPet::getPhotoUrls() {
    return photoUrls;
}
void
SWGPet::setPhotoUrls(QList<QString*>* photoUrls) {
    this->photoUrls = photoUrls;
}

QList<SWGTag*>*
SWGPet::getTags() {
    return tags;
}
void
SWGPet::setTags(QList<SWGTag*>* tags) {
    this->tags = tags;
}

QString*
SWGPet::getStatus() {
    return status;
}
void
SWGPet::setStatus(QString* status) {
    this->status = status;
}



} /* namespace Swagger */

