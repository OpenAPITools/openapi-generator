
#include "SWGOrder.h"

#include "SWGHelpers.h"

#include <QJsonDocument>
#include <QJsonArray>
#include <QObject>
#include <QDebug>

namespace Swagger {


SWGOrder::SWGOrder(QString* json) {
    init();
    this->fromJson(*json);
}

SWGOrder::SWGOrder() {
    init();
}

SWGOrder::~SWGOrder() {
    this->cleanup();
}

void
SWGOrder::init() {
    id = 0L;
    petId = 0L;
    quantity = 0;
    shipDate = NULL;
    status = new QString("");
    complete = false;
    
}

void
SWGOrder::cleanup() {
    
    
    
    if(shipDate != NULL) {
        delete shipDate;
    }
    if(status != NULL) {
        delete status;
    }
    
    
}

SWGOrder*
SWGOrder::fromJson(QString &json) {
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonObject jsonObject = doc.object();
    this->fromJsonObject(jsonObject);
    return this;
}

void
SWGOrder::fromJsonObject(QJsonObject &pJson) {
    setValue(&id, pJson["id"], "qint64", "");
    setValue(&petId, pJson["petId"], "qint64", "");
    setValue(&quantity, pJson["quantity"], "qint32", "");
    setValue(&shipDate, pJson["shipDate"], "QDateTime", "QDateTime");
    setValue(&status, pJson["status"], "QString", "QString");
    setValue(&complete, pJson["complete"], "bool", "");
    
}

QString
SWGOrder::asJson ()
{
    QJsonObject* obj = this->asJsonObject();
    
    QJsonDocument doc(*obj);
    QByteArray bytes = doc.toJson();
    return QString(bytes);
}

QJsonObject*
SWGOrder::asJsonObject() {
    QJsonObject* obj = new QJsonObject();
    obj->insert("id", QJsonValue(id));
    obj->insert("petId", QJsonValue(petId));
    obj->insert("quantity", QJsonValue(quantity));
    
    
    toJsonValue(QString("shipDate"), shipDate, obj, QString("QDateTime"));
    
    
    
    
    
    toJsonValue(QString("status"), status, obj, QString("QString"));
    
    
    
    obj->insert("complete", QJsonValue(complete));
    

    return obj;
}

qint64
SWGOrder::getId() {
    return id;
}
void
SWGOrder::setId(qint64 id) {
    this->id = id;
}

qint64
SWGOrder::getPetId() {
    return petId;
}
void
SWGOrder::setPetId(qint64 petId) {
    this->petId = petId;
}

qint32
SWGOrder::getQuantity() {
    return quantity;
}
void
SWGOrder::setQuantity(qint32 quantity) {
    this->quantity = quantity;
}

QDateTime*
SWGOrder::getShipDate() {
    return shipDate;
}
void
SWGOrder::setShipDate(QDateTime* shipDate) {
    this->shipDate = shipDate;
}

QString*
SWGOrder::getStatus() {
    return status;
}
void
SWGOrder::setStatus(QString* status) {
    this->status = status;
}

bool
SWGOrder::getComplete() {
    return complete;
}
void
SWGOrder::setComplete(bool complete) {
    this->complete = complete;
}



} /* namespace Swagger */

