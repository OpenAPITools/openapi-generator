
#include "SWGUser.h"

#include "SWGHelpers.h"

#include <QJsonDocument>
#include <QJsonArray>
#include <QObject>
#include <QDebug>

namespace Swagger {


SWGUser::SWGUser(QString* json) {
    init();
    this->fromJson(*json);
}

SWGUser::SWGUser() {
    init();
}

SWGUser::~SWGUser() {
    this->cleanup();
}

void
SWGUser::init() {
    id = 0L;
    username = new QString("");
    firstName = new QString("");
    lastName = new QString("");
    email = new QString("");
    password = new QString("");
    phone = new QString("");
    userStatus = 0;
    
}

void
SWGUser::cleanup() {
    
    if(username != NULL) {
        delete username;
    }
    if(firstName != NULL) {
        delete firstName;
    }
    if(lastName != NULL) {
        delete lastName;
    }
    if(email != NULL) {
        delete email;
    }
    if(password != NULL) {
        delete password;
    }
    if(phone != NULL) {
        delete phone;
    }
    
    
}

SWGUser*
SWGUser::fromJson(QString &json) {
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonObject jsonObject = doc.object();
    this->fromJsonObject(jsonObject);
    return this;
}

void
SWGUser::fromJsonObject(QJsonObject &pJson) {
    setValue(&id, pJson["id"], "qint64", "");
    setValue(&username, pJson["username"], "QString", "QString");
    setValue(&firstName, pJson["firstName"], "QString", "QString");
    setValue(&lastName, pJson["lastName"], "QString", "QString");
    setValue(&email, pJson["email"], "QString", "QString");
    setValue(&password, pJson["password"], "QString", "QString");
    setValue(&phone, pJson["phone"], "QString", "QString");
    setValue(&userStatus, pJson["userStatus"], "qint32", "");
    
}

QString
SWGUser::asJson ()
{
    QJsonObject* obj = this->asJsonObject();
    
    QJsonDocument doc(*obj);
    QByteArray bytes = doc.toJson();
    return QString(bytes);
}

QJsonObject*
SWGUser::asJsonObject() {
    QJsonObject* obj = new QJsonObject();
    obj->insert("id", QJsonValue(id));
    
    
    toJsonValue(QString("username"), username, obj, QString("QString"));
    
    
    
    
    
    toJsonValue(QString("firstName"), firstName, obj, QString("QString"));
    
    
    
    
    
    toJsonValue(QString("lastName"), lastName, obj, QString("QString"));
    
    
    
    
    
    toJsonValue(QString("email"), email, obj, QString("QString"));
    
    
    
    
    
    toJsonValue(QString("password"), password, obj, QString("QString"));
    
    
    
    
    
    toJsonValue(QString("phone"), phone, obj, QString("QString"));
    
    
    
    obj->insert("userStatus", QJsonValue(userStatus));
    

    return obj;
}

qint64
SWGUser::getId() {
    return id;
}
void
SWGUser::setId(qint64 id) {
    this->id = id;
}

QString*
SWGUser::getUsername() {
    return username;
}
void
SWGUser::setUsername(QString* username) {
    this->username = username;
}

QString*
SWGUser::getFirstName() {
    return firstName;
}
void
SWGUser::setFirstName(QString* firstName) {
    this->firstName = firstName;
}

QString*
SWGUser::getLastName() {
    return lastName;
}
void
SWGUser::setLastName(QString* lastName) {
    this->lastName = lastName;
}

QString*
SWGUser::getEmail() {
    return email;
}
void
SWGUser::setEmail(QString* email) {
    this->email = email;
}

QString*
SWGUser::getPassword() {
    return password;
}
void
SWGUser::setPassword(QString* password) {
    this->password = password;
}

QString*
SWGUser::getPhone() {
    return phone;
}
void
SWGUser::setPhone(QString* phone) {
    this->phone = phone;
}

qint32
SWGUser::getUserStatus() {
    return userStatus;
}
void
SWGUser::setUserStatus(qint32 userStatus) {
    this->userStatus = userStatus;
}



} /* namespace Swagger */

