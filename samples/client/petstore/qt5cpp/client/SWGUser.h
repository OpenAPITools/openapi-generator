/*
 * SWGUser.h
 * 
 * 
 */

#ifndef SWGUser_H_
#define SWGUser_H_

#include <QJsonObject>


#include <QString>

#include "SWGObject.h"


namespace Swagger {

class SWGUser: public SWGObject {
public:
    SWGUser();
    SWGUser(QString* json);
    virtual ~SWGUser();
    void init();
    void cleanup();

    QString asJson ();
    QJsonObject* asJsonObject();
    void fromJsonObject(QJsonObject &json);
    SWGUser* fromJson(QString &jsonString);

    qint64 getId();
    void setId(qint64 id);
    QString* getUsername();
    void setUsername(QString* username);
    QString* getFirstName();
    void setFirstName(QString* firstName);
    QString* getLastName();
    void setLastName(QString* lastName);
    QString* getEmail();
    void setEmail(QString* email);
    QString* getPassword();
    void setPassword(QString* password);
    QString* getPhone();
    void setPhone(QString* phone);
    qint32 getUserStatus();
    void setUserStatus(qint32 userStatus);
    

private:
    qint64 id;
    QString* username;
    QString* firstName;
    QString* lastName;
    QString* email;
    QString* password;
    QString* phone;
    qint32 userStatus;
    
};

} /* namespace Swagger */

#endif /* SWGUser_H_ */
