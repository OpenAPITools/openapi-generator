/*
 * SWGTag.h
 * 
 * 
 */

#ifndef SWGTag_H_
#define SWGTag_H_

#include <QJsonObject>


#include <QString>

#include "SWGObject.h"


namespace Swagger {

class SWGTag: public SWGObject {
public:
    SWGTag();
    SWGTag(QString* json);
    virtual ~SWGTag();
    void init();
    void cleanup();

    QString asJson ();
    QJsonObject* asJsonObject();
    void fromJsonObject(QJsonObject &json);
    SWGTag* fromJson(QString &jsonString);

    qint64 getId();
    void setId(qint64 id);
    QString* getName();
    void setName(QString* name);
    

private:
    qint64 id;
    QString* name;
    
};

} /* namespace Swagger */

#endif /* SWGTag_H_ */
