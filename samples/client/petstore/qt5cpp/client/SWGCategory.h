/*
 * SWGCategory.h
 * 
 * 
 */

#ifndef SWGCategory_H_
#define SWGCategory_H_

#include <QJsonObject>


#include <QString>

#include "SWGObject.h"


namespace Swagger {

class SWGCategory: public SWGObject {
public:
    SWGCategory();
    SWGCategory(QString* json);
    virtual ~SWGCategory();
    void init();
    void cleanup();

    QString asJson ();
    QJsonObject* asJsonObject();
    void fromJsonObject(QJsonObject &json);
    SWGCategory* fromJson(QString &jsonString);

    qint64 getId();
    void setId(qint64 id);
    QString* getName();
    void setName(QString* name);
    

private:
    qint64 id;
    QString* name;
    
};

} /* namespace Swagger */

#endif /* SWGCategory_H_ */
