/*
 * SWGPet.h
 * 
 * 
 */

#ifndef SWGPet_H_
#define SWGPet_H_

#include <QJsonObject>


#include "SWGCategory.h"
#include "SWGTag.h"
#include <QList>
#include <QString>

#include "SWGObject.h"


namespace Swagger {

class SWGPet: public SWGObject {
public:
    SWGPet();
    SWGPet(QString* json);
    virtual ~SWGPet();
    void init();
    void cleanup();

    QString asJson ();
    QJsonObject* asJsonObject();
    void fromJsonObject(QJsonObject &json);
    SWGPet* fromJson(QString &jsonString);

    qint64 getId();
    void setId(qint64 id);
    SWGCategory* getCategory();
    void setCategory(SWGCategory* category);
    QString* getName();
    void setName(QString* name);
    QList<QString*>* getPhotoUrls();
    void setPhotoUrls(QList<QString*>* photoUrls);
    QList<SWGTag*>* getTags();
    void setTags(QList<SWGTag*>* tags);
    QString* getStatus();
    void setStatus(QString* status);
    

private:
    qint64 id;
    SWGCategory* category;
    QString* name;
    QList<QString*>* photoUrls;
    QList<SWGTag*>* tags;
    QString* status;
    
};

} /* namespace Swagger */

#endif /* SWGPet_H_ */
