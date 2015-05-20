/*
 * SWGOrder.h
 * 
 * 
 */

#ifndef SWGOrder_H_
#define SWGOrder_H_

#include <QJsonObject>


#include "QDateTime.h"
#include <QString>

#include "SWGObject.h"


namespace Swagger {

class SWGOrder: public SWGObject {
public:
    SWGOrder();
    SWGOrder(QString* json);
    virtual ~SWGOrder();
    void init();
    void cleanup();

    QString asJson ();
    QJsonObject* asJsonObject();
    void fromJsonObject(QJsonObject &json);
    SWGOrder* fromJson(QString &jsonString);

    qint64 getId();
    void setId(qint64 id);
    qint64 getPetId();
    void setPetId(qint64 petId);
    qint32 getQuantity();
    void setQuantity(qint32 quantity);
    QDateTime* getShipDate();
    void setShipDate(QDateTime* shipDate);
    QString* getStatus();
    void setStatus(QString* status);
    bool getComplete();
    void setComplete(bool complete);
    

private:
    qint64 id;
    qint64 petId;
    qint32 quantity;
    QDateTime* shipDate;
    QString* status;
    bool complete;
    
};

} /* namespace Swagger */

#endif /* SWGOrder_H_ */
