/*
 * SWGApiResponse.h
 * 
 * 
 */

#ifndef SWGApiResponse_H_
#define SWGApiResponse_H_

#include <QJsonObject>


#include <QString>

#include "SWGObject.h"


namespace Swagger {

class SWGApiResponse: public SWGObject {
public:
    SWGApiResponse();
    SWGApiResponse(QString* json);
    virtual ~SWGApiResponse();
    void init();
    void cleanup();

    QString asJson ();
    QJsonObject* asJsonObject();
    void fromJsonObject(QJsonObject &json);
    SWGApiResponse* fromJson(QString &jsonString);

    qint32 getCode();
    void setCode(qint32 code);
QString* getType();
    void setType(QString* type);
QString* getMessage();
    void setMessage(QString* message);

private:
    qint32 code;
QString* type;
QString* message;
};

} /* namespace Swagger */

#endif /* SWGApiResponse_H_ */
