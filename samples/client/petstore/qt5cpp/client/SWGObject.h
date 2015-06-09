#ifndef _SWG_OBJECT_H_
#define _SWG_OBJECT_H_

#include <QJsonValue>

class SWGObject {
  public:
    virtual QJsonObject* asJsonObject() {
      return NULL;
    }
    virtual ~SWGObject() {}
    virtual SWGObject* fromJson(QString &jsonString) {
        Q_UNUSED(jsonString);
        return NULL;
    }
    virtual void fromJsonObject(QJsonObject &json) {
        Q_UNUSED(json);
    }
    virtual QString asJson() {
        return QString("");
    }
};

#endif /* _SWG_OBJECT_H_ */
