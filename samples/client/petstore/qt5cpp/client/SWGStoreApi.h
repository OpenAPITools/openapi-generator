#ifndef _SWG_SWGStoreApi_H_
#define _SWG_SWGStoreApi_H_

#include "SWGHttpRequest.h"

#include <QString>
#include "QMap.h"
#include "SWGOrder.h"

#include <QObject>

namespace Swagger {

class SWGStoreApi: public QObject {
    Q_OBJECT

public:
    SWGStoreApi();
    SWGStoreApi(QString host, QString basePath);
    ~SWGStoreApi();

    QString host;
    QString basePath;

    void deleteOrder(QString* orderId);
    void getInventory();
    void getOrderById(qint64 orderId);
    void placeOrder(SWGOrder body);
    
private:
    void deleteOrderCallback (HttpRequestWorker * worker);
    void getInventoryCallback (HttpRequestWorker * worker);
    void getOrderByIdCallback (HttpRequestWorker * worker);
    void placeOrderCallback (HttpRequestWorker * worker);
    
signals:
    void deleteOrderSignal();
    void getInventorySignal(QMap<QString, qint32>* summary);
    void getOrderByIdSignal(SWGOrder* summary);
    void placeOrderSignal(SWGOrder* summary);
    
};
}
#endif