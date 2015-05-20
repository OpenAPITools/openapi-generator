#ifndef _SWG_SWGStoreApi_H_
#define _SWG_SWGStoreApi_H_

#include "SWGHttpRequest.h"

#include "QMap.h"
#include "SWGOrder.h"
#include <QString>

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

    void getInventory();
    void placeOrder(SWGOrder body);
    void getOrderById(QString* orderId);
    void deleteOrder(QString* orderId);
    
private:
    void getInventoryCallback (HttpRequestWorker * worker);
    void placeOrderCallback (HttpRequestWorker * worker);
    void getOrderByIdCallback (HttpRequestWorker * worker);
    void deleteOrderCallback (HttpRequestWorker * worker);
    
signals:
    void getInventorySignal(QMap<QString, qint32>* summary);
    void placeOrderSignal(SWGOrder* summary);
    void getOrderByIdSignal(SWGOrder* summary);
    void deleteOrderSignal();
    
};
}
#endif