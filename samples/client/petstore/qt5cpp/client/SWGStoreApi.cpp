#include "SWGStoreApi.h"
#include "SWGHelpers.h"

#include <QJsonArray>
#include <QJsonDocument>

namespace Swagger {
SWGStoreApi::SWGStoreApi() {}

SWGStoreApi::~SWGStoreApi() {}

SWGStoreApi::SWGStoreApi(QString host, QString basePath) {
    this->host = host;
    this->basePath = basePath;
}

void
SWGStoreApi::getInventory() {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/store/inventory");

    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGStoreApi::getInventoryCallback);

    worker->execute(&input);
}

void
SWGStoreApi::getInventoryCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: QMap<String, qint32>*

    
    

    
    QString json(worker->response);
    QMap&lt;String, qint32&gt;* output = new QMap(&json);

    
    

    worker->deleteLater();

    emit getInventorySignal(output);
    
}
void
SWGStoreApi::placeOrder(SWGOrder body) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/store/order");

    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "POST");

    
    // body
    input.request_body.append(body.asJson());
    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGStoreApi::placeOrderCallback);

    worker->execute(&input);
}

void
SWGStoreApi::placeOrderCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: SWGOrder*

    
    

    
    QString json(worker->response);
    SWGOrder* output = new SWGOrder(&json);

    
    

    worker->deleteLater();

    emit placeOrderSignal(output);
    
}
void
SWGStoreApi::getOrderById(QString* orderId) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/store/order/{orderId}");

    
    QString orderIdPathParam("{"); orderIdPathParam.append("orderId").append("}");
    fullPath.replace(orderIdPathParam, stringValue(orderId));
    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGStoreApi::getOrderByIdCallback);

    worker->execute(&input);
}

void
SWGStoreApi::getOrderByIdCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: SWGOrder*

    
    

    
    QString json(worker->response);
    SWGOrder* output = new SWGOrder(&json);

    
    

    worker->deleteLater();

    emit getOrderByIdSignal(output);
    
}
void
SWGStoreApi::deleteOrder(QString* orderId) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/store/order/{orderId}");

    
    QString orderIdPathParam("{"); orderIdPathParam.append("orderId").append("}");
    fullPath.replace(orderIdPathParam, stringValue(orderId));
    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "DELETE");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGStoreApi::deleteOrderCallback);

    worker->execute(&input);
}

void
SWGStoreApi::deleteOrderCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: 

    

    worker->deleteLater();

    
    emit deleteOrderSignal();
}
} /* namespace Swagger */
