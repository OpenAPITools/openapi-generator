#include "SWGPetApi.h"
#include "SWGHelpers.h"

#include <QJsonArray>
#include <QJsonDocument>

namespace Swagger {
SWGPetApi::SWGPetApi() {}

SWGPetApi::~SWGPetApi() {}

SWGPetApi::SWGPetApi(QString host, QString basePath) {
    this->host = host;
    this->basePath = basePath;
}

void
SWGPetApi::updatePet(SWGPet body) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet");

    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "PUT");

    
    // body
    input.request_body.append(body.asJson());
    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::updatePetCallback);

    worker->execute(&input);
}

void
SWGPetApi::updatePetCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: 

    

    worker->deleteLater();

    
    emit updatePetSignal();
}
void
SWGPetApi::addPet(SWGPet body) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet");

    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "POST");

    
    // body
    input.request_body.append(body.asJson());
    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::addPetCallback);

    worker->execute(&input);
}

void
SWGPetApi::addPetCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: 

    

    worker->deleteLater();

    
    emit addPetSignal();
}
void
SWGPetApi::findPetsByStatus(QList&lt;QString*&gt;* status) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet/findByStatus");

    

    if(fullPath.compare("?") > 0) fullPath.append("?");
    else fullPath.append("&");
    fullPath.append(QUrl::toPercentEncoding("status"))
        .append("=")
        .append(QUrl::toPercentEncoding(stringValue(status)));
    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::findPetsByStatusCallback);

    worker->execute(&input);
}

void
SWGPetApi::findPetsByStatusCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: QList<SWGPet*>*

    
    
    QList<SWGPet*>* output = new QList<SWGPet*>();
    QString json(worker->response);
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonArray jsonArray = doc.array();

    foreach(QJsonValue obj, jsonArray) {
        SWGPet* o = new SWGPet();
        QJsonObject jv = obj.toObject();
        QJsonObject * ptr = (QJsonObject*)&jv;
        o->fromJsonObject(*ptr);
        output->append(o);
    }


    // void toJsonArray(QList<void*>* value, QJsonArray* output, QString innerName, QString innerType);
    

    

    worker->deleteLater();

    emit findPetsByStatusSignal(output);
    
}
void
SWGPetApi::findPetsByTags(QList&lt;QString*&gt;* tags) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet/findByTags");

    

    if(fullPath.compare("?") > 0) fullPath.append("?");
    else fullPath.append("&");
    fullPath.append(QUrl::toPercentEncoding("tags"))
        .append("=")
        .append(QUrl::toPercentEncoding(stringValue(tags)));
    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::findPetsByTagsCallback);

    worker->execute(&input);
}

void
SWGPetApi::findPetsByTagsCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: QList<SWGPet*>*

    
    
    QList<SWGPet*>* output = new QList<SWGPet*>();
    QString json(worker->response);
    QByteArray array (json.toStdString().c_str());
    QJsonDocument doc = QJsonDocument::fromJson(array);
    QJsonArray jsonArray = doc.array();

    foreach(QJsonValue obj, jsonArray) {
        SWGPet* o = new SWGPet();
        QJsonObject jv = obj.toObject();
        QJsonObject * ptr = (QJsonObject*)&jv;
        o->fromJsonObject(*ptr);
        output->append(o);
    }


    // void toJsonArray(QList<void*>* value, QJsonArray* output, QString innerName, QString innerType);
    

    

    worker->deleteLater();

    emit findPetsByTagsSignal(output);
    
}
void
SWGPetApi::getPetById(qint64 petId) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet/{petId}");

    
    QString petIdPathParam("{"); petIdPathParam.append("petId").append("}");
    fullPath.replace(petIdPathParam, stringValue(petId));
    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::getPetByIdCallback);

    worker->execute(&input);
}

void
SWGPetApi::getPetByIdCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: SWGPet*

    
    

    
    QString json(worker->response);
    SWGPet* output = new SWGPet(&json);

    
    

    worker->deleteLater();

    emit getPetByIdSignal(output);
    
}
void
SWGPetApi::updatePetWithForm(QString* petId, QString* name, QString* status) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet/{petId}");

    
    QString petIdPathParam("{"); petIdPathParam.append("petId").append("}");
    fullPath.replace(petIdPathParam, stringValue(petId));
    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "POST");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::updatePetWithFormCallback);

    worker->execute(&input);
}

void
SWGPetApi::updatePetWithFormCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: 

    

    worker->deleteLater();

    
    emit updatePetWithFormSignal();
}
void
SWGPetApi::deletePet(QString* api_key, qint64 petId) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet/{petId}");

    
    QString petIdPathParam("{"); petIdPathParam.append("petId").append("}");
    fullPath.replace(petIdPathParam, stringValue(petId));
    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "DELETE");

    

    
    input.headers
    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::deletePetCallback);

    worker->execute(&input);
}

void
SWGPetApi::deletePetCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: 

    

    worker->deleteLater();

    
    emit deletePetSignal();
}
void
SWGPetApi::uploadFile(qint64 petId, QString* additionalMetadata, SWGFile* file) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/pet/{petId}/uploadImage");

    
    QString petIdPathParam("{"); petIdPathParam.append("petId").append("}");
    fullPath.replace(petIdPathParam, stringValue(petId));
    

    

    // qDebug() << fullPath;

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "POST");

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGPetApi::uploadFileCallback);

    worker->execute(&input);
}

void
SWGPetApi::uploadFileCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    // return type: 

    

    worker->deleteLater();

    
    emit uploadFileSignal();
}
} /* namespace Swagger */
