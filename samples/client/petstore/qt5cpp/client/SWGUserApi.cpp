#include "SWGUserApi.h"
#include "SWGHelpers.h"
#include "SWGModelFactory.h"

#include <QJsonArray>
#include <QJsonDocument>

namespace Swagger {
SWGUserApi::SWGUserApi() {}

SWGUserApi::~SWGUserApi() {}

SWGUserApi::SWGUserApi(QString host, QString basePath) {
    this->host = host;
    this->basePath = basePath;
}

void
SWGUserApi::createUser(SWGUser body) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user");

    

    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "POST");

    

    
    
    
    QString output = body.asJson();
    input.request_body.append(output);
    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::createUserCallback);

    worker->execute(&input);
}

void
SWGUserApi::createUserCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    worker->deleteLater();

    
    emit createUserSignal();
}
void
SWGUserApi::createUsersWithArrayInput(QList<SWGUser*>* body) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user/createWithArray");

    

    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "POST");

    

    
    
    QJsonArray* bodyArray = new QJsonArray();
    toJsonArray((QList<void*>*)body, bodyArray, QString("body"), QString("SWGUser*"));

    QJsonDocument doc(*bodyArray);
    QByteArray bytes = doc.toJson();

    input.request_body.append(bytes);
    
    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::createUsersWithArrayInputCallback);

    worker->execute(&input);
}

void
SWGUserApi::createUsersWithArrayInputCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    worker->deleteLater();

    
    emit createUsersWithArrayInputSignal();
}
void
SWGUserApi::createUsersWithListInput(QList<SWGUser*>* body) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user/createWithList");

    

    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "POST");

    

    
    
    QJsonArray* bodyArray = new QJsonArray();
    toJsonArray((QList<void*>*)body, bodyArray, QString("body"), QString("SWGUser*"));

    QJsonDocument doc(*bodyArray);
    QByteArray bytes = doc.toJson();

    input.request_body.append(bytes);
    
    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::createUsersWithListInputCallback);

    worker->execute(&input);
}

void
SWGUserApi::createUsersWithListInputCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    worker->deleteLater();

    
    emit createUsersWithListInputSignal();
}
void
SWGUserApi::loginUser(QString* username, QString* password) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user/login");

    

    
    
    if (fullPath.indexOf("?") > 0) 
      fullPath.append("&");
    else 
      fullPath.append("?");
    fullPath.append(QUrl::toPercentEncoding("username"))
        .append("=")
        .append(QUrl::toPercentEncoding(stringValue(username)));
    

    
    
    
    if (fullPath.indexOf("?") > 0) 
      fullPath.append("&");
    else 
      fullPath.append("?");
    fullPath.append(QUrl::toPercentEncoding("password"))
        .append("=")
        .append(QUrl::toPercentEncoding(stringValue(password)));
    

    
    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::loginUserCallback);

    worker->execute(&input);
}

void
SWGUserApi::loginUserCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    
    
    
    QString json(worker->response);
    QString* output = static_cast<QString*>(create(json, QString("QString")));
    
    
    

    worker->deleteLater();

    emit loginUserSignal(output);
    
}
void
SWGUserApi::logoutUser() {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user/logout");

    

    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::logoutUserCallback);

    worker->execute(&input);
}

void
SWGUserApi::logoutUserCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    worker->deleteLater();

    
    emit logoutUserSignal();
}
void
SWGUserApi::getUserByName(QString* username) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user/{username}");

    
    QString usernamePathParam("{"); usernamePathParam.append("username").append("}");
    fullPath.replace(usernamePathParam, stringValue(username));
    

    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "GET");

    

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::getUserByNameCallback);

    worker->execute(&input);
}

void
SWGUserApi::getUserByNameCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    
    
    
    QString json(worker->response);
    SWGUser* output = static_cast<SWGUser*>(create(json, QString("SWGUser")));
    
    
    

    worker->deleteLater();

    emit getUserByNameSignal(output);
    
}
void
SWGUserApi::updateUser(QString* username, SWGUser body) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user/{username}");

    
    QString usernamePathParam("{"); usernamePathParam.append("username").append("}");
    fullPath.replace(usernamePathParam, stringValue(username));
    

    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "PUT");

    

    
    
    
    QString output = body.asJson();
    input.request_body.append(output);
    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::updateUserCallback);

    worker->execute(&input);
}

void
SWGUserApi::updateUserCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    worker->deleteLater();

    
    emit updateUserSignal();
}
void
SWGUserApi::deleteUser(QString* username) {
    QString fullPath;
    fullPath.append(this->host).append(this->basePath).append("/user/{username}");

    
    QString usernamePathParam("{"); usernamePathParam.append("username").append("}");
    fullPath.replace(usernamePathParam, stringValue(username));
    

    

    HttpRequestWorker *worker = new HttpRequestWorker();
    HttpRequestInput input(fullPath, "DELETE");

    

    

    

    connect(worker,
            &HttpRequestWorker::on_execution_finished,
            this,
            &SWGUserApi::deleteUserCallback);

    worker->execute(&input);
}

void
SWGUserApi::deleteUserCallback(HttpRequestWorker * worker) {
    QString msg;
    if (worker->error_type == QNetworkReply::NoError) {
        msg = QString("Success! %1 bytes").arg(worker->response.length());
    }
    else {
        msg = "Error: " + worker->error_str;
    }

    

    worker->deleteLater();

    
    emit deleteUserSignal();
}
} /* namespace Swagger */
