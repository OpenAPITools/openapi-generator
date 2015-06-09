#ifndef _SWG_SWGUserApi_H_
#define _SWG_SWGUserApi_H_

#include "SWGHttpRequest.h"

#include "SWGUser.h"
#include <QList>
#include <QString>

#include <QObject>

namespace Swagger {

class SWGUserApi: public QObject {
    Q_OBJECT

public:
    SWGUserApi();
    SWGUserApi(QString host, QString basePath);
    ~SWGUserApi();

    QString host;
    QString basePath;

    void createUser(SWGUser body);
    void createUsersWithArrayInput(QList<SWGUser*>* body);
    void createUsersWithListInput(QList<SWGUser*>* body);
    void loginUser(QString* username, QString* password);
    void logoutUser();
    void getUserByName(QString* username);
    void updateUser(QString* username, SWGUser body);
    void deleteUser(QString* username);
    
private:
    void createUserCallback (HttpRequestWorker * worker);
    void createUsersWithArrayInputCallback (HttpRequestWorker * worker);
    void createUsersWithListInputCallback (HttpRequestWorker * worker);
    void loginUserCallback (HttpRequestWorker * worker);
    void logoutUserCallback (HttpRequestWorker * worker);
    void getUserByNameCallback (HttpRequestWorker * worker);
    void updateUserCallback (HttpRequestWorker * worker);
    void deleteUserCallback (HttpRequestWorker * worker);
    
signals:
    void createUserSignal();
    void createUsersWithArrayInputSignal();
    void createUsersWithListInputSignal();
    void loginUserSignal(QString* summary);
    void logoutUserSignal();
    void getUserByNameSignal(SWGUser* summary);
    void updateUserSignal();
    void deleteUserSignal();
    
};
}
#endif