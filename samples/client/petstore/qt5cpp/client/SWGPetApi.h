#ifndef _SWG_SWGPetApi_H_
#define _SWG_SWGPetApi_H_

#include "SWGHttpRequest.h"

#include "SWGPet.h"
#include <QString>
#include "SWGHttpRequest.h"

#include <QObject>

namespace Swagger {

class SWGPetApi: public QObject {
    Q_OBJECT

public:
    SWGPetApi();
    SWGPetApi(QString host, QString basePath);
    ~SWGPetApi();

    QString host;
    QString basePath;

    void updatePet(SWGPet body);
    void addPet(SWGPet body);
    void findPetsByStatus(QList<QString*>* status);
    void findPetsByTags(QList<QString*>* tags);
    void getPetById(qint64 petId);
    void updatePetWithForm(QString* petId, QString* name, QString* status);
    void deletePet(qint64 petId, QString* apiKey);
    void uploadFile(qint64 petId, QString* additionalMetadata, SWGHttpRequestInputFileElement* file);
    void getPetByIdWithByteArray(qint64 petId);
    void addPetUsingByteArray(QString* body);
    
private:
    void updatePetCallback (HttpRequestWorker * worker);
    void addPetCallback (HttpRequestWorker * worker);
    void findPetsByStatusCallback (HttpRequestWorker * worker);
    void findPetsByTagsCallback (HttpRequestWorker * worker);
    void getPetByIdCallback (HttpRequestWorker * worker);
    void updatePetWithFormCallback (HttpRequestWorker * worker);
    void deletePetCallback (HttpRequestWorker * worker);
    void uploadFileCallback (HttpRequestWorker * worker);
    void getPetByIdWithByteArrayCallback (HttpRequestWorker * worker);
    void addPetUsingByteArrayCallback (HttpRequestWorker * worker);
    
signals:
    void updatePetSignal();
    void addPetSignal();
    void findPetsByStatusSignal(QList<SWGPet*>* summary);
    void findPetsByTagsSignal(QList<SWGPet*>* summary);
    void getPetByIdSignal(SWGPet* summary);
    void updatePetWithFormSignal();
    void deletePetSignal();
    void uploadFileSignal();
    void getPetByIdWithByteArraySignal(QString* summary);
    void addPetUsingByteArraySignal();
    
};
}
#endif