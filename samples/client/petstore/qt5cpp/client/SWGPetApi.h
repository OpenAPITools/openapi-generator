#ifndef _SWG_SWGPetApi_H_
#define _SWG_SWGPetApi_H_

#include "SWGHttpRequest.h"

#include "SWGPet.h"
#include <QString>
#include "SWGApiResponse.h"
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

    void addPet(SWGPet body);
    void deletePet(qint64 petId, QString* apiKey);
    void findPetsByStatus(QList<QString*>* status);
    void findPetsByTags(QList<QString*>* tags);
    void getPetById(qint64 petId);
    void updatePet(SWGPet body);
    void updatePetWithForm(qint64 petId, QString* name, QString* status);
    void uploadFile(qint64 petId, QString* additionalMetadata, SWGHttpRequestInputFileElement* file);
    
private:
    void addPetCallback (HttpRequestWorker * worker);
    void deletePetCallback (HttpRequestWorker * worker);
    void findPetsByStatusCallback (HttpRequestWorker * worker);
    void findPetsByTagsCallback (HttpRequestWorker * worker);
    void getPetByIdCallback (HttpRequestWorker * worker);
    void updatePetCallback (HttpRequestWorker * worker);
    void updatePetWithFormCallback (HttpRequestWorker * worker);
    void uploadFileCallback (HttpRequestWorker * worker);
    
signals:
    void addPetSignal();
    void deletePetSignal();
    void findPetsByStatusSignal(QList<SWGPet*>* summary);
    void findPetsByTagsSignal(QList<SWGPet*>* summary);
    void getPetByIdSignal(SWGPet* summary);
    void updatePetSignal();
    void updatePetWithFormSignal();
    void uploadFileSignal(SWGApiResponse* summary);
    
};
}
#endif