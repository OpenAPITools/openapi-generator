#include <FNet.h>

#include "SamiHelpers.h"
#include "SamiError.h"

using namespace Tizen::Net::Http;
using namespace Tizen::Base;
using namespace Tizen::Base::Collection;
using namespace Tizen::Text;

#ifndef APICLIENT_H_
#define APICLIENT_H_

namespace Swagger {

class SamiApiClient: public IHttpTransactionEventListener {
public:
  SamiApiClient();
  virtual ~SamiApiClient();

  result
  execute(String host, String path, String method, IMap* queryParams, String* body, IMap* headerParams, IMap* formParams, String contentType);

  void success(void (*res) (HttpResponse*, void (*cb)(void*, SamiError*)), void (*cb)(void*, SamiError*));

  virtual void
  OnTransactionCompleted(HttpSession& httpSession, HttpTransaction& pHttpTransaction);

  virtual void
  OnAudioInAudioFocusChanged(void) {
    AppLog("OnAudioInAudioFocusChanged");
  }

  virtual void
  OnTransactionCertVerificationRequiredN(HttpSession& httpSession, HttpTransaction& httpTransaction, 
  Tizen::Base::String* pCert) {
    AppLog("OnTransactionCertVerificationRequiredN");
    httpTransaction.Resume();

    delete pCert;
  }
  
  virtual void
  OnTransactionReadyToWrite(HttpSession& httpSession, HttpTransaction& httpTransaction, int recommendedChunkSize) {
    AppLog("OnTransactionReadyToWrite");
  }

  virtual void
  OnTransactionReadyToRead(HttpSession& httpSession, HttpTransaction& httpTransaction, int availableBodyLen) {
    AppLog("OnTransactionReadyToRead");
  }
  
  virtual void
  OnTransactionAborted(HttpSession& httpSession, HttpTransaction& httpTransaction, result r) {
    AppLog("OnTransactionAborted: %ls", GetErrorMessage(r));
    delete &httpTransaction;
  }
  
  virtual void
  OnTransactionHeaderCompleted(HttpSession& httpSession, HttpTransaction& httpTransaction, int headerLen, bool 
  bAuthRequired) {
    AppLog("OnTransactionHeaderCompleted");
}

private:
  void (*successFunction) (HttpResponse*, void (*success)(void*, SamiError*));
  void (*cb)(void*, SamiError*);
  Utf8Encoding* enc;
};


} /* namespace Swagger */
#endif /* APICLIENT_H_ */
