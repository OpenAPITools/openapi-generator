#include "PFXOauth.h"

namespace OpenAPI {

/*
 * Base class to perform oauth2 flows
 *
 */


void OauthBase::onFinish(QNetworkReply *rep)
{
    //TODO emit error signal when token is wrong
    QJsonDocument document = QJsonDocument::fromJson(rep->readAll());
    QJsonObject rootObj = document.object();
    QString token = rootObj.find("access_token").value().toString();
    QString scope = rootObj.find("scope").value().toString();
    QString type = rootObj.find("token_type").value().toString();
    int expiresIn = rootObj.find("expires_in").value().toInt();
    addToken(oauthToken(token, expiresIn, scope, type));
}

oauthToken OauthBase::getToken(QString scope)
{
    auto tokenIt = m_oauthTokenMap.find(scope);
    return tokenIt != m_oauthTokenMap.end() ? tokenIt.value() : oauthToken();
}

void OauthBase::addToken(oauthToken token)
{
    m_oauthTokenMap.insert(token.getScope(),token);
    emit tokenReceived();

}

void OauthBase::removeToken(QString scope)
{
    m_oauthTokenMap.remove(scope);
}

/*
 * Class to perform the authorization code flow
 *
 */

OauthCode::OauthCode(QObject *parent) : OauthBase(parent){}

void OauthCode::link(){
    connect(&m_server, SIGNAL(dataReceived(QMap<QString,QString>)), this, SLOT(onVerificationReceived(QMap<QString,QString>)));
    connect(this, SIGNAL(authenticationNeeded()), this, SLOT(authenticationNeededCallback()));
    connect(this, SIGNAL(tokenReceived()), &m_server, SLOT(stop()));
}

void OauthCode::unlink()
{
    disconnect(this,0,0,0);
    disconnect(&m_server,0,0,0);
}

void OauthCode::setVariables(QString authUrl, QString tokenUrl, QString scope, QString state, QString redirectUri, QString clientId, QString clientSecret, QString accessType){

    m_authUrl = QUrl(authUrl);
    m_tokenUrl = QUrl(tokenUrl);
    m_scope = scope;
    m_accessType = accessType;
    m_state = state;
    m_redirectUri = redirectUri;
    m_clientId = clientId;
    m_clientSecret = clientSecret;

}

void OauthCode::authenticationNeededCallback()
{
    QDesktopServices::openUrl(QUrl(m_authUrl.toString() + "?scope=" + m_scope + (m_accessType=="" ? "" : "&access_type=" + m_accessType) + "&response_type=code" + "&state=" + m_state + "&redirect_uri=" + m_redirectUri + "&client_id=" + m_clientId));
    m_server.start();
}

void OauthCode::onVerificationReceived(const QMap<QString, QString> response) {

        // Save access code
        QString state(response.value("state"));
        QString scope(response.value("scope"));
        QString code(response.value("code"));

        //create query with the required fields
        QUrlQuery postData;
        postData.addQueryItem("grant_type", "authorization_code");
        postData.addQueryItem("client_id", m_clientId);
        postData.addQueryItem("client_secret", m_clientSecret);
        postData.addQueryItem("code", code);
        postData.addQueryItem("redirect_uri", m_redirectUri);
        QNetworkAccessManager * manager = new QNetworkAccessManager(this);

        QNetworkRequest request(m_tokenUrl);

        request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded");

        connect(manager, SIGNAL(finished(QNetworkReply *)), this, SLOT(onFinish(QNetworkReply *)));

        manager->post(request, postData.query().toUtf8());
}

/*
 * Class to perform the implicit flow
 *
 */

OauthImplicit::OauthImplicit(QObject *parent) : OauthBase(parent){}

void OauthImplicit::link()
{
    //TODO correct linking
    connect(&m_server, SIGNAL(dataReceived(QMap<QString,QString>)), this, SLOT(ImplicitTokenReceived(QMap<QString,QString>)));
    connect(this, SIGNAL(authenticationNeeded()), this, SLOT(authenticationNeededCallback()));
    connect(this, SIGNAL(tokenReceived()), &m_server, SLOT(stop()));
    m_linked = true;
}

void OauthImplicit::unlink()
{
     disconnect(this,0,0,0);
     disconnect(&m_server,0,0,0);
     m_linked = false;
}

void OauthImplicit::setVariables(QString authUrl, QString scope, QString state, QString redirectUri, QString clientId, QString accessType){

    m_authUrl = QUrl(authUrl);
    m_scope = scope;
    m_accessType = accessType;
    m_state = state;
    m_redirectUri = redirectUri;
    m_clientId = clientId;

}

void OauthImplicit::authenticationNeededCallback()
{
     QDesktopServices::openUrl(QUrl(m_authUrl.toString() + "?scope=" + m_scope + (m_accessType=="" ? "" : "&access_type=" + m_accessType) + "&response_type=token" + "&state=" + m_state + "&redirect_uri=" + m_redirectUri + "&client_id=" + m_clientId));
     m_server.start();
}

void OauthImplicit::ImplicitTokenReceived(const QMap<QString, QString> response)
{
    QString token = response.find("access_token").value();
    QString scope = response.find("scope").value();
    QString type = response.find("token_type").value();
    int expiresIn = response.find("expires_in").value().toInt();
    addToken(oauthToken(token, expiresIn, scope, type));
}


/*
 * Class to perform the client credentials flow
 *
 */
OauthCredentials::OauthCredentials(QObject *parent) : OauthBase(parent){}
void OauthCredentials::link()
{
    connect(this, SIGNAL(authenticationNeeded()), this, SLOT(authenticationNeededCallback()));
}

void OauthCredentials::unlink()
{
    disconnect(this,0,0,0);
}

void OauthCredentials::setVariables(QString tokenUrl, QString scope, QString clientId, QString clientSecret){

    m_tokenUrl = QUrl(tokenUrl);
    m_scope = scope;
    m_clientId = clientId;
    m_clientSecret = clientSecret;

}

void OauthCredentials::authenticationNeededCallback()
{
    //create query with the required fields
    QUrlQuery postData;
    postData.addQueryItem("grant_type", "client_credentials");
    postData.addQueryItem("client_id", m_clientId);
    postData.addQueryItem("client_secret", m_clientSecret);
    postData.addQueryItem("scope", m_scope);
    QNetworkAccessManager * manager = new QNetworkAccessManager(this);

    QNetworkRequest request(m_tokenUrl);

    request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded");

    connect(manager, SIGNAL(finished(QNetworkReply *)), this, SLOT(onFinish(QNetworkReply *)));

    manager->post(request, postData.query().toUtf8());
}

/*
 * Class to perform the resource owner password flow
 *
 */
OauthPassword::OauthPassword(QObject *parent) : OauthBase(parent){}
void OauthPassword::link()
{
    connect(this, SIGNAL(authenticationNeeded()), this, SLOT(authenticationNeededCallback()));
}

void OauthPassword::unlink()
{
    disconnect(this,0,0,0);
}

void OauthPassword::setVariables(QString tokenUrl, QString scope, QString clientId, QString clientSecret, QString username, QString password){

    m_tokenUrl = QUrl(tokenUrl);
    m_scope = scope;
    m_clientId = clientId;
    m_clientSecret = clientSecret;
    m_username = username;
    m_password = password;

}
void OauthPassword::authenticationNeededCallback()
{
    //create query with the required fields
    QUrlQuery postData;
    postData.addQueryItem("grant_type", "password");
    postData.addQueryItem("username", m_username);
    postData.addQueryItem("password", m_password);
    postData.addQueryItem("client_id", m_clientId);
    postData.addQueryItem("client_secret", m_clientSecret);
    postData.addQueryItem("scope", m_scope);
    QNetworkAccessManager * manager = new QNetworkAccessManager(this);

    QNetworkRequest request(m_tokenUrl);

    request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded");

    connect(manager, SIGNAL(finished(QNetworkReply *)), this, SLOT(onFinish(QNetworkReply *)));

    manager->post(request, postData.query().toUtf8());
}


/*
 * Class that provides a simple reply server
 *
 */

ReplyServer::ReplyServer(QObject *parent) : QTcpServer(parent)
{
      connect(this, SIGNAL(newConnection()), this, SLOT(onConnected()));
      m_reply ="you can close this window now!";
}

void ReplyServer::start()
{
    if(!listen(QHostAddress::Any, 9999))
    {
        qDebug() << "Server could not start";
    }
    else
    {
        qDebug() << "Server started!";
    }
}

void ReplyServer::stop()
{
    qDebug() << "Stopping the Server...";
    QTcpServer::close();
}

void ReplyServer::onConnected()
{
    // need to grab the socket
    QTcpSocket *socket = nextPendingConnection();
    connect(socket, SIGNAL(readyRead()), this, SLOT(read()), Qt::UniqueConnection);
    connect(socket, SIGNAL(disconnected()), socket, SLOT(deleteLater()));

}

void ReplyServer::read()
{
    QTcpSocket *socket = qobject_cast<QTcpSocket *>(sender());
    if (!socket) {
        qDebug() << "No socket available";
        return;
    }
    qDebug() << "Socket connected";

    QTextStream os(socket);
                     os.setAutoDetectUnicode(true);
                     os << "HTTP/1.0 200 Ok\r\n"
                         "Content-Type: text/html; charset=\"utf-8\"\r\n"
                         "\r\n"
                        <<"<!DOCTYPE html>\
                        <html>\
                        <head>\
                        <script>\
                        window.onload = function hashFunction() {\
                             var query = location.hash.substr(1);\
                             if (query != \"\") {\
                                 var xhttp = new XMLHttpRequest();\
                                 xhttp.open(\"GET\", \"/?\" + query, true);\
                                 xhttp.send();\
                             }\
                        }\
                        </script>\
                        </head>\
                        <body>\
                        <h2>You can close this window now!</h2>\
                        </body>\
                        </html>";

    QByteArray data = socket->readLine();
    QString splitGetLine = QString(data);
    splitGetLine.remove("GET");
    splitGetLine.remove("HTTP/1.1");
    splitGetLine.remove("\r\n");
    splitGetLine.remove(" ");
    //prefix is needed to extract query params
    QUrl getTokenUrl("http://" + splitGetLine);
    QList< QPair<QString, QString> > tokens;

    QUrlQuery query(getTokenUrl);
    tokens = query.queryItems();


    QMap<QString, QString> queryParams;
    QPair<QString, QString> tokenPair;
    foreach (tokenPair, tokens) {
        QString key = QUrl::fromPercentEncoding(QByteArray().append(tokenPair.first.trimmed().toLatin1()));
        QString value = QUrl::fromPercentEncoding(QByteArray().append(tokenPair.second.trimmed().toLatin1()));
        queryParams.insert(key, value);
    }
    if (!queryParams.contains("state")) {

        socket->close();
        return;
    }
    socket->close();

    emit dataReceived(queryParams);
}


}