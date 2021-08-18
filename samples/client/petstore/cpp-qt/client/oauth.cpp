#include "oauth.h"

namespace OpenAPI {

OauthCode::OauthCode(QObject *parent) : OauthBase(parent)
{
    connect(&m_server, SIGNAL(dataReceived(QMap<QString,QString>)), this, SLOT(onVerificationReceived(QMap<QString,QString>)));
    connect(this, SIGNAL(authenticationNeeded()), this, SLOT(authenticationNeededCallback()));
    connect(this, SIGNAL(tokenReceived()), &m_server, SLOT(stop()));

}

void OauthCode::setVariables(QString authUrl, QString tokenUrl, QString scope, QString accessType, QString state, QString redirectUri, QString clientId, QString clientSecret ){

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
    QDesktopServices::openUrl(QUrl(m_authUrl.toString() + "?scope=" + m_scope + "&access_type=" + m_accessType + "&response_type=code" + "&state=" + m_state + "&redirect_uri=" + m_redirectUri + "&client_id=" + m_clientId));
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

void OauthCode::onFinish(QNetworkReply *rep)
{
    //TODO emit error signal when token is wrong
    QJsonDocument document = QJsonDocument::fromJson(rep->readAll());
    QJsonObject rootObj = document.object();
    QString token = rootObj.find("access_token").value().toString();
    QString scope = rootObj.find("scope").value().toString();
    QString type = rootObj.find("token_type").value().toString();
    int expiresIn = rootObj.find("expires_in").value().toInt();

    this->m_oauthTokenMap.insert(scope,oauthToken(token, expiresIn, scope, type));
    emit tokenReceived();
}

oauthToken OauthBase::getToken(QString scope)
{
    auto tokenIt = m_oauthTokenMap.find(scope);
    return tokenIt != m_oauthTokenMap.end() ? tokenIt.value() : oauthToken();
}

void OauthBase::removeToken(QString scope)
{
    m_oauthTokenMap.remove(scope);
}

ReplyServer::ReplyServer(QObject *parent) : QTcpServer(parent)
{
      connect(this, SIGNAL(newConnection()), this, SLOT(onConnected()));
      m_reply = "You can close this window now!";

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

    socket->write(m_reply);

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
        socket->write(m_reply);
        socket->close();
        return;
    }
    socket->close();

    emit dataReceived(queryParams);
}
}
