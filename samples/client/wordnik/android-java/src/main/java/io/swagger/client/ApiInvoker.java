package io.swagger.client;


import org.apache.http.entity.StringEntity;
import org.apache.http.util.EntityUtils;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.security.GeneralSecurityException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ApiInvoker {
    private static ApiInvoker INSTANCE = new ApiInvoker();
    private Map<String, String> defaultHeaderMap = new HashMap<String, String>();

    private HttpClient client = null;

    private boolean ignoreSSLCertificates = false;

    private ClientConnectionManager ignoreSSLConnectionManager;

    public ApiInvoker() {
        initConnectionManager();
    }

    public static ApiInvoker getInstance() {
        return INSTANCE;
    }

    public static Object deserialize(String json, String containerType, Class cls) throws ApiException {
        try {
            if ("List".equals(containerType)) {
                JavaType typeInfo = JsonUtil.getJsonMapper().getTypeFactory().constructCollectionType(List.class, cls);
                return (List<?>) JsonUtil.getJsonMapper().readValue(json, typeInfo);
            } else if (String.class.equals(cls)) {
                if (json != null && json.startsWith("\"") && json.endsWith("\"") && json.length() > 1) {
                    return json.substring(1, json.length() - 2);
                } else {
                    return json;
                }
            } else {
                return JsonUtil.getJsonMapper().readValue(json, cls);
            }
        } catch (IOException e) {
            throw new ApiException(500, e.getMessage());
        }
    }

    public static String serialize(Object obj) throws ApiException {
        try {
            if (obj != null) {
                return JsonUtil.getJsonMapper().writeValueAsString(obj);
            } else {
                return null;
            }
        } catch (Exception e) {
            throw new ApiException(500, e.getMessage());
        }
    }

    public void ignoreSSLCertificates(boolean ignoreSSLCertificates) {
        this.ignoreSSLCertificates = ignoreSSLCertificates;
    }

    public void addDefaultHeader(String key, String value) {
        defaultHeaderMap.put(key, value);
    }

    public String escapeString(String str) {
        return str;
    }

    public String invokeAPI(String host, String path, String method, Map<String, String> queryParams, Object body, Map<String, String> headerParams, String contentType) throws ApiException {
        HttpClient client = getClient(host);

        StringBuilder b = new StringBuilder();
        for (String key : queryParams.keySet()) {
            String value = queryParams.get(key);
            if (value != null) {
                if (b.toString().length() == 0) {
                    b.append("?");
                } else {
                    b.append("&");
                }
                b.append(escapeString(key)).append("=").append(escapeString(value));
            }
        }
        String url = host + path + b.toString();

        HashMap<String, String> headers = new HashMap<String, String>();

        for (String key : headerParams.keySet()) {
            headers.put(key, headerParams.get(key));
        }

        for (String key : defaultHeaderMap.keySet()) {
            if (!headerParams.containsKey(key)) {
                headers.put(key, defaultHeaderMap.get(key));
            }
        }
        headers.put("Accept", "application/json");

        HttpResponse response = null;
        try {
            if ("GET".equals(method)) {
                HttpGet get = new HttpGet(url);
                get.addHeader("Accept", "application/json");
                for (String key : headers.keySet()) {
                    get.setHeader(key, headers.get(key));
                }
                response = client.execute(get);
            } else if ("POST".equals(method)) {
                HttpPost post = new HttpPost(url);

                if (body != null) {
                    post.setHeader("Content-Type", contentType);
                    post.setEntity(new StringEntity(serialize(body), "UTF-8"));
                }
                for (String key : headers.keySet()) {
                    post.setHeader(key, headers.get(key));
                }
                response = client.execute(post);
            } else if ("PUT".equals(method)) {
                HttpPut put = new HttpPut(url);
                if (body != null) {
                    put.setHeader("Content-Type", contentType);
                    put.setEntity(new StringEntity(serialize(body), "UTF-8"));
                }
                for (String key : headers.keySet()) {
                    put.setHeader(key, headers.get(key));
                }
                response = client.execute(put);
            } else if ("DELETE".equals(method)) {
                HttpDelete delete = new HttpDelete(url);
                for (String key : headers.keySet()) {
                    delete.setHeader(key, headers.get(key));
                }
                response = client.execute(delete);
            } else if ("PATCH".equals(method)) {
                HttpPatch patch = new HttpPatch(url);

                if (body != null) {
                    patch.setHeader("Content-Type", contentType);
                    patch.setEntity(new StringEntity(serialize(body), "UTF-8"));
                }
                for (String key : headers.keySet()) {
                    patch.setHeader(key, headers.get(key));
                }
                response = client.execute(patch);
            }

            int code = response.getStatusLine().getStatusCode();
            String responseString = null;
            if (code == 204) {
                responseString = "";
            } else if (code >= 200 && code < 300) {
                if (response.getEntity() != null) {
                    HttpEntity resEntity = response.getEntity();
                    responseString = EntityUtils.toString(resEntity);
                }
            } else {
                if (response.getEntity() != null) {
                    HttpEntity resEntity = response.getEntity();
                    responseString = EntityUtils.toString(resEntity);
                } else {
                    responseString = "no data";
                }
                throw new ApiException(code, responseString);
            }
            return responseString;
        } catch (IOException e) {
            throw new ApiException(500, e.getMessage());
        }
    }

    private HttpClient getClient(String host) {
        if (client == null) {
            if (ignoreSSLCertificates && ignoreSSLConnectionManager != null) {
                // Trust self signed certificates
                client = new DefaultHttpClient(ignoreSSLConnectionManager, new BasicHttpParams());
            } else {
                client = new DefaultHttpClient();
            }
        }
        return client;
    }

    private void initConnectionManager() {
        try {
            final SSLContext sslContext = SSLContext.getInstance("SSL");

            // set up a TrustManager that trusts everything
            TrustManager[] trustManagers = new TrustManager[]{
                    new X509TrustManager() {
                        public X509Certificate[] getAcceptedIssuers() {
                            return null;
                        }

                        public void checkClientTrusted(X509Certificate[] certs, String authType) {
                        }

                        public void checkServerTrusted(X509Certificate[] certs, String authType) {
                        }
                    }};

            sslContext.init(null, trustManagers, new SecureRandom());

            SSLSocketFactory sf = new SSLSocketFactory((KeyStore) null) {
                private javax.net.ssl.SSLSocketFactory sslFactory = sslContext.getSocketFactory();

                public Socket createSocket(Socket socket, String host, int port, boolean autoClose)
                        throws IOException, UnknownHostException {
                    return sslFactory.createSocket(socket, host, port, autoClose);
                }

                public Socket createSocket() throws IOException {
                    return sslFactory.createSocket();
                }
            };

            sf.setHostnameVerifier(SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);
            Scheme httpsScheme = new Scheme("https", sf, 443);
            SchemeRegistry schemeRegistry = new SchemeRegistry();
            schemeRegistry.register(httpsScheme);
            schemeRegistry.register(new Scheme("http", PlainSocketFactory.getSocketFactory(), 80));

            ignoreSSLConnectionManager = new SingleClientConnManager(new BasicHttpParams(), schemeRegistry);
        } catch (NoSuchAlgorithmException e) {
            // This will only be thrown if SSL isn't available for some reason.
        } catch (KeyManagementException e) {
            // This might be thrown when passing a key into init(), but no key is being passed.
        } catch (GeneralSecurityException e) {
            // This catches anything else that might go wrong.
            // If anything goes wrong we default to the standard connection manager.
        }
    }
}
