package org.openapitools.client.auth;

import java.util.Optional;
import java.util.function.Supplier;
import org.springframework.http.HttpHeaders;
import org.springframework.util.MultiValueMap;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class HttpBearerAuth implements Authentication {
    private final String scheme;
    private Supplier<String> tokenSupplier;

    public HttpBearerAuth(String scheme) {
        this.scheme = scheme;
    }

    /**
     * Gets the token, which together with the scheme, will be sent as the value of the Authorization header.
     *
     * @return The bearer token
     */
    public String getBearerToken() {
        return tokenSupplier.get();
    }

    /**
     * Sets the token, which together with the scheme, will be sent as the value of the Authorization header.
     *
     * @param bearerToken The bearer token to send in the Authorization header
     */
    public void setBearerToken(String bearerToken) {
        this.tokenSupplier = () -> bearerToken;
    }

    /**
     * Sets the supplier of tokens, which together with the scheme, will be sent as the value of the Authorization header.
     *
     * @param tokenSupplier The supplier of bearer tokens to send in the Authorization header
     */
    public void setBearerToken(Supplier<String> tokenSupplier) {
        this.tokenSupplier = tokenSupplier;
    }

    @Override
    public void applyToParams(MultiValueMap<String, String> queryParams, HttpHeaders headerParams, MultiValueMap<String, String> cookieParams) {
        String bearerToken = Optional.ofNullable(tokenSupplier).map(Supplier::get).orElse(null);
        if (bearerToken == null) {
            return;
        }
        headerParams.add(HttpHeaders.AUTHORIZATION, (scheme != null ? upperCaseBearer(scheme) + " " : "") + bearerToken);
    }

    private static String upperCaseBearer(String scheme) {
        return ("bearer".equalsIgnoreCase(scheme)) ? "Bearer" : scheme;
    }
}
