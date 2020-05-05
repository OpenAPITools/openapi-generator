package org.openapitools.codegen;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.models.security.OAuthFlow;
import io.swagger.v3.oas.models.security.Scopes;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

public class OpenIdConnect {

    private static final ObjectMapper mapper = new ObjectMapper();

    private final DefaultCodegen codegen;

    private final URL discovery;

    private JsonNode rootNode;

    // https://stackoverflow.com/questions/28658735/what-are-keycloaks-oauth2-openid-connect-endpoints
    private Set<String> grantTypes = new HashSet<>();
    private Set<String> responseTypes = new HashSet<>();
    private Set<String> claims = new HashSet<>();
    private Set<String> scopes = new HashSet<>();

    private String authorizationUrl;
    private String tokenUrl;

    public OpenIdConnect(DefaultCodegen codegen, String url) throws MalformedURLException {
        this.codegen = codegen;
        this.discovery = new URL(url);
    }

    public OpenIdConnect retrieve() throws IOException {
        if (rootNode == null) {
            rootNode = mapper.readTree(discovery);
            authorizationUrl = rootNode.get("authorization_endpoint").asText();
            tokenUrl = rootNode.get("token_endpoint").asText();
            JsonNode grantArray = rootNode.get("grant_types_supported");
            if (grantArray.isArray()) {
                for (JsonNode el: grantArray) {
                    grantTypes.add(el.asText());
                }
            }
            JsonNode responseArray = rootNode.get("response_types_supported");
            if (responseArray.isArray()) {
                for (JsonNode el: responseArray) {
                    responseTypes.add(el.asText());
                }
            }
            JsonNode claimArray = rootNode.get("claims_supported");
            if (claimArray.isArray()) {
                for (JsonNode el: claimArray) {
                    claims.add(el.asText());
                }
            }
            JsonNode scopeArray = rootNode.get("scopes_supported");
            if (scopeArray.isArray()) {
                for (JsonNode el: scopeArray) {
                    scopes.add(el.asText());
                }
            }
        }
        return this;
    }

    public List<CodegenSecurity> getCodegenSecurities(CodegenSecurity toCopy) {
        CodegenSecurity cs = CodegenSecurity.copy(toCopy);
        // cs.name = "openIdConnect";
        cs.isKeyInHeader = cs.isKeyInQuery = cs.isKeyInCookie = cs.isApiKey = cs.isBasic = false;
        cs.isOpenIdConnect = true;
        cs.isOAuth = true;
        cs.authorizationUrl = authorizationUrl;
        cs.tokenUrl = tokenUrl;
        cs.scopes = new ArrayList<>();

        Scopes flowScopes = new Scopes();
        for (String s: scopes) {
            // we have no description
            flowScopes.addString(s, s);
        }
        OAuthFlow flow = new OAuthFlow();
        flow = flow.authorizationUrl(authorizationUrl).tokenUrl(tokenUrl).scopes(flowScopes);
        cs.scopes.add(new HashMap<>(flowScopes));

        if (grantTypes.isEmpty()) {
            throw new RuntimeException("missing oauth flow in " + cs.name);
        }

        List<CodegenSecurity> result = new ArrayList<>();
        // Can be all of this at the same time!
        if (grantTypes.contains("password")) {
            CodegenSecurity csPassword = CodegenSecurity.copy(cs);
            codegen.setOauth2Info(csPassword, flow);
            csPassword.isPassword = true;
            csPassword.isImplicit = false;
            csPassword.isApplication = false;
            csPassword.isCode = false;
            csPassword.flow = "password";
            result.add(csPassword);
        }
        if (grantTypes.contains("implicit")) {
            CodegenSecurity csImplicit = CodegenSecurity.copy(cs);
            codegen.setOauth2Info(csImplicit, flow);
            csImplicit.isPassword = false;
            csImplicit.isImplicit = true;
            csImplicit.isApplication = false;
            csImplicit.isCode = false;
            csImplicit.flow = "implicit";
            result.add(csImplicit);
        }
        if (grantTypes.contains("client_credentials")) {
            CodegenSecurity csClient = CodegenSecurity.copy(cs);
            codegen.setOauth2Info(csClient, flow);
            csClient.isPassword = false;
            csClient.isImplicit = false;
            csClient.isApplication = true;
            csClient.isCode = false;
            csClient.flow = "application";
            result.add(csClient);
        }
        if (grantTypes.contains("authorization_code")) {
            CodegenSecurity csCode = CodegenSecurity.copy(cs);
            codegen.setOauth2Info(csCode, flow);
            csCode.isPassword = false;
            csCode.isImplicit = false;
            csCode.isApplication = false;
            csCode.isCode = true;
            csCode.flow = "accessCode";
            result.add(csCode);
        }
        if (result.isEmpty()) {
            throw new RuntimeException("Could not identify any openIdConnect flow in " + cs.name);
        }
        return result;
    }

}
