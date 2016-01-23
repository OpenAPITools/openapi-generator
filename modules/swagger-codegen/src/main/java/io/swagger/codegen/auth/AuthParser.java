package io.swagger.codegen.auth;

import io.swagger.models.auth.AuthorizationValue;

import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

public class AuthParser {

    public static List<AuthorizationValue> parse(String urlEncodedAuthStr) {
        List<AuthorizationValue> auths = new ArrayList<AuthorizationValue>();
        if (isNotEmpty(urlEncodedAuthStr)) {
            String[] parts = urlEncodedAuthStr.split(",");
            for (String part : parts) {
                String[] kvPair = part.split(":");
                if (kvPair.length == 2) {
                    auths.add(new AuthorizationValue(URLDecoder.decode(kvPair[0]), URLDecoder.decode(kvPair[1]), "header")); // FIXME replace the deprecated method by decode(string, encoding). Which encoding is used ? Default UTF-8 ?
                }
            }
        }
        return auths;
    }

    public static String reconstruct(List<AuthorizationValue> authorizationValueList) {
        if (authorizationValueList != null) {
            StringBuilder b = new StringBuilder();
            for (AuthorizationValue v : authorizationValueList) {
                try {
                    if (b.toString().length() > 0) {
                        b.append(",");
                    }
                    b.append(URLEncoder.encode(v.getKeyName(), "UTF-8"))
                            .append(":")
                            .append(URLEncoder.encode(v.getValue(), "UTF-8"));
                } catch (Exception e) {
                    // continue
                    e.printStackTrace();
                }
            }
            return b.toString();
        } else {
            return null;
        }
    }

}
