/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.aliyun.api.gateway.demo.util;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.StringUtils;

import com.aliyun.api.gateway.demo.constant.Constants;
import com.aliyun.api.gateway.demo.constant.HttpHeader;
import com.aliyun.api.gateway.demo.constant.SystemHeader;

/**
 * 签名工具
 */
public class SignUtil {

    /**
     * 计算签名
     *
     * @param secret APP密钥
     * @param method HttpMethod
     * @param path
     * @param headers
     * @param querys
     * @param bodys
     * @param signHeaderPrefixList 自定义参与签名Header前缀
     * @return 签名后的字符串
     */
    public static String sign(String secret, String method, String path, 
    							Map<String, String> headers, 
    							Map<String, String> querys, 
    							Map<String, String> bodys, 
    							List<String> signHeaderPrefixList) {
        try {
            Mac hmacSha256 = Mac.getInstance(Constants.HMAC_SHA256);
            byte[] keyBytes = secret.getBytes(Constants.ENCODING);
            hmacSha256.init(new SecretKeySpec(keyBytes, 0, keyBytes.length, Constants.HMAC_SHA256));

            return new String(Base64.encodeBase64(
                    hmacSha256.doFinal(buildStringToSign(method, path, headers, querys, bodys, signHeaderPrefixList)
                            .getBytes(Constants.ENCODING))),
                    Constants.ENCODING);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 构建待签名字符串
     * @param method
     * @param path
     * @param headers
     * @param querys
     * @param bodys
     * @param signHeaderPrefixList
     * @return
     */
    private static String buildStringToSign(String method, String path, 
    										Map<String, String> headers, 
    										Map<String, String> querys,
    										Map<String, String> bodys,
                                            List<String> signHeaderPrefixList) {
        StringBuilder sb = new StringBuilder();

        sb.append(method.toUpperCase()).append(Constants.LF);
        if (null != headers) {
        	if (null != headers.get(HttpHeader.HTTP_HEADER_ACCEPT)) {
                sb.append(headers.get(HttpHeader.HTTP_HEADER_ACCEPT));
            }
        	sb.append(Constants.LF);
        	if (null != headers.get(HttpHeader.HTTP_HEADER_CONTENT_MD5)) {
                sb.append(headers.get(HttpHeader.HTTP_HEADER_CONTENT_MD5));
            }
            sb.append(Constants.LF);
            if (null != headers.get(HttpHeader.HTTP_HEADER_CONTENT_TYPE)) {
                sb.append(headers.get(HttpHeader.HTTP_HEADER_CONTENT_TYPE));
            }
            sb.append(Constants.LF);
            if (null != headers.get(HttpHeader.HTTP_HEADER_DATE)) {
                sb.append(headers.get(HttpHeader.HTTP_HEADER_DATE));
            }
        }
        sb.append(Constants.LF);
        sb.append(buildHeaders(headers, signHeaderPrefixList));
        sb.append(buildResource(path, querys, bodys));
        
        return sb.toString();
    }

    /**
     * 构建待签名Path+Query+BODY
     *
     * @param path
     * @param querys
     * @param bodys
     * @return 待签名
     */
    private static String buildResource(String path, Map<String, String> querys, Map<String, String> bodys) {
    	StringBuilder sb = new StringBuilder();
    	
    	if (!StringUtils.isBlank(path)) {
    		sb.append(path);
        }
        Map<String, String> sortMap = new TreeMap<String, String>();
        if (null != querys) {
        	for (Map.Entry<String, String> query : querys.entrySet()) {
        		if (!StringUtils.isBlank(query.getKey())) {
        			sortMap.put(query.getKey(), query.getValue());
                }
        	}
        }
        
        if (null != bodys) {
        	for (Map.Entry<String, String> body : bodys.entrySet()) {
        		if (!StringUtils.isBlank(body.getKey())) {
        			sortMap.put(body.getKey(), body.getValue());
                }
        	}
        }
        
        StringBuilder sbParam = new StringBuilder();
        for (Map.Entry<String, String> item : sortMap.entrySet()) {
    		if (!StringUtils.isBlank(item.getKey())) {
    			if (0 < sbParam.length()) {
    				sbParam.append(Constants.SPE3);
    			}
    			sbParam.append(item.getKey());
    			if (!StringUtils.isBlank(item.getValue())) {
    				sbParam.append(Constants.SPE4).append(item.getValue());
    			}
            }
    	}
        if (0 < sbParam.length()) {
        	sb.append(Constants.SPE5);
        	sb.append(sbParam);
        }
        
        return sb.toString();
    }

    /**
     * 构建待签名Http头
     *
     * @param headers 请求中所有的Http头
     * @param signHeaderPrefixList 自定义参与签名Header前缀
     * @return 待签名Http头
     */
    private static String buildHeaders(Map<String, String> headers, List<String> signHeaderPrefixList) {
    	StringBuilder sb = new StringBuilder();
    	
    	if (null != signHeaderPrefixList) {
    		signHeaderPrefixList.remove(SystemHeader.X_CA_SIGNATURE);
    		signHeaderPrefixList.remove(HttpHeader.HTTP_HEADER_ACCEPT);
    		signHeaderPrefixList.remove(HttpHeader.HTTP_HEADER_CONTENT_MD5);
    		signHeaderPrefixList.remove(HttpHeader.HTTP_HEADER_CONTENT_TYPE);
    		signHeaderPrefixList.remove(HttpHeader.HTTP_HEADER_DATE);
    		Collections.sort(signHeaderPrefixList);
    		if (null != headers) {
    			Map<String, String> sortMap = new TreeMap<String, String>();
    			sortMap.putAll(headers);
    			StringBuilder signHeadersStringBuilder = new StringBuilder();
    			for (Map.Entry<String, String> header : sortMap.entrySet()) {
                    if (isHeaderToSign(header.getKey(), signHeaderPrefixList)) {
                    	sb.append(header.getKey());
                    	sb.append(Constants.SPE2);
                        if (!StringUtils.isBlank(header.getValue())) {
                        	sb.append(header.getValue());
                        }
                        sb.append(Constants.LF);
                        if (0 < signHeadersStringBuilder.length()) {
                        	signHeadersStringBuilder.append(Constants.SPE1);
                        }
                        signHeadersStringBuilder.append(header.getKey());
                    }
                }
    			headers.put(SystemHeader.X_CA_SIGNATURE_HEADERS, signHeadersStringBuilder.toString());
    		}
    	}
        
        return sb.toString();
    }

    /**
     * Http头是否参与签名 return
     */
    private static boolean isHeaderToSign(String headerName, List<String> signHeaderPrefixList) {
        if (StringUtils.isBlank(headerName)) {
            return false;
        }

        if (headerName.startsWith(Constants.CA_HEADER_TO_SIGN_PREFIX_SYSTEM)) {
            return true;
        }

        if (null != signHeaderPrefixList) {
            for (String signHeaderPrefix : signHeaderPrefixList) {
                if (headerName.equalsIgnoreCase(signHeaderPrefix)) {
                    return true;
                }
            }
        }

        return false;
    }
}
