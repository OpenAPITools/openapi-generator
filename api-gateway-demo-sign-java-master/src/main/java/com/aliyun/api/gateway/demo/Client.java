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
package com.aliyun.api.gateway.demo;

import com.aliyun.api.gateway.demo.util.HttpUtil;

/**
 * Client
 */
public class Client {
    /**
     * 发送请求
     *
     * @param request request对象
     * @return Response
     * @throws Exception
     */
    public static Response execute(Request request) throws Exception {
        switch (request.getMethod()) {
            case GET:
                return HttpUtil.httpGet(request.getHost(), request.getPath(), 
                		request.getTimeout(), 
                		request.getHeaders(), 
                		request.getQuerys(),
                		request.getSignHeaderPrefixList(), 
                		request.getAppKey(), request.getAppSecret());
            case POST_FORM:
                return HttpUtil.httpPost(request.getHost(), request.getPath(), 
                		request.getTimeout(), 
                		request.getHeaders(), 
                		request.getQuerys(),
                		request.getBodys(),
                		request.getSignHeaderPrefixList(), 
                		request.getAppKey(), request.getAppSecret());
            case POST_STRING:
                return HttpUtil.httpPost(request.getHost(), request.getPath(), 
                		request.getTimeout(), 
                		request.getHeaders(), 
                		request.getQuerys(),
                		request.getStringBody(),
                		request.getSignHeaderPrefixList(), 
                		request.getAppKey(), request.getAppSecret());
            case POST_BYTES:
                return HttpUtil.httpPost(request.getHost(), request.getPath(), 
                		request.getTimeout(), 
                		request.getHeaders(), 
                		request.getQuerys(),
                		request.getBytesBody(),
                		request.getSignHeaderPrefixList(), 
                		request.getAppKey(), request.getAppSecret());
            case PUT_STRING:
                return HttpUtil.httpPut(request.getHost(), request.getPath(), 
                		request.getTimeout(), 
                		request.getHeaders(), 
                		request.getQuerys(),
                		request.getStringBody(),
                		request.getSignHeaderPrefixList(), 
                		request.getAppKey(), request.getAppSecret());
            case PUT_BYTES:
                return HttpUtil.httpPut(request.getHost(), request.getPath(), 
                		request.getTimeout(), 
                		request.getHeaders(), 
                		request.getQuerys(),
                		request.getBytesBody(),
                		request.getSignHeaderPrefixList(), 
                		request.getAppKey(), request.getAppSecret());
            case DELETE:
                return HttpUtil.httpDelete(request.getHost(), request.getPath(), 
                		request.getTimeout(), 
                		request.getHeaders(), 
                		request.getQuerys(),
                		request.getSignHeaderPrefixList(), 
                		request.getAppKey(), request.getAppSecret());
            default:
                throw new IllegalArgumentException(String.format("unsupported method:%s", request.getMethod()));
        }
    }
}
