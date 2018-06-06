/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.virtualan.cache;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import javax.annotation.PostConstruct;

import org.openapitools.virtualan.VirtualServiceUtil;
import org.openapitools.virtualan.dao.VirtualServiceRepository;
import org.openapitools.virtualan.entity.VirtualServiceEntity;
import org.openapitools.virtualan.model.Cache;
import org.openapitools.virtualan.model.VirtualServiceRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;;

@Service("virtualCache")
public class VirtualCacheUtil implements Cache<Long, VirtualServiceRequest> {

	@Autowired
	private VirtualServiceRepository mockRepository;
	
	
	@Autowired
	private VirtualServiceUtil virtualServiceUtil;

	private Map<Long, VirtualServiceRequest> loadInitial() {
		Iterable<VirtualServiceEntity> mockEntityList = mockRepository.findAll();
		final Map<Long, VirtualServiceRequest> map = new HashMap<Long, VirtualServiceRequest>();
		for (VirtualServiceEntity mockEntity : mockEntityList) {
			VirtualServiceRequest mockTransferObject = virtualServiceUtil.converterEToR(mockEntity);
			mockCache.put(mockTransferObject.getId(), mockTransferObject);
		}
		return map;
	}

	private LoadingCache<Long, VirtualServiceRequest> mockCache;
	
	@PostConstruct
	public void load(){
		
		CacheLoader<Long, VirtualServiceRequest> cacheLoader =  new CacheLoader<Long, VirtualServiceRequest>() {
			@Override
			public VirtualServiceRequest load(Long id) {
				return readById(id);
			}

		};
		mockCache = CacheBuilder.newBuilder().build(cacheLoader);
		loadInitial();
	}
	
	@Override
	public Map<Long, VirtualServiceRequest> readAll() {
		if(mockCache == null || mockCache.size() ==0){
			loadInitial();
		}
		return mockCache.asMap();
	}

	@Override
	public void update(Long key, VirtualServiceRequest mock) {
		mockCache.put(key, mock);
	}

	@Override
	public void remove(Long key) {
		mockCache.invalidate(key);
	}

	@Override
	public VirtualServiceRequest readById(Long id) {
		try {
			return mockCache.get(id);
		} catch (ExecutionException e) {
		}
		return null;
	}

}