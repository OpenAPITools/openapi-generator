package org.openapitools.virtualan.cache;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import javax.annotation.PostConstruct;

import org.openapitools.virtualan.VirtualServiceUtil;
import org.openapitools.virtualan.dao.MockRepository;
import org.openapitools.virtualan.entity.MockEntity;
import org.openapitools.virtualan.model.Cache;
import org.openapitools.virtualan.model.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;;

@Service("virtualCache")
public class VirtualCacheUtil implements Cache<Long, Mock> {

	@Autowired
	private MockRepository mockRepository;
	
	
	@Autowired
	private VirtualServiceUtil virtualServiceUtil;

	private Map<Long, Mock> loadInitial() {
		Iterable<MockEntity> mockEntityList = mockRepository.findAll();
		final Map<Long, Mock> map = new HashMap<Long, Mock>();
		for (MockEntity mockEntity : mockEntityList) {
			Mock mockTransferObject = virtualServiceUtil.converterEToR(mockEntity);
			mockCache.put(mockTransferObject.getId(), mockTransferObject);
		}
		return map;
	}

	private LoadingCache<Long, Mock> mockCache;
	
	@PostConstruct
	public void load(){
		
		CacheLoader<Long, Mock> cacheLoader =  new CacheLoader<Long, Mock>() {
			@Override
			public Mock load(Long id) {
				return readById(id);
			}

		};
		mockCache = CacheBuilder.newBuilder().build(cacheLoader);
		loadInitial();
	}
	
	@Override
	public Map<Long, Mock> readAll() {
		if(mockCache == null || mockCache.size() ==0){
			loadInitial();
		}
		return mockCache.asMap();
	}

	@Override
	public void update(Long key, Mock mock) {
		mockCache.put(key, mock);
	}

	@Override
	public void remove(Long key) {
		mockCache.invalidate(key);
	}

	@Override
	public Mock readById(Long id) {
		try {
			return mockCache.get(id);
		} catch (ExecutionException e) {
		}
		return null;
	}

}