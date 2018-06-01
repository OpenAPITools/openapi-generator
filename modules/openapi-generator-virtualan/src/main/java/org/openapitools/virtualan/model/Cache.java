package org.openapitools.virtualan.model;

import java.util.Map;

public interface Cache<K, T> {

	Map<K, T> readAll();

	void update(K key, T mock);

	void remove(K key);

	T readById(K id) ;

}
