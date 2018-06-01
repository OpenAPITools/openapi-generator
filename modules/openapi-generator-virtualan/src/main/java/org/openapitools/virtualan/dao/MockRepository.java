package org.openapitools.virtualan.dao;

import org.openapitools.virtualan.entity.MockEntity;
import org.springframework.data.repository.CrudRepository;

public interface MockRepository extends CrudRepository<MockEntity, Long> {
}