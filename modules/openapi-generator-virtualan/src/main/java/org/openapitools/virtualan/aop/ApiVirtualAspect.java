package org.openapitools.virtualan.aop;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.openapitools.virtualan.VirtualServiceUtil;
import org.openapitools.virtualan.model.MockServiceRequest;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@Aspect
@Component
public class ApiVirtualAspect {

	@org.springframework.beans.factory.annotation.Autowired
	private VirtualServiceUtil virtualServiceUtil;

	
    public Optional<VirtualServiceUtil> getVirtualServiceUtil() {
        return Optional.ofNullable(virtualServiceUtil);
    }
	
	public String addQueryParamValue(Object value){
		return String.join(",", (java.util.List)value);
	}

	
	@Around("@annotation(org.openapitools.virtualan.annotation.ApiVirtual)")
    public Object  aroundAddAdvice(ProceedingJoinPoint thisJoinPoint) throws IOException{
		Object[] args = thisJoinPoint.getArgs();
        MethodSignature methodSignature = (MethodSignature) thisJoinPoint.getStaticPart().getSignature();
        Method method = methodSignature.getMethod();
        Annotation[][] parameterAnnotations = method.getParameterAnnotations();
        assert args.length == parameterAnnotations.length;
		Map<String, String> paramMap =  new HashMap<>();
		MockServiceRequest mockServiceRequest = new MockServiceRequest();
		mockServiceRequest.setResource(thisJoinPoint.getTarget().getClass().getSimpleName().toLowerCase().replace("apicontroller", ""));
		mockServiceRequest.setOperationId(method.getName());
        for (int argIndex = 0; argIndex < args.length; argIndex++) {
            for (Annotation annotation : parameterAnnotations[argIndex]) {
                if (annotation instanceof RequestParam){
                	RequestParam requestParam = (RequestParam) annotation;
                	paramMap.put(requestParam.value(), (String)args[argIndex]);
                }

                else if (annotation instanceof PathVariable){
                	PathVariable pathParam = (PathVariable) annotation;
                	paramMap.put(pathParam.value(), (String)args[argIndex]);
                }
                else if (annotation instanceof RequestBody){
               		RequestBody requestBody = (RequestBody) annotation;
        			try {
						mockServiceRequest.setInputObjectType(Class.forName((methodSignature.getParameterTypes()[argIndex]).getName()));
					} catch (ClassNotFoundException e) {
						e.printStackTrace();
					}
        			mockServiceRequest.setInputObject(args[argIndex]);
                }
            }
        }
        mockServiceRequest.setParams(paramMap);
		try {
	    	thisJoinPoint.proceed();
		} catch (Throwable e) {
		}
		return getVirtualServiceUtil().get().returnResponse(mockServiceRequest);
	}
	
}
