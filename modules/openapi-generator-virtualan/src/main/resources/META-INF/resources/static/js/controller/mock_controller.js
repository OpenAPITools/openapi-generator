'use strict';

myApp.controller('MockController', ['$scope',  '$filter', '$modal', 'MockService', function($scope, $filter,  $modal, MockService) {
    var self = this;
    self.mockRequest={id:'',resource:'',url:'',method:'',operationId:'',input:'',output:'',excludeList:'', httpStatus:'',availableParams:[]};
    self.mockCreateRequest= {id:'',resource:'',method:'',url:'',operationId:'',input:'',output:'',excludeList:'', httpStatus:'',availableParams:[]};
    self.mockRequests=[];
    self.mockLoadRequests=[];
    self.mockLoadRequest='';
    self.selectedOperationId ='';
    self.edit = edit;
    self.remove = remove;
    self.message ='';
    self.type = '';
    self.filtered = {};
    self.searchText ='';
    self.currentPage = 1;
    self.viewby = 5;
    self.perPage = 5;
    self.maxSize = 5;
	self.prettyJson = '';
	self.treeJson = '';
    
    loadAllMockRequest();
    
    self.setPage = function (pageNo) {
    	self.currentPage = pageNo;
    };
    
    
   self.jsonObj = JSON.parse("{  \"errorCode\": \"____NOT_FOUND\",  \"errorMessage\": \" Missing?????\"}");
   
    self.loadJson = function (value) {
 		console.log(value);
 		self.jsonObj = JSON.parse(value);
 		self.jsonStr = JSON.parse(value);
    }; 

    
    $scope.$watch(self.searchText, function (term) {
      var obj = term;
      self.filterList = $filter('filter')(self.mockRequests, obj);
      self.currentPage = 1;
    }); 
    
  
    self.setItemsPerPage = function(num) {
	    self.perPage = num;
	    self.currentPage = 1; //reset to first page
    };
        
    self.loadData = fetchAllMockRequest();
    	  
    function fetchAllMockRequest(){
    	MockService.fetchAllMockRequest()
            .then(
            function(d) {
                self.mockRequests = d;
                self.filterList = self.mockRequests;
            },
            function(errResponse){
                console.error('Error while fetching Mocks');
            }
        );
    };
    
    function loadAllMockRequest(){
        MockService.loadAllMockRequest()
            .then(
            function(d) {
            	self.mockLoadRequests = d;
           	 console.log("ALL API's", self.mockLoadRequests);
            },
            function(errResponse){
                console.error('Error while fetching mockLoadRequests');
            }
        );
    }

	self.showDialog = false;
	
	self.showAlert = function(operationId) {
		if(operationId === self.selectedOperationId){
			self.closeAlertDialog = true;
			return true;
		}
		return false;
	}
	
	self.closeAlert = function(operationId) {
		self.showDialog = false;
	}
	
	
	self.showMessage = function(operationId) {
		if(operationId === self.selectedOperationId){
			return self.message;
		}
		return;
	}
    
    function createMockRequest(mockRequest){
		self.showDialog = false;
    	self.selectedOperationId = mockRequest.operationId;
    	self.message = 'Mock response added successfully for the given request parameter(s)!!!!';
    	self.type = 'success';
		MockService.createMockRequest(mockRequest)
            .then(
            		fetchAllMockRequest,
            function(errResponse){
            	self.type = 'danger';
            	self.message = errResponse.data.code;
                console.error('Error while creating MockRequest');
            }
        );
		self.showDialog = true;
    }

    function updateMockRequest(mockRequest, id){
        MockService.updateMockRequest(mockRequest, id)
            .then(
            		fetchAllMockRequest,
            function(errResponse){
                console.error('Error while updating MockRequest');
            }
        );
    }

    function deleteMockRequest(id){
        MockService.deleteMockRequest(id)
            .then(
            		fetchAllMockRequest,
            function(errResponse){
                console.error('Error while deleting MockRequest');
            }
        );
    }

    self.submit= function (mockRequest) {
        console.log('Saving New mockRequest', mockRequest);
        self.mockCreateRequest= {id:'', 
        			resource:mockRequest.resource,
        			url:mockRequest.url,
        			operationId:mockRequest.operationId,
        			input:mockRequest.input,
        			output:mockRequest.output,
        			excludeList:mockRequest.excludeList, 
        			httpStatusCode:mockRequest.httpStatusCode,
        			method:mockRequest.method,
        			availableParams:mockRequest.availableParams};
        console.log('Saving New mockRequest', self.mockCreateRequest);
        createMockRequest(self.mockCreateRequest);

    }

    function edit(id){
        console.log('id to be edited', id);
        for(var i = 0; i < self.mockRequests.length; i++){
            if(self.mockRequests[i].id === id) {
                self.mockRequest = angular.copy(self.mockRequests[i]);
                break;
            }
        }
    }

    function remove(id){
        console.log('id to be deleted', id);
        if(self.mockRequest.id === id) {//clean form if the mockRequest to be deleted is shown there.
            reset();
        }
        deleteMockRequest(id);
    }


    self.reset = function (myForm, mockRequest){
        self.message ='';
        self.classCode = '';
        mockRequest.id = null;
        mockRequest.output= null;
        mockRequest.excludeList = null;
        mockRequest.httpStatusCode ='';
        for (var parm in mockRequest.availableParams) {
        	mockRequest.availableParams[parm].value =  '';
        }
        if(mockRequest.input != null) {
        	mockRequest.input = '';
    	}
        myForm.$setPristine(); //reset Form
    }

}]);

