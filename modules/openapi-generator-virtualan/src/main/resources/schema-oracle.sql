CREATE SEQUENCE MOCK_SEQ START WITH 1 INCREMENT BY 1;
CREATE TABLE  mock (id NUMBER DEFAULT MOCK_SEQ.NEXTVAL PRIMARY KEY,  operationid varchar2(250), 
                input clob, output clob, resources varchar2(50),url varchar2(250), method varchar2(50), httpStatusCode varchar2(50),excludeList varchar2(250), availableParamsList varchar(4000) ); 