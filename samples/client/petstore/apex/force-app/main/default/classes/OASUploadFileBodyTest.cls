@isTest
private class OASUploadFileBodyTest {
    @isTest
    private static void equalsSameInstance() {
        OASUploadFileBody uploadFileBody1 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody2 = uploadFileBody1;
        OASUploadFileBody uploadFileBody3 = new OASUploadFileBody();
        OASUploadFileBody uploadFileBody4 = uploadFileBody3;

        System.assert(uploadFileBody1.equals(uploadFileBody2));
        System.assert(uploadFileBody2.equals(uploadFileBody1));
        System.assert(uploadFileBody1.equals(uploadFileBody1));
        System.assert(uploadFileBody3.equals(uploadFileBody4));
        System.assert(uploadFileBody4.equals(uploadFileBody3));
        System.assert(uploadFileBody3.equals(uploadFileBody3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        OASUploadFileBody uploadFileBody1 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody2 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody3 = new OASUploadFileBody();
        OASUploadFileBody uploadFileBody4 = new OASUploadFileBody();

        System.assert(uploadFileBody1.equals(uploadFileBody2));
        System.assert(uploadFileBody2.equals(uploadFileBody1));
        System.assert(uploadFileBody3.equals(uploadFileBody4));
        System.assert(uploadFileBody4.equals(uploadFileBody3));
    }

    @isTest
    private static void notEqualsDifferentType() {
        OASUploadFileBody uploadFileBody1 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody2 = new OASUploadFileBody();

        System.assertEquals(false, uploadFileBody1.equals('foo'));
        System.assertEquals(false, uploadFileBody2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        OASUploadFileBody uploadFileBody1 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody2 = new OASUploadFileBody();
        OASUploadFileBody uploadFileBody3;

        System.assertEquals(false, uploadFileBody1.equals(uploadFileBody3));
        System.assertEquals(false, uploadFileBody2.equals(uploadFileBody3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        OASUploadFileBody uploadFileBody1 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody2 = new OASUploadFileBody();

        System.assertEquals(uploadFileBody1.hashCode(), uploadFileBody1.hashCode());
        System.assertEquals(uploadFileBody2.hashCode(), uploadFileBody2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        OASUploadFileBody uploadFileBody1 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody2 = OASUploadFileBody.getExample();
        OASUploadFileBody uploadFileBody3 = new OASUploadFileBody();
        OASUploadFileBody uploadFileBody4 = new OASUploadFileBody();

        System.assert(uploadFileBody1.equals(uploadFileBody2));
        System.assert(uploadFileBody3.equals(uploadFileBody4));
        System.assertEquals(uploadFileBody1.hashCode(), uploadFileBody2.hashCode());
        System.assertEquals(uploadFileBody3.hashCode(), uploadFileBody4.hashCode());
    }
}
