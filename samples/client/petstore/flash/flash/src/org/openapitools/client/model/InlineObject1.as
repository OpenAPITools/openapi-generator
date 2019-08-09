package org.openapitools.client.model {

import flash.filesystem.File;

    [XmlRootNode(name="InlineObject1")]
    public class InlineObject1 {
        /* Additional data to pass to server */
        [XmlElement(name="additionalMetadata")]
        public var additionalMetadata: String = null;
        /* file to upload */
        [XmlElement(name="file")]
        public var file: File = null;

    public function toString(): String {
        var str: String = "InlineObject1: ";
        str += " (additionalMetadata: " + additionalMetadata + ")";
        str += " (file: " + file + ")";
        return str;
    }

}

}
