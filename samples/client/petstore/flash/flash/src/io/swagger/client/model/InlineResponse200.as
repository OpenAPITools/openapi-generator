package io.swagger.client.model {

import io.swagger.client.model.Object;
import io.swagger.client.model.Tag;

    [XmlRootNode(name="InlineResponse200")]
    public class InlineResponse200 {
                // This declaration below of _tags_obj_class is to force flash compiler to include this class
        private var _tags_obj_class: Array = null;
        [XmlElementWrapper(name="tags")]
        [XmlElements(name="tags", type="Array")]
                public var tags: Array = new Array();
                [XmlElement(name="id")]
        public var id: Number = 0;
                [XmlElement(name="category")]
        public var category: Object = NaN;
        /* pet status in the store */
        [XmlElement(name="status")]
        public var status: String = null;
                [XmlElement(name="name")]
        public var name: String = null;
                // This declaration below of _photoUrls_obj_class is to force flash compiler to include this class
        private var _photoUrls_obj_class: Array = null;
        [XmlElementWrapper(name="photoUrls")]
        [XmlElements(name="photoUrls", type="Array")]
                public var photoUrls: Array = new Array();

    public function toString(): String {
        var str: String = "InlineResponse200: ";
        str += " (tags: " + tags + ")";
        str += " (id: " + id + ")";
        str += " (category: " + category + ")";
        str += " (status: " + status + ")";
        str += " (name: " + name + ")";
        str += " (photoUrls: " + photoUrls + ")";
        return str;
    }

}

}
