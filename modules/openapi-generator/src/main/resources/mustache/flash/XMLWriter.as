package org.openapitools.common
{
	public class XMLWriter
	{
		public var xml:XML;
		
		public function XMLWriter() {
			xml=<obj/>;
		}
		
		public function reset():void {
			xml=new XML();
		}
		
		public function addProperty(propertyName:String, propertyValue:String):XML {
			var xmlProperty:XML=<new/>
			xmlProperty.setName(propertyName);
			xmlProperty.appendChild(propertyValue);
			xml.appendChild(xmlProperty);
			return xmlProperty;		
		}
		
		public function addAttribute(propertyName:String, attribute:String, attributeValue:String):void {
			xml.elements(propertyName)[0].@[attribute]=attributeValue;
		}
	}
}