package org.openapitools.client.infrastructure

public class CollectionFormats {

    public open class CSVParams {

        public var params: List<String>

        public constructor(params: List<String>) {
            this.params = params
        }

        public constructor(vararg params: String) {
            this.params = listOf(*params)
        }

        override fun toString(): String {
            return params.joinToString(",")
        }
    }

    public open class SSVParams : CSVParams {

        public constructor(params: List<String>) : super(params)

        public constructor(vararg params: String) : super(*params)

        override fun toString(): String {
            return params.joinToString(" ")
        }
    }

    public class TSVParams : CSVParams {

        public constructor(params: List<String>) : super(params)

        public constructor(vararg params: String) : super(*params)

        override fun toString(): String {
            return params.joinToString("\t")
        }
    }

    public class PIPESParams : CSVParams {

        public constructor(params: List<String>) : super(params)

        public constructor(vararg params: String) : super(*params)

        override fun toString(): String {
            return params.joinToString("|")
        }
    }

    public class SPACEParams : SSVParams()
}
