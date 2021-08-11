using Org.OpenAPITools.Model;
using System;
using System.Collections.Generic;
using System.Text;
using Xunit;
namespace Org.OpenAPITools.Test.Model
{
    public class SerializeBasedOnInitialization
    {
        public SerializeBasedOnInitialization()
        {
            
        }

        /// <summary>
        /// Apple has two properties, here we serialize only origin property
        /// </summary>
        [Fact]
        public void SerializeAppleOrigin()
        {
            Apple apple = new Apple();
            apple.Origin = "India";
            string expectedJson = "{\r\n  \"origin\": \"India\"\r\n}";
            Assert.Equal(expectedJson, apple.ToJson());
        }

        /// <summary>
        /// Apple has two properties, here we serialize only cultivar property
        /// </summary>
        [Fact]
        public void SerializeAppleCultivate()
        {
            Apple apple = new Apple();
            apple.Cultivar = "Kashmiri";
            string expectedJson = "{\r\n  \"cultivar\": \"Kashmiri\"\r\n}";
            Assert.Equal(expectedJson, apple.ToJson());
        }

        /// <summary>
        /// Here we serialze all the properties of Apple that it origin and cultivar both.
        /// </summary>
        [Fact]
        public void SerializeApple()
        {
            Apple apple = new Apple();
            apple.Origin = "India";
            apple.Cultivar = "Kashmiri";
            string expectedJson = "{\r\n  \"cultivar\": \"Kashmiri\",\r\n  \"origin\": \"India\"\r\n}";
            Assert.Equal(expectedJson, apple.ToJson());
        }
    }
}
