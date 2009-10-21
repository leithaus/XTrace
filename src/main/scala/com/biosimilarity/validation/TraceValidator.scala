// -*- mode: Scala;-*- 
// Filename:    TraceValidator.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct 20 14:42:45 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import java.net.URI
import java.net.URL
import java.io._
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation._
import org.xml.sax.SAXException

import com.eaio.uuid.UUID

trait TraceValidation {
    def validateTrace( property : String, trace : String ) : Boolean = {

      // 1. Lookup a factory for the W3C XML Schema language
      val factory : SchemaFactory = 
            SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema")
        
      // 2. Compile the schema. 
      // Here the schema is loaded from a java.io.File, but you could use 
      // a java.net.URL or a javax.xml.transform.Source instead.
      val schemaLocation : File = new File( property )
      val schema : Schema = factory.newSchema( schemaLocation )
    
      // 3. Get a validator from the schema.
      val validator : Validator = schema.newValidator()
        
      // 4. Parse the document you want to check.
      val source : Source = new StreamSource( new StringReader( trace ) )
        
        // 5. Check the document
      try {
        validator.validate( source ) 
        //println( trace + " is valid.")
	true
      }
      catch {
	case ex : SAXException => {
          println( trace + " is not valid because ")
          println( ex.getMessage() )
	  false
	}
      }  
        
    }

}

class TraceValidator( monitor : TraceMonitor )
extends TraceValidation
{
  def validate( property : String ) : Boolean = {
    validateTrace( property, monitor.getFinalLog )
  }
}

case object ATraceValidator extends TraceValidator( ATraceMonitor )
