// -*- mode: Scala;-*- 
// Filename:    Meth.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan  8 16:04:34 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import java.net.URI
import com.eaio.uuid.UUID

import scala.continuations._ 
import scala.continuations.ControlContext._ 
import scala.collection.mutable._
import scala.actors._
import Actor._

import com.thoughtworks.xstream._

import java.lang.reflect.Field
import java.lang.reflect.Method

trait MsgCaseClassGenerator {
  def generateCaseClassMembers( mthd : Method ) : Array[String] = {
    for( fmlType <- mthd.getParameterTypes )
    yield {
      "p" + (new UUID()).toString + " : " + fmlType.getName
    }
  }
  def generateCaseClassDeclaration(
    obj : {def getClass : Class[_]},
    mthd : Method
  ) : String = {
    ("case class"
     + " "
     + mthd.getName
     + "("
     + (
	generateCaseClassMembers( mthd ).toList match {
	  case Nil => ""	  
	  case t :: r => {
	    (
	      t
	      + ( "" /: r )(
		{ ( acc, elem ) => { acc + "," + elem } }
	      )
	    )
	  }
	}      
     )
     + ")"
     + " "
     + "extends"
     + " "
     + generateTraitName( obj )
   )
  }
  val exclusionList : List[String] = {
    List(
      // these come from scala.Product
      "productPrefix",
      "productElements",
      "productIterator",
      "productArity",
      "productElement",
      "canEqual",
      "copy$default$1",
      "copy$default$2",
      "copy$default$3",
      "copy$default$4",
      // these come from java.lang.Object
      "hashCode",
      "equals",
      "toString",
      "copy",
      "getClass",
      "wait",
      "notify",
      "notifyAll"
    )
  }
  def methodExcluded( mthd : Method ) : Boolean = {
    exclusionList.contains( mthd.getName )
  }
  var _traitName : Option[String] = None
  def generateTraitName( obj : {def getClass : Class[_]} )
  : String = {
    _traitName match {
      case None => {
	val tn = obj.getClass.getName + "MessageSet"
	_traitName = Some( tn )
	tn
      }
      case Some( tn ) => tn
    }    
  }
  def generateTraitDecl( obj : {def getClass : Class[_]} ) 
  : String = {
    (
      "trait"
      + " "
      + generateTraitName( obj )
      + "{" 
      + " "
      + "}"
      )
  }
  def generateCaseClassDecls( obj : {def getClass : Class[_]} )
  : String = {
    (
      generateTraitDecl( obj )
      + "\n"
      +
      ( "" /: obj.getClass.getMethods )( 
	{ ( acc, elem ) => {
	  (
	    acc
	    + (if ( !methodExcluded( elem ) )
	      {
		"\n" + generateCaseClassDeclaration( obj, elem )
	      }
	       else "")
	  )
	}
       }
      )
    )
  }
  def trgtResourceClassName( obj : {def getClass : Class[_]} )
  : String = { generateTraitName( obj ) }

  def dumpCaseClassMessageSetToFile(
    trgtLocation : String,
    packageName : String,
    obj : {def getClass : Class[_]}
  ) : Unit = {
    val trgtFile = new java.io.File( trgtLocation )    

    if (!( trgtFile ).exists) {
      trgtFile.mkdirs()
    }

    val scalaSrcFileName : String =
      trgtLocation + "/" + trgtResourceClassName( obj ) + ".scala";

    val scalaSrcWriter : java.io.BufferedWriter =
      new java.io.BufferedWriter(
	new java.io.FileWriter(
	  scalaSrcFileName
	)
      )

    scalaSrcWriter.write(
      (
	"package"
	+ " "
	+ packageName
	+ "\n"
	+ generateCaseClassDecls( obj )
      )
    )

    scalaSrcWriter.flush()
    scalaSrcWriter.close()
  }
}

object theMsgCaseClassGenerator extends MsgCaseClassGenerator
