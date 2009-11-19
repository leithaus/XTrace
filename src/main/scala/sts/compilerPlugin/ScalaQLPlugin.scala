/******************************************************************************* 
 * This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Anastasia Izmaylova, 
 *     Miguel Garcia, http://www.sts.tu-harburg.de/people/mi.garcia 
 *******************************************************************************/ 

package sts.compilerPlugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

class ScalaQLPlugin(val global: Global) extends Plugin {
  
  println("Hallo, Ich bin im Plugin ...")
  
  val name = PluginProperties.pluginName

  val runsAfter = "parser"

  val description = PluginProperties.pluginDescription
  
  override val optionsHelp = Some(
    "  -P:"+ name +":option     sets some option for this plugin")

  override def processOptions(options: List[String], error: String => Unit) {
    super.processOptions(options, error)
  }

  val components = ScalaQLPlugin.components(global)
  
}

object ScalaQLPlugin {

  def components(global: Global) =
    List(new ScalaASTTransformer(global))
}
