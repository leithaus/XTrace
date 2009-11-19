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


package sts.compilerPlugin.standalone

import scala.tools.nsc.{Global, Settings, SubComponent}
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}

/** This class is a compiler that will be used for running
 *  the plugin in standalone mode.
 */
class PluginRunner(settings: Settings, reporter: Reporter)
extends Global(settings, reporter) {
  def this(settings: Settings) = this(settings, new ConsoleReporter(settings))

  override def phaseDescriptors: List[SubComponent] = List(
    syntaxAnalyzer) ::: ScalaQLPlugin.components(this)
}
