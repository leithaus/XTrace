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

import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Settings

/** An object for running the plugin as standalone application.
 * 
 *  @todo: print, parse and apply plugin options !!!
 *  ideally re-use the TemplatePlugin (-> runsAfter, optionsHelp,
 *  processOptions, components, annotationChecker) instead of
 *  duplicating it here and in PluginRunner.
 */
object Main {
  def main(args: Array[String]) {
    val settings = new Settings

    val command = new CompilerCommand(args.toList, settings, println, false) {
      /** The command name that will be printed in in the usage message.
       *  This is autmatically set to the value of 'plugin.commandname' in the
       *  file build.properties.
       */
      override val cmdName = PluginProperties.pluginCommand
    }

    if (!command.ok)
      return()

    /** The version number of this plugin is read from the porperties file
     */
    if (settings.version.value) {
      println(command.cmdName +" version "+ PluginProperties.versionString)
      return()
    }
    if (settings.help.value) {
      println(command.usageMsg)
      return()
    }

    val runner = new PluginRunner(settings)
    val run = new runner.Run
    run.compile(command.files)
  }
}
