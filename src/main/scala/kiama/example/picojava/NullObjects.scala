/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2009 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava

object NullObjects {

    import AbstractSyntax._
    import NameResolution._
    import kiama.attribution.Attributable
    import kiama.attribution.Attribution._

    /**
     * A declaration object representing an unknown entity.
     *
     * syn lazy UnknownDecl Program.unknownDecl() = (UnknownDecl) localLookup("$unknown");
     * inh Decl TypeDecl.unknownDecl();
     * inh Decl Block.unknownDecl();
     * eq Program.getBlock().unknownDecl() = unknownDecl();
     * eq Program.getPredefinedType().unknownDecl() = unknownDecl();
     */
    val unknownDecl : Attributable ==> UnknownDecl =
        attr {
            case p : Program => (p->localLookup ("$unknown")).asInstanceOf[UnknownDecl]
            // FIXME: need NTA case?
            case t           => t.parent->unknownDecl
        }

}
