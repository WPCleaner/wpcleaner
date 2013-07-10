/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import org.wikipediacleaner.i18n.GT;


/**
 * A simple class keeping version information. 
 */
public final class Version {

  public final static String VERSION = "1.27";
  public final static Date   DATE = new GregorianCalendar(2013, Calendar.APRIL, 13).getTime();

  public final static String PROGRAM = "WPCleaner";

  public final static String MESSAGE =
    GT._("I hope you''ll like {0}.", PROGRAM) +
    "<br><br><b>" +
    GT._("Disambiguation extension has been deployed to all WMF wikis.") + " " +
    GT._("It defines the new magic word __DISAMBIG__.") + "<br>" +
    GT._("This magic word should be added in every disambiguation pages (either through templates or directly).") + "<br>" +
    GT._("WPCleaner future release (v1.28) will also use this as the default system.") + "<br>" +
    GT._("An action is required on your wiki before this release. Either:") + "<br>" +
    "<ul>" +
    "<li>" + GT._("Add __DISAMBIG__ in every disambiguation pages.") + "</li>" +
    "<li>" + GT._("Configure dab_templates_list in WPCleaner System configuration.") + "</li>" +
    "</ul>" +
    "</b><br><br>" +
    GT._(
        "WPCleaner configuration is available online, check the {0}System configuration{1}.",
        new Object[] { "<a href=\"http://fr.wikipedia.org/wiki/Utilisateur:NicoV/WikiCleanerConfigurationDocumentation\">", "</a>" }) +
    "<br>" +
    GT._("Please, report any other problem you find to me.");

  public final static boolean HIGHLIGHT = true;

  public final static String OLD_MESSAGES =
      GT._("Many new features were added recently : try them !") +
      "<ul>" +
      "<li>" + GT._("Leaving a note on talk pages with the list of links to disambiguation pages.") + "</li>" +
      "<li>" + GT._("Fixing errors detected by the Check Wiki project, in the full analysis window.") + "</li>" +
      "<li>" + GT._("Fixing speling or typography based on a list of suggestions.") + "</li>" +
      "</ul>";
}
