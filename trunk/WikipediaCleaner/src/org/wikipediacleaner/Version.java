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
 * A simple class keeping version informations. 
 */
public final class Version {

  public final static String VERSION = "1.00";
  public final static Date   DATE = new GregorianCalendar(2011, Calendar.APRIL, 7).getTime();

  public final static String MESSAGE =
    GT._("I hope you'll like WikiCleaner.") +
    "<br><br>" +
    GT._(
        "WikiCleaner configuration is available online, check the {0}System configuration{1}.",
        new Object[] { "<a href=\"http://fr.wikipedia.org/wiki/Utilisateur:NicoV/WikiCleanerConfigurationDocumentation\">", "</a>" }) +
    "<br>" +
    GT._(
        "Many new features added for the {0}Check Wiki project{1} : try them !",
        new Object[] { "<a href=\"http://de.wikipedia.org/wiki/Benutzer:Stefan_K%C3%BChn/Check_Wikipedia\">", "</a>" }) +
    "<br><br>" +
    GT._("Please, report any other problem you find to me.");
  public final static boolean HIGHLIGHT = false;
}
