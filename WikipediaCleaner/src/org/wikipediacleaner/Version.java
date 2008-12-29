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


/**
 * A simple class keeping version informations. 
 */
public final class Version {

  public final static String VERSION = "0.86";
  public final static Date   DATE = new GregorianCalendar(2008, Calendar.DECEMBER, 29).getTime();

  public final static String MESSAGE =
    "<html>" +
    "I hope you'll like WikiCleaner.<br><br>" +
    "There's still a problem with 'bad token': from time to time (usually first one?), an edit doesn't work.<br>" +
    "Please, report any other problem you find to me." +
    "</html>";
  public final static boolean HIGHLIGHT = false;
}
