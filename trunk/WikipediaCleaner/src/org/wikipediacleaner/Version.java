/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

  public final static String VERSION = "1.30";
  public final static Date   DATE = new GregorianCalendar(2013, Calendar.OCTOBER, 29).getTime();

  public final static String PROGRAM = "WPCleaner";

  public final static boolean HIGHLIGHT = false;

  public final static String MESSAGE =
    "<b>" +
    GT._("Disambiguator extension has been deployed to all WMF wikis.") + " " +
    GT._("It defines the new magic word __DISAMBIG__.") + "<br>" +
    GT._("This magic word should be added in every disambiguation pages (either through templates or directly).") + "<br>" +
    GT._("WPCleaner v1.28 also uses this as the default system.") + "<br>" +
    GT._("An action may be required on your wiki:") + "<br>" +
    "<ul>" +
    "<li>" + GT._("You can check if {0} is used on your wiki by opening an existing disambiguation page (like \"Smith\"). " +
                  "Then, click on \"Page information\" in the menu on the left. " +
                  "Finally, search for {0} in the information page.",
                  "<tt>__DISAMBIG__</tt>") + "</li>" +
    "<li>" + GT._("If {0} is not used on your wiki, you should add {1} to the page {2}.",
                  new Object[] {
                    "<tt>__DISAMBIG__</tt>",
                    "<i><tt>dab_use_disambig=false END</tt></i>",
                    "<i>User:NicoV/WikiCleanerConfiguration</i>" }) + "</li>" +
    "</ul>" +
    "</b><br><br>" +
    GT._("I hope you''ll like {0}.", PROGRAM) +
    "<br><br>" +
    GT._(
        "WPCleaner configuration is available online, check the {0}System configuration{1}.",
        new Object[] { "<a href=\"http://fr.wikipedia.org/wiki/Utilisateur:NicoV/WikiCleanerConfigurationDocumentation\">", "</a>" }) +
    "<br>" +
    GT._("Please, report any other problem you find to me.");

  public final static String OLD_MESSAGES =
      GT._("Many new features were added recently : try them !") +
      "<ul>" +
      "<li>" + GT._("Leaving a note on talk pages with the list of links to disambiguation pages.") + "</li>" +
      "<li>" + GT._("Fixing errors detected by the Check Wiki project, in the full analysis window.") + "</li>" +
      "<li>" + GT._("Fixing speling or typography based on a list of suggestions.") + "</li>" +
      "</ul>";
}
