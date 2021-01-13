/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * A class to manage link replacement.
 */
public class LinkReplacement {

  final private static Configuration configuration = Configuration.getConfiguration();

  /* ================================================================================= */
  /* Management of last replacement                                                    */
  /* ================================================================================= */

  /**
   * Memorize last replacement for each link.
   */
  private final static Map<String, String> lastReplacement = new HashMap<>();

  /**
   * Memorize suffix used in the last replacement.
   */
  private static String lastSuffix = null;

  /**
   * Memorize a replacement.
   * 
   * @param from From.
   * @param to To.
   */
  public static void addLastReplacement(String from, String to) {
    if ((from == null) || (to == null)) {
      return;
    }
    if (to.startsWith(from)) {
      lastSuffix = to.substring(from.length());
    } else {
      lastSuffix = null;
    }
    synchronized (lastReplacement) {
      lastReplacement.put(from, to);
    }
    if (configuration.getBoolean(
        null,
        ConfigurationValueBoolean.SAVE_LAST_REPLACEMENT)) {
      configuration.setSubString(
          null,
          Configuration.PROPERTIES_LAST_REPLACEMENT,
          from, to);
    }
  }

  /**
   * Get a replacement.
   * 
   * @param from From.
   * @return Replacement.
   */
  public static String getLastReplacement(String from) {
    if (from == null) {
      return null;
    }

    synchronized (lastReplacement) {
      return lastReplacement.get(from);
    }
  }

  /**
   * Return a link matching the last suffix replacement.
   * 
   * @param from Initial link.
   * @param links Possible links.
   * @return Link matching the last suffix replacement if it exists. 
   */
  public static String getPossibleLastSuffix(String from, Collection<Page> links) {
    if ((lastSuffix == null) ||
        (lastSuffix.length() == 0) ||
        (links == null)) {
      return null;
    }
    for (Page link : links) {
      if (link.getTitle().startsWith(from) && link.getTitle().endsWith(lastSuffix)) {
        return link.getTitle();
      }
    }
    return null;
  }

  /* ================================================================================= */
  /* Static initialization of the class                                                */
  /* ================================================================================= */

  static {
    // Initialize map of last replacements
    Properties tmp = configuration.getProperties(
        null, Configuration.PROPERTIES_LAST_REPLACEMENT);
    if (tmp != null) {
      for (Object object : tmp.keySet()) {
        if (object instanceof String) {
          lastReplacement.put((String) object, tmp.getProperty((String) object, "")); 
        }
      }
    }
  }
}
