/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.api.check.algorithm;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.i18n.GT;


/**
 * Helper class for the algorithms.
 */
public final class CheckErrorAlgorithms {

  public final static int PRIORITY_UNKOWN = -1;
  public final static int PRIORITY_DEACTIVATED = 0;
  public final static int PRIORITY_TOP = 1;
  public final static int PRIORITY_MIDDLE = 2;
  public final static int PRIORITY_LOWEST = 3;
  public final static int PRIORITY_BOT_ONLY = 4;

  public static final int MAX_ALGORITHMS = 999;

  private static Map<EnumWikipedia, List<CheckErrorAlgorithm>> algorithmsMap =
    new HashMap<EnumWikipedia, List<CheckErrorAlgorithm>>();

  /**
   * Initializes algorithms for a Wikipedia.
   * 
   * @param wikipedia Wikipedia.
   */
  public static synchronized void initializeAlgorithms(EnumWikipedia wikipedia) {
    List<CheckErrorAlgorithm> algorithms = new ArrayList<CheckErrorAlgorithm>(MAX_ALGORITHMS);
    DecimalFormat errorNumberFormat = new DecimalFormat("000");
    for (int i = 0; i < MAX_ALGORITHMS; i++) {
      int errorNumber = i + 1;
      String className = CheckErrorAlgorithm.class.getName() + errorNumberFormat.format(errorNumber);
      CheckErrorAlgorithm algorithm = null;
      try {
        Class algorithmClass = Class.forName(className);
        algorithm = (CheckErrorAlgorithm) algorithmClass.newInstance();
        algorithm.setPriority(getPriority(wikipedia, errorNumber));
        algorithm.setShortDescription(getShortDescription(wikipedia, errorNumber));
        algorithm.setLongDescription(getLongDescription(wikipedia, errorNumber));
        algorithm.setLink(getLink(wikipedia, errorNumber));
        algorithm.setWhiteList(getWhiteList(wikipedia, errorNumber));
      } catch (ClassNotFoundException e) {
        // Not found: error not yet available in WikiCleaner.
      } catch (InstantiationException e) {
        System.err.println("InstantiationException for " + className);
      } catch (IllegalAccessException e) {
        System.err.println("IllegalAccessException for " + className);
      } catch (ClassCastException e) {
        System.err.println(
            "Class " + className +
            " doesn't implement " + CheckErrorAlgorithm.class.getName());
      }
      algorithms.add(algorithm);
    }
    algorithmsMap.put(wikipedia, algorithms);
  }

  /**
   * Retrieve all algorithms for a wikipedia.
   * 
   * @param wikipedia Wikipedia.
   * @return All algorithms.
   */
  public static List<CheckErrorAlgorithm> getAlgorithms(EnumWikipedia wikipedia) {
    List<CheckErrorAlgorithm> algorithms = algorithmsMap.get(wikipedia);
    if (algorithms == null) {
      return null;
    }
    List<CheckErrorAlgorithm> tmpAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    for (CheckErrorAlgorithm algorithm : algorithms) {
      if (algorithm != null) {
        tmpAlgorithms.add(algorithm);
      }
    }
    return tmpAlgorithms;
  }

  /**
   * Retrieve an algorithm.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Algorithm requested.
   */
  public static CheckErrorAlgorithm getAlgorithm(
      EnumWikipedia wikipedia, int errorNumber) {
    List<CheckErrorAlgorithm> algorithms = algorithmsMap.get(wikipedia);
    if (algorithms == null) {
      initializeAlgorithms(wikipedia);
      algorithms = algorithmsMap.get(wikipedia);
    }
    if (algorithms == null) {
      return null;
    }
    if ((errorNumber < 1) || (errorNumber >= MAX_ALGORITHMS)) {
      return null;
    }
    return algorithms.get(errorNumber - 1);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Algorithm active ?
   */
  public static boolean isAlgorithmActive(
      EnumWikipedia wikipedia, int errorNumber) {
    CheckErrorAlgorithm algorithm = getAlgorithm(wikipedia, errorNumber);
    if (algorithm == null) {
      return false;
    }
    int priority = algorithm.getPriority();
    return isPriorityActive(priority);
  }

  /**
   * Retrieve error priority from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Priority.
   */
  public static int getPriority(
      EnumWikipedia wikipedia, int errorNumber) {
    int errorPriority = PRIORITY_UNKOWN;
    String prioWiki = wikipedia.getCheckWikiProperty("prio", errorNumber, true, false, false);
    if (prioWiki != null) {
      try {
        errorPriority = Integer.parseInt(prioWiki);
      } catch (NumberFormatException e) {
        //
      }
    }
    if (errorPriority == PRIORITY_UNKOWN) {
      String prioScript = wikipedia.getCheckWikiProperty("prio", errorNumber, false, true, false);
      if (prioScript != null) {
        try {
          errorPriority = Integer.parseInt(prioScript);
        } catch (NumberFormatException e) {
          //
        }
      }
    }
    if (errorPriority == PRIORITY_DEACTIVATED) {
      String botOnly = wikipedia.getCheckWikiProperty("bot", errorNumber, true, true, false);
      if ((botOnly != null) && Boolean.valueOf(botOnly.trim())) {
        errorPriority = PRIORITY_BOT_ONLY;
      }
    }
    return errorPriority;
  }

  /**
   * @param priority Priority.
   * @return Flag indicating if the priority is active.
   */
  public static boolean isPriorityActive(int priority) {
    if ((priority == PRIORITY_TOP) ||
        (priority == PRIORITY_MIDDLE) ||
        (priority == PRIORITY_LOWEST) ||
        (priority == PRIORITY_BOT_ONLY)) {
      return true;
    }
    return false;
  }

  /**
   * Compare 2 priorities.
   * 
   * @param p1 Priority 1.
   * @param p2 Priority 2.
   * @return 0 if priorities are equal, -1 if p1 < p2, 1 if p1 > p2.
   */
  public static int comparePriority(int p1, int p2) {
    if (p1 == p2) {
      return 0;
    }
    if (p1 == PRIORITY_UNKOWN) {
      return -1;
    }
    if (p2 == PRIORITY_UNKOWN) {
      return 1;
    }
    if (p1 == PRIORITY_DEACTIVATED) {
      return -1;
    }
    if (p2 == PRIORITY_DEACTIVATED) {
      return 1;
    }
    return (p1 < p2) ? -1 : 1;
  }

  /**
   * @param priority Priority.
   * @return Textual description of the priority.
   */
  public static String getPriorityString(int priority) {
    switch (priority) {
    case PRIORITY_DEACTIVATED:
      return GT._("Deactivated");
    case PRIORITY_LOWEST:
      return GT._("Low priority");
    case PRIORITY_MIDDLE:
      return GT._("Middle priority");
    case PRIORITY_TOP:
      return GT._("Top priority");
    case PRIORITY_BOT_ONLY:
      return GT._("For Bot");
    default:
      return GT._("Priority unknown");
    }
  }

  /**
   * Retrieve error short description from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Short description.
   */
  public static String getShortDescription(
      EnumWikipedia wikipedia, int errorNumber) {
    return wikipedia.getCheckWikiProperty("head", errorNumber, true, true, false);
  }

  /**
   * Retrieve error long description from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Long description.
   */
  public static String getLongDescription(
      EnumWikipedia wikipedia, int errorNumber) {
    return wikipedia.getCheckWikiProperty("desc", errorNumber, true, true, false);
  }

  /**
   * Retrieve link to error description from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Link to error description.
   */
  public static String getLink(
      EnumWikipedia wikipedia, int errorNumber) {
    return wikipedia.getCheckWikiProperty("link", errorNumber, true, true, false);
  }

  /**
   * Retrieve white list from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return White list
   */
  public static String[] getWhiteList(
      EnumWikipedia wikipedia, int errorNumber) {
    String whiteListString = wikipedia.getCheckWikiProperty(
        "whitelist", errorNumber, true, false, false);
    if (whiteListString == null) {
      return null;
    }
    return wikipedia.convertPropertyToStringArray(whiteListString);
  }
}
