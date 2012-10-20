/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

package org.wikipediacleaner.api.constants;

import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.wikipediacleaner.api.constants.CWConfiguration.Origin;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * Configuration for an error in Check Wiki project.
 */
/**
 * 
 */
public class CWConfigurationError {

  /**
   * Error number.
   */
  private final int errorNumber;

  /**
   * @param errorNumber Error number.
   */
  CWConfigurationError(int errorNumber) {
    this.errorNumber = errorNumber;
    this.generalConfiguration = new Properties();
    this.wikiConfiguration = new Properties();
  }

  /**
   * @return Error number.
   */
  public int getErrorNumber() {
    return errorNumber;
  }

  private final Properties generalConfiguration;
  private final Properties wikiConfiguration;

  /**
   * Clear general configuration.
   */
  void clearGeneralConfiguration() {
    generalConfiguration.clear();
    priorityGeneral = null;
  }

  /**
   * Clear wiki configuration.
   */
  void clearWikiConfiguration() {
    wikiConfiguration.clear();
    priorityWiki = null;
    botWiki = false;
  }

  /**
   * @param name Property name.
   * @param value Property value.
   * @param origin Origin of the configuration.
   */
  void addProperty(
      String name, String value,
      CWConfiguration.Origin origin) {
    if (name == null) {
      return;
    }
    if (value != null) {
      value = value.trim();
    }
    if (name.equals("prio")) {
      setPriority(value, origin);
    } else if (name.equals("bot")) {
      setBot(value, origin);
    } else if (name.equals("head")) {
      setShortDescription(value, origin);
    } else if (name.equals("desc")) {
      setLongDescription(value, origin);
    } else if (name.equals("link")) {
      setLink(value, origin);
    } else if (name.equals("whitelist")) {
      setWhiteList(value, origin);
    } else if (name.equals("whitelistpage")) {
      setWhiteListPageName(value, origin);
    } else {
      switch (origin) {
      case GENERAL_CONFIGURATION:
        if (value == null) {
          generalConfiguration.remove(name);
        } else {
          generalConfiguration.setProperty(name, value);
        }
        break;

      case WIKI_CONFIGURATION:
        if (value == null) {
          wikiConfiguration.remove(name);
        } else {
          wikiConfiguration.setProperty(name, value);
        }
        break;
      }
    }
  }

  /**
   * @param name Property name.
   * @param useWiki Flag indicating if wiki configuration can be used.
   * @param useGeneral Flag indicating if general configuration can be used.
   * @param acceptEmpty Flag indicating if empty strings are accepted.
   * @return Property value.
   */
  public String getSpecificProperty(
      String name,
      boolean useWiki, boolean useGeneral, boolean acceptEmpty) {
    String result = null;
    if (useWiki) {
      result = wikiConfiguration.getProperty(name);
      if ((result != null) && (acceptEmpty || (result.length() > 0))) {
        return result;
      }
    }
    if (useGeneral) {
      result = generalConfiguration.getProperty(name);
    }
    return result;
  }

  /* ================================================================================= */
  /* Priority                                                                          */
  /* ================================================================================= */

  public final static int PRIORITY_UNKOWN = -1;
  public final static int PRIORITY_DEACTIVATED = 0;
  public final static int PRIORITY_TOP = 1;
  public final static int PRIORITY_MIDDLE = 2;
  public final static int PRIORITY_LOWEST = 3;
  public final static int PRIORITY_BOT_ONLY = 4;

  /**
   * Priority (general configuration).
   */
  private Integer priorityGeneral = null;

  /**
   * Priority (wiki configuration).
   */
  private Integer priorityWiki = null;

  /**
   * Priority (user configuration).
   */
  private Integer priorityUser = null;

  /**
   * Flag indicating if error should be considered by bots even if deactivated.
   */
  private boolean botWiki = false;

  /**
   * @param value Priority value.
   * @param origin Origin of the configuration.
   */
  private void setPriority(String value, Origin origin) {
    Integer intValue = null;
    if (value != null) {
      try {
        intValue = Integer.valueOf(Integer.parseInt(value));
      } catch (NumberFormatException e) {
        //
      }
    }
    switch (origin) {
    case GENERAL_CONFIGURATION:
      priorityGeneral = intValue;
      break;
    case WIKI_CONFIGURATION:
      priorityWiki = intValue;
      break;
    case USER_CONFIGURATION:
      priorityUser = intValue;
      break;
    }
  }

  /**
   * @param value Value.
   * @param origin Origin of the configuration.
   */
  private void setBot(String value, Origin origin) {
    if (origin == Origin.GENERAL_CONFIGURATION) {
      return;
    }
    if (value == null) {
      botWiki = false;
    } else {
      botWiki = Boolean.valueOf(value);
    }
  }

  /**
   * @return Error priority.
   */
  public int getPriority() {
    int priority = PRIORITY_UNKOWN;
    if ((priority == PRIORITY_UNKOWN) && (priorityUser != null)) {
      priority = priorityUser.intValue();
    }
    if ((priority == PRIORITY_UNKOWN) && (priorityWiki != null)) {
      priority = priorityWiki.intValue();
    }
    if ((priority == PRIORITY_UNKOWN) && (priorityGeneral != null)) {
      priority = priorityGeneral.intValue();
    }
    if ((priority == PRIORITY_DEACTIVATED) && botWiki) {
      priority = PRIORITY_BOT_ONLY;
    }
    return priority;
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
   * @param priority Priority.
   * @return Flag indicating if the priority is fully active.
   */
  public static boolean isPriorityFullyActive(int priority) {
    if ((priority == PRIORITY_TOP) ||
        (priority == PRIORITY_MIDDLE) ||
        (priority == PRIORITY_LOWEST)) {
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

  /* ================================================================================= */
  /* Short description                                                                 */
  /* ================================================================================= */

  /**
   * Short description of the error (general configuration).
   */
  private String shortDescriptionGeneral = null;
  private String shortDescriptionReplacedGeneral = null;

  /**
   * Short description of the error (wiki configuration).
   */
  private String shortDescriptionWiki = null;
  private String shortDescriptionReplacedWiki = null;

  /**
   * Short description of the error (user configuration).
   */
  private String shortDescriptionUser = null;
  private String shortDescriptionReplacedUser = null;

  /**
   * @param value Short description value.
   * @param origin Origin of the configuration.
   */
  private void setShortDescription(String value, Origin origin) {
    String replaced = value;
    if (replaced != null) {
      replaced = replaced.replaceAll("&lt;", "<");
      replaced = replaced.replaceAll("&gt;", ">");
    }
    switch (origin) {
    case GENERAL_CONFIGURATION:
      shortDescriptionGeneral = value;
      shortDescriptionReplacedGeneral = replaced;
      break;
    case WIKI_CONFIGURATION:
      shortDescriptionWiki = value;
      shortDescriptionReplacedWiki = replaced;
      break;
    case USER_CONFIGURATION:
      shortDescriptionUser = value;
      shortDescriptionReplacedUser = replaced;
      break;
    }
  }

  /**
   * @return Short description.
   */
  public String getShortDescription() {
    if ((shortDescriptionUser != null) && (shortDescriptionUser.length() > 0)) {
      return shortDescriptionUser;
    }
    if ((shortDescriptionWiki != null) && (shortDescriptionWiki.length() > 0)) {
      return shortDescriptionWiki;
    }
    return shortDescriptionGeneral;
  }

  /**
   * @return Short description.
   */
  public String getShortDescriptionReplaced() {
    if ((shortDescriptionReplacedUser != null) &&
        (shortDescriptionReplacedUser.length() > 0)) {
      return shortDescriptionReplacedUser;
    }
    if ((shortDescriptionReplacedWiki != null) &&
        (shortDescriptionReplacedWiki.length() > 0)) {
      return shortDescriptionReplacedWiki;
    }
    return shortDescriptionReplacedGeneral;
  }

  /* ================================================================================= */
  /* Long description                                                                 */
  /* ================================================================================= */

  /**
   * Long description of the error (general configuration).
   */
  private String longDescriptionGeneral = null;

  /**
   * Long description of the error (wiki configuration).
   */
  private String longDescriptionWiki = null;

  /**
   * Long description of the error (user configuration).
   */
  private String longDescriptionUser = null;

  /**
   * @param value Long description value.
   * @param origin Origin of the configuration.
   */
  private void setLongDescription(String value, Origin origin) {
    switch (origin) {
    case GENERAL_CONFIGURATION:
      longDescriptionGeneral = value;
      break;
    case WIKI_CONFIGURATION:
      longDescriptionWiki = value;
      break;
    case USER_CONFIGURATION:
      longDescriptionUser = value;
      break;
    }
  }

  /**
   * @return Long description.
   */
  public String getLongDescription() {
    if ((longDescriptionUser != null) && (longDescriptionUser.length() > 0)) {
      return longDescriptionUser;
    }
    if ((longDescriptionWiki != null) && (longDescriptionWiki.length() > 0)) {
      return longDescriptionWiki;
    }
    return longDescriptionGeneral;
  }

  /* ================================================================================= */
  /* Link                                                                              */
  /* ================================================================================= */

  /**
   * Link to page describing the error.
   */
  private String linkWiki = null;

  /**
   * @param value Link value.
   * @param origin Origin of the configuration.
   */
  private void setLink(String value, Origin origin) {
    if (origin == Origin.GENERAL_CONFIGURATION) {
      return;
    }
    linkWiki = value;
  }

  /**
   * @return Link to page describing the error.
   */
  public String getLink() {
    return linkWiki;
  }

  /* ================================================================================= */
  /* White List                                                                        */
  /* ================================================================================= */

  /**
   * Page containing the white list.
   */
  private String whiteListPageName = null;

  /**
   * @param value Page containing the white list.
   * @param origin Origin of the configuration.
   */
  private void setWhiteListPageName(String value, Origin origin) {
    if (origin == Origin.GENERAL_CONFIGURATION) {
      return;
    }
    whiteListPageName = value;
  }

  /**
   * @return Page containing the white list.
   */
  public String getWhiteListPageName() {
    return whiteListPageName;
  }

  /**
   * White list.
   */
  private Set<String> whiteListPages = null;

  /**
   * @param whiteListPage Page containing the white list.
   */
  public void setWhiteList(Page whiteListPage) {
    List<Page> pages = whiteListPage.getLinks();
    whiteListPages = null;
    if ((pages != null) && (pages.size() > 0)) {
      whiteListPages = new HashSet<String>();
      for (Page page : pages) {
        whiteListPages.add(page.getTitle());
      }
    }
  }

  /**
   * White list.
   */
  private Set<String> whiteListWiki = null;

  /**
   * @param value White list value.
   * @param origin Origin of the configuration.
   */
  private void setWhiteList(String value, Origin origin) {
    if (origin == Origin.GENERAL_CONFIGURATION) {
      return;
    }
    whiteListWiki = null;
    if (value != null) {
      String[] tmp = EnumWikipedia.convertPropertyToStringArray(value);
      if (tmp != null) {
        whiteListWiki = new HashSet<String>();
        for (int i = 0; i < tmp.length; i++) {
          whiteListWiki.add(tmp[i]);
        }
      }
    }
  }

  /**
   * @return White list.
   */
  private Set<String> getWhiteList() {
    if (whiteListPages != null) {
      return whiteListPages;
    }
    return whiteListWiki;
  }

  /**
   * Tell if a page is among the white list.
   * 
   * @param title Page title.
   * @return Page among the white list ?
   */
  public boolean isInWhiteList(String title) {
    Set<String> whiteList = getWhiteList();
    if ((whiteList == null) || (title == null)) {
      return false;
    }
    return whiteList.contains(title);
  }
}
