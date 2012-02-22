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

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * Configuration for an error in Check Wiki project.
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
   * @param general Flag indicating if dealing with a general property or a wiki property.
   */
  void addProperty(String name, String value, boolean general) {
    if (name == null) {
      return;
    }
    if (value != null) {
      value = value.trim();
    }
    if (name.equals("prio")) {
      setPriority(value, general);
    } else if (name.equals("bot")) {
      setBot(value, general);
    } else if (name.equals("head")) {
      setShortDescription(value, general);
    } else if (name.equals("desc")) {
      setLongDescription(value, general);
    } else if (name.equals("link")) {
      setLink(value, general);
    } else if (name.equals("whitelist")) {
      setWhiteList(value, general);
    } else if (name.equals("whitelistpage")) {
      setWhiteListPageName(value, general);
    } else {
      if (general) {
        if (value == null) {
          generalConfiguration.remove(name);
        } else {
          generalConfiguration.setProperty(name, value);
        }
      } else {
        if (value == null) {
          wikiConfiguration.remove(name);
        } else {
          wikiConfiguration.setProperty(name, value);
        }
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
   * Flag indicating if error should be considered by bots even if deactivated.
   */
  private boolean botWiki = false;

  /**
   * @param value Priority value.
   * @param general Flag indicating if general configuration is concerned.
   */
  private void setPriority(String value, boolean general) {
    Integer intValue = null;
    if (value != null) {
      try {
        intValue = Integer.valueOf(Integer.parseInt(value));
      } catch (NumberFormatException e) {
        //
      }
    }
    if (general) {
      priorityGeneral = intValue;
    } else {
      priorityWiki = intValue;
    }
  }

  /**
   * @param value Value.
   * @param general Flag indicating if general configuration is concerned.
   */
  private void setBot(String value, boolean general) {
    if (general) {
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
    if (priorityWiki != null) {
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
   * @param value Short description value.
   * @param general Flag indicating if general configuration is concerned.
   */
  private void setShortDescription(String value, boolean general) {
    String replaced = value;
    if (replaced != null) {
      replaced = replaced.replaceAll("&lt;", "<");
      replaced = replaced.replaceAll("&gt;", ">");
    }
    if (general) {
      shortDescriptionGeneral = value;
      shortDescriptionReplacedGeneral = replaced;
    } else {
      shortDescriptionWiki = value;
      shortDescriptionReplacedWiki = replaced;
    }
  }

  /**
   * @return Short description.
   */
  public String getShortDescription() {
    if ((shortDescriptionWiki != null) && (shortDescriptionWiki.length() > 0)) {
      return shortDescriptionWiki;
    }
    return shortDescriptionGeneral;
  }

  /**
   * @return Short description.
   */
  public String getShortDescriptionReplaced() {
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
   * @param value Long description value.
   * @param general Flag indicating if general configuration is concerned.
   */
  private void setLongDescription(String value, boolean general) {
    if (general) {
      longDescriptionGeneral = value;
    } else {
      longDescriptionWiki = value;
    }
  }

  /**
   * @return Long description.
   */
  public String getLongDescription() {
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
   * @param general Flag indicating if general configuration is concerned.
   */
  private void setLink(String value, boolean general) {
    if (general) {
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
   * @param general Flag indicating if general configuration is concerned.
   */
  private void setWhiteListPageName(String value, boolean general) {
    if (general) {
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
   * @param general Flag indicating if general configuration is concerned.
   */
  private void setWhiteList(String value, boolean general) {
    if (general) {
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
