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

package org.wikipediacleaner.api.data;


/**
 * A class for describing a matcher for template.
 */
public abstract class TemplateMatcher {

  private final String templateName;
  private final boolean good;
  private final boolean helpNeeded;

  /**
   * @param templateName Template name.
   */
  public TemplateMatcher(
      String templateName,
      boolean isGood, boolean helpNeeded) {
    this.templateName = templateName;
    this.good = isGood;
    this.helpNeeded = helpNeeded;
  }

  /**
   * @param template Template being analyzed.
   * @return Link (if any) created by the template for this matcher.
   */
  public abstract String linksTo(PageElementTemplate template);

  /**
   * @return Template name.
   */
  public String getTemplateName() {
    return templateName;
  }

  /**
   * @return Is good ?
   */
  public boolean isGood() {
    return good;
  }

  /**
   * @return Is help needed ?
   */
  public boolean isHelpNeeded() {
    return helpNeeded;
  }
}
