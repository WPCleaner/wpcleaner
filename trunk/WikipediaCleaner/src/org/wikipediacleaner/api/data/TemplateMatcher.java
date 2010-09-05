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

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * A class for describing a matcher for template.
 */
public abstract class TemplateMatcher {

  private final EnumWikipedia wikipedia;
  private final String templateName;
  private final String explanation;
  private final boolean good;
  private final boolean helpNeeded;

  /**
   * @param wikipedia Wikipedia.
   * @param templateName Template name.
   * @param explanation Explanation.
   * @param isGood Is good ?
   * @param helpNeeded Is help needed ?
   */
  public TemplateMatcher(
      EnumWikipedia wikipedia,
      String templateName, String explanation,
      boolean isGood, boolean helpNeeded) {
    this.wikipedia = wikipedia;
    this.templateName = (templateName != null) ? Page.getStringUcFirst(templateName) : null;
    this.explanation = explanation;
    this.good = isGood;
    this.helpNeeded = helpNeeded;
  }

  /**
   * @param page Page.
   * @param template Template being analyzed.
   * @return Link (if any) created by the template for this matcher.
   */
  public abstract String linksTo(Page page, PageElementTemplate template);

  /**
   * @param page Page.
   * @param template Template.
   * @return List of possible kinds of replacements.
   */
  public abstract List<String> getReplacements(Page page, PageElementTemplate template);

  /**
   * @param page Page.
   * @param template Template.
   * @param index Replacement index.
   * @param text Replacement text.
   * @return Full replacement.
   */
  public abstract String getReplacement(
      Page page, PageElementTemplate template,
      int index, String text);

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /**
   * @return Template name.
   */
  public String getTemplateName() {
    return templateName;
  }

  /**
   * @return Explanation.
   */
  public String getExplanation() {
    return explanation;
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
