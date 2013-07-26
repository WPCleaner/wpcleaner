/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.i18n.GT;


/**
 * A class for describing a matcher for template creating internal links from parameter value.
 * {{templateName|parameter1=value1}} => [[value1]]
 * {{templateName|parameter1=value1|parameter2=value2}} => [[value1|value2]]
 */
public class TemplateMatcher1L2T extends TemplateMatcher {

  private final String parameterName1;
  private final String parameterName2;

  /**
   * @param wikipedia Wikipedia.
   * @param templateName Template name.
   * @param explanation Explanation.
   * @param parameterName1 First parameter name.
   * @param parameterName2 Second parameter name.
   */
  public TemplateMatcher1L2T(
      EnumWikipedia wikipedia, String templateName,
      String explanation,
      String parameterName1, String parameterName2) {
    super(wikipedia, templateName, explanation, false, false);
    this.parameterName1 = parameterName1;
    this.parameterName2 = parameterName2;
  }

  /**
   * @param page Page.
   * @param template Template being analyzed.
   * @return Link (if any) created by the template for this matcher.
   */
  @Override
  public String linksTo(Page page, PageElementTemplate template) {
    if ((template == null) || (parameterName1 == null) || (parameterName2 == null)) {
      return null;
    }
    return getParameterValue1(page, template);
  }

  /**
   * @param page Page.
   * @param template Template.
   * @return List of possible kinds of replacements.
   */
  @Override
  public List<String> getReplacements(Page page, PageElementTemplate template) {
    List<String> replacements = new ArrayList<String>();
    if (!isGood()) {
      replacements.add(GT._("Link parameter {0} to", parameterName1));
    }
    return replacements;
  }

  /**
   * @param page Page.
   * @param template Template.
   * @param index Replacement index.
   * @param text Replacement text.
   * @return Full replacement.
   */
  @Override
  public String getReplacement(
      Page page, PageElementTemplate template,
      int index, String text) {
    String parameterValue1 = text;
    String parameterValue2 = getParameterValue2(page, template);
    if (parameterValue2 == null) {
      parameterValue2 = getParameterValue1(page, template);
    }
    return template.getParameterReplacement(
        parameterName1, parameterValue1,
        parameterName2, parameterValue2);
  }

  /**
   * @param page Page.
   * @param template Template
   * @return Parameter value.
   */
  private String getParameterValue1(Page page, PageElementTemplate template) {
    String value = template.getParameterValue(parameterName1);
    return PageContents.expandText(page, value);
  }

  /**
   * @param page Page.
   * @param template Template
   * @return Parameter value.
   */
  private String getParameterValue2(Page page, PageElementTemplate template) {
    String value = template.getParameterValue(parameterName2);
    return PageContents.expandText(page, value);
  }
}
