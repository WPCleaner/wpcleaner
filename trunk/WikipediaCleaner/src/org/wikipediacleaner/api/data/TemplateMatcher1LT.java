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

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.i18n.GT;


/**
 * A class for describing a matcher for template creating internal links directly from parameter value.
 * {{templateName|parameterName=parameterValue}} => [[parameterValue]]
 */
public class TemplateMatcher1LT extends TemplateMatcher {

  private final String parameterName;
  private final String parameterDefaultValue;
  private final String neededParameter;

  /**
   * @param wikipedia Wikipedia.
   * @param templateName Template name.
   * @param isGood Is good ?
   * @param parameterName Parameter name.
   * @param parameterDefaultValue Parameter default value.
   * @param neededParameter Parameter eventually required.
   */
  public TemplateMatcher1LT(
      EnumWikipedia wikipedia, String templateName,
      boolean isGood,
      String parameterName, String parameterDefaultValue,
      String neededParameter) {
    super(wikipedia, templateName, isGood, false);
    this.parameterName = parameterName;
    this.parameterDefaultValue = parameterDefaultValue;
    this.neededParameter = neededParameter;
  }

  /**
   * @param template Template being analyzed.
   * @return Link (if any) created by the template for this matcher.
   */
  @Override
  public String linksTo(PageElementTemplate template) {
    if ((template == null) || (parameterName == null)) {
      return null;
    }
    if (neededParameter != null) {
      String parameter = template.getParameterValue(neededParameter);
      if ((parameter == null) || (parameter.length() == 0)) {
        return null;
      }
    }
    return getParameterValue(template);
  }

  /**
   * @param template Template.
   * @return List of possible kinds of replacements.
   */
  @Override
  public List<String> getReplacements(PageElementTemplate template) {
    List<String> replacements = new ArrayList<String>();
    if (!isGood()) {
      replacements.add(GT._(
          "Replace parameter {0} with {1}",
          new Object[] {
              parameterName,
              "???" } ));
      String value = getParameterValue(template);
      if ((value != null) && (value.trim().length() > 0)) {
        replacements.add(GT._(
            "Replace parameter {0} with {1}",
            new Object[] {
                parameterName,
                "???{{" + getWikipedia().getPipeTemplate() + "}}" + value} ));
      }
    }
    return replacements;
  }

  /**
   * @param template Template.
   * @param index Replacement index.
   * @param text Replacement text.
   * @return Full replacement.
   */
  @Override
  public String getReplacement(
      PageElementTemplate template,
      int index, String text) {
    String parameterValue = null;
    switch (index) {
    case 0:
      parameterValue = text;
      break;
    case 1:
      parameterValue = text + "{{" + getWikipedia().getPipeTemplate() + "}}" + getParameterValue(template);
      break;
    }
    if (parameterValue == null) {
      return null;
    }
    return template.getParameterReplacement(parameterName, parameterValue, neededParameter);
  }

  /**
   * @param template Template
   * @return Parameter value.
   */
  private String getParameterValue(PageElementTemplate template) {
    String value = template.getParameterValue(parameterName);
    if (value == null) {
      value = parameterDefaultValue;
    }
    return value;
  }
}
