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
 * A class for describing a matcher for template creating internal links directly from parameter value.
 */
public class TemplateMatcherDirectInternalLink extends TemplateMatcher {

  private final String parameterName;
  private final String parameterDefaultValue;
  private final String neededParameter;

  /**
   * @param templateName Template name.
   * @param parameterName Parameter name.
   * @param parameterDefaultValue Parameter default value.
   * @param neededParameter Parameter eventually required.
   */
  public TemplateMatcherDirectInternalLink(
      String templateName, String parameterName,
      String parameterDefaultValue,
      String neededParameter) {
    super(templateName, false, false);
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
    String value = template.getParameterValue(parameterName);
    if (value == null) {
      value = parameterDefaultValue;
    }
    return value;
  }
}
