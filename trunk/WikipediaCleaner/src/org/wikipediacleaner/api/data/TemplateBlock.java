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


/**
 * Class containing informations about a complete template ({{<i>template</i>|...}}). 
 */
public class TemplateBlock {
  private final String templateName;
  private final int beginIndex;
  private final List<String> parameterNames;
  private final List<String> parameterValues;
  private final int endIndex;

  /**
   * Analyze contents to check if it matches a block for the given template name.
   * 
   * @param templateName Template name.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static TemplateBlock analyzeBlock(String templateName, String contents, int index) {
    // Verify arguments
    if ((templateName == null) || (templateName.isEmpty()) || (contents == null)) {
      return null;
    }
    String templateName1 = Character.toLowerCase(templateName.charAt(0)) + templateName.substring(1);
    String templateName2 = Character.toUpperCase(templateName.charAt(0)) + templateName.substring(1);

    // Look for '{{'
    int beginIndex = index;
    int tmpIndex = beginIndex;
    if ((tmpIndex >= contents.length() - 1) ||
        (contents.charAt(tmpIndex) != '{') ||
        (contents.charAt(tmpIndex + 1) != '{')) {
      return null;
    }
    tmpIndex += 2;

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Check Template Name
    if (tmpIndex >= contents.length()) {
      return null;
    }
    if ((!contents.startsWith(templateName1, tmpIndex)) &&
        (!contents.startsWith(templateName2, tmpIndex))) {
      return null;
    }
    tmpIndex += templateName.length();
    if (tmpIndex >= contents.length()) {
      return null;
    }

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }
    if (tmpIndex >= contents.length()) {
      return null;
    }

    // Check if parameters are present
    if (contents.charAt(tmpIndex) == '|') {
      tmpIndex++;
      int endIndex = contents.indexOf("}}", tmpIndex);
      if (endIndex < 0) {
        return null;
      }
      List<String> parameterNames = new ArrayList<String>();
      List<String> parameterValues = new ArrayList<String>();
      if (!analyzeTemplateParameters(
          contents.substring(tmpIndex, endIndex),
          parameterNames, parameterValues)) {
        return null;
      }
      return new TemplateBlock(templateName, beginIndex, endIndex + 2, parameterNames, parameterValues);
    } else if (contents.startsWith("}}", tmpIndex)) {
      return new TemplateBlock(templateName, beginIndex, tmpIndex + 2, null, null);
    }
    return null;
  }

  /**
   * Analyze the parameters of template.
   * 
   * @param parameters String containing the parameters.
   * @param parameterNames Parameter names.
   * @param parameterValues Parameter values.
   * @return Flag indicating if the analyze is correct.
   */
  private static boolean analyzeTemplateParameters(
      String parameters,
      List<String> parameterNames, List<String> parameterValues) {
    if ((parameters == null) || (parameters.trim().isEmpty())) {
      return true;
    }
    parameters = parameters.trim();
    String[] params = parameters.split("|");
    for (int i = 0; i < params.length; i++) {
      String param = params[i].trim();
      int pipeIndex = param.indexOf('=');
      if (pipeIndex < 0) {
        parameterNames.add("");
        parameterValues.add(param);
      } else {
        parameterNames.add(param.substring(0, pipeIndex));
        parameterValues.add(param.substring(pipeIndex + 1));
      }
    }
    return true;
  }

  /**
   * Retrieve parameter value.
   * 
   * @param name Parameter name.
   * @return Parameter value.
   */
  public String getParameter(String name) {
    int index = 0;
    while ((index < parameterNames.size()) && (index < parameterValues.size())) {
      if (name.endsWith(parameterNames.get(index))) {
        return parameterValues.get(index);
      }
    }
    return null;
  }

  public int getBeginIndex() {
    return beginIndex;
  }

  public int getEndIndex() {
    return endIndex;
  }

  private TemplateBlock(
      String templateName,
      int beginIndex, int endIndex,
      List<String> parameterNames, List<String> parameterValues) {
    this.templateName = templateName;
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
    this.parameterNames = parameterNames;
    this.parameterValues = parameterValues;
  }

  public String getPartBeforeParameters() {
    StringBuilder sb = new StringBuilder();
    sb.append("{{");
    sb.append(templateName);
    return sb.toString();
  }

  public String getPartFromParameters() {
    StringBuilder sb = new StringBuilder();
    int index = 0;
    while ((index < parameterNames.size()) && (index < parameterValues.size())) {
      sb.append('|');
      String parameterName = parameterNames.get(index);
      if ((parameterName != null) && (!parameterName.trim().isEmpty())) {
        sb.append(parameterNames.get(index));
        sb.append('=');
      }
      sb.append(parameterValues.get(index));
    }
    sb.append("}}");
    return sb.toString();
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(getPartBeforeParameters());
    sb.append(getPartFromParameters());
    return sb.toString();
  }
}
