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
public class PageElementTemplate {
  private final String templateName;
  private final int beginIndex;
  private final List<Parameter> parameters;
  private final int endIndex;

  private static class Parameter {
    final String name;
    final int nameStartIndex;
    final String value;
    final int valueStartIndex;

    public Parameter(String name, int nameStartIndex, String value, int valueStartIndex) {
      this.name = name;
      this.nameStartIndex = nameStartIndex;
      this.value = value;
      this.valueStartIndex = valueStartIndex;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      if ((name != null) && (!name.isEmpty())) {
        return name + "=" + value;
      }
      return value;
    }
  }

  /**
   * Analyze contents to check if it matches a block for the given template name.
   * 
   * @param templateName Template name.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementTemplate analyzeBlock(String templateName, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }
    templateName = ((templateName != null) && (templateName.trim().length() > 0)) ? templateName.trim() : "";

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
    if (templateName.length() > 0) {
      String templateName1 = templateName;
      String templateName2 = templateName;
      if (templateName.length() > 1) {
        templateName1 = Character.toLowerCase(templateName.charAt(0)) + templateName.substring(1);
        templateName2 = Character.toUpperCase(templateName.charAt(0)) + templateName.substring(1);
      } else {
        templateName1 = templateName.toLowerCase();
        templateName2 = templateName.toUpperCase();
      }
      if ((!contents.startsWith(templateName1, tmpIndex)) &&
          (!contents.startsWith(templateName2, tmpIndex))) {
        return null;
      }
      tmpIndex += templateName.length();
    } else {
      int pipeIndex = contents.indexOf('|', tmpIndex);
      int endIndex = contents.indexOf("}}", tmpIndex);
      if ((pipeIndex < 0) && (endIndex < 0)) {
        return null;
      }
      int endNameindex = Math.min(
          (pipeIndex < 0) ? contents.length() : pipeIndex,
          (endIndex < 0) ? contents.length() : endIndex);
      templateName = contents.substring(tmpIndex, endNameindex).trim();
      tmpIndex = endNameindex;
    }
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
      int depth = 1;
      int endIndex = tmpIndex;
      while ((depth > 0) && (endIndex < contents.length())) {
        if (contents.startsWith("{{", endIndex)) {
          endIndex += 2;
          depth++;
        } else if (contents.startsWith("}}", endIndex)) {
          endIndex += 2;
          depth--;
        } else {
          endIndex++;
        }
      }
      if (depth > 0) {
        return null;
      }
      List<Parameter> parameters = new ArrayList<Parameter>();
      if (!analyzeTemplateParameters(
          contents.substring(tmpIndex, endIndex - 2), tmpIndex,
          parameters)) {
        return null;
      }
      return new PageElementTemplate(templateName, beginIndex, endIndex, parameters);
    } else if (contents.startsWith("}}", tmpIndex)) {
      return new PageElementTemplate(templateName, beginIndex, tmpIndex + 2, null);
    }
    return null;
  }

  /**
   * Analyze the parameters of template.
   * 
   * @param strParameters String containing the parameters.
   * @param offset Position of the String.
   * @param parameters Parameters.
   * @return Flag indicating if the analyze is correct.
   */
  private static boolean analyzeTemplateParameters(
      String strParameters, int offset,
      List<Parameter> parameters) {
    if ((strParameters == null) || (strParameters.trim().isEmpty())) {
      return true;
    }
    int beginIndex = 0;
    int tmpIndex = 0;
    int depthCurlyBrackets = 0;
    int depthSquareBrackets = 0;
    while (tmpIndex < strParameters.length()) {
      if (strParameters.startsWith("{{", tmpIndex)) {
        tmpIndex += 2;
        depthCurlyBrackets++;
      } else if (strParameters.startsWith("}}", tmpIndex)) {
        tmpIndex += 2;
        depthCurlyBrackets--;
      } else if (strParameters.startsWith("[[", tmpIndex)) {
        tmpIndex += 2;
        depthSquareBrackets++;
      } else if (strParameters.startsWith("]]", tmpIndex)) {
        tmpIndex += 2;
        depthSquareBrackets--;
      } else {
        if ((depthCurlyBrackets == 0) &&
            (depthSquareBrackets == 0) &&
            (strParameters.charAt(tmpIndex) == '|')) {
          addParameter(parameters, strParameters.substring(beginIndex, tmpIndex), offset + beginIndex);
          tmpIndex++;
          beginIndex = tmpIndex;
        } else {
          tmpIndex++;
        }
      }
    }
    addParameter(parameters, strParameters.substring(beginIndex), offset + beginIndex);
    return true;
  }

  private static void addParameter(
      List<Parameter> parameters, String parameter, int offset) {
    int equalIndex = parameter.indexOf('=');
    if (equalIndex < 0) {
      int spaces = 0;
      while ((spaces < parameter.length()) && (parameter.charAt(spaces) == ' ')) {
        spaces++;
      }
      parameters.add(new Parameter(
          "", offset + spaces, parameter.trim(), offset + spaces));
    } else {
      int spacesName = 0;
      while ((spacesName < equalIndex) && (parameter.charAt(spacesName) == ' ')) {
        spacesName++;
      }
      int spacesValue = equalIndex + 1;
      while ((spacesValue < parameter.length()) && (parameter.charAt(spacesValue) == ' ')) {
        spacesValue++;
      }
      parameters.add(new Parameter(
          parameter.substring(0, equalIndex).trim(), offset + spacesName,
          parameter.substring(spacesValue).trim(), offset + spacesValue));
    }
  }

  /**
   * @return Template name.
   */
  public String getTemplateName() {
    return templateName;
  }

  /**
   * Get parameter count.
   * 
   * @return Parameter count.
   */
  public int getParameterCount() {
    if (parameters == null) {
      return 0;
    }
    return parameters.size();
  }

  /**
   * Retrieve parameter name offset.
   * 
   * @param index Parameter index.
   * @return Parameter name offset.
   */
  public int getParameterNameOffset(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).nameStartIndex;
    }
    return 0;
  }

  /**
   * Retrieve parameter value.
   * 
   * @param index Parameter index.
   * @return Parameter value.
   */
  public String getParameterValue(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).value;
    }
    return null;
  }

  /**
   * Retrieve parameter value offset.
   * 
   * @param index Parameter index.
   * @return Parameter value offset.
   */
  public int getParameterValueOffset(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).valueStartIndex;
    }
    return 0;
  }

  /**
   * Retrieve parameter value.
   * 
   * @param name Parameter name.
   * @return Parameter value.
   */
  public String getParameterValue(String name) {
    int index = 0;
    while (index < parameters.size()) {
      if (name.equals(parameters.get(index).name)) {
        return parameters.get(index).value;
      }
      index++;
    }
    return null;
  }

  public int getBeginIndex() {
    return beginIndex;
  }

  public int getEndIndex() {
    return endIndex;
  }

  private PageElementTemplate(
      String templateName,
      int beginIndex, int endIndex,
      List<Parameter> parameters) {
    this.templateName = templateName;
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
    this.parameters = parameters;
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
    while (index < parameters.size()) {
      sb.append('|');
      String parameterName = parameters.get(index).name;
      if ((parameterName != null) && (!parameterName.trim().isEmpty())) {
        sb.append(parameters.get(index).name);
        sb.append('=');
      }
      sb.append(parameters.get(index).value);
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
