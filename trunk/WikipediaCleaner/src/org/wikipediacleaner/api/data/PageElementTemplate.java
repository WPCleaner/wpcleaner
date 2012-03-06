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


/**
 * Class containing information about a complete template ({{<i>template</i>|...}}). 
 */
public class PageElementTemplate extends PageElement {

  private final String templateName;
  private final String templateNameNotTrimmed;
  private final List<Parameter> parameters;

  private static class Parameter {
    final String name;
    final String nameNotTrimmed;
    final int nameStartIndex;
    final String value;
    final String valueNotTrimmed;
    final int valueStartIndex;

    public Parameter(String name, int nameStartIndex, String value, int valueStartIndex) {
      this.nameNotTrimmed = name;
      this.name = (name != null) ? name.trim() : null;
      this.nameStartIndex = nameStartIndex;
      this.valueNotTrimmed = value;
      this.value = (value != null) ? value.trim() : null;
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
   * @param wikipedia Wikipedia.
   * @param templateName Template name.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementTemplate analyzeBlock(
      EnumWikipedia wikipedia, String templateName,
      String contents, int index) {
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
    int startTemplateName = tmpIndex;

    // Possible whitespace characters
    while ((tmpIndex < contents.length()) &&
           ((contents.charAt(tmpIndex) == ' ') ||
            (contents.charAt(tmpIndex) == '\n'))) {
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

    // Possible whitespace characters
    while ((tmpIndex < contents.length()) &&
           ((contents.charAt(tmpIndex) == ' ') ||
            (contents.charAt(tmpIndex) == '\n'))) {
      tmpIndex++;
    }
    if (tmpIndex >= contents.length()) {
      return null;
    }
    int endTemplateName = tmpIndex;
    templateName = contents.substring(startTemplateName, endTemplateName);

    // Check that it's not a DEFAULTSORT
    int colonIndex = templateName.indexOf(':');
    if (colonIndex > 0) {
      MagicWord magicDefaultsort = wikipedia.getMagicWord(MagicWord.DEFAULT_SORT);
      if ((magicDefaultsort != null) &&
          (magicDefaultsort.isPossibleAlias(templateName.substring(0, colonIndex + 1)))) {
        return null;
      }
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
      return new PageElementTemplate(
          templateName,
          beginIndex, endIndex, parameters);
    } else if (contents.startsWith("}}", tmpIndex)) {
      return new PageElementTemplate(
          templateName,
          beginIndex, tmpIndex + 2, null);
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
    if ((strParameters == null) || (strParameters.trim().length() == 0)) {
      return true;
    }
    int beginIndex = 0;
    int tmpIndex = 0;
    int depthCurlyBrackets = 0;
    int depthSquareBrackets = 0;
    int depthNoWikiTag = 0;
    int depthRefTag = 0;
    int equalIndex = -1;
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
      } else if (strParameters.startsWith("<", tmpIndex)) {
        PageElementTag tag = PageElementTag.analyzeBlock(strParameters, tmpIndex);
        if (tag != null) {
          int count = 0;
          if (tag.isFullTag()) {
            count = 0;
          } else if (tag.isEndTag()) {
            count = -1;
          } else {
            count = 1;
          }
          if (PageElementTag.TAG_NOWIKI.equals(tag.getName())) {
            depthNoWikiTag += count;
          } else if (PageElementTag.TAG_REF.equals(tag.getName())) {
            depthRefTag += count;
          }
          tmpIndex = tag.getEndIndex();
        } else {
          tmpIndex++;
        }
      } else {
        if ((depthCurlyBrackets <= 0) &&
            (depthSquareBrackets <= 0) &&
            (depthNoWikiTag <= 0) &&
            (depthRefTag <= 0) &&
            (equalIndex < 0) &&
            (strParameters.charAt(tmpIndex) == '=')) {
          equalIndex = tmpIndex;
          tmpIndex++;
        } else if ((depthCurlyBrackets <= 0) &&
            (depthSquareBrackets <= 0) &&
            (depthNoWikiTag <= 0) &&
            (depthRefTag <= 0) &&
            (strParameters.charAt(tmpIndex) == '|')) {
          depthCurlyBrackets = 0;
          depthSquareBrackets = 0;
          addParameter(
              parameters, strParameters.substring(beginIndex, tmpIndex),
              equalIndex - beginIndex, offset + beginIndex);
          tmpIndex++;
          equalIndex = -1;
          beginIndex = tmpIndex;
        } else {
          tmpIndex++;
        }
      }
    }
    addParameter(
        parameters, strParameters.substring(beginIndex),
        equalIndex - beginIndex, offset + beginIndex);
    return true;
  }

  private static void addParameter(
      List<Parameter> parameters, String parameter,
      int equalIndex, int offset) {
    if (equalIndex < 0) {
      int spaces = 0;
      while ((spaces < parameter.length()) && (Character.isWhitespace(parameter.charAt(spaces)))) {
        spaces++;
      }
      parameters.add(new Parameter(
          "", offset + spaces, parameter, offset + spaces));
    } else {
      int spacesName = 0;
      while ((spacesName < equalIndex) && (Character.isWhitespace(parameter.charAt(spacesName)))) {
        spacesName++;
      }
      int spacesValue = equalIndex + 1;
      while ((spacesValue < parameter.length()) && (Character.isWhitespace(parameter.charAt(spacesValue)))) {
        spacesValue++;
      }
      parameters.add(new Parameter(
          parameter.substring(0, equalIndex), offset + spacesName,
          parameter.substring(equalIndex + 1), offset + spacesValue));
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
    if (parameters == null) {
      return null;
    }
    int index = 0;
    int paramNum = 1;
    while (index < parameters.size()) {
      String parameterName = parameters.get(index).name;
      if ((parameterName == null) || (parameterName.length() == 0)) {
        parameterName = Integer.toString(paramNum);
      }
      if (parameterName.equals(Integer.toString(paramNum))) {
        paramNum++;
      }
      if (name.equals(parameterName)) {
        return parameters.get(index).value;
      }
      index++;
    }
    return null;
  }

  private PageElementTemplate(
      String templateName,
      int beginIndex, int endIndex,
      List<Parameter> parameters) {
    super(beginIndex, endIndex);
    this.templateNameNotTrimmed = templateName;
    this.templateName = (templateName != null) ? Page.getStringUcFirst(templateName.trim()) : null;
    this.parameters = parameters;
  }

  private void addPartBeforeParameters(StringBuilder sb) {
    sb.append("{{");
    sb.append(templateNameNotTrimmed);
  }

  private void addPartFromParameters(StringBuilder sb) {
    for (Parameter parameter : parameters) {
      addParameter(sb, parameter.name, parameter.value);
    }
    sb.append("}}");
  }

  private void addParameter(StringBuilder sb, String parameterName, String parameterValue) {
    sb.append('|');
    if ((parameterName != null) && (parameterName.trim().length() > 0)) {
      sb.append(parameterName);
      sb.append('=');
    }
    sb.append(parameterValue);
  }

  /**
   * Create a template with a parameter value modified.
   * 
   * @param parameterName Parameter name that needs to be modified.
   * @param parameterValue New parameter value.
   * @param previousParameter Previous parameter.
   * @return Complete template with parameter value replaced.
   */
  public String getParameterReplacement(
      String parameterName, String parameterValue, String previousParameter) {
    boolean parameterExist = false;
    if (parameters != null) {
      for (Parameter parameter : parameters) {
        if (parameter.name.equals(parameterName)) {
          parameterExist = true;
        }
      }
    }
    StringBuilder sb = new StringBuilder();
    addPartBeforeParameters(sb);
    boolean parameterAdded = false;
    String tmpParameterName = parameterName;
    String tmpParameterValue = parameterValue;
    int paramNum = 1;
    if (parameters != null) {
      for (Parameter parameter : parameters) {
  
        // Managing unname
        String currentParameterName = parameter.name;
        if ((currentParameterName == null) || (currentParameterName.length() == 0)) {
          currentParameterName = Integer.toString(paramNum);
        }
        int tmpParamNum = paramNum;
        if (currentParameterName.equals(Integer.toString(paramNum))) {
          tmpParamNum++;
        }
  
        // Manage whitespace characters before/after name/value
        tmpParameterName = parameterName;
        tmpParameterValue = parameterValue;
        if ((parameter.name != null) && (parameter.name.length() > 0)) {
          // Whitespace characters before name
          int spaces = 0;
          while ((spaces < parameter.nameNotTrimmed.length()) &&
                 (Character.isWhitespace(parameter.nameNotTrimmed.charAt(spaces)))) {
            spaces++;
          }
          if (spaces > 0) {
            tmpParameterName = parameter.nameNotTrimmed.substring(0, spaces) + parameterName;
          }
  
          // Whitespace characters after name
          spaces = parameter.nameNotTrimmed.length();
          while ((spaces > 0) &&
                 (Character.isWhitespace(parameter.nameNotTrimmed.charAt(spaces - 1)))) {
            spaces--;
          }
          if (spaces < parameter.nameNotTrimmed.length()) {
            tmpParameterName += parameter.nameNotTrimmed.substring(spaces);
          }
        }
  
        if (parameter.value != null) {
          // Whitespace characters before value
          int spaces = 0;
          while ((spaces < parameter.valueNotTrimmed.length()) &&
                 (Character.isWhitespace(parameter.valueNotTrimmed.charAt(spaces)))) {
            spaces++;
          }
          if ((spaces > 0) && (tmpParameterValue != null)) {
            tmpParameterValue = parameter.valueNotTrimmed.substring(0, spaces) + parameterValue;
          }
  
          // Whitespace characters after value
          spaces = parameter.valueNotTrimmed.length();
          while ((spaces > 0) &&
                 (Character.isWhitespace(parameter.valueNotTrimmed.charAt(spaces - 1)))) {
            spaces--;
          }
          if ((spaces < parameter.valueNotTrimmed.length()) && (tmpParameterValue != null)) {
            tmpParameterValue += parameter.valueNotTrimmed.substring(spaces);
          }
        }
  
        // Add parameter
        if (currentParameterName.equals(parameterName)) {
          if (tmpParameterValue != null) {
            addParameter(sb, parameter.nameNotTrimmed, tmpParameterValue);
            paramNum = tmpParamNum;
          }
          parameterAdded = true;
        } else if ((!parameterExist) &&
                   (currentParameterName.equals(previousParameter))) {
          addParameter(sb, parameter.nameNotTrimmed, parameter.valueNotTrimmed);
          addParameter(sb, tmpParameterName, tmpParameterValue);
          paramNum = tmpParamNum;
          parameterAdded = true;
        } else {
          addParameter(sb, parameter.nameNotTrimmed, parameter.valueNotTrimmed);
          paramNum = tmpParamNum;
        }
      }
    }
    if (!parameterAdded) {
      if (tmpParameterName.equals(Integer.toString(paramNum))) {
        addParameter(sb, null, tmpParameterValue);
      } else {
        addParameter(sb, tmpParameterName, tmpParameterValue);
      }
    }
    sb.append("}}");
    return sb.toString();
  }

  /**
   * Create a template with 2 parameter values modified.
   * 
   * @param parameterName1 Parameter name that needs to be modified.
   * @param parameterValue1 New parameter value.
   * @param parameterName2 Parameter name that needs to be modified.
   * @param parameterValue2 New parameter value.
   * @return Complete template with parameter value replaced.
   */
  public String getParameterReplacement(
      String parameterName1, String parameterValue1,
      String parameterName2, String parameterValue2) {
    boolean parameterExist1 = false;
    boolean parameterExist2 = false;
    for (Parameter parameter : parameters) {
      if (parameter.name.equals(parameterName1)) {
        parameterExist1 = true;
      }
      if (parameter.name.equals(parameterName2)) {
        parameterExist2 = true;
      }
    }
    StringBuilder sb = new StringBuilder();
    addPartBeforeParameters(sb);
    boolean parameterAdded1 = false;
    boolean parameterAdded2 = false;
    String tmpParameterName1 = parameterName1;
    String tmpParameterValue1 = parameterValue1;
    String tmpParameterName2 = parameterName2;
    String tmpParameterValue2 = parameterValue2;
    int paramNum = 1;
    for (Parameter parameter : parameters) {

      // Managing unname
      String currentParameterName = parameter.name;
      if ((currentParameterName == null) || (currentParameterName.length() == 0)) {
        currentParameterName = Integer.toString(paramNum);
      }
      if (currentParameterName.equals(Integer.toString(paramNum))) {
        paramNum++;
      }

      // Manage whitespace characters before/after name/value
      tmpParameterName1 = parameterName1;
      tmpParameterValue1 = parameterValue1;
      if ((parameter.name != null) && (parameter.name.length() > 0)) {
        // Whitespace characters before name
        int spaces = 0;
        while ((spaces < parameter.nameNotTrimmed.length()) &&
               (Character.isWhitespace(parameter.nameNotTrimmed.charAt(spaces)))) {
          spaces++;
        }
        if (spaces > 0) {
          tmpParameterName1 = parameter.nameNotTrimmed.substring(0, spaces) + parameterName1;
          tmpParameterName2 = parameter.nameNotTrimmed.substring(0, spaces) + parameterName2;
        }

        // Whitespace characters after name
        spaces = parameter.nameNotTrimmed.length();
        while ((spaces > 0) &&
               (Character.isWhitespace(parameter.nameNotTrimmed.charAt(spaces - 1)))) {
          spaces--;
        }
        if (spaces < parameter.nameNotTrimmed.length()) {
          tmpParameterName1 += parameter.nameNotTrimmed.substring(spaces);
          tmpParameterName2 += parameter.nameNotTrimmed.substring(spaces);
        }
      }

      if (parameter.value != null) {
        // Whitespace characters before value
        int spaces = 0;
        while ((spaces < parameter.valueNotTrimmed.length()) &&
               (Character.isWhitespace(parameter.valueNotTrimmed.charAt(spaces)))) {
          spaces++;
        }
        if (spaces > 0) {
          tmpParameterValue1 = parameter.valueNotTrimmed.substring(0, spaces) + parameterValue1;
          tmpParameterValue2 = parameter.valueNotTrimmed.substring(0, spaces) + parameterValue2;
        }

        // Whitespace characters after value
        spaces = parameter.valueNotTrimmed.length();
        while ((spaces > 0) &&
               (Character.isWhitespace(parameter.valueNotTrimmed.charAt(spaces - 1)))) {
          spaces--;
        }
        if (spaces < parameter.valueNotTrimmed.length()) {
          tmpParameterValue1 += parameter.valueNotTrimmed.substring(spaces);
          tmpParameterValue2 += parameter.valueNotTrimmed.substring(spaces);
        }
      }

      // Add parameter
      if (currentParameterName.equals(parameterName1)) {
        addParameter(sb, parameter.nameNotTrimmed, tmpParameterValue1);
        parameterAdded1 = true;
        if (!parameterExist2) {
          addParameter(sb, tmpParameterName2, tmpParameterValue2);
          parameterAdded2 = true;
        }
      } else if (currentParameterName.equals(parameterName2)) {
        if (!parameterExist1) {
          addParameter(sb, tmpParameterName1, tmpParameterValue1);
          parameterAdded1 = true;
        }
        addParameter(sb, parameter.nameNotTrimmed, tmpParameterValue2);
        parameterAdded2 = true;
      } else {
        addParameter(sb, parameter.nameNotTrimmed, parameter.valueNotTrimmed);
      }
    }
    if (!parameterAdded1) {
      addParameter(sb, tmpParameterName1, tmpParameterValue1);
    }
    if (!parameterAdded2) {
      addParameter(sb, tmpParameterName2, tmpParameterValue2);
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
    addPartBeforeParameters(sb);
    addPartFromParameters(sb);
    return sb.toString();
  }
}
