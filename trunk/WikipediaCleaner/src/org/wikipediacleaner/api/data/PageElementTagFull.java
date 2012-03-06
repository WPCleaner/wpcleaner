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
 * Class containing information about a complete tag (&lt;<i>tag</i>&gt;Text&lt;/<i>tag</i>&gt;). 
 */
public class PageElementTagFull extends PageElement {

  private final String tagName;
  private final int startTagEndIndex;
  private final List<String> parameterNames;
  private final List<String> parameterValues;
  private final boolean simpleTag;
  private final String text;
  private final int endTagBeginIndex;

  /**
   * Analyze contents to check if it matches a block for the given tag name.
   * 
   * @param tagName Tag name.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementTagFull analyzeBlock(
      String tagName,
      String contents, int index) {
    // Verify arguments
    if ((tagName == null) || (tagName.length() == 0) || (contents == null)) {
      return null;
    }

    // Look for '<'
    int startTagBeginIndex = index;
    int tmpIndex = startTagBeginIndex;
    if ((tmpIndex >= contents.length()) || (contents.charAt(tmpIndex) != '<')) {
      return null;
    }
    tmpIndex++;

    // Possible whitespace characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Check Tag Name
    if ((tmpIndex + tagName.length() >= contents.length()) ||
        (!tagName.equalsIgnoreCase(contents.substring(tmpIndex, tmpIndex + tagName.length())))) {
      return null;
    }
    tmpIndex += tagName.length();
    if (tmpIndex >= contents.length()) {
      return null;
    }
    if ((contents.charAt(tmpIndex) != ' ') &&
        (contents.charAt(tmpIndex) != '/') &&
        (contents.charAt(tmpIndex) != '>')) {
      return null;
    }

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Find Tag end
    if (tmpIndex >= contents.length()) {
      return null;
    }
    int startTagEndIndex = contents.indexOf('>', tmpIndex);
    if (startTagEndIndex < 0) {
      return null;
    }
    startTagEndIndex++;
    int startParametersIndex = tmpIndex;
    if (contents.charAt(startTagEndIndex - 2) == '/') {
      List<String> parameterNames = new ArrayList<String>();
      List<String> parameterValues = new ArrayList<String>();
      if (!analyzeTagParameters(
          contents.substring(startParametersIndex, startTagEndIndex - 2),
          parameterNames, parameterValues)) {
        return null;
      }
      return new PageElementTagFull(
          tagName,
          startTagBeginIndex, startTagEndIndex,
          parameterNames, parameterValues);
    }
    tmpIndex = startTagEndIndex;

    // Find Tag end
    if (tmpIndex >= contents.length()) {
      return null;
    }
    boolean endTagFound = false;
    int endTagBeginIndex = tmpIndex;
    while (!endTagFound) {
      endTagBeginIndex = contents.indexOf('<', tmpIndex);
      if (endTagBeginIndex < 0) {
        return null;
      }
      tmpIndex = endTagBeginIndex + 1;
      if (!contents.startsWith("<!--", endTagBeginIndex)) {
        endTagFound = true;
      }
    }

    // Look for '/'
    if ((tmpIndex >= contents.length()) || (contents.charAt(tmpIndex) != '/')) {
      return null;
    }
    tmpIndex++;

    // Possible whitespace characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Check Tag Name
    if ((tmpIndex >= contents.length()) ||
        (!tagName.equalsIgnoreCase(contents.substring(tmpIndex, tmpIndex + tagName.length())))) {
      return null;
    }
    tmpIndex += tagName.length();
    if (tmpIndex >= contents.length()) {
      return null;
    }

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Find tag end
    if ((tmpIndex >= contents.length()) || (contents.charAt(tmpIndex) != '>')) {
      return null;
    }
    int endTagEndIndex = tmpIndex + 1;

    List<String> parameterNames = new ArrayList<String>();
    List<String> parameterValues = new ArrayList<String>();
    if (!analyzeTagParameters(
        contents.substring(startParametersIndex, startTagEndIndex - 1),
        parameterNames, parameterValues)) {
      return null;
    }
    return new PageElementTagFull(
        tagName, startTagBeginIndex, startTagEndIndex,
        parameterNames, parameterValues,
        contents.substring(startTagEndIndex, endTagBeginIndex),
        endTagBeginIndex, endTagEndIndex);
  }

  /**
   * Analyze the parameters of tag.
   * 
   * @param parameters String containing the parameters.
   * @param parameterNames Parameter names.
   * @param parameterValues Parameter values.
   * @return Flag indicating if the analyze is correct.
   */
  private static boolean analyzeTagParameters(
      String parameters,
      List<String> parameterNames, List<String> parameterValues) {
    if ((parameters == null) || (parameters.trim().length() == 0)) {
      return true;
    }
    parameters = parameters.trim();
    int equalIndex = parameters.indexOf('=');
    if (equalIndex <= 0) {
      return false;
    }
    String parameterName = parameters.substring(0, equalIndex);
    if (parameterName.indexOf(' ') >= 0) {
      return false;
    }
    parameterNames.add(parameterName);
    int tmpIndex = equalIndex + 1;
    if (tmpIndex >= parameters.length()) {
      return false;
    }
    char quoteCharacter = parameters.charAt(tmpIndex);
    int endParameterIndex = tmpIndex;
    if ((quoteCharacter == '\'') || (quoteCharacter == '"')) {
      int quoteIndex = parameters.indexOf(quoteCharacter, tmpIndex + 1);
      if (quoteIndex < 0) {
        return false;
      }
      parameterValues.add(parameters.substring(tmpIndex, quoteIndex + 1));
      endParameterIndex = quoteIndex + 1;
    } else {
      int spaceIndex = parameters.indexOf(' ', tmpIndex + 1);
      if (spaceIndex < 0) {
        parameterValues.add(parameters.substring(tmpIndex));
        endParameterIndex = parameters.length();
      } else {
        parameterValues.add(parameters.substring(tmpIndex, spaceIndex));
        endParameterIndex = spaceIndex + 1;
      }
    }
    return analyzeTagParameters(parameters.substring(endParameterIndex), parameterNames, parameterValues);
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
      index++;
    }
    return null;
  }

  public int getStartTagBeginIndex() {
    return getBeginIndex();
  }

  public int getStartTagEndIndex() {
    return startTagEndIndex;
  }

  public boolean isSimpleTag() {
    return simpleTag;
  }

  public String getText() {
    return text;
  }

  public int getEndTagBeginIndex() {
    return endTagBeginIndex;
  }

  public int getEndTagEndIndex() {
    return getEndIndex();
  }

  private PageElementTagFull(
      String tagName,
      int startTagBeginIndex, int startTagEndIndex,
      List<String> parameterNames, List<String> parameterValues) {
    super(startTagBeginIndex, startTagEndIndex);
    this.tagName = tagName;
    this.startTagEndIndex = startTagEndIndex;
    this.parameterNames = parameterNames;
    this.parameterValues = parameterValues;
    this.simpleTag = true;
    this.text = null;
    this.endTagBeginIndex = startTagEndIndex;
  }

  private PageElementTagFull(
      String tagName,
      int startTagBeginIndex, int startTagEndIndex,
      List<String> parameterNames, List<String> parameterValues,
      String text,
      int endTagBeginIndex, int endTagEndIndex) {
    super(startTagBeginIndex, endTagEndIndex);
    this.tagName = tagName;
    this.startTagEndIndex = startTagEndIndex;
    this.parameterNames = parameterNames;
    this.parameterValues = parameterValues;
    this.simpleTag = false;
    this.text = text;
    this.endTagBeginIndex = endTagBeginIndex;
  }

  public String getPartBeforeParameters() {
    StringBuilder sb = new StringBuilder();
    sb.append('<');
    sb.append(tagName);
    return sb.toString();
  }

  public String getPartFromParameters() {
    StringBuilder sb = new StringBuilder();
    int index = 0;
    while ((index < parameterNames.size()) && (index < parameterValues.size())) {
      sb.append(' ');
      sb.append(parameterNames.get(index));
      sb.append('=');
      sb.append(parameterValues.get(index));
      index++;
    }
    if (simpleTag) {
      sb.append('/');
    } else {
      sb.append('>');
      sb.append(text);
      sb.append("</");
      sb.append(tagName);
    }
    sb.append('>');
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
