/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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
 * Class containing information about a tag (&lt;<i>tag</i>&gt;).
 */
public class PageElementTag extends PageElement {

  public final static String TAG_B          = "b";
  public final static String TAG_BR         = "br";
  public final static String TAG_I          = "i";
  public final static String TAG_NOWIKI     = "nowiki";
  public final static String TAG_P          = "p";
  public final static String TAG_REF        = "ref";
  public final static String TAG_REFERENCES = "references";
  public final static String TAG_SMALL      = "small";
  public final static String TAG_U          = "u";


  /**
   * Tag name.
   */
  private final String name;

  /**
   * Normalized tag name.
   */
  private final String normalizedName;

  /**
   * Tag parameters.
   */
  private final List<PageElementTag.Parameter> parameters;

  /**
   * Flag indicating if this is a closing tag.
   */
  private final boolean endTag;

  /**
   * Flag indicating if this is a full tag (opening + closing.
   */
  private final boolean fullTag;

  /**
   * Matching tag.
   */
  private PageElementTag matchingTag;

  /**
   * Analyze contents to check if it matches a tag.
   * 
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementTag analyzeBlock(
      String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }
    int maxLength = contents.length();

    // Check for '<'
    int tmpIndex = index;
    if ((tmpIndex >= maxLength) ||
        (contents.charAt(tmpIndex) != '<')) {
      return null;
    }
    tmpIndex++;

    // Possible whitespace characters
    while ((tmpIndex < maxLength) &&
           (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Check for possible end tag
    if (tmpIndex >= maxLength) {
      return null;
    }
    boolean endTag = false;
    if (contents.charAt(tmpIndex) == '/') {
      endTag = true;
      tmpIndex++;
      while ((tmpIndex < maxLength) &&
             (contents.charAt(tmpIndex) == ' ')) {
        tmpIndex++;
      }
    }
    int beginIndex = tmpIndex;

    // Retrieve tag name
    if (tmpIndex >= maxLength) {
      return null;
    }
    while ((tmpIndex < maxLength) &&
           (Character.isLetter(contents.charAt(tmpIndex)))) {
      tmpIndex++;
    }
    String name = contents.substring(beginIndex, tmpIndex);

    // Possible whitespace characters
    while ((tmpIndex < maxLength) &&
           (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Find end of tag
    int endIndex = contents.indexOf('>', tmpIndex);
    if (endIndex < 0) {
      return null;
    }

    // Possible whitespace characters
    int tmpIndex2 = endIndex - 1;
    while ((tmpIndex2 > tmpIndex) &&
           (contents.charAt(tmpIndex2) == ' ')) {
      tmpIndex2--;
    }

    // Check for possible full tag
    boolean fullTag = false;
    if (contents.charAt(tmpIndex2) == '/') {
      fullTag = true;
      tmpIndex2--;
      while ((tmpIndex2 > tmpIndex) &&
             (contents.charAt(tmpIndex2) == ' ')) {
        tmpIndex2--;
      }
    }

    // Check for parameters
    List<Parameter> parameters = null;
    if (tmpIndex2 > tmpIndex) {
      parameters = new ArrayList<PageElementTag.Parameter>();
      if (!analyzeParameters(contents.substring(tmpIndex, tmpIndex2 + 1), parameters)) {
        return null;
      }
    }

    // Create tag
    return new PageElementTag(
        index, endIndex + 1,
        name, parameters,
        endTag, fullTag);
  }

  /**
   * Analyze tag parameters.
   * 
   * @param paramString String containing the parameters.
   * @param parameters Parameters.
   * @return True if analyze is correct.
   */
  private static boolean analyzeParameters(
      String paramString,
      List<Parameter> parameters) {
    if ((paramString == null) || (paramString.trim().length() == 0)) {
      return true;
    }
    paramString = paramString.trim();
    int equalIndex = paramString.indexOf('=');
    if ((equalIndex <= 0) || (equalIndex + 2 >= paramString.length())) {
      return false;
    }
    String name = paramString.substring(0, equalIndex);
    paramString = paramString.substring(equalIndex + 1);
    String value = null;
    int endIndex = 0;
    String marker = null;
    if ((paramString.charAt(0) == '\'') ||
        (paramString.charAt(0) == '\"')) {
      endIndex = paramString.indexOf(paramString.charAt(0), 1);
      if (endIndex < 0) {
        return false;
      }
      marker = paramString.substring(0, 1);
      value = paramString.substring(1, endIndex);
      endIndex++;
    } else {
      endIndex = paramString.indexOf(' ');
      if (endIndex < 0) {
        value = paramString;
        endIndex = paramString.length();
      } else {
        value = paramString.substring(0, endIndex);
      }
    }
    if (parameters != null) {
      Parameter param = new Parameter(name, value, marker);
      parameters.add(param);
    }
    if (endIndex < paramString.length()) {
      return analyzeParameters(paramString.substring(endIndex), parameters);
    }
    return true;
  }

  /**
   * @return Tag name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Normalized tag name.
   */
  public String getNormalizedName() {
    return normalizedName;
  }

  /**
   * @return Number of parameters.
   */
  public int getParametersCount() {
    if (parameters != null) {
      return parameters.size();
    }
    return 0;
  }

  public PageElementTag.Parameter getParameter(int index) {
    if (parameters == null) {
      return null;
    }
    if ((index < 0) || (index >= parameters.size())) {
      return null;
    }
    return parameters.get(index);
  }
  /**
   * @return Is it an end tag ?
   */
  public boolean isEndTag() {
    return endTag;
  }

  /**
   * @return Is it a full tag ?
   */
  public boolean isFullTag() {
    return fullTag;
  }

  /**
   * @return Matching tag.
   */
  public PageElementTag getMatchingTag() {
    return matchingTag;
  }

  /**
   * @param tag Matching tag.
   */
  void setMatchingTag(PageElementTag tag) {
    if (tag == matchingTag) {
      return;
    }
    PageElementTag oldMatchingTag = matchingTag;
    matchingTag = tag;
    if (oldMatchingTag != null) {
      oldMatchingTag.setMatchingTag(null);
    }
    if (matchingTag != null) {
      matchingTag.setMatchingTag(this);
    }
  }

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param name Tag name.
   * @param parameters Parameters.
   * @param endTag Is it a closing tag ?
   * @param fullTag Is it a full tag ?
   */
  private PageElementTag(
      int beginIndex, int endIndex,
      String name, List<PageElementTag.Parameter> parameters,
      boolean endTag, boolean fullTag) {
    super(beginIndex, endIndex);
    this.name = name;
    this.normalizedName = (name != null) ? name.trim().toLowerCase() : null;
    this.parameters = parameters;
    this.endTag = endTag;
    this.fullTag = fullTag;
  }

  /**
   * Class for managing a parameter
   */
  public static class Parameter {

    /**
     * Parameter name.
     */
    private final String name;

    /**
     * Parameter value.
     */
    private final String value;

    /**
     * Marker.
     */
    private final String marker;

    Parameter(String name, String value, String marker) {
      this.name = name;
      this.value = value;
      this.marker = marker;
    }

    /**
     * @return Parameter name.
     */
    public String getName() {
      return name;
    }

    /**
     * @return Parameter value.
     */
    public String getValue() {
      return value;
    }

    /**
     * @return Text equivalent to the parameter.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      StringBuilder builder = new StringBuilder();
      builder.append(name);
      builder.append('=');
      if (marker != null) {
        builder.append(marker);
      }
      builder.append(value);
      if (marker != null) {
        builder.append(marker);
      }
      return builder.toString();
    }
  }
}
