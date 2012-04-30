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

  /* ========================================================================= */
  /* HTML tags                                                                 */
  /* ========================================================================= */
  public final static String TAG_HTML_B           = "b";
  public final static String TAG_HTML_BIG         = "big";
  public final static String TAG_HTML_BR          = "br";
  public final static String TAG_HTML_FONT        = "font";
  public final static String TAG_HTML_H1          = "h1";
  public final static String TAG_HTML_H2          = "h2";
  public final static String TAG_HTML_H3          = "h3";
  public final static String TAG_HTML_H4          = "h4";
  public final static String TAG_HTML_H5          = "h5";
  public final static String TAG_HTML_H6          = "h6";
  public final static String TAG_HTML_H7          = "h7";
  public final static String TAG_HTML_H8          = "h8";
  public final static String TAG_HTML_H9          = "h9";
  public final static String TAG_HTML_I           = "i";
  public final static String TAG_HTML_LI          = "li";
  public final static String TAG_HTML_OL          = "ol";
  public final static String TAG_HTML_P           = "p";
  public final static String TAG_HTML_SMALL       = "small";
  public final static String TAG_HTML_TABLE       = "table";
  public final static String TAG_HTML_TD          = "td";
  public final static String TAG_HTML_TH          = "th";
  public final static String TAG_HTML_TR          = "tr";
  public final static String TAG_HTML_U           = "u";
  public final static String TAG_HTML_UL          = "ul";

  /* ========================================================================= */
  /* Wiki tags                                                                 */
  /* ========================================================================= */
  public final static String TAG_WIKI_CODE        = "code";
  public final static String TAG_WIKI_GALLERY     = "gallery";
  public final static String TAG_WIKI_INCLUDEONLY = "includeonly";
  public final static String TAG_WIKI_MATH        = "math";
  public final static String TAG_WIKI_NOINCLUDE   = "noinclude";
  public final static String TAG_WIKI_NOWIKI      = "nowiki";
  public final static String TAG_WIKI_PRE         = "pre";
  public final static String TAG_WIKI_REF         = "ref";
  public final static String TAG_WIKI_REFERENCES  = "references";
  public final static String TAG_WIKI_SOURCE      = "source";

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
    if (tmpIndex == beginIndex) {
      return null;
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
    if (paramString == null) {
      return true;
    }
    int maxLength = paramString.length();

    // Find parameter name
    int startNameIndex = 0;
    while ((startNameIndex < maxLength) &&
           (paramString.charAt(startNameIndex) == ' ')) {
      startNameIndex++;
    }
    if (startNameIndex >= maxLength) {
      return true;
    }
    int endNameIndex = startNameIndex;
    while ((endNameIndex < maxLength) &&
           (Character.isLetter(paramString.charAt(endNameIndex)))) {
      endNameIndex++;
    }
    if ((endNameIndex < maxLength) &&
        (paramString.charAt(endNameIndex) != ' ') &&
        (paramString.charAt(endNameIndex) != '=')) {
      return false;
    }
    String name = paramString.substring(startNameIndex, endNameIndex);

    // Find equal sign
    int equalIndex = endNameIndex;
    while ((equalIndex < maxLength) &&
           (paramString.charAt(equalIndex) == ' ')) {
      equalIndex++;
    }
    if (equalIndex >= maxLength) {
      Parameter param = new Parameter(name, null, null);
      parameters.add(param);
      return true;
    }
    if (paramString.charAt(equalIndex) != '=') {
      Parameter param = new Parameter(name, null, null);
      parameters.add(param);
      return analyzeParameters(paramString.substring(equalIndex), parameters);
    }

    // Find beginning of parameter value
    int startValueIndex = equalIndex + 1;
    while ((startValueIndex < maxLength) &&
           (paramString.charAt(startValueIndex) == ' ')) {
      startValueIndex++;
    }
    if (startValueIndex >= maxLength) {
      return false;
    }

    // Find parameter value
    String value = null;
    String marker = null;
    int endValueIndex = startValueIndex;
    if ((paramString.charAt(startValueIndex) == '\'') ||
        (paramString.charAt(startValueIndex) == '\"')) {
      marker = paramString.substring(startValueIndex, startValueIndex + 1);
      endValueIndex = paramString.indexOf(paramString.charAt(startValueIndex), startValueIndex + 1);
      if (endValueIndex < 0) {
        return false;
      }
      startValueIndex++;
      value = paramString.substring(startValueIndex, endValueIndex);
      endValueIndex++;
    } else {
      while ((endValueIndex < maxLength) &&
             (paramString.charAt(endValueIndex) != ' ')) {
        endValueIndex++;
      }
      value = paramString.substring(startValueIndex, endValueIndex);
    }
    Parameter param = new Parameter(name, value, marker);
    parameters.add(param);

    // Deal with next parameter
    if (endValueIndex < maxLength) {
      return analyzeParameters(paramString.substring(endValueIndex), parameters);
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

  /**
   * @param index Index of parameter.
   * @return Parameter.
   */
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
   * @param parameterName Parameter name.
   * @return Parameter.
   */
  public PageElementTag.Parameter getParameter(String parameterName) {
    if (parameters == null) {
      return null;
    }
    for (PageElementTag.Parameter param : parameters) {
      if (param.getName().equals(parameterName)) {
        return param;
      }
    }
    return null;
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
   * @return Beginning of the complete tag.
   */
  public int getCompleteBeginIndex() {
    if (isEndTag() && (matchingTag != null)) {
      return matchingTag.getBeginIndex();
    }
    return getBeginIndex();
  }

  /**
   * @return Beginning of the value.
   */
  public int getValueBeginIndex() {
    if (isFullTag() || !isComplete()) {
      return getEndIndex();
    }
    if (isEndTag()) {
      return getMatchingTag().getEndIndex();
    }
    return getEndIndex();
  }

  /**
   * @return End of the value.
   */
  public int getValueEndIndex() {
    if (isFullTag() || !isComplete()) {
      return getEndIndex();
    }
    if (isEndTag()) {
      return getBeginIndex();
    }
    return getMatchingTag().getBeginIndex();
  }

  /**
   * @return End of the complete tag.
   */
  public int getCompleteEndIndex() {
    if (isEndTag() || (matchingTag == null)) {
      return getEndIndex();
    }
    return matchingTag.getEndIndex();
  }

  /**
   * @return True if the tag is complete (either full or with matching tag).
   */
  public boolean isComplete() {
    return (fullTag || (matchingTag != null));
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
