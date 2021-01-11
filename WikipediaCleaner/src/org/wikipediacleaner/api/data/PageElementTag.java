/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Class containing information about a tag (&lt;<i>tag</i>&gt;).
 */
public class PageElementTag extends PageElement {

  /** Possible characters for parameter name */
  private final static String PARAM_NAME_CHARS =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "abcdefghijklmnopqrstuvwxyz" +
      "0123456789" +
      "-";

  private final static String PARAM_VALUE_UNQUOTED_CHARS =
      "!$%&()*,-.:;<@[]^_`{|}~" + // List from https://en.wikipedia.org/wiki/Wikipedia:REFNAME
      "#"; // Other for common HTML parameter values

  /** Possible separation characters after tag name */
  private final static String SEP_CHARS_AFTER_TAG_NAME = " \n";

  /** Possible separation characters at the end */
  private final static String SEP_CHARS_END = " \n" + '\u00A0';

  /** Tag name */
  private final String name;

  /** Normalized tag name */
  private final String normalizedName;

  /** Type of the tag */
  private final TagType type;

  /** Tag parameters */
  private final List<PageElementTag.Parameter> parameters;

  /** Flag indicating if this is a closing tag */
  private final boolean endTag;

  /** Flag indicating if this is a full tag (opening + closing) */
  private final boolean fullTag;

  /** Flag indicating if there are white space characters at the end */
  private final boolean endWithSpace;

  /** Matching tag */
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

    // No whitespace characters between '<' and the tag name
    if ((tmpIndex >= maxLength) ||
        (" \u00A0\n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
      return null;
    }

    // Check for possible end tag
    if (tmpIndex >= maxLength) {
      return null;
    }
    boolean endTag = false;
    if (contents.charAt(tmpIndex) == '/') {
      endTag = true;
      tmpIndex++;
      tmpIndex = ContentsUtil.moveIndexAfterWhitespace(contents, tmpIndex);
    }
    int beginIndex = tmpIndex;

    // Retrieve tag name
    if (tmpIndex >= maxLength) {
      return null;
    }
    while ((tmpIndex < maxLength) &&
           (Character.isLetterOrDigit(contents.charAt(tmpIndex))) &&
           ((tmpIndex > beginIndex) ||
            (Character.isLetter(contents.charAt(tmpIndex))))) {
      tmpIndex++;
    }
    if (tmpIndex == beginIndex) {
      return null;
    }
    String name = contents.substring(beginIndex, tmpIndex);

    // Possible whitespace characters
    tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, tmpIndex, SEP_CHARS_AFTER_TAG_NAME);

    // Find end of tag
    int endIndex = tmpIndex;
    boolean inQuotes = false;
    while (endIndex < contents.length()) {
      if ((contents.charAt(endIndex) == '>') && !inQuotes) {
        break;
      }
      if (contents.charAt(endIndex) == '"') {
        if (inQuotes) {
          inQuotes = false;
        } else if (contents.charAt(endIndex - 1) == '=') {
          inQuotes = true;
        }
      }
      endIndex++;
    }
    if (endIndex >= contents.length()) {
      return null;
    }

    // Possible whitespace characters
    int tmpIndex2 = endIndex - 1;
    boolean endWithSpace = false;
    while ((tmpIndex2 > tmpIndex) &&
           (SEP_CHARS_END.indexOf(contents.charAt(tmpIndex2)) >= 0)) {
      tmpIndex2--;
      endWithSpace = true;
    }

    // Check for possible full tag
    boolean fullTag = false;
    if (contents.charAt(tmpIndex2) == '/') {
      if (endTag) {
        return null; // Tag with / at the beginning and at the end
      }
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
      if (!analyzeParameters(contents.substring(tmpIndex, tmpIndex2 + 1), tmpIndex - index, parameters)) {
        return null;
      }
    }

    // Create tag
    return new PageElementTag(
        index, endIndex + 1,
        name, parameters,
        endTag, fullTag, endWithSpace);
  }

  /**
   * Analyze tag parameters.
   * 
   * @param paramString String containing the parameters.
   * @param offset Offset of the string in the tag.
   * @param parameters Parameters.
   * @return True if analyze is correct.
   */
  private static boolean analyzeParameters(
      String paramString, int offset,
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
           (PARAM_NAME_CHARS.indexOf(paramString.charAt(endNameIndex)) >= 0)) {
      endNameIndex++;
    }
    if ((endNameIndex < maxLength) &&
        (paramString.charAt(endNameIndex) != ' ') &&
        (paramString.charAt(endNameIndex) != '=')) {
      return false;
    }
    String name = paramString.substring(startNameIndex, endNameIndex);

    // Find equal sign
    int equalIndex = ContentsUtil.moveIndexAfterWhitespace(paramString, endNameIndex);
    if (equalIndex >= maxLength) {
      Parameter param = new Parameter(
          name, offset + startNameIndex, offset + endNameIndex);
      parameters.add(param);
      return true;
    }
    if (paramString.charAt(equalIndex) != '=') {
      Parameter param = new Parameter(name, offset + startNameIndex, offset + endNameIndex);
      parameters.add(param);
      return analyzeParameters(
          paramString.substring(equalIndex), offset + equalIndex, parameters);
    }

    // Find beginning of parameter value
    int startValueIndex = ContentsUtil.moveIndexAfterWhitespace(paramString, equalIndex + 1);
    if (startValueIndex >= maxLength) {
      Parameter param = new Parameter(name, offset + startNameIndex, offset + endNameIndex);
      parameters.add(param);
      return analyzeParameters(
          paramString.substring(startValueIndex), offset + startValueIndex, parameters);
    }

    // Find parameter value
    String value = null;
    int endValueIndex = startValueIndex;
    char startValueChar = paramString.charAt(startValueIndex);
    String beforeMarker = null;
    String preferredAfterMarker = null;
    if (startValueChar == '\"') {
      beforeMarker = "\"";
      preferredAfterMarker = beforeMarker;
    } else if (startValueChar == '\'') {
      beforeMarker = "\'";
      preferredAfterMarker = beforeMarker;
    } else if (startValueChar == '«') {
      beforeMarker = "«";
      preferredAfterMarker = "»";
    }
    String afterMarker = null;
    if (beforeMarker != null) {
      endValueIndex = startValueIndex + beforeMarker.length();
      while ((endValueIndex < paramString.length()) &&
             (afterMarker == null)) {
        if (paramString.startsWith(preferredAfterMarker, endValueIndex)) {
          afterMarker = preferredAfterMarker;
        } else {
          endValueIndex++;
        }
      }
      startValueIndex++;
      value = paramString.substring(startValueIndex, endValueIndex);
      if (afterMarker != null) {
        endValueIndex += afterMarker.length();
      }
    } else {
      while ((endValueIndex < maxLength) &&
             (paramString.charAt(endValueIndex) != ' ')) {
        char currentChar = paramString.charAt(endValueIndex);
        if (!Character.isLetterOrDigit(currentChar) &&
            (PARAM_VALUE_UNQUOTED_CHARS.indexOf(currentChar) < 0)) {
          return false;
        }
        endValueIndex++;
      }
      value = paramString.substring(startValueIndex, endValueIndex);
    }
    Parameter param = new Parameter(
        name, value,
        offset + startNameIndex, offset + startValueIndex, offset + endValueIndex,
        beforeMarker, afterMarker);
    parameters.add(param);

    // Deal with next parameter
    if (endValueIndex < maxLength) {
      return analyzeParameters(
          paramString.substring(endValueIndex), offset + endValueIndex, parameters);
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
   * @return Tag type.
   */
  public TagType getType() {
    return type;
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
    for (int index = parameters.size() - 1; index >= 0; index--) {
      PageElementTag.Parameter param = parameters.get(index);
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
   * @return Ends with extra space characters ?
   */
  public boolean endWithSpace() {
    return endWithSpace;
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
  public void setMatchingTag(PageElementTag tag) {
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
   * @param Extra white space characters at the end ?
   */
  private PageElementTag(
      int beginIndex, int endIndex,
      String name, List<PageElementTag.Parameter> parameters,
      boolean endTag, boolean fullTag, boolean endWithSpace) {
    super(beginIndex, endIndex);
    this.name = name;
    this.normalizedName = (name != null) ? name.trim().toLowerCase() : null;
    this.type = TagType.getByNormalizedName(normalizedName);
    this.parameters = parameters;
    this.endTag = endTag;
    this.fullTag = fullTag;
    this.endWithSpace = endWithSpace;
  }

  /**
   * Retrieve the group name of a ref tag.
   * 
   * @param analysis Page analysis.
   * @return Group of the ref tag.
   */
  public String getGroupOfRef(PageAnalysis analysis) {
    String result = null;

    // Check for a group parameter in the tag
    Parameter group = getParameter("group");
    if (group != null) {
      result = group.getValue();
    } else {

      // Check for a group parameter in the references tag
      PageElementTag references = analysis.getSurroundingTag(
          WikiTagType.REFERENCES, getBeginIndex());
      if (references != null) {
        group = references.getParameter("group");
        if (group != null) {
          result = group.getValue();
        }
      } else {
  
        // Check for a group parameter in the references templates
        WPCConfiguration config = analysis.getWPCConfiguration();
        List<String[]> templates = config.getStringArrayList(WPCConfigurationStringList.REFERENCES_TEMPLATES);
        if (templates != null) {
          PageElementTemplate template = analysis.isInTemplate(getBeginIndex());
          if (template != null) {
            for (String[] elements : templates) {
              if ((elements.length > 1) &&
                  (Page.areSameTitle(template.getTemplateName(), elements[0]))) {
                String[] argNames = elements[1].split(",");
                for (String argName : argNames) {
                  String tmp = template.getParameterValue(argName);
                  if ((result == null) && (tmp != null)) {
                    result = tmp;
                    if ((result.length() > 2) &&
                        (result.charAt(0) == '"') &&
                        (result.charAt(result.length() - 1) == '"')) {
                      result = result.substring(1, result.length() - 2);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    if ((result == null) || (result.trim().length() == 0)) {
      return null;
    }
    return result.trim();
  }

  /**
   * Find the main reference tag in a list of reference tags.
   * 
   * @param refs List of reference tags.
   * @param references List of references tags.
   * @param analysis Page analysis.
   * @return Main reference tag in the list.
   */
  public static PageElementTag getMainRef(
      List<PageElementTag> refs,
      List<PageElementTag> references,
      PageAnalysis analysis) {
    if (refs == null) {
      return null;
    }

    // Configuration
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> templates = config.getStringArrayList(WPCConfigurationStringList.REFERENCES_TEMPLATES);

    // Search for a named reference tag
    PageElementTag namedTag = null;
    PageElementTag namedTagInReferences = null;
    PageElementTag namedTagInTemplate = null;
    for (PageElementTag tag : refs) {

      // Check that the tag has a name
      boolean hasName = false;
      Parameter name = tag.getParameter("name");
      if ((name != null) &&
          (name.getTrimmedValue() != null) &&
          (!name.getTrimmedValue().isEmpty())) {
        hasName = true;
      }

      // Check that the tag has a value
      boolean hasValue = false;
      int beginValue = tag.getValueBeginIndex();
      int endValue = tag.getValueEndIndex();
      String value = analysis.getContents().substring(beginValue, endValue);
      if ((value != null) && (!value.trim().isEmpty())) {
        hasValue = true;
      }

      // Check if the tag can be the main tag
      if (hasName && hasValue) {

        // Direct tag
        if (namedTag == null) {
          namedTag = tag;
        }

        // Tag inside <references />
        for (PageElementTag reference : references) {
          if ((tag.getCompleteBeginIndex() > reference.getCompleteBeginIndex()) &&
              (tag.getCompleteEndIndex() < reference.getCompleteEndIndex())) {
            if (namedTagInReferences == null) {
              namedTagInReferences = tag;
            }
          }
        }

        // Tag inside references template
        if (templates != null) {
          PageElementTemplate template = analysis.isInTemplate(tag.getCompleteBeginIndex());
          if (template != null) {
            for (String[] elements : templates) {
              if ((elements.length > 0) &&
                  (Page.areSameTitle(template.getTemplateName(), elements[0])) &&
                  (namedTagInTemplate == null)) {
                namedTagInTemplate = tag;
              }
            }
          }
        }
      }
    }

    // Deal with named reference tag inside <references/>
    if (namedTagInReferences != null) {
      return namedTagInReferences;
    }

    // Deal with named reference tag inside template
    if (namedTagInTemplate != null) {
      return namedTagInTemplate;
    }

    // Deal with named references tag outside <references/>
    if (namedTag != null) {
      return namedTag;
    }

    return null;
  }

  /**
   * Retrieve index of matching end tag.
   * 
   * @param tags List of tags.
   * @param tagIndex Index of opening tag.
   * @return Index of matching end tag.
   */
  public static int getMatchingTagIndex(List<PageElementTag> tags, int tagIndex) {
    PageElementTag tag = tags.get(tagIndex);
    if (!tag.isFullTag() && tag.isComplete()) {
      int endIndex = tags.indexOf(tag.getMatchingTag());
      if (endIndex > tagIndex) {
        return endIndex;
      }
    }
    return tagIndex;
  }

  /**
   * Group consecutive tags.
   * 
   * @param tags List of tags.
   * @param firstTagIndex Index of first tag in the list.
   * @param contents Page contents.
   * @param punctuation Possible punctuation elements between tags.
   * @param separator Possible separator between tags.
   * @return Index of last tag in the group of consecutive tags.
   */
  public static int groupTags(
      List<PageElementTag> tags,
      int firstTagIndex,
      String contents,
      String punctuation, String separator) {
    // TODO: Check if still necessary after refactoring of <ref> errors.
    if (tags == null) {
      return firstTagIndex;
    }
    int tagIndex = firstTagIndex;
    while (tagIndex < tags.size()) {

      // Search for matching end tag
      int lastTagIndex = getMatchingTagIndex(tags, tagIndex);
      tagIndex = lastTagIndex + 1;

      // Check text before next tag
      if (tagIndex >= tags.size()) {
        return lastTagIndex;
      }
      int nextBeginIndex = tags.get(tagIndex).getBeginIndex();
      int currentIndex = tags.get(lastTagIndex).getEndIndex();
      boolean separatorFound = false;
      while (currentIndex < nextBeginIndex) {
        if (!separatorFound &&
            (separator != null) &&
            contents.startsWith(separator, currentIndex)) {
          separatorFound = true;
          currentIndex += separator.length();
        } else if (contents.startsWith("&nbsp;", currentIndex)) {
          currentIndex += "&nbsp;".length();
        } else if (!Character.isWhitespace(contents.charAt(currentIndex)) &&
            ((punctuation == null) ||
             (punctuation.indexOf(contents.charAt(currentIndex)) < 0))) {
          return lastTagIndex;
        } else {
          currentIndex++;
        }
      }
    }
    return tagIndex;
  }

  /**
   * Create a textual representation of a list of tags.
   * 
   * @param tags List of tags.
   * @param firstTagIndex Index of first tag in the list.
   * @param lastTagIndex Index of last tag in the list.
   * @param contents Page contents.
   * @param separator Separator.
   * @return Textual representation of a list of tags.
   */
  public static String createListOfTags(
      List<PageElementTag> tags,
      int firstTagIndex, int lastTagIndex,
      String contents, String separator) {
    // TODO: Check if still necessary after refactoring of <ref> errors.
    StringBuilder buffer = new StringBuilder();
    int tagIndex = firstTagIndex;
    while (tagIndex <= lastTagIndex) {
      if ((tagIndex > firstTagIndex) && (separator != null)) {
        buffer.append(separator);
      }
      int beginIndex = tags.get(tagIndex).getBeginIndex();
      tagIndex = getMatchingTagIndex(tags, tagIndex);
      int endIndex = tags.get(tagIndex).getEndIndex();
      tagIndex++;
      buffer.append(contents.substring(beginIndex, endIndex));
    }
    return buffer.toString();
  }

  /**
   * Create a reduced textual representation of a list of tags.
   * 
   * @param tags List of tags.
   * @param firstTagIndex Index of first tag in the list.
   * @param lastTagIndex Index of last tag in the list.
   * @param separator Separator.
   * @return Reduced textual representation of a list of tags.
   */
  public static String createReducedListOfTags(
      List<PageElementTag> tags,
      int firstTagIndex, int lastTagIndex,
      String separator) {
    // TODO: Check if still necessary after refactoring of <ref> errors.
    int tagIndex = firstTagIndex;
    int count = 0;
    while (tagIndex <= lastTagIndex) {
      count++;
      tagIndex = getMatchingTagIndex(tags, tagIndex);
      tagIndex++;
    }
    String refBlock = WikiTagType.REF.getCompleteTag();
    if (count > 2) {
      return refBlock + separator + "..." + separator + refBlock;
    }
    if (count > 1) {
      return refBlock + separator + refBlock;
    }
    return refBlock;
  }

  /** Class for managing a parameter */
  public static class Parameter {

    /** Parameter name */
    private final String name;

    /** Parameter value */
    private final String value;

    /** Offset for the beginning of the parameter in the tag */
    private final int offsetBegin;

    /** Offset for the value of the parameter in the tag */
    private final int offsetValue;

    /** Offset for the end of the parameter in the tag */
    private final int offsetEnd;

    /** Marker */
    private final String beforeMarker;
    private final String afterMarker;

    /**
     * @param name Parameter name.
     * @param offsetBegin Offset for the beginning of the parameter in the tag.
     * @param offsetEnd Offset for the end of the parameter in the tag.
     */
    Parameter(String name, int offsetBegin, int offsetEnd) {
      this(name, null, offsetBegin, offsetEnd, offsetEnd, null, null);
    }

    /**
     * @param name Parameter name.
     * @param value Parameter value.
     * @param offsetBegin Offset for the beginning of the parameter in the tag.
     * @param offsetValue Offset for the value of the parameter in the tag.
     * @param offsetEnd Offset for the end of the parameter in the tag.
     * @param beforeMarker Marker (like quote) before the parameter value.
     * @param afterMarker Marker (like quote) after the parameter value.
     */
    Parameter(
        String name, String value,
        int offsetBegin, int offsetValue, int offsetEnd,
        String beforeMarker, String afterMarker) {
      this.name = name;
      this.value = value;
      this.offsetBegin = offsetBegin;
      this.offsetValue = offsetValue;
      this.offsetEnd = offsetEnd;
      this.beforeMarker = beforeMarker;
      this.afterMarker = afterMarker;
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
     * @return Parameter value.
     */
    public String getTrimmedValue() {
      if (value == null) {
        return null;
      }
      return value.trim();
    }

    /**
     * @return Offset for the beginning of the parameter in the tag.
     */
    public int getOffsetBegin() {
      return offsetBegin;
    }

    /**
     * @return Offset for the value of the parameter in the tag.
     */
    public int getOffsetValue() {
      return offsetValue;
    }

    /**
     * @return Offset for the end of the parameter in the tag.
     */
    public int getOffsetEnd() {
      return offsetEnd;
    }

    /**
     * @return True if parameter has unbalanced quotes.
     */
    public boolean hasUnbalancedQuotes() {
      if ("\"".equals(beforeMarker) && (afterMarker == null)) {
        return true;
      }
      if ((beforeMarker == null) && "\"".equals(afterMarker)) {
        return true;
      }
      return false;
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
      if (beforeMarker != null) {
        builder.append(beforeMarker);
      }
      builder.append(value);
      if (afterMarker != null) {
        builder.append(afterMarker);
      }
      return builder.toString();
    }
  }
}
