/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

  private final static String templateNameUnauthorizedCharacters = "{}[]|<>";

  /**
   * Class containing information about a template parameter.
   */
  public static class Parameter {
    final int pipeIndex;
    final String name;
    final String nameNotTrimmed;
    final String computedName;
    final int nameStartIndex;
    final String value;
    final String strippedValue;
    final String valueNotTrimmed;
    final int valueStartIndex;

    /**
     * @param pipeIndex Index of the pipe "|" in page contents.
     * @param name Parameter name.
     * @param computedName Computed name.
     * @param nameStartIndex Index of parameter name in page contents.
     * @param value Parameter value.
     * @param strippedValue Parameter stripped value.
     * @param valueStartIndex Index of parameter value in page contents.
     */
    public Parameter(
        int pipeIndex,
        String name, String computedName, int nameStartIndex,
        String value, String strippedValue, int valueStartIndex) {
      this.pipeIndex = pipeIndex;
      this.nameNotTrimmed = name;
      this.name = (name != null) ? name.trim() : null;
      this.computedName = (computedName != null) ? computedName : this.name;
      this.nameStartIndex = nameStartIndex;
      this.valueNotTrimmed = value;
      this.value = (value != null) ? value.trim() : null;
      this.strippedValue = (strippedValue != null) ? strippedValue.trim() : null;
      this.valueStartIndex = valueStartIndex;
    }

    /**
     * @return Parameter pipe index.
     */
    public int getPipeIndex() {
      return pipeIndex;
    }

    /**
     * @return Parameter name.
     */
    public String getNameNotTrimmed() {
      return nameNotTrimmed;
    }

    /**
     * @return Parameter name.
     */
    public String getName() {
      return name;
    }

    /**
     * @return Computed parameter name.
     */
    public String getComputedName() {
      return computedName;
    }

    /**
     * @return Parameter name start index.
     */
    public int getNameStartIndex() {
      return nameStartIndex;
    }

    /**
     * @return Parameter value.
     */
    public String getValueNotTrimmed() {
      return valueNotTrimmed;
    }

    /**
     * @return Parameter value.
     */
    public String getValue() {
      return value;
    }

    /**
     * @return Parameter stripped value.
     */
    public String getStrippedValue() {
      return strippedValue;
    }

    /**
     * @return Parameter value start index.
     */
    public int getValueStartIndex() {
      return valueStartIndex;
    }

    @Override
    public String toString() {
      if ((name != null) && (!name.isEmpty())) {
        return name + "=" + value;
      }
      return value;
    }
  }

  /**
   * Analyze contents to check if it matches a block.
   * 
   * @param wiki Wiki.
   * @param contents Contents.
   * @param index Block start index.
   * @param comments Comments in the page.
   * @param tags Tags in the page.
   * @return Block details it there's a block.
   */
  public static PageElementTemplate analyzeBlock(
      EnumWikipedia wiki,
      String contents, int index,
      List<PageElementComment> comments,
      List<PageElementTag> tags) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Look for '{{'
    int beginIndex = index;
    int tmpIndex = beginIndex;
    if ((tmpIndex >= contents.length() - 1) ||
        (contents.charAt(tmpIndex) != '{') ||
        (contents.charAt(tmpIndex + 1) != '{')) {
      return null;
    }
    tmpIndex += 2;

    boolean moved = false;
    do {
      moved = false;

      // Possible whitespace characters
      while ((tmpIndex < contents.length()) &&
             ((contents.charAt(tmpIndex) == ' ') ||
              (contents.charAt(tmpIndex) == '\n'))) {
        tmpIndex++;
        moved = true;
      }
  
      // Possible comment
      if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '<')) {
        PageElementComment comment = null;
        if (comments != null) {
          for (PageElementComment tmpComment : comments) {
            if (tmpComment.getBeginIndex() == tmpIndex) {
              comment = tmpComment;
            }
          }
        }
        if (comment == null) {
          return null;
        }
        tmpIndex = comment.getEndIndex();
        moved = true;
      }
    } while (moved);

    int startTemplateName = tmpIndex;

    // Retrieve template name
    while (tmpIndex < contents.length()) {
      char currentChar = contents.charAt(tmpIndex);
      if (templateNameUnauthorizedCharacters.indexOf(currentChar) >= 0) {
        break;
      }
      tmpIndex++;
    }
    if (tmpIndex >= contents.length()) {
      return null;
    }
    String templateName = contents.substring(startTemplateName, tmpIndex).trim();
    if (templateName.length() == 0) {
      return null;
    }

    // Check that it's not a function
    String firstPart = templateName;
    int colonIndex = templateName.indexOf(':');
    if (colonIndex > 0) {
      firstPart = templateName.substring(0, colonIndex);
    }
    MagicWord magicWord = wiki.getWikiConfiguration().getFunctionMagicWord(firstPart, colonIndex > 0);
    if (magicWord != null) {
      return null;
    }

    do {
      moved = false;

      // Possible comment
      if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '<')) {
        PageElementComment comment = null;
        if (comments != null) {
          for (PageElementComment tmpComment : comments) {
            if (tmpComment.getBeginIndex() == tmpIndex) {
              comment = tmpComment;
            }
          }
        }
        if (comment == null) {
          return null;
        }
        tmpIndex = comment.getEndIndex();
        moved = true;
      }
  
      // Possible whitespace characters
      while ((tmpIndex < contents.length()) &&
             ((contents.charAt(tmpIndex) == ' ') ||
              (contents.charAt(tmpIndex) == '\n'))) {
        tmpIndex++;
        moved = true;
      }
    } while (moved);

    // Check if it's a template without parameters
    if (contents.startsWith("}}", tmpIndex)) {
      return new PageElementTemplate(
          templateName,
          beginIndex, tmpIndex + 2, null);
    }

    // Check if it's a template
    if (contents.charAt(tmpIndex) != '|') {
      return null;
    }

    // Analyze parameters
    tmpIndex++;
    List<Parameter> parameters = new ArrayList<Parameter>();
    int endIndex = analyzeTemplateParameters(
        wiki, contents, beginIndex, tmpIndex - 1, tmpIndex, parameters,
        comments, tags);
    if (endIndex < 0) {
      return null;
    }
    return new PageElementTemplate(
        templateName,
        beginIndex, endIndex, parameters);
  }

  /**
   * Analyze the parameters of template.
   * 
   * @param wiki Wiki.
   * @param contents Contents of the page.
   * @param templateBeginIndex Start index of the template in the page.
   * @param pipeIndex Index of the previous pipe.
   * @param parametersBeginIndex Start index of the parameters in the page.
   * @param parameters Parameters.
   * @param comments Comments in the page.
   * @param tags Tags in the page.
   * @return Position of the end of the template, or -1 if no template was found.
   */
  private static int analyzeTemplateParameters(
      EnumWikipedia wiki, String contents,
      int templateBeginIndex, int pipeIndex, int parametersBeginIndex,
      List<Parameter> parameters,
      List<PageElementComment> comments,
      List<PageElementTag> tags) {
    if (contents == null) {
      return -1;
    }

    // Compute max length
    int maxLength = contents.length();
    if (tags != null) {
      for (PageElementTag tag : tags) {
        if ((tag.getCompleteBeginIndex() < parametersBeginIndex) &&
            (tag.getCompleteEndIndex() > parametersBeginIndex) &&
            (PageElementTag.TAG_WIKI_REF.equals(tag.getName()))) {
          maxLength = Math.min(maxLength, tag.getCompleteEndIndex());
        }
      }
    }

    int tmpIndex = parametersBeginIndex;
    int depth2CurlyBrackets = 0;
    int depth3CurlyBrackets = 0;
    int depth2SquareBrackets = 0;
    int parameterBeginIndex = parametersBeginIndex;
    int equalIndex = -1;
    boolean equalDone = false;
    while (tmpIndex < maxLength) {
      if (contents.startsWith("{{{", tmpIndex)) {
        // Possible start of a parameter
        tmpIndex += 3;
        depth3CurlyBrackets++;
      } else if (contents.startsWith("{{", tmpIndex)) {
        // Possible start of nested template
        tmpIndex += 2;
        depth2CurlyBrackets++;
      } else if (contents.startsWith("}}", tmpIndex)) {
        if (contents.startsWith("}}}", tmpIndex) &&
            (depth3CurlyBrackets > 0)) {
          // Possible end of parameter
          tmpIndex += 3;
          depth3CurlyBrackets--;
        } else {
          // Possible end of template
          tmpIndex += 2;
          if (depth2CurlyBrackets > 0) {
            depth2CurlyBrackets--;
          } else {
            addParameter(
                parameters, pipeIndex,
                contents.substring(parameterBeginIndex, tmpIndex - 2),
                equalIndex - parameterBeginIndex,
                parameterBeginIndex,
                comments);
            return tmpIndex;
          }
        }
      } else if (contents.startsWith("[[", tmpIndex)) {
        // Possible start of nested internal links
        tmpIndex += 2;
        depth2SquareBrackets++;
      } else if (contents.startsWith("]]", tmpIndex)) {
        // Possible end of nested internal link
        tmpIndex += 2;
        if (depth2SquareBrackets > 0) {
          depth2SquareBrackets--;
        } else {
          return -1;
        }
      } else if (contents.startsWith("<", tmpIndex)) {
        // Possible start of a tag
        PageElementTag tag = null;
        if (tags != null) {
          for (PageElementTag tmpTag : tags) {
            if (tmpTag.getBeginIndex() == tmpIndex) {
              tag = tmpTag;
            }
          }
        }
        if (tag != null) {
          String tagName = tag.getName();
          if (PageElementTag.TAG_WIKI_NOWIKI.equals(tagName) ||
              PageElementTag.TAG_WIKI_MATH.equals(tagName) ||
              PageElementTag.TAG_WIKI_MATH_CHEM.equals(tagName) ||
              PageElementTag.TAG_WIKI_REF.equals(tagName) ||
              PageElementTag.TAG_WIKI_SCORE.equals(tagName)) {
            tmpIndex = tag.getCompleteEndIndex();
          } else {
            tmpIndex = tag.getEndIndex();
          }
        } else {
          // Possible start of a comment
          PageElementComment comment = null;
          if (comments != null) {
            for (PageElementComment tmpComment : comments) {
              if (tmpComment.getBeginIndex() == tmpIndex) {
                comment = tmpComment;
              }
            }
          }
          if (comment != null) {
            tmpIndex = comment.getEndIndex();
          } else {
            tmpIndex++;
          }
        }
      } else {
        if ((depth2CurlyBrackets <= 0) &&
            (depth3CurlyBrackets <= 0) &&
            (depth2SquareBrackets <= 0)) {
          char currentChar = contents.charAt(tmpIndex);
          if (currentChar == '|') {
            // Separation with next parameter
            depth2CurlyBrackets = 0;
            depth3CurlyBrackets = 0;
            depth2SquareBrackets = 0;
            addParameter(
                parameters, pipeIndex,
                contents.substring(parameterBeginIndex, tmpIndex),
                equalIndex - parameterBeginIndex,
                parameterBeginIndex,
                comments);
            pipeIndex = tmpIndex;
            tmpIndex++;
            parameterBeginIndex = tmpIndex;
            equalIndex = -1;
            equalDone = false;
          } else if ((currentChar == '=') &&
                     (equalIndex < 0) &&
                     (equalDone == false)) {
            /*int i = parameterBeginIndex;
            while ((i < tmpIndex) &&
                   ((contents.charAt(i) == ' ') || (contents.charAt(i) == '\n'))) {
              i++;
            }
            boolean nameFound = false;
            while ((i < tmpIndex) && (contents.charAt(i) != '=')) {
              if ((contents.charAt(i) != ' ') && (contents.charAt(i) != '\n')) {
                nameFound = true;
              }
              i++;
            }
            if (nameFound) {
              equalIndex = tmpIndex;
            }*/
            equalIndex = tmpIndex;
            equalDone = true;
            tmpIndex++;
          } else {
            tmpIndex++;
          }
        } else {
          tmpIndex++;
        }
      }
    }
    return -1;
  }

  /**
   * @param parameters List of parameters.
   * @param pipeIndex Index of "|".
   * @param parameter New parameter (name=value or value).
   * @param equalIndex Index of "=" in the parameter or < 0 if doesn't exist.
   * @param offset Offset of parameter start index in page contents.
   * @param comments Comments in the page.
   */
  private static void addParameter(
      List<Parameter> parameters,
      int pipeIndex, String parameter,
      int equalIndex, int offset,
      List<PageElementComment> comments) {

    // Check if the "=" is meaningful
    if (equalIndex >= 0) {
      boolean meaningful = false;
      for (int index = 0; index < equalIndex; index++) {
        if ((index < parameter.length()) && (!Character.isWhitespace(parameter.charAt(index)))) {
          meaningful = true;
        }
      }
      if (!meaningful) {
        equalIndex = -1;
      }
    }

    // Analyze parameter
    if (equalIndex < 0) {
      int spaces = 0;
      while ((spaces < parameter.length()) && (Character.isWhitespace(parameter.charAt(spaces)))) {
        spaces++;
      }
      int paramNum = 1;
      for (Parameter param : parameters) {
        String paramName = param.getName();
        if ((paramName == null) || ("".equals(paramName))) {
          paramNum++;
        }
      }
      String strippedValue = PageElementComment.stripComments(comments, parameter, offset);
      parameters.add(new Parameter(
          pipeIndex, "", Integer.toString(paramNum), offset + spaces, parameter, strippedValue, offset + spaces));
    } else {
      int spacesName = 0;
      while ((spacesName < equalIndex) && (Character.isWhitespace(parameter.charAt(spacesName)))) {
        spacesName++;
      }
      int spacesValue = equalIndex + 1;
      while ((spacesValue < parameter.length()) && (Character.isWhitespace(parameter.charAt(spacesValue)))) {
        spacesValue++;
      }
      String value = parameter.substring(equalIndex + 1);
      String strippedValue = PageElementComment.stripComments(comments, value, offset + equalIndex + 1);
      parameters.add(new Parameter(
          pipeIndex,
          parameter.substring(0, equalIndex), null, offset + spacesName,
          parameter.substring(equalIndex + 1), strippedValue, offset + spacesValue));
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
   * @param name Parameter name.
   * @return Parameter index.
   */
  public int getParameterIndex(String name) {
    if (parameters == null) {
      return -1;
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
        return index;
      }
      index++;
    }
    return -1;
  }

  /**
   * Retrieve parameter.
   * 
   * @param index Parameter index.
   * @return Parameter.
   */
  public Parameter getParameter(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index);
    }
    return null;
  }

  /**
   * Retrieve parameter.
   * 
   * @param index Text index in the page.
   * @return Parameter.
   */
  public Parameter getParameterAtIndex(int index) {
    if ((index < getBeginIndex()) || (index >= getEndIndex())) {
      return null;
    }
    if (parameters == null) {
      return null;
    }
    for (int paramNum = 0; paramNum < parameters.size() - 1; paramNum++) {
      if (parameters.get(paramNum + 1).getPipeIndex() >= index) {
        return parameters.get(paramNum);
      }
    }
    if (!parameters.isEmpty()) {
      return parameters.get(parameters.size() - 1);
    }
    return null;
  }

  /**
   * Retrieve pipe index.
   * 
   * @param index Parameter index.
   * @return Pipe index.
   */
  public int getParameterPipeIndex(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).pipeIndex;
    }
    return 0;
  }

  /**
   * Retrieve parameter name.
   * 
   * @param index Parameter index.
   * @return Parameter name.
   */
  public String getParameterName(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).name;
    }
    return null;
  }

  /**
   * Retrieve parameter name start index.
   * 
   * @param index Parameter index.
   * @return Parameter name start index.
   */
  public int getParameterNameStartIndex(int index) {
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
   * Retrieve parameter value start index.
   * 
   * @param index Parameter index.
   * @return Parameter value start index.
   */
  public int getParameterValueStartIndex(int index) {
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
    int index = getParameterIndex(name);
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).value;
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
    /*System.err.println("Template: " + this.templateName);
    if (parameters != null) {
      for (Parameter parameter : this.parameters) {
        System.err.println(" Parameter: " + parameter.name);
        System.err.println(" Value: " + parameter.value);
      }
    }*/
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
  
        // Managing unnamed
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

  /**
   * Create a template.
   * 
   * @param name Template name.
   * @return Template.
   */
  public static String createTemplate(String name) {
    StringBuilder sb = new StringBuilder();
    sb.append("{{");
    sb.append(name);
    sb.append("}}");
    return sb.toString();
  }
}
