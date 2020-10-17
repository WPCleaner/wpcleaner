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
import org.wikipediacleaner.api.data.contents.ContainerComment;
import org.wikipediacleaner.api.data.contents.ContentsComment;


/**
 * Class containing information about a complete function ({{<i>function</i>:...}}). 
 */
public class PageElementFunction extends PageElement {

  private final MagicWord magicWord;
  private final String functionName;
  private final String functionNameNotTrimmed;
  private final List<Parameter> parameters;

  private final static String functionNameUnauthorizedCharacters = "{}[]|<>:";

  /**
   * Class containing information about a function parameter.
   */
  private static class Parameter {
    final int separatorIndex;
    final String fullText;
    final String name;
    final int nameStartIndex;
    final String valueNotTrimmed;
    final String value;
    final int valueStartIndex;
    final boolean correct;

    /**
     * @param separatorIndex Index of the separator in page contents.
     * @param fullText Full text of the parameter.
     * @param name Parameter name.
     * @param nameStartIndex Index of parameter name in page contents.
     * @param value Parameter value.
     * @param valueStartIndex Index of parameter value in page contents.
     * @param correct True if parameter seems correct.
     */
    public Parameter(
        int separatorIndex, String fullText,
        String name, int nameStartIndex,
        String value, int valueStartIndex,
        boolean correct) {
      this.separatorIndex = separatorIndex;
      this.fullText = fullText;
      this.name = (name != null) ? name.trim() : null;
      this.nameStartIndex = nameStartIndex;
      this.valueNotTrimmed = value;
      this.value = (value != null) ? value.trim() : null;
      this.valueStartIndex = valueStartIndex;
      this.correct = correct;
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
   * Analyze contents to check if it matches a block.
   * 
   * @param wiki Wiki.
   * @param contents Contents.
   * @param index Block start index.
   * @param comments Comments in the page.
   * @param tags Tags in the page.
   * @return Block details it there's a block.
   */
  public static PageElementFunction analyzeBlock(
      EnumWikipedia wiki,
      String contents, int index,
      ContainerComment comments,
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
        ContentsComment comment = comments.getBeginsAt(tmpIndex);
        if (comment == null) {
          return null;
        }
        tmpIndex = comment.getEndIndex();
        moved = true;
      }
    } while (moved);

    int startFunctionName = tmpIndex;

    // Retrieve function name
    while (tmpIndex < contents.length()) {
      char currentChar = contents.charAt(tmpIndex);
      if (functionNameUnauthorizedCharacters.indexOf(currentChar) >= 0) {
        break;
      }
      tmpIndex++;
    }
    if (tmpIndex >= contents.length()) {
      return null;
    }
    String functionName = contents.substring(startFunctionName, tmpIndex).trim();
    if (functionName.length() == 0) {
      return null;
    }

    // Check that it's a function
    MagicWord magicWord = wiki.getWikiConfiguration().getFunctionMagicWord(
        functionName, contents.charAt(tmpIndex) == ':');
    if (magicWord == null) {
      return null;
    }

    do {
      moved = false;

      // Possible comment
      if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '<')) {
        ContentsComment comment = comments.getBeginsAt(tmpIndex);
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

    // Check if it's a function without parameters
    if (contents.startsWith("}}", tmpIndex)) {
      return new PageElementFunction(
          magicWord, functionName,
          beginIndex, tmpIndex + 2, null);
    }

    // Check if it's a function
    if (contents.charAt(tmpIndex) != ':') {
      return null;
    }

    // Analyze parameters
    tmpIndex++;
    List<Parameter> parameters = new ArrayList<Parameter>();
    int endIndex = analyzeFunctionParameters(
        wiki, contents, beginIndex, tmpIndex - 1, tmpIndex, parameters,
        comments, tags);
    if (endIndex < 0) {
      return null;
    }
    return new PageElementFunction(
        magicWord, functionName,
        beginIndex, endIndex, parameters);
  }

  /**
   * Analyze the parameters of function.
   * 
   * @param wiki Wiki.
   * @param contents Contents of the page.
   * @param functionBeginIndex Start index of the function in the page.
   * @param separatorIndex Index of the previous separator.
   * @param parametersBeginIndex Start index of the parameters in the page.
   * @param parameters Parameters.
   * @param comments Comments in the page.
   * @param tags Tags in the page.
   * @return Position of the end of the function, or -1 if no function was found.
   */
  private static int analyzeFunctionParameters(
      EnumWikipedia wiki, String contents,
      int functionBeginIndex, int separatorIndex, int parametersBeginIndex,
      List<Parameter> parameters,
      ContainerComment comments,
      List<PageElementTag> tags) {
    if (contents == null) {
      return -1;
    }
    int tmpIndex = parametersBeginIndex;
    int maxLength = contents.length();
    int depth2CurlyBrackets = 0;
    int depth3CurlyBrackets = 0;
    int depth2SquareBrackets = 0;
    int depthTagNoWiki = 0;
    int depthTagRef = 0;
    int parameterBeginIndex = parametersBeginIndex;
    int equalIndex = -1;
    boolean parameterCorrect = true;
    while (tmpIndex < maxLength) {
      if (contents.startsWith("{{{", tmpIndex)) {
        // Possible start of a parameter
        tmpIndex += 3;
        if (depthTagNoWiki == 0) {
          depth3CurlyBrackets++;
        }
      } else if (contents.startsWith("{{", tmpIndex)) {
        // Possible start of nested template
        tmpIndex += 2;
        if (depthTagNoWiki == 0) {
          depth2CurlyBrackets++;
        }
      } else if (contents.startsWith("}}", tmpIndex)) {
        if (contents.startsWith("}}}", tmpIndex) &&
            (depth3CurlyBrackets > 0)) {
          // Possible end of parameter
          tmpIndex += 3;
          if (depthTagNoWiki == 0) {
            depth3CurlyBrackets--;
          }
        } else {
          // Possible end of function
          tmpIndex += 2;
          if (depthTagNoWiki == 0) {
            if (depth2CurlyBrackets > 0) {
              depth2CurlyBrackets--;
            } else {
              addParameter(
                  parameters, separatorIndex,
                  contents.substring(parameterBeginIndex, tmpIndex - 2),
                  equalIndex - parameterBeginIndex,
                  parameterBeginIndex,
                  parameterCorrect);
              return tmpIndex;
            }
          }
        }
      } else if (contents.startsWith("[[", tmpIndex)) {
        // Possible start of nested internal links
        tmpIndex += 2;
        if (depthTagNoWiki == 0) {
          depth2SquareBrackets++;
        }
      } else if (contents.startsWith("]]", tmpIndex)) {
        // Possible end of nested internal link
        tmpIndex += 2;
        if (depthTagNoWiki == 0) {
          if (depth2SquareBrackets > 0) {
            depth2SquareBrackets--;
          } else {
            parameterCorrect = false;
          }
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
          int count = 0;
          if (tag.isFullTag()) {
            count = 0;
          } else if (tag.isEndTag()) {
            count = -1;
          } else {
            count = 1;
          }
          if (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getName())) {
            depthTagNoWiki += count;
            if (depthTagNoWiki < 0) {
              depthTagNoWiki = 0;
            }
          } else if (PageElementTag.TAG_WIKI_REF.equals(tag.getName())) {
            if (depthTagNoWiki == 0) {
              depthTagRef += count;
              if (depthTagRef < 0) {
                depthTagRef = 0;
              }
            }
          }
          tmpIndex = tag.getEndIndex();
        } else {
          // Possible start of a comment
          ContentsComment comment = comments.getBeginsAt(tmpIndex);
          if (comment != null) {
            tmpIndex = comment.getEndIndex();
          } else {
            tmpIndex++;
          }
        }
      } else {
        if ((depth2CurlyBrackets <= 0) &&
            (depth2SquareBrackets <= 0) &&
            (depthTagNoWiki <= 0) &&
            (depthTagRef <= 0)) {
          char currentChar = contents.charAt(tmpIndex);
          if (currentChar == '|') {
            // Separation with next parameter
            depth2CurlyBrackets = 0;
            depth2SquareBrackets = 0;
            addParameter(
                parameters, separatorIndex,
                contents.substring(parameterBeginIndex, tmpIndex),
                equalIndex - parameterBeginIndex,
                parameterBeginIndex,
                parameterCorrect);
            separatorIndex = tmpIndex;
            tmpIndex++;
            parameterBeginIndex = tmpIndex;
            parameterCorrect = true;
            equalIndex = -1;
          } else if ((currentChar == '=') && (equalIndex < 0)) {
            equalIndex = tmpIndex;
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
   * @param separatorIndex Index of separator.
   * @param parameter New parameter (name=value or value).
   * @param equalIndex Index of "=" in the parameter or < 0 if doesn't exist.
   * @param offset Offset of parameter start index in page contents.
   * @param correct True if parameter seems correct.
   */
  private static void addParameter(
      List<Parameter> parameters,
      int separatorIndex, String parameter,
      int equalIndex, int offset,
      boolean correct) {
    if (equalIndex < 0) {
      int spaces = 0;
      while ((spaces < parameter.length()) && (Character.isWhitespace(parameter.charAt(spaces)))) {
        spaces++;
      }
      parameters.add(new Parameter(
          separatorIndex, parameter,
          "", offset + spaces, parameter, offset + spaces, correct));
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
          separatorIndex, parameter,
          parameter.substring(0, equalIndex), offset + spacesName,
          parameter.substring(equalIndex + 1), offset + spacesValue,
          correct));
    }
  }

  /**
   * @return Magic word.
   */
  public MagicWord getMagicWord() {
    return magicWord;
  }

  /**
   * @return Function name.
   */
  public String getFunctionName() {
    return functionName;
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
   * Retrieve separator offset.
   * 
   * @param index Parameter index.
   * @return Separator offset.
   */
  public int getParameterSeparatorOffset(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).separatorIndex;
    }
    return 0;
  }

  /**
   * Retrieve parameter full text.
   * 
   * @param index Parameter index.
   * @return Parameter full text.
   */
  public String getParameterFullText(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).fullText;
    }
    return null;
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
   * Retrieve parameter value not trimmed.
   * 
   * @param index Parameter index.
   * @return Parameter value not trimmed.
   */
  public String getParameterValueNotTrimmed(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).valueNotTrimmed;
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
  
  /**
   * Retrieve if parameter is correct
   * 
   * @param index Parameter index.
   * @return True if the parameter seems correct.
   */
  public boolean isParameterCorrect(int index) {
    if ((index >= 0) && (index < parameters.size())) {
      return parameters.get(index).correct;
    }
    return false;
  }

  private PageElementFunction(
      MagicWord magicWord, String functionName,
      int beginIndex, int endIndex,
      List<Parameter> parameters) {
    super(beginIndex, endIndex);
    this.magicWord = magicWord;
    this.functionNameNotTrimmed = functionName;
    this.functionName = (functionName != null) ? functionName.trim() : null;
    this.parameters = parameters;
  }

  private void addPartBeforeParameters(StringBuilder sb) {
    sb.append("{{");
    sb.append(functionNameNotTrimmed);
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
   * @param name Function name.
   * @param value Value of the function.
   * @return Textual representation of the function.
   */
  public static String createFunction(String name, String value) {
    StringBuilder sb = new StringBuilder();
    sb.append("{{");
    sb.append(name);
    if (value != null) {
      if ((name.length() > 0) && (name.charAt(name.length() - 1) != ':')) {
        sb.append(':');
      }
      sb.append(value.trim());
    }
    sb.append("}}");
    return sb.toString();
  }
}
